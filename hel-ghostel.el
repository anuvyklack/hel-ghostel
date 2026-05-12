;;; hel-ghostel.el --- Hel integration for ghostel -*- lexical-binding: t; -*-

;; Copyright (c) 2026 Yuriy Artemyev

;; Author: Yuriy Artemyev <anuvyklack@gmail.com>
;; URL: https://github.com/anuvyklack/hel-ghostel
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (hel "0.10.0") (ghostel "0.8.0"))
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; Provides Hel integration for the ghostel terminal emulator.
;; Synchronizes the terminal cursor with Emacs point during Hel state
;; transitions so that normal-state navigation works correctly.
;;
;; Unlike `evil-ghostel', this extension uses separate commands bound in
;; a minor-mode keymap rather than advices on Hel commands.  Only the two
;; Ghostel-internal functions `ghostel--redraw' and `ghostel--set-cursor-style'
;; are advised, because Ghostel provides no hooks for them.
;;
;; Enable by adding to your init:
;;
;;   (use-package hel-ghostel
;;     :after (ghostel hel)
;;     :hook (ghostel-mode . hel-ghostel-mode))

;;; Code:

(require 'hel)
(require 'ghostel)

(declare-function ghostel--mode-enabled "ghostel-module")

(defvar hel-ghostel-mode)

;;; Customization

(defgroup hel-ghostel nil
  "Hel integration for ghostel."
  :group 'ghostel
  :prefix "hel-ghostel-")

(defcustom hel-ghostel-initial-state 'insert
  "Initial Hel state for new `ghostel-mode' buffers.
Setting this option via `customize-set-variable', `setopt', or the
Customize UI calls `hel-set-initial-state' so the change takes effect
immediately."
  :type '(choice (const :tag "Insert" insert)
                 (const :tag "Normal" normal)
                 (symbol :tag "Other state"))
  :group 'hel-ghostel
  :set (lambda (sym val)
         (set-default-toplevel-value sym val)
         (hel-set-initial-state 'ghostel-mode val)))

(defcustom hel-ghostel-escape 'auto
  "Where insert-state ESC is routed in ghostel buffers.

`auto'      — when the inner app is in alt-screen mode (DECSET 1049,
              used by vim, less, htop, nvim, etc.) ESC is sent to the
              terminal; otherwise Hel's binding runs and switches to
              normal state.
`terminal'  — always send ESC to the terminal.
`hel'       — always run Hel's binding (ESC enters normal state).

Sets the initial value of the buffer-local state.  Use
\\[hel-ghostel-toggle-send-escape] to change it for the current buffer."
  :type '(choice (const :tag "Auto (alt-screen heuristic)" auto)
                 (const :tag "Always to terminal" terminal)
                 (const :tag "Always to Hel" hel))
  :group 'hel-ghostel)

;; Apply the current value at load.  Covers the case where the user set
;; the variable with plain `setq' before loading the package — in that
;; path `defcustom' preserves the value without invoking `:set'.
(hel-set-initial-state 'ghostel-mode hel-ghostel-initial-state)

;;; Guard predicates

(defun hel-ghostel--active-p ()
  "Return non-nil when hel-ghostel editing should intercept.
Requires: minor mode on, terminal live, not in alt-screen, semi-char mode."
  (and hel-ghostel-mode
       ghostel--term
       (not (ghostel--mode-enabled ghostel--term 1049))
       (eq ghostel--input-mode 'semi-char)))

(defun hel-ghostel--line-mode-active-p ()
  "Return non-nil when line mode editing is in effect.
In line mode, shell input is buffered as plain text in the region
[`ghostel--line-input-start', `ghostel--line-input-end'].  Hel's
default buffer-editing operators work correctly there."
  (and hel-ghostel-mode
       (eq ghostel--input-mode 'line)
       (markerp ghostel--line-input-start)
       (markerp ghostel--line-input-end)))

;;; Cursor synchronization

(defun hel-ghostel--scrollback ()
  "Return the number of scrollback lines above the viewport, or 0."
  (if ghostel--term-rows
      (max 0 (- (count-lines (point-min) (point-max))
                ghostel--term-rows))
    0))

(defun hel-ghostel--reset-cursor-point ()
  "Move Emacs point to the terminal cursor position.
`ghostel--cursor-pos' holds the viewport-relative (COL . ROW), so
the row must be offset by the scrollback line count."
  (when (and ghostel--term ghostel--term-rows)
    (let ((pos ghostel--cursor-pos))
      (when pos
        (let ((scrollback (hel-ghostel--scrollback)))
          (goto-char (point-min))
          (forward-line (+ scrollback (cdr pos)))
          (move-to-column (car pos)))))))

(defun hel-ghostel--cursor-buffer-line ()
  "Return the 0-indexed buffer line of the terminal cursor, or nil."
  (when (and ghostel--term ghostel--term-rows)
    (let ((pos ghostel--cursor-pos))
      (when pos
        (+ (hel-ghostel--scrollback) (cdr pos))))))

(defun hel-ghostel--point-viewport-row ()
  "Return the viewport row of point, 0-indexed, or nil.
Subtracts the scrollback line count from the buffer line so the
result is comparable to `ghostel--cursor-pos''s row."
  (when ghostel--term-rows
    (- (line-number-at-pos (point) t) 1 (hel-ghostel--scrollback))))

(defvar-local hel-ghostel--last-cursor-line nil
  "Buffer line where the previous redraw placed the terminal cursor.
Used by `hel-ghostel--around-redraw' to detect prompt-line scrolling.")

(defvar-local hel-ghostel--shadow-cursor nil
  "Pending terminal cursor (COL . VIEWPORT-ROW), or nil to read live state.
Within a single command we may emit several key sequences before any
are echoed by the PTY.  `ghostel--cursor-pos' lags queued keys, so a
second sync reads the shadow instead of the stale live value.
Reset by `hel-ghostel--around-redraw' after each render.")

(defun hel-ghostel--shadow-or-live ()
  "Return best-known terminal cursor (COL . VIEWPORT-ROW), or nil."
  (or hel-ghostel--shadow-cursor ghostel--cursor-pos))

(defun hel-ghostel--invalidate-shadow ()
  "Clear `hel-ghostel--shadow-cursor'.
Call after operations whose cursor effect cannot be modelled."
  (setq hel-ghostel--shadow-cursor nil))

(defun hel-ghostel--cursor-to-point ()
  "Move the terminal cursor to Emacs point by sending arrow keys.
Reads the shadow cursor in preference to the live libghostty cursor,
and updates the shadow to point's position so a follow-up call within
the same operation sees the post-keys baseline."
  (when ghostel--term
    (let* ((tpos (hel-ghostel--shadow-or-live))
           (tcol (car tpos))
           (trow (cdr tpos))
           (ecol (current-column))
           (erow (or (hel-ghostel--point-viewport-row) 0))
           (dy (- erow trow))
           (dx (- ecol tcol)))
      (cond ((> dy 0) (dotimes (_ dy) (ghostel--send-encoded "down" "")))
            ((< dy 0) (dotimes (_ (abs dy)) (ghostel--send-encoded "up" ""))))
      (cond ((> dx 0) (dotimes (_ dx) (ghostel--send-encoded "right" "")))
            ((< dx 0) (dotimes (_ (abs dx)) (ghostel--send-encoded "left" ""))))
      (setq hel-ghostel--shadow-cursor (cons ecol erow)))))

;;; Redraw: preserve point across the native call

(defvar-local hel-ghostel--sync-point-on-next-redraw nil
  "When non-nil, the next `ghostel--redraw' moves point to the terminal cursor.
Set by operations that send PTY commands which reposition the terminal
cursor, where point would otherwise be left at a stale position.")

(defun hel-ghostel--around-redraw (orig-fn term &optional full)
  "Preserve point across the native redraw call.
Native `ghostel--redraw' rewrites the viewport region and moves point.
In non-insert states point is restored after the redraw unless the
user was parked on the prompt line and the cursor scrolled to a new
line — in that case the renderer's placement wins so the user follows
the new prompt position.

The standard Emacs mark is preserved by the native module;
`hel--extend-selection' is a boolean flag unaffected by buffer
positions — neither needs saving here.

ORIG-FN is the advised `ghostel--redraw' called with TERM and FULL.
Skipped in alt-screen mode (1049) where apps own the screen."
  (if (and hel-ghostel-mode
           (not (ghostel--mode-enabled term 1049)))
      (let* ((sync-flag hel-ghostel--sync-point-on-next-redraw)
             (preserve-point (and (not sync-flag)
                                  (not hel-insert-state)))
             (saved-point (and preserve-point (point)))
             (pre-line (and preserve-point
                            (- (line-number-at-pos (point) t) 1)))
             (was-on-prompt-line (and pre-line
                                      hel-ghostel--last-cursor-line
                                      (= pre-line
                                         hel-ghostel--last-cursor-line))))
        (funcall orig-fn term full)
        (when sync-flag
          (setq hel-ghostel--sync-point-on-next-redraw nil)
          (hel-ghostel--reset-cursor-point))
        (let* ((post-cursor-line (hel-ghostel--cursor-buffer-line))
               (prompt-moved (and was-on-prompt-line
                                  post-cursor-line
                                  (not (= post-cursor-line pre-line)))))
          (when (and preserve-point (not prompt-moved))
            (goto-char (min saved-point (point-max))))
          (setq hel-ghostel--last-cursor-line post-cursor-line))
        (hel-ghostel--invalidate-shadow))
    (funcall orig-fn term full)))

;;; Cursor style: let Hel control cursor shape

(defun hel-ghostel--override-cursor-style (orig-fn style visible)
  "Let Hel control cursor shape instead of the terminal.
ORIG-FN is the advised setter called with STYLE and VISIBLE.
In alt-screen mode, defer to the terminal's cursor style."
  (if (and hel-ghostel-mode
           ghostel--term
           (not (ghostel--mode-enabled ghostel--term 1049)))
      (hel-update-cursor)
    (funcall orig-fn style visible)))

;;; Insert state entry hook

(defvar-local hel-ghostel--sync-inhibit nil
  "When non-nil, skip arrow-key sync in the insert-state-entry hook.
Set by the I/A commands which send Ctrl-a/Ctrl-e directly.")

(defun hel-ghostel--insert-state-entry ()
  "Sync terminal cursor to Emacs point when entering insert state.
Skipped when `hel-ghostel--sync-inhibit' is set (by I/A commands
which already sent Ctrl-a/Ctrl-e).  Also skipped outside semi-char.
When point is on a different row from the terminal cursor, snap back
to the terminal cursor to avoid sending arrows the shell interprets
as history navigation."
  (when (derived-mode-p 'ghostel-mode)
    (if hel-ghostel--sync-inhibit
        (setq hel-ghostel--sync-inhibit nil)
      (when (hel-ghostel--active-p)
        (let* ((tpos ghostel--cursor-pos)
               (trow (cdr tpos))
               (erow (or (hel-ghostel--point-viewport-row) 0)))
          (if (= erow trow)
              (hel-ghostel--cursor-to-point)
            (hel-ghostel--reset-cursor-point)))))))

;;; Editing primitives

(defun hel-ghostel--meaningful-length (text)
  "Length of TEXT, stripping per-line trailing whitespace in multi-line ranges.
Heuristic for TUIs that draw a fixed-width input box (e.g. prompt_toolkit)
that fill each input row to the terminal width — those trailing spaces appear
in the buffer but are not part of the TUI's input model.

Only applied when TEXT spans more than one buffer line; single-line trailing
whitespace is treated as real content."
  (if (string-match-p "\n" text)
      (length (replace-regexp-in-string "[ \t]+\\(\n\\|\\'\\)" "\\1" text))
    (length text)))

(defun hel-ghostel--delete-region (beg end)
  "Delete text between BEG and END via the terminal PTY.
Moves terminal cursor to END, then sends one backspace per
meaningful character (see `hel-ghostel--meaningful-length')."
  (let ((count (hel-ghostel--meaningful-length
                (buffer-substring-no-properties beg end))))
    (when (> count 0)
      (goto-char end)
      (hel-ghostel--cursor-to-point)
      (dotimes (_ count)
        (ghostel--send-encoded "backspace" ""))
      (goto-char beg)
      (hel-ghostel--invalidate-shadow))))

(defun hel-ghostel--point-on-cursor-row-p ()
  "Non-nil when point is on the same viewport row as the terminal cursor."
  (when ghostel--term
    (let ((trow (cdr ghostel--cursor-pos))
          (prow (hel-ghostel--point-viewport-row)))
      (and trow prow (= prow trow)))))

(defun hel-ghostel--clear-input-line ()
  "Clear the active input line via Ctrl-e Ctrl-u.
Readline/zle/prompt_toolkit all bind this to end-of-line then
kill-from-start, clearing the input without needing prompt geometry.
Sets `hel-ghostel--sync-point-on-next-redraw' so the redraw triggered
by the shell's echo lands point at the new cursor position."
  (ghostel--send-encoded "e" "ctrl")
  (ghostel--send-encoded "u" "ctrl")
  (hel-ghostel--invalidate-shadow)
  (setq hel-ghostel--sync-point-on-next-redraw t))

;;; Normal-state commands

(hel-define-command hel-ghostel-insert-line ()
  "Switch to insert state at the beginning of the current line.
In semi-char mode, syncs the terminal cursor to point's row first,
then sends Ctrl-a so readline/zle moves to the start of that input
line.  In line mode, jumps to `ghostel--line-input-start' directly.
Outside ghostel, falls through to `hel-insert-line'."
  :multiple-cursors nil
  (interactive "*")
  (cond
   ((hel-ghostel--active-p)
    (hel-ghostel--cursor-to-point)
    (ghostel--send-encoded "a" "ctrl")
    (hel-ghostel--invalidate-shadow)
    (setq hel-ghostel--sync-inhibit t)
    (hel-insert-state 1))
   ((hel-ghostel--line-mode-active-p)
    (goto-char (marker-position ghostel--line-input-start))
    (setq hel-ghostel--sync-inhibit t)
    (hel-insert-state 1))
   (t (call-interactively #'hel-insert-line))))

(hel-define-command hel-ghostel-append-line ()
  "Switch to insert state at the end of the current line.
Symmetric to `hel-ghostel-insert-line': sends Ctrl-e in semi-char,
jumps to `ghostel--line-input-end' in line mode."
  :multiple-cursors nil
  (interactive "*")
  (cond
   ((hel-ghostel--active-p)
    (hel-ghostel--cursor-to-point)
    (ghostel--send-encoded "e" "ctrl")
    (hel-ghostel--invalidate-shadow)
    (setq hel-ghostel--sync-inhibit t)
    (hel-insert-state 1))
   ((hel-ghostel--line-mode-active-p)
    (goto-char (marker-position ghostel--line-input-end))
    (setq hel-ghostel--sync-inhibit t)
    (hel-insert-state 1))
   (t (call-interactively #'hel-append-line))))

(hel-define-command hel-ghostel-beginning-of-line ()
  "Route `g s' to `ghostel-beginning-of-input-or-line' on prompt rows.
In a shell or REPL, column 0 lands point on the prompt — almost never
what the user wants.  Falls through to `hel-beginning-of-line-command'
in scrollback and non-prompt rows."
  :multiple-cursors t
  (interactive)
  (if (or (hel-ghostel--active-p)
          (hel-ghostel--line-mode-active-p))
      (ghostel-beginning-of-input-or-line)
    (call-interactively #'hel-beginning-of-line-command)))

(hel-define-command hel-ghostel-first-non-blank ()
  "Route `g h' to `ghostel-beginning-of-input-or-line' on prompt rows.
Falls through to `hel-first-non-blank' elsewhere."
  :multiple-cursors t
  (interactive)
  (if (or (hel-ghostel--active-p)
          (hel-ghostel--line-mode-active-p))
      (ghostel-beginning-of-input-or-line)
    (call-interactively #'hel-first-non-blank)))

(hel-define-command hel-ghostel-cut (count)
  "Kill selection via PTY in ghostel buffers, otherwise `hel-cut'.
With region: copies to kill-ring then deletes via PTY backspaces.
Without region: sends COUNT backspaces to PTY."
  :multiple-cursors t
  (interactive "*p")
  (if (hel-ghostel--active-p)
      (if (use-region-p)
          (let ((beg (region-beginning))
                (end (region-end)))
            (copy-region-as-kill beg end)
            (hel-ghostel--delete-region beg end)
            (deactivate-mark)
            (hel-extend-selection -1))
        (dotimes (_ (or count 1))
          (ghostel--send-encoded "backspace" "")))
    (hel-cut count)))

(hel-define-command hel-ghostel-delete (count)
  "Delete selection in ghostel buffers via PTY without saving to kill-ring.
With region: deletes via PTY backspaces.
Without region: sends COUNT forward-delete keys to PTY."
  :multiple-cursors t
  (interactive "*p")
  (if (hel-ghostel--active-p)
      (if (use-region-p)
          (let ((beg (region-beginning))
                (end (region-end)))
            (hel-ghostel--delete-region beg end)
            (deactivate-mark)
            (hel-extend-selection -1))
        (dotimes (_ (or count 1))
          (ghostel--send-encoded "delete" "")))
    (hel-delete count)))

(hel-define-command hel-ghostel-change ()
  "Delete selection via PTY and enter insert state.
With region: copies to kill-ring, deletes via PTY, enters insert state.
Uses the Ctrl-e Ctrl-u readline shortcut for linewise selections on the
cursor row.  Outside ghostel, falls through to `hel-change'."
  :multiple-cursors nil
  (interactive "*")
  (if (hel-ghostel--active-p)
      (progn
        (when (use-region-p)
          (let ((beg (region-beginning))
                (end (region-end)))
            (copy-region-as-kill beg end)
            (if (and (hel-linewise-selection-p) (hel-ghostel--point-on-cursor-row-p))
                (hel-ghostel--clear-input-line)
              (hel-ghostel--delete-region beg end))
            (deactivate-mark)
            (hel-extend-selection -1)))
        (setq hel-ghostel--sync-inhibit t)
        (hel-insert-state 1))
    (call-interactively #'hel-change)))

(hel-define-command hel-ghostel-paste-after (count)
  "Paste after cursor via PTY in ghostel buffers, otherwise `hel-paste-after'."
  :multiple-cursors nil
  (interactive "*P")
  (if (hel-ghostel--active-p)
      (let ((text (current-kill 0))
            (n (prefix-numeric-value count)))
        (when text
          (hel-ghostel--cursor-to-point)
          (ghostel--send-encoded "right" "")
          (dotimes (_ n)
            (ghostel--paste-text text))
          (hel-ghostel--invalidate-shadow)))
    (call-interactively #'hel-paste-after)))

(hel-define-command hel-ghostel-paste-before (count)
  "Paste before the cursor via PTY; otherwise call `hel-paste-before'."
  :multiple-cursors nil
  (interactive "*P")
  (if (hel-ghostel--active-p)
      (let ((text (current-kill 0))
            (n (prefix-numeric-value count)))
        (when text
          (hel-ghostel--cursor-to-point)
          (dotimes (_ n)
            (ghostel--paste-text text))
          (hel-ghostel--invalidate-shadow)))
    (call-interactively #'hel-paste-before)))

(hel-define-command hel-ghostel-undo (count)
  "Undo via PTY in ghostel (sends Ctrl-_ COUNT times), otherwise `hel-undo'."
  :multiple-cursors nil
  (interactive "*p")
  (if (hel-ghostel--active-p)
      (dotimes (_ (or count 1))
        (ghostel--send-encoded "_" "ctrl"))
    (hel-undo)))

(hel-define-command hel-ghostel-redo (_count)
  "Redo is not supported in the terminal; outside ghostel calls `hel-redo'."
  :multiple-cursors nil
  (interactive "*p")
  (if (hel-ghostel--active-p)
      (message "Redo not supported in terminal")
    (hel-redo)))

;;; ESC routing

(defvar-local hel-ghostel--escape-mode nil
  "Buffer-local override for ESC routing.
Initialized from `hel-ghostel-escape' when the minor mode turns on.
Valid values: `auto', `terminal', `hel'.")

(defconst hel-ghostel--escape-modes '(auto terminal hel)
  "Cycle order for `hel-ghostel-toggle-send-escape'.")

(defun hel-ghostel--escape ()
  "Dispatch insert-state ESC based on `hel-ghostel--escape-mode'.
In `auto' mode, sends ESC to the terminal when alt-screen is active
(DECSET 1049 — used by vim, less, htop, etc.); otherwise enters normal state."
  (interactive)
  (let* ((mode hel-ghostel--escape-mode)
         (to-terminal (or (eq mode 'terminal)
                          (and (eq mode 'auto)
                               ghostel--term
                               (ghostel--mode-enabled ghostel--term 1049)))))
    (if to-terminal
        (progn
          (ghostel--snap-to-input)
          (ghostel--send-encoded "escape" ""))
      (hel-normal-state))))

(defun hel-ghostel-toggle-send-escape (&optional arg)
  "Cycle or set the ESC routing mode for the current buffer.
Without ARG, cycle through `auto' → `terminal' → `hel' → `auto'.
With numeric prefix 1, set to `auto'; 2 to `terminal'; 3 to `hel'.
Other numeric prefixes signal a `user-error'.

The mode is buffer-local; see `hel-ghostel-escape' for the default."
  (interactive "P")
  (let ((target
         (if arg
             (let ((n (prefix-numeric-value arg)))
               (or (nth (1- n) hel-ghostel--escape-modes)
                   (user-error
                    "Invalid prefix %d; use 1 (auto), 2 (terminal), or 3 (hel)"
                    n)))
           (let ((next (cdr (memq hel-ghostel--escape-mode
                                  hel-ghostel--escape-modes))))
             (or (car next) (car hel-ghostel--escape-modes))))))
    (setq hel-ghostel--escape-mode target)
    (message "hel-ghostel ESC mode: %s" target)))

;;; Keymap

(defvar hel-ghostel-mode-map (make-sparse-keymap)
  "Keymap for `hel-ghostel-mode'.
State-specific bindings are set up via `hel-keymap-set'.")

(hel-keymap-set hel-ghostel-mode-map :state 'normal
  "I"   #'hel-ghostel-insert-line
  "A"   #'hel-ghostel-append-line
  "g s" #'hel-ghostel-beginning-of-line
  "g h" #'hel-ghostel-first-non-blank
  "c"   #'hel-ghostel-change
  "d"   #'hel-ghostel-cut
  "D"   #'hel-ghostel-delete
  "p"   #'hel-ghostel-paste-after
  "P"   #'hel-ghostel-paste-before
  "u"   #'hel-ghostel-undo
  "U"   #'hel-ghostel-redo)

(hel-keymap-set hel-ghostel-mode-map :state 'insert
  "<escape>" #'hel-ghostel--escape)

;;; Minor mode

;;;###autoload
(define-minor-mode hel-ghostel-mode
  "Minor mode for Hel integration in ghostel terminal buffers.
Synchronizes the terminal cursor with Emacs point during Hel
state transitions."
  :lighter nil
  :keymap hel-ghostel-mode-map
  (if hel-ghostel-mode
      (progn
        (setq hel-ghostel--escape-mode hel-ghostel-escape)
        (add-hook 'hel-insert-state-enter-hook
                  #'hel-ghostel--insert-state-entry nil t)
        (advice-add 'ghostel--redraw :around #'hel-ghostel--around-redraw)
        (advice-add 'ghostel--set-cursor-style :around
                    #'hel-ghostel--override-cursor-style)
        (hel-update-cursor))
    (remove-hook 'hel-insert-state-enter-hook
                 #'hel-ghostel--insert-state-entry t)
    (advice-remove 'ghostel--redraw #'hel-ghostel--around-redraw)
    (advice-remove 'ghostel--set-cursor-style
                   #'hel-ghostel--override-cursor-style)))

;;; .
(provide 'hel-ghostel)
;;; hel-ghostel.el ends here
