;;; hel-ghostel.el --- Hel integration for Ghostel    -*- lexical-binding: t -*-

;; Copyright (c) 2026 Yuriy Artemyev

;; Author: Yuriy Artemyev <anuvyklack@gmail.com>
;; URL: https://github.com/anuvyklack/hel-ghostel
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (hel "0.10.0") (ghostel "0.8.0"))
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is NOT part of GNU Emacs.

;;; Code:

(require 'dash)
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
Must be set via `setopt'!"
  :type '(choice (const :tag "Insert" insert)
                 (const :tag "Normal" normal)
                 (symbol :tag "Other state"))
  :group 'hel-ghostel
  :set (lambda (symbol value)
         (set-default-toplevel-value symbol value)
         (hel-set-initial-state 'ghostel-mode value)))

(defcustom hel-ghostel-send-escape 'auto
  "Where ESC in Hel Insert state should be send to the terminal.

`auto'      When the inner app is in alt-screen mode (DECSET 1049,
          used by vim, less, htop, nvim, etc.) ESC is sent to the
          terminal, otherwise Hel will switch to Normal state.

`terminal'  Always send ESC to the terminal.

`hel'       Switch Hel to Normal state on ESC.

Sets the initial value of the buffer-local state.
Use \\[hel-ghostel-toggle-send-escape] to change it for the current buffer."
  :type '(choice (const :tag "Auto (alt-screen heuristic)" auto)
                 (const :tag "Always to terminal" terminal)
                 (const :tag "Always to Hel" hel))
  :group 'hel-ghostel)

;;; Guard predicates

(defun hel-ghostel--active-p ()
  "Return non-nil when `hel-ghostel' editing should intercept."
  (and hel-ghostel-mode
       ghostel--term       ; terminal alive
       ghostel--cursor-pos ; cursor position defined
       (not (ghostel--mode-enabled ghostel--term 1049))
       (eq ghostel--input-mode 'semi-char)))

(defun hel-ghostel--line-mode-active-p ()
  "Return non-nil when ghostel in line mode.
In line mode, shell input is buffered as plain text in the
[`ghostel--line-input-start', `ghostel--line-input-end'] region.
Hel's default buffer-editing operators work correctly there."
  (and hel-ghostel-mode
       (eq ghostel--input-mode 'line)
       (markerp ghostel--line-input-start)
       (markerp ghostel--line-input-end)))

;;; Cursor synchronization

(defun hel-ghostel--scrollback ()
  "Return the number of scrollback lines above the viewport, or 0."
  (max 0 (- (count-lines (point-min) (point-max))
            ghostel--term-rows)))

(defun hel-ghostel--reset-cursor-point ()
  "Move Emacs point to the terminal cursor position.
`ghostel--cursor-pos' holds the viewport-relative (COL . ROW), so
the row must be offset by the scrollback line count."
  (-when-let ((col . row) ghostel--cursor-pos)
    (goto-char (point-min))
    (forward-line (+ row (hel-ghostel--scrollback)))
    (move-to-column col)))

(defun hel-ghostel--cursor-buffer-line ()
  "Return the 0-indexed buffer line of the terminal cursor, or nil."
  (-if-let ((_ . row) ghostel--cursor-pos)
      (+ row (hel-ghostel--scrollback))))

(defun hel-ghostel--point-viewport-row ()
  "Return the viewport row of point, 0-indexed.
Subtracts the scrollback line count from the buffer line so the
result is comparable to `ghostel--cursor-pos''s row."
  (if ghostel--term-rows
      (- (line-number-at-pos (point) t)
         (hel-ghostel--scrollback)
         1)
    0))

(defvar-local hel-ghostel--last-cursor-line nil
  "Buffer line where the previous redraw placed the terminal cursor.
Used by `hel-ghostel--redraw-a' to detect prompt-line scrolling.")

(defvar-local hel-ghostel--shadow-cursor nil
  "Pending terminal cursor (COL . VIEWPORT-ROW), or nil to read live state.
Within a single command we may emit several key sequences before any
are echoed by the PTY.  `ghostel--cursor-pos' lags queued keys, so a
second sync reads the shadow instead of the stale live value.
Reset by `hel-ghostel--redraw-a' after each render.")

(defun hel-ghostel--shadow-or-live ()
  "Return best-known terminal cursor (COL . VIEWPORT-ROW), or nil."
  (or hel-ghostel--shadow-cursor
      ghostel--cursor-pos))

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
    (-let* (((tcol . trow) (hel-ghostel--shadow-or-live))
            (ecol (current-column))
            (erow (hel-ghostel--point-viewport-row))
            (dy (- erow trow))
            (dx (- ecol tcol)))
      (cond ((> dy 0) (dotimes (_ dy) (ghostel--send-encoded "down" "")))
            ((< dy 0) (dotimes (_ (abs dy)) (ghostel--send-encoded "up" ""))))
      (cond ((> dx 0) (dotimes (_ dx) (ghostel--send-encoded "right" "")))
            ((< dx 0) (dotimes (_ (abs dx)) (ghostel--send-encoded "left" ""))))
      (setq hel-ghostel--shadow-cursor (cons ecol erow)))))

;;; Redraw: preserve point across the native call

(defvar-local hel-ghostel--sync-point-on-next-redraw nil
  "When non-nil, the next `ghostel--redraw' will move point to the position of
the terminal cursor.

Commands that send PTY commands which moves the terminal cursor should set
it or otherwise point would be left at a stale position.")

(defun hel-ghostel--redraw-a (orig-fun term &optional full)
  "Preserve point position across the native redraw call.
`ghostel--redraw' is a native function that unconditionally places
point at the terminal cursor on exit (\"point is owned by the
renderer\", Renderer.zig).  In normal state the user navigates point
independently of the terminal cursor (scrollback, etc.), so without
this advice every incoming PTY byte would snap point back to the
cursor, breaking normal-state navigation entirely.

In non-insert states point is restored after the redraw unless the
user was parked on the prompt line and the cursor scrolled to a new
line — in that case the renderer's placement wins so the user follows
the new prompt position.

The standard Emacs mark is preserved by the native module;
`hel--extend-selection' is a boolean flag unaffected by buffer
positions — neither needs saving here.

ORIG-FUN is the advised `ghostel--redraw' called with TERM and FULL.
Skipped when the terminal is in alt-screen mode (1049); apps there
own the screen and drive their own redraw cycle."
  (if (and hel-ghostel-mode
           (not (ghostel--mode-enabled term 1049)))
      (let* ((preserve-point? (not (or hel-ghostel--sync-point-on-next-redraw
                                       hel-insert-state)))
             (saved-point (if preserve-point? (point)))
             (pre-line    (if preserve-point?
                              (1- (line-number-at-pos (point) t))))
             (was-on-prompt-line (and pre-line
                                      hel-ghostel--last-cursor-line
                                      (= pre-line
                                         hel-ghostel--last-cursor-line))))
        (funcall orig-fun term full)
        (when hel-ghostel--sync-point-on-next-redraw
          (setq hel-ghostel--sync-point-on-next-redraw nil)
          (hel-ghostel--reset-cursor-point))
        (let* ((post-cursor-line (hel-ghostel--cursor-buffer-line))
               (prompt-moved (and was-on-prompt-line
                                  post-cursor-line
                                  (/= post-cursor-line pre-line))))
          (when (and preserve-point? (not prompt-moved))
            (goto-char (min saved-point (point-max))))
          (setq hel-ghostel--last-cursor-line post-cursor-line))
        (hel-ghostel--invalidate-shadow))
    ;; else
    (funcall orig-fun term full)))

;;; Cursor style: let Hel control cursor shape

(defun hel-ghostel--override-cursor-style (orig-fun style visible)
  "Let Hel control cursor shape instead of the terminal.
ORIG-FUN is the advised setter called with STYLE and VISIBLE.
In alt-screen mode, defer to the terminal's cursor style."
  (if (and ghostel--term
           (not (ghostel--mode-enabled ghostel--term 1049)))
      (hel-update-cursor)
    (funcall orig-fun style visible)))

;;; Insert state entry hook

(defvar-local hel-ghostel--sync-inhibit nil
  "When non-nil, skip arrow-key sync in the insert-state-entry hook.
Set by the \"I\" / \"A\" commands which send Ctrl-a / Ctrl-e directly.")

(defun hel-ghostel--insert-state-enter-h ()
  "Sync terminal cursor with Emacs point when on switching to Hel Insert state.
Skipped when `hel-ghostel--sync-inhibit' is set (by I/A commands
which already sent Ctrl-a/Ctrl-e).  Also skipped outside semi-char.
When point is on a different row from the terminal cursor, snap back
to the terminal cursor to avoid sending arrows the shell interprets
as history navigation."
  (cond (hel-ghostel--sync-inhibit
         (setq hel-ghostel--sync-inhibit nil))
        ((hel-ghostel--active-p)
         (-let ((erow (hel-ghostel--point-viewport-row))
                ((_ . trow) ghostel--cursor-pos))
           (if (= erow trow)
               (hel-ghostel--cursor-to-point)
             (hel-ghostel--reset-cursor-point))))))

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
    (when (< 0 count)
      (goto-char end)
      (hel-ghostel--cursor-to-point)
      (dotimes (_ count)
        (ghostel--send-encoded "backspace" ""))
      (goto-char beg)
      (hel-ghostel--invalidate-shadow))))

(defun hel-ghostel--point-on-cursor-row-p ()
  "Non-nil when point is on the same viewport row as the terminal cursor."
  (and-let* ((trow (cdr ghostel--cursor-pos))
             (prow (hel-ghostel--point-viewport-row))
             ((= prow trow)))))

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

;; I
(hel-define-command hel-ghostel-insert-line ()
  "Switch to insert state at the beginning of the current line.
In semi-char mode, syncs the terminal cursor to point's row first,
then sends Ctrl-a so readline/zle moves to the start of that input
line.  In line mode, jumps to `ghostel--line-input-start' directly.
Outside ghostel, falls through to `hel-insert-line'."
  :multiple-cursors nil
  (interactive "*")
  (cond ((hel-ghostel--active-p)
         (hel-ghostel--cursor-to-point)
         (ghostel--send-encoded "a" "ctrl")
         (hel-ghostel--invalidate-shadow)
         (setq hel-ghostel--sync-inhibit t)
         (hel-insert-state 1))
        ((hel-ghostel--line-mode-active-p)
         (goto-char (marker-position ghostel--line-input-start))
         (setq hel-ghostel--sync-inhibit t)
         (hel-insert-state 1))
        (t
         (call-interactively #'hel-insert-line))))

;; A
(hel-define-command hel-ghostel-append-line ()
  "Switch to insert state at the end of the current line.
Symmetric to `hel-ghostel-insert-line': sends Ctrl-e in semi-char,
jumps to `ghostel--line-input-end' in line mode."
  :multiple-cursors nil
  (interactive "*")
  (cond ((hel-ghostel--active-p)
         (hel-ghostel--cursor-to-point)
         (ghostel--send-encoded "e" "ctrl")
         (hel-ghostel--invalidate-shadow)
         (setq hel-ghostel--sync-inhibit t)
         (hel-insert-state 1))
        ((hel-ghostel--line-mode-active-p)
         (goto-char (marker-position ghostel--line-input-end))
         (setq hel-ghostel--sync-inhibit t)
         (hel-insert-state 1))
        (t
         (call-interactively #'hel-append-line))))

;; gs
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

;; gh
(hel-define-command hel-ghostel-first-non-blank ()
  "Route `g h' to `ghostel-beginning-of-input-or-line' on prompt rows.
Falls through to `hel-first-non-blank' elsewhere."
  :multiple-cursors t
  (interactive)
  (if (or (hel-ghostel--active-p)
          (hel-ghostel--line-mode-active-p))
      (ghostel-beginning-of-input-or-line)
    (call-interactively #'hel-first-non-blank)))

;; d
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

;; D
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

;; c
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

;; p
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

;; P
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

;; u
(hel-define-command hel-ghostel-undo (count)
  "Undo via PTY in ghostel (sends Ctrl-_ COUNT times), otherwise `hel-undo'."
  :multiple-cursors nil
  (interactive "*p")
  (if (hel-ghostel--active-p)
      (dotimes (_ (or count 1))
        (ghostel--send-encoded "_" "ctrl"))
    (hel-undo)))

;; U
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
Initialized from `hel-ghostel-send-escape' when the minor mode turns on.
Valid values: `auto', `terminal', `hel'.")

(defun hel-ghostel-escape ()
  "Dispatch insert-state ESC based on `hel-ghostel--escape-mode'.
In `auto' mode, sends ESC to the terminal when an alternate screen buffer
is active (DECSET 1049 — used by vim, less, htop, etc.), otherwise switch
to Hel Normal state."
  (interactive nil ghostel-mode)
  (if (pcase hel-ghostel--escape-mode
        ('auto (-some-> ghostel--term (ghostel--mode-enabled 1049)))
        ('terminal t))
      (progn
        (ghostel--snap-to-input)
        (ghostel--send-encoded "escape" ""))
    ;; else
    (hel-normal-state)))

(defun hel-ghostel-toggle-send-escape (&optional arg)
  "Cycle or set the ESC routing mode for the current buffer.
Without ARG, cycle through `auto' → `terminal' → `hel' → `auto'.
With numeric prefix 1, set to `auto'; 2 to `terminal'; 3 to `hel'.
Other numeric prefixes signal a `user-error'.

The mode is buffer-local; see `hel-ghostel-send-escape' for the default."
  (interactive "P")
  (let ((modes '(auto terminal hel)))
    (setq hel-ghostel--escape-mode
          (or (if arg (nth (-> (prefix-numeric-value arg)
                               (- 1)
                               (% (length modes)))
                           modes))
              (cadr (memq hel-ghostel--escape-mode
                          modes))
              (car modes))))
  (message "hel-ghostel ESC mode: %s" hel-ghostel--escape-mode))

;;; Minor mode

(defvar-keymap hel-ghostel-mode-map
  :doc "Keymap for `hel-ghostel-mode'."
  "<remap> <hel-beginning-of-line-command>" #'hel-ghostel-beginning-of-line ; "g s"
  "<remap> <hel-first-non-blank>"           #'hel-ghostel-first-non-blank   ; "g h"
  "<remap> <hel-insert-line>"  #'hel-ghostel-insert-line  ; "I"
  "<remap> <hel-append-line>"  #'hel-ghostel-append-line  ; "A"
  "<remap> <hel-change>"       #'hel-ghostel-change       ; "c"
  "<remap> <hel-cut>"          #'hel-ghostel-cut          ; "d"
  "<remap> <hel-delete>"       #'hel-ghostel-delete       ; "D"
  "<remap> <hel-paste-after>"  #'hel-ghostel-paste-after  ; "p"
  "<remap> <hel-paste-before>" #'hel-ghostel-paste-before ; "P"
  "<remap> <hel-undo>"         #'hel-ghostel-undo         ; "u"
  "<remap> <hel-redo>"         #'hel-ghostel-redo)        ; "U"

(hel-keymap-set hel-ghostel-mode-map :state 'insert
  "<escape>" #'hel-ghostel-escape)

;;;###autoload
(define-minor-mode hel-ghostel-mode
  "Minor mode for Hel integration in ghostel terminal buffers.
Synchronizes the terminal cursor with Emacs point during Hel
state transitions."
  :lighter " Hel"
  :keymap hel-ghostel-mode-map
  (if hel-ghostel-mode
      (progn
        (setq hel-ghostel--escape-mode hel-ghostel-send-escape)
        (add-hook 'hel-insert-state-enter-hook
                  #'hel-ghostel--insert-state-enter-h
                  90 t)
        (advice-add 'ghostel--redraw
                    :around #'hel-ghostel--redraw-a)
        (advice-add 'ghostel--set-cursor-style
                    :around #'hel-ghostel--override-cursor-style)
        (hel-update-cursor))
    ;; else
    (remove-hook 'hel-insert-state-enter-hook #'hel-ghostel--insert-state-enter-h t)
    (advice-remove 'ghostel--redraw #'hel-ghostel--redraw-a)
    (advice-remove 'ghostel--set-cursor-style #'hel-ghostel--override-cursor-style)))

;;; .
(provide 'hel-ghostel)
;;; hel-ghostel.el ends here
