;;; hel-ghostel.el --- Hel integration for Ghostel -*- lexical-binding: t -*-
;;
;; Copyright (c) 2026 Yuriy Artemyev
;;
;; Author: Yuriy Artemyev <anuvyklack@gmail.com>
;; URL: https://github.com/anuvyklack/hel-ghostel
;; Version: 0.1.0
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; This file is NOT part of GNU Emacs.
;;
;;; Code:

(require 'hel)
(require 'ghostel)

(declare-function ghostel--mode-enabled "ghostel-module")

;;; Hooks and advices

(add-hook 'ghostel-mode-hook 'hel-ghostel-h)

(defun hel-ghostel-h ()
  (setopt ghostel-readonly-fast-exit nil)
  (when (memq ghostel--input-mode '(semi-char char))
    (hel-local-mode -1))
  (setq-local truncate-lines nil
              word-wrap t))

(hel-advice-add 'ghostel-semi-char-mode :after 'hel-ghostel--disable-hel)
(hel-advice-add 'ghostel-char-mode      :after 'hel-ghostel--disable-hel)
(hel-advice-add 'ghostel-line-mode      :after 'hel-ghostel--enable-hel)

(defun hel-ghostel--disable-hel ()
  (when hel-local-mode
    (hel-local-mode -1)))

(defun hel-ghostel--enable-hel ()
  (unless hel-local-mode
    (hel-local-mode 1)))

(hel-define-advice ghostel-emacs-mode (:override ())
  "Switch to Ghostel Emacs mode. Terminal keep running and scrollback growing.
To return to terminal use:
- i, a    switch to `ghostel-semi-char-mode'
- I, A    switch to `ghostel-line-mode'"
  (interactive)
  (unless (eq ghostel--input-mode 'emacs)
    (ghostel--enter-readonly
     'emacs nil ":Emacs"
     (format "Terminal live, %s to return to terminal"
             (propertize "i" 'face 'help-key-binding)))
    (hel-local-mode 1)))

(hel-define-advice ghostel-copy-mode (:override ())
  "Switch to Ghostel Copy mode. Terminal is frozen — live output is paused.
To return to terminal use:
- i, a    switch to `ghostel-semi-char-mode'
- I, A    switch to `ghostel-line-mode'"
  (interactive)
  (if (eq ghostel--input-mode 'copy)
      (ghostel-readonly-exit)
    ;; else
    (ghostel--enter-readonly
     'copy t ":Copy"
     (format "Terminal frozen, %s to return to terminal"
             (propertize "i" 'face 'help-key-binding)))
    (hel-local-mode 1)))

(hel-define-advice ghostel--set-cursor-style (:around (orig-fun style visible))
  "Let Hel control the cursor shape if it is acitve."
  (if hel-local-mode
      (hel-update-cursor)
    (funcall orig-fun style visible)))

;;; Keybindings

(setq ghostel-keymap-exceptions (->> ghostel-keymap-exceptions
                                     (delete "C-u")
                                     (delete "C-h")
                                     (-cons* "<f1>" "M-u")
                                     (delete-dups)))

(hel-keymap-set ghostel-semi-char-mode-map
  ;; Hel rebinds `universal-argument' to "M-u"
  "C-u"     'ghostel--self-insert
  "M-u"      nil
  ;; Hel encourages using "F1" instead of "C-h" as the help prefix.
  "C-h"     'ghostel--self-insert
  "<f1>"     nil
  ;;
  "<escape>" 'ghostel--send-event
  "C-c I"   '("line-mode" . ghostel-line-mode)
  ;; paste
  "C-S-v"   'ghostel-yank
  "C-c p"   '("paste" . ghostel-yank)
  "C-c C-p" '("paste pop" . ghostel-yank-pop)
  ;; Fix missing keybindigs
  "C-;"     'hel-ghostel--send-C-semicolon)

(hel-keymap-set ghostel-semi-char-mode-map
  "C-c <escape>" 'hel-ghostel-escape
  "C-c w"        '("window-map" . hel-window-map)
  "C-c C-w"      '("window-map" . hel-window-map))

(hel-keymap-set ghostel-line-mode-map
  "C-c <escape>" 'hel-ghostel-escape
  "C-c i"        '("semi-char mode" . ghostel-semi-char-mode))

(hel-keymap-set ghostel-line-mode-map :state 'normal
  "<remap> <hel-beginning-of-line-command>" 'hel-ghostel-beginning-of-line-command ; gh
  "<remap> <hel-first-non-blank>"           'hel-ghostel-first-non-blank           ; gs
  "C-j" 'ghostel-line-mode-history-next
  "C-k" 'ghostel-line-mode-history-previous)

(hel-keymap-set ghostel-readonly-mode-map :state 'normal
  "y" 'hel-ghostel-copy
  ;;
  "<remap> <hel-insert>"      'ghostel-semi-char-mode ; "i"
  "<remap> <hel-append>"      'ghostel-semi-char-mode ; "a"
  "<remap> <hel-insert-line>" 'ghostel-line-mode      ; "I"
  "<remap> <hel-append-line>" 'ghostel-line-mode)     ; "A"

(hel-keymap-set ghostel-mode-map :state 'normal
  "] ]"  'ghostel-next-prompt
  "[ ["  'ghostel-previous-prompt
  "] l"  'ghostel-next-hyperlink
  "[ l"  'ghostel-previous-hyperlink)

(setq ghostel-hyperlink-repeat-map
      (define-keymap
        "]" 'ghostel-next-hyperlink
        "[" 'ghostel-previous-hyperlink))

(setq ghostel-prompt-repeat-map
      (define-keymap
        "]" 'ghostel-next-prompt
        "[" 'ghostel-previous-prompt))

;;; Commands

;; Esc
(hel-define-command hel-ghostel-escape (arg)
  "Switch to `ghostel-emacs-mode'.
With \\[universal-argument] switch to `ghostel-copy-mode' instead."
  :multiple-cursors nil
  (interactive "P")
  (if arg
      (ghostel-copy-mode)
    (ghostel-emacs-mode)))

(defun hel-ghostel-beginning-of-line ()
  "Move point to prompt start or beginning of current line."
  (let ((bol (line-beginning-position))
        (eol (line-end-position)))
    (goto-char (or (if-let* (((eq ghostel--input-mode 'line))
                             ((markerp ghostel--line-input-start))
                             (pos (marker-position ghostel--line-input-start))
                             ((<= bol pos eol)))
                       pos)
                   ;; Text property fallback
                   (when (get-text-property bol 'ghostel-prompt)
                     (let ((pos (next-single-property-change
                                 bol 'ghostel-prompt nil eol)))
                       (if (< pos eol)
                           pos)))
                   ;; Regex fallback for shells/REPLs without OSC 133.
                   (ghostel--regex-prompt-end bol)
                   ;; Final fallback
                   (hel-beginning-of-line)))))

;; gh
(hel-define-command hel-ghostel-beginning-of-line-command ()
  "Move point to prompt start or beginning of current line."
  :multiple-cursors t
  :merge-selections t
  (declare (interactive-only hel-beginning-of-line-command))
  (interactive)
  (hel-set-region (if hel--extend-selection (mark) (point))
                  (hel-ghostel-beginning-of-line)))

;; gs
(hel-define-command hel-ghostel-first-non-blank ()
  "Move point to beginning of current line skipping indentation.
Use visual line when `visual-line-mode' is active."
  :multiple-cursors t
  :merge-selections t
  (interactive)
  (hel-set-region (if hel--extend-selection (mark) (point))
                  (progn (hel-ghostel-beginning-of-line)
                         (skip-syntax-forward " " (line-end-position))
                         (backward-prefix-chars)
                         (point))))

;; y
(hel-define-command hel-ghostel-copy ()
  :multiple-cursors nil
  (interactive)
  (let ((filter-buffer-substring-function
         (lambda (beg end _delete)
           (->> (buffer-substring beg end)
                (ghostel--clean-copy-text)))))
    (hel-copy)))

;; C-;
(defun hel-ghostel--send-C-semicolon ()
  "Forward \"C-;\" to the terminal using the Kitty keyboard protocol."
  (interactive)
  (ghostel--on-user-input)
  (ghostel--send-string "\e[59;5u"))

;;; zsh-helix-mode integration
;; https://github.com/Multirious/zsh-helix-mode

(add-hook 'ghostel-pre-spawn-hook 'hel-ghostel--zsh-helix-mode)

(defun hel-ghostel--zsh-helix-mode ()
  "Setup `zsh-helix-mode' styling environment variables according to the
current Emacs theme."
  (let* ((region-bg  (ghostel--face-hex-color 'region  :background))
         (region-fg  (ghostel--face-hex-color 'region  :foreground))
         (cursor-bg  (ghostel--face-hex-color 'cursor  :background))
         (default-fg (ghostel--face-hex-color 'default :foreground)))
    (setenv "ZHM_STYLE_SELECTION"     (format "fg=%s,bg=%s" region-fg region-bg))
    (setenv "ZHM_STYLE_CURSOR_NORMAL" (format "fg=%s,bg=%s" default-fg cursor-bg))
    (setenv "ZHM_CURSOR_NORMAL"       (format "\e[0m\e[2 q\e]12;%s\a" cursor-bg))))

;;; .
(provide 'hel-ghostel)
;;; hel-ghostel.el ends here
