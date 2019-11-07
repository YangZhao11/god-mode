; -*- coding: utf-8; lexical-binding: t -*-
;;; god-mode.el --- God-like command entering minor mode

;; Copyright (C) 2013 Chris Done
;; Copyright (C) 2013 Magnar Sveen
;; Copyright (C) 2013 Rüdiger Sonderfeld
;; Copyright (C) 2013 Dillon Kearns
;; Copyright (C) 2013 Fabián Ezequiel Gallina

;; Author: Chris Done <chrisdone@gmail.com>
;; URL: https://github.com/chrisdone/god-mode
;; Version: 2.15.0

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; See README.md.

;;; Code:

(require 'cl-lib)

(add-hook 'after-change-major-mode-hook 'god-mode-maybe-activate)

(defvar god-local-mode-paused nil)
(make-variable-buffer-local 'god-local-mode-paused)

(defcustom god-mod-alist
  '((nil . "C-")
    ("g" . "M-")
    ("G" . "C-M-"))
  "List of keys and their associated modifer."
  :group 'god
  :type '(alist))

(defcustom god-literal-key
  "SPC"
  "The key used for literal interpretation."
  :group 'god
  :type 'string)

(defcustom god-mode-can-omit-literal-key
  nil
  "When non-nil, user can omit the literal key when unambiguous."
  :group 'god
  :type 'bool)

(defcustom god-mode-translate-alist
  '(("C-x C-1" "C-x 1")
    ("C-x C-2" "C-x 2")
    ("C-x C-3" "C-x 3")
    ("C-x C-4" "C-x 4" t)
    ("C-x C-5" "C-x 5" t)
    ("C-x C-6" "C-x 6" t)
    ("C-x C-8" "C-x 8" t)
    ("C-x C-9" "C-x 9")
    ("C-x C-0" "C-x 0"))
  "Translation table for god-mode command keys.
A third element means to treat the literal key as pressed."
  :group 'god
  :type '(alist))

(defcustom god-mode-low-priority-exempt
  '(self-insert-command c-electric-lt-gt c-electric-brace)
  "Commands that do not trigger for `god-mode-low-priority'."
  :group 'god
  :type '(repeat symbol))

(defcustom god-mode-is-low-priority nil
  "Whether god-mode should look for local bindings first.
This overrides settings for individual keys in
`god-mode-low-priority-keys'."
  :group 'god
  :type '(boolean))

(defcustom god-mode-low-priority-keys '()
  "List of keys that should be low priority."
  :group 'god
  :type '(repeat character))

(defcustom god-exempt-major-modes
  '(dired-mode
    grep-mode
    vc-annotate-mode
    git-commit-mode  ; For versions prior to Magit 2.1.0
    magit-popup-mode)
  "List of major modes that should not start in god-local-mode."
  :group 'god
  :type '(function))

(defcustom god-exempt-predicates
  (list #'god-exempt-mode-p
        #'god-comint-mode-p
        #'god-git-commit-mode-p
        #'god-view-mode-p
        #'god-special-mode-p)
  "List of predicates checked before enabling god-local-mode.
All predicates must return nil for god-local-mode to start."
  :group 'god
  :type '(repeat function))

(defvar god-local-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map t)
    (define-key map [remap self-insert-command] 'god-mode-self-insert)
    (let ((i ?\s))
      (while (< i 256)
        (define-key map (vector i) 'god-mode-self-insert)
        (setq i (1+ i)))
      (define-key map (kbd "DEL") nil))
    map))

;;;###autoload
(define-minor-mode god-local-mode
  "Minor mode for running commands."
  nil " God" god-local-mode-map
  (if god-local-mode
      (run-hooks 'god-mode-enabled-hook)
    (run-hooks 'god-mode-disabled-hook)))

(defun god-local-mode-pause ()
  "Pause god-mode local to the buffer, if it's
enabled. See also `god-local-mode-resume'."
  (when god-local-mode
    (god-local-mode -1)
    (setq god-local-mode-paused t)))

(defun god-local-mode-resume ()
  "Will re-enable god-mode, if it was active when
`god-local-mode-pause' was called. If not, nothing happens."
  (when (bound-and-true-p god-local-mode-paused)
    (setq god-local-mode-paused nil)
    (god-local-mode 1)))

(defvar god-global-mode nil
  "Activate God mode on all buffers?")

(defvar god-literal-sequence nil
  "Activated after space is pressed in a command sequence.")

;;;###autoload
(defun god-mode ()
  "Toggle global God mode."
  (interactive)
  (setq god-global-mode (not god-global-mode))
  (if god-global-mode
      (god-local-mode 1)
    (god-local-mode -1)))

;;;###autoload
(defun god-mode-all ()
  "Toggle God mode in all buffers."
  (interactive)
  (let ((new-status (if (bound-and-true-p god-local-mode) -1 1)))
    (setq god-global-mode t)
    (mapc (lambda (buffer)
            (with-current-buffer buffer
              (god-mode-activate new-status)))
          (buffer-list))
    (setq god-global-mode (= new-status 1))))

(defun god-mode-maybe-universal-argument-more ()
  "If god mode is enabled, call `universal-argument-more'."
  (interactive)
  (if god-local-mode
      (call-interactively #'universal-argument-more)
    (let ((binding (god-mode-lookup-command "u")))
      (if binding
          (if (commandp binding t)
              (call-interactively binding)
            (execute-kbd-macro binding))
        (user-error "God: Unknown key binding for `%s'"
                    (concat (cdr (assq nil god-mod-alist)) "u"))))))

(define-key universal-argument-map (kbd "u")
  #'god-mode-maybe-universal-argument-more)

(defun god-mode--maybe-local-binding (initial-key)
  "Return a local binding when INITIAL-KEY is low priority"
  (when (or god-mode-is-low-priority
            (memq initial-key god-mode-low-priority-keys))
    (let ((binding (local-key-binding (char-to-string initial-key))))
      (if (and binding
               (commandp binding t)
               (not (memq binding god-mode-low-priority-exempt))
               (not (eq binding 'god-mode-self-insert)))
          binding))))

(defun god-mode-self-insert ()
  "Handle self-insert keys."
  (interactive)
  (let* ((initial-key (aref (this-command-keys-vector)
                            (- (length (this-command-keys-vector)) 1)))
         (binding (or (god-mode--maybe-local-binding initial-key)
                      (god-mode-lookup-key-sequence initial-key))))
    (when (god-mode-upper-p initial-key)
      (setq this-command-keys-shift-translated t))
    (setq this-original-command binding)
    (setq this-command binding)
    ;; `real-this-command' is used by emacs to populate
    ;; `last-repeatable-command', which is used by `repeat'.
    (setq real-this-command binding)
    (setq god-literal-sequence nil)
    (if (commandp binding t)
        (call-interactively binding)
      (execute-kbd-macro binding))))

(defun god-mode-upper-p (char)
  "Is the given CHAR upper case?"
  (and (>= char ?A)
       (<= char ?Z)
       (/= char ?G)))

(defun god-mode-lookup-key-sequence (&optional key key-string-so-far)
  "Lookup the command for the given KEY.
If KEY is nil, read the next keypress. This function sometimes recurses.
KEY-STRING-SO-FAR should be nil for the first call in the
sequence."
  (interactive)
  (let* ((sanitized-key
          (god-mode-sanitized-key-string
           (or key (read-event key-string-so-far))))
         (key-list (god-mode-read-key-list sanitized-key key-string-so-far))
         (key-string (god-mode-maybe-translate (apply 'concat key-list)))
         (binding (god-mode-lookup-command key-string))
         alt-key-string)
    (if binding binding
      (setq alt-key-string (god-mode--maybe-omit-literal-key key-list))
      (or (and alt-key-string (god-mode-lookup-command alt-key-string))
          (god-mode-help-func key-list)
          (user-error "God: Unknown key binding for `%s'" key-string)))))

(defun god-mode--maybe-omit-literal-key (key-list)
  "Return an alternative key string of KEY-LIST by assuming
  `god-literal-key' was pressed right before the last keystroke."
  (when (and god-mode-can-omit-literal-key
             (not (string= "" (cadr key-list))))
    (let* ((alt-key-string
            (concat (car key-list) (caddr key-list)))
           (key-binding (read-kbd-macro alt-key-string t)))
      (when key-binding
        (setq god-literal-sequence 't)
        alt-key-string))))

(defun god-mode--remove-trailing-help-char (key-string)
  "Remove trailing help char from KEY-STRING."
  (replace-regexp-in-string
   (concat " "
           (if god-literal-sequence "" (cdr (assq nil god-mod-alist)))
           (char-to-string help-char)
           "$")
   "" key-string))

(defun god-mode-help-func (key-list)
  "Returns a function, when called, show help on the prefix. If
KEY-LIST does not end on a help char, then return nil."
  (let ((prefix (car key-list)))
    (when (and prefix
               (string= (char-to-string help-char) (caddr key-list)))
    (lambda ()
      (interactive)
      (describe-bindings (read-kbd-macro prefix))))))

(defun god-mode-sanitized-key-string (key)
  "Convert any special events to textual."
  (cl-case key
    (tab "TAB")
    (?\  "SPC")
    (left "<left>")
    (right "<right>")
    (S-left "S-<left>")
    (S-right "S-<right>")
    (prior "<prior>")
    (next "<next>")
    (backspace "DEL")
    (return "RET")
    (t (char-to-string key))))

(defun god-mode-read-key-list (key key-string-so-far)
  "Interpret god-mode special keys for key (consumes more keys if
appropriate). Returns a list of (prefix modifier key)."
  (let ((key-consumed t) (next-modifier "") next-key)
    (message key-string-so-far)
    (cond
     ;; Don't check for god-literal-key with the first key
     ((and key-string-so-far (string= key god-literal-key))
      (setq god-literal-sequence t))
     (god-literal-sequence
      (setq key-consumed nil))
     ((and (stringp key) (assoc key god-mod-alist))
      (setq next-modifier (cdr (assoc key god-mod-alist))))
     (t
      (setq key-consumed nil
            next-modifier (cdr (assoc nil god-mod-alist)))))
    (setq next-key
          (if key-consumed
              (god-mode-sanitized-key-string (read-event key-string-so-far))
            key))
    (when (and (= (length next-key) 1)
               (string= (get-char-code-property (aref next-key 0) 'general-category) "Lu")
               ;; If C- is part of the modifier, S- needs to be given
               ;; in order to distinguish the uppercase from the
               ;; lowercase bindings. If C- is not in the modifier,
               ;; then emacs natively treats uppercase differently
               ;; from lowercase, and the S- modifier should not be
               ;; given
               (string-prefix-p "C-" next-modifier))
      (setq next-modifier (concat next-modifier "S-")))
    (list (if key-string-so-far (concat key-string-so-far " "))
          next-modifier
          next-key)))

(defun god-mode-maybe-translate (key-string)
  "Translate KEY-STRING according to `god-mode-translate-alist'."
  (let ((translation (cdr (assoc key-string god-mode-translate-alist))))
    (if (not translation)
        key-string
      (setq god-literal-sequence (cadr translation))
      (car translation))))

(defun god-mode-lookup-command (key-string)
  "Resolve key binding for KEY-STRING.
This function recursively call `god-mode-lookup-key-sequence'. If
a command is not found, return nil, otherwise return the
command."
  (let* ((key-vector (read-kbd-macro key-string t))
         (binding (key-binding key-vector)))
    (cond ((commandp binding)
           (setq last-command-event (aref key-vector (- (length key-vector) 1)))
           binding)
          ((keymapp binding)
           (god-mode-lookup-key-sequence nil key-string)))))

;;;###autoload
(defun god-mode-maybe-activate (&optional status)
  "Activate God mode locally on individual buffers when appropriate."
  (when (not (minibufferp))
    (god-mode-activate status)))

(defun god-mode-activate (&optional status)
  "Activate God mode locally on individual buffers when appropriate."
  (when (and god-global-mode
             (god-passes-predicates-p))
    (god-local-mode (if status status 1))))

(defun god-exempt-mode-p ()
  "Return non-nil if major-mode is exempt.
Members of the `god-exempt-major-modes' list are exempt."
  (memq major-mode god-exempt-major-modes))

(defun god-mode-child-of-p (mode parent-mode)
  "Return non-nil if MODE is derived from PARENT-MODE."
  (let ((parent (get mode 'derived-mode-parent)))
    (cond ((eq parent parent-mode))
          ((not (null parent))
           (god-mode-child-of-p parent parent-mode))
          (t nil))))

(defun god-comint-mode-p ()
  "Return non-nil if major-mode is child of comint-mode."
  (god-mode-child-of-p major-mode 'comint-mode))

(defun god-special-mode-p ()
  "Return non-nil if major-mode is special or a child of special-mode."
  (eq (get major-mode 'mode-class) 'special))

(defun god-view-mode-p ()
  "Return non-nil if view-mode is enabled in current buffer."
  view-mode)

(defun god-git-commit-mode-p ()
  "Return non-nil if a `git-commit-mode' will be enabled in this buffer."
  (and (bound-and-true-p global-git-commit-mode)
       ;; `git-commit-filename-regexp' defined in the same library as
       ;; `global-git-commit-mode'.  Expression above maybe evaluated
       ;; to true because of autoload cookie.  So we perform
       ;; additional check.
       (boundp 'git-commit-filename-regexp)
       buffer-file-name
       (string-match-p git-commit-filename-regexp buffer-file-name)))

(defun god-passes-predicates-p ()
  "Return non-nil if all `god-exempt-predicates' return nil."
  (not
   (catch 'disable
     (let ((preds god-exempt-predicates))
       (while preds
         (when (funcall (car preds))
           (throw 'disable t))
         (setq preds (cdr preds)))))))

(provide 'god-mode)

;;; god-mode.el ends here
