;;; god-mode.el --- God-like command entering minor mode -*- coding: utf-8; lexical-binding: t -*-

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
(require 'cl-macs)

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
      (define-key map (kbd "DEL") nil)
      (define-key map (kbd "C-h k") #'god-mode-describe-key))
    map))

;;;###autoload
(define-minor-mode god-local-mode
  "Minor mode for running commands."
  nil " God" god-local-mode-map
  (if god-local-mode
      (run-hooks 'god-mode-enabled-hook)
    (run-hooks 'god-mode-disabled-hook)))

(defun god-local-mode-pause ()
  "Pause `god-mode' local to the buffer, if it's enabled.
See also `god-local-mode-resume'."
  (when god-local-mode
    (god-local-mode -1)
    (setq god-local-mode-paused t)))

(defun god-local-mode-resume ()
  "Re-enable `god-mode', if it was paused by `god-local-mode-pause'.
If not, nothing happens."
  (when (bound-and-true-p god-local-mode-paused)
    (setq god-local-mode-paused nil)
    (god-local-mode 1)))

(defvar god-global-mode nil
  "Activate God mode on all buffers?")

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
    (let ((binding (god-mode-read-command
                    (make-god-mode-k :key "u" :trace "u" :literal 't))))
      (if binding
          (if (commandp binding t)
              (call-interactively binding)
            (execute-kbd-macro binding))
        (user-error "God: Unknown key binding for `%s'"
                    (concat (cdr (assq nil god-mod-alist)) "u"))))))

(define-key universal-argument-map (kbd "u")
  #'god-mode-maybe-universal-argument-more)

;; god-mode-k is the state machine for partially entered key sequence.
(cl-defstruct god-mode-k
  key                                ; next key to be processed
  modifier                           ; modifier applicable to key
  prefix                             ; already confirmed key prefix
  binding                            ; binding for current prefix
  trace                              ; a trace of entered keys as string
  literal)                           ; if literal key was pressed

(defun god-mode--maybe-local-binding (initial-key)
  "Return a local binding when INITIAL-KEY is low priority."
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
         (sanitized-key (god-mode-sanitized-key-string initial-key))
         (binding (or (god-mode--maybe-local-binding initial-key)
                      (god-mode-read-command
                       (make-god-mode-k
                        :key sanitized-key :trace sanitized-key)))))
    (when (god-mode-upper-p initial-key)
      (setq this-command-keys-shift-translated t))
    (setq this-original-command binding)
    (setq this-command binding)
    ;; `real-this-command' is used by emacs to populate
    ;; `last-repeatable-command', which is used by `repeat'.
    (setq real-this-command binding)
    (if (commandp binding t)
        (call-interactively binding)
      (execute-kbd-macro binding))))

(defun god-mode-upper-p (char)
  "Is the given CHAR upper case?"
  (and (>= char ?A)
       (<= char ?Z)
       (/= char ?G)))

(defun god-mode-k-regular-key-string (k)
  "Return regular key string interpretation for K.

If this interpretation makes sense, then :binding is set."
  (let* ((key-string
          (if (god-mode-k-prefix k)
              (concat (god-mode-k-prefix k) " "
                      (god-mode-k-modifier k)
                      (god-mode-k-key k))
            (concat (god-mode-k-modifier k)
                    (god-mode-k-key k))))
         (translated-key-string (god-mode-maybe-translate k key-string))
         (key-binding (key-binding (read-kbd-macro translated-key-string t))))
    (when key-binding
      (setf (god-mode-k-prefix k) translated-key-string)
      (setf (god-mode-k-modifier k) nil)
      (setf (god-mode-k-key k) nil)
      (setf (god-mode-k-binding k) key-binding))
    translated-key-string))

(defun god-mode-k-sanitized-read (k)
  "Maybe read a keystroke for K."
  (unless (god-mode-k-key k)
    (let* ((sanitized-key
            (god-mode-sanitized-key-string
             (read-event (god-mode-k-prefix k)))))
      (setf (god-mode-k-key k) sanitized-key)
      (setf (god-mode-k-trace k)
            (concat (god-mode-k-trace k) " " sanitized-key))
      (setf (god-mode-k-binding k) nil)
      (setf (god-mode-k-modifier k) nil))))

(defun god-mode-describe-key ()
  "Describe key for god-mode sequences."
  (interactive)
  (let* ((initial-key (read-event "Describe key (God): "))
         (local-binding (god-mode--maybe-local-binding initial-key))
         (sanitized-key (god-mode-sanitized-key-string initial-key))
         (k (make-god-mode-k
             :key sanitized-key :trace sanitized-key)))
    (if local-binding
        (progn (describe-function local-binding)
          (message "Low priority binding found for %s" sanitized-key))
      (god-mode-read-command k)
      (describe-key
       (read-kbd-macro (god-mode-k-prefix k) 't)
       (read-kbd-macro (god-mode-k-trace k) 't)))))

(defun god-mode-read-command (k)
  "Read command for K, until a command is found."
  (while (let ((binding (god-mode-k-binding k)))
           (or (not binding) (keymapp binding)))
    (god-mode-k-read-one-key k))
  (when (commandp (god-mode-k-binding k))
    (setq last-command-event (god-mode-k-last-key k)))
  (god-mode-k-binding k))

(defun god-mode-k-read-one-key (k)
  "Lookup the command for K by reading one more (combo) key."
  (interactive)
  (god-mode-k-sanitized-read k)
  (god-mode-interpret-k k)

  (let* ((key-string (god-mode-k-regular-key-string k)))
    (or (god-mode-k-binding k)
        (god-mode--maybe-omit-literal-key k)
        (god-mode--maybe-help k))
    (unless (god-mode-k-binding k)
      (user-error "God: Unknown key binding for `%s'" key-string))))

(defun god-mode--maybe-omit-literal-key (k)
  "Return an alternative key string of K by assuming
  `god-literal-key' was pressed right before the last keystroke."
  (when (and god-mode-can-omit-literal-key
             (god-mode-k-prefix k))
    (let* ((alt-key-string
            (concat (god-mode-k-prefix k) " " (god-mode-k-key k)))
           (key-binding (key-binding (read-kbd-macro alt-key-string t))))
      (when key-binding
        (setf (god-mode-k-literal k) 't)
        (setf (god-mode-k-prefix k) alt-key-string)
        (setf (god-mode-k-modifier k) nil)
        (setf (god-mode-k-key k) nil)
        (setf (god-mode-k-binding k) key-binding)
        alt-key-string))))

(defun god-mode--maybe-help (k)
  "Maybe trigger key binding help for K.

If :key is not `help-char', then return nil."
  (let ((prefix (god-mode-k-prefix k)))
    (when (and prefix
               (string= (char-to-string help-char) (god-mode-k-key k)))
      (describe-bindings (read-kbd-macro prefix))
      (setf (god-mode-k-binding k) #'ignore)
      (setf (god-mode-k-prefix k) (concat prefix " " (god-mode-k-key k)))
      (setf (god-mode-k-modifier k) nil)
      (setf (god-mode-k-key k) nil)
      k)))

(defun god-mode-sanitized-key-string (key)
  "Convert any special KEY to textual.

This should be the inverse of `read-kbd-macro' for a single key."
  (cond
   ((eq key 'tab) "TAB")
   ((eq key ?\ ) "SPC")
   ((eq key 'backspace) "DEL")
   ((eq key 'return) "RET")
   ((symbolp key)
    (replace-regexp-in-string "\\([^ -]+\\)$" "<\\1>"
                              (symbol-name key)))
   (t (char-to-string key))))

(defun god-mode-interpret-k (k)
  "Interpret god-mode special keys for key (consumes more keys if
appropriate). Returns a list of (prefix modifier key)."
  (let ((key (god-mode-k-key k))
        (next-modifier "") next-key)
    (cond
     ;; Don't check for god-literal-key with the first key
     ((and (god-mode-k-prefix k) (string= key god-literal-key))
      (setf (god-mode-k-literal k) 't)
      (setf (god-mode-k-key k) nil))
     ((god-mode-k-literal k))         ;do nothing, key is not consumed
     ((and (stringp key) (assoc key god-mod-alist))
      (setf (god-mode-k-key k) nil)
      (setq next-modifier (cdr (assoc key god-mod-alist))))
     (t
      (setq next-modifier (cdr (assoc nil god-mod-alist)))))
    (god-mode-k-sanitized-read k)
    (setq next-key (god-mode-k-key k))
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
    (setf (god-mode-k-modifier k) next-modifier)
    k))

(defun god-mode-maybe-translate (k key-string)
  "Translate KEY-STRING according to `god-mode-translate-alist'."
  (let ((translation (cdr (assoc key-string god-mode-translate-alist))))
    (if (not translation)
        key-string
      (setf (god-mode-k-literal k) (cadr translation))
      (car translation))))

(defun god-mode-k-last-key (k)
  "Return last key character for K."
  (let* ((key-string (god-mode-k-prefix k))
         (key-vector (read-kbd-macro key-string t)))
    (aref key-vector (- (length key-vector) 1))))

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
