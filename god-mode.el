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
    (?g . "M-")
    (?G . "C-M-"))
  "List of keys and their associated modifer."
  :group 'god
  :type '(alist :key-type character :value-type string))

(defcustom god-literal-key
  ?\  "The key used for literal interpretation."
  :group 'god
  :type 'character)

(defcustom god-mode-can-omit-literal-key
  nil
  "When non-nil, user can omit the literal key when unambiguous."
  :group 'god
  :type 'boolean)

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
  :type '(alist :key-type string))

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
  :type 'boolean)

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
  :type '(repeat symbol))

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
                    (god-mode--k-init ?u 't))))
      (if binding
          (if (commandp binding t)
              (call-interactively binding)
            (execute-kbd-macro binding))
        (user-error "God: Unknown key binding for `%s'"
                    (concat (cdr (assq nil god-mod-alist)) "u"))))))

(define-key universal-argument-map (kbd "u")
  #'god-mode-maybe-universal-argument-more)

;; god-mode--k is the state machine for partially entered key sequence.
(cl-defstruct god-mode--k
  key                                ; next key to be processed
  modifier                           ; modifier applicable to key
  prefix                             ; already confirmed key prefix
  binding                            ; binding for current prefix
  trace                              ; a trace of entered keys as string
  literal)                           ; if literal key was pressed

(setq god-mode--current-state nil)
(defun god-mode--k-init (&optional initial-key literal)
  (if (not initial-key)
      (setq god-mode--current-state
            (make-god-mode--k :literal literal))
    (let ((sanitized-key (single-key-description initial-key)))
      (setq god-mode--current-state
            (make-god-mode--k :key initial-key :trace sanitized-key :literal literal)))))

(defun god-mode--maybe-local-binding (k)
  "Return a local binding when K is low priority."
  (let ((key (god-mode--k-key k)))
  (when (or god-mode-is-low-priority
            (memq key god-mode-low-priority-keys))
    (let ((binding (local-key-binding (char-to-string key))))
      (cond
       ((and binding
             (commandp binding t)
             (not (memq binding god-mode-low-priority-exempt))
             (not (eq binding 'god-mode-self-insert)))
        binding)
       ;;
       ((keymapp binding)
        (set-transient-map binding)
        'ignore))))))

(defun god-mode-self-insert ()
  "Handle self-insert keys."
  (interactive)
  (let* ((initial-key (aref (this-command-keys-vector)
                            (- (length (this-command-keys-vector)) 1)))
         (k (god-mode--k-init initial-key))
         (binding (or (god-mode--maybe-local-binding k)
                      (god-mode-read-command k))))
    (setq this-original-command binding)
    (setq this-command binding)
    ;; `real-this-command' is used by emacs to populate
    ;; `last-repeatable-command', which is used by `repeat'.
    (setq real-this-command binding)
    (if (commandp binding t)
        (call-interactively binding)
      (execute-kbd-macro binding))))

(defun god-mode-describe-key (initial-key)
  "Describe key for god-mode sequences starting with INITIAL-KEY."
  (interactive (list (read-key "Press key: ")))
    (if (eq (key-binding (vector initial-key)) 'god-mode-self-insert)
        (let* ((k (god-mode--k-init initial-key))
               (local-binding (god-mode--maybe-local-binding k))
               (sanitized-key (single-key-description initial-key)))
          (if local-binding
              (if (eq local-binding 'ignore)
                  (message "Local binding is a prefix key for %s" sanitized-key)
                (describe-function local-binding)
                (message "Local binding found for %s" sanitized-key))
            (god-mode-read-command k)
            (describe-key
             (read-kbd-macro (god-mode--k-prefix k) 't)
             (read-kbd-macro (god-mode--k-trace k) 't))))
      ;; initial-key not bound to god-mode-self-insert, call regular
      ;; describe-key
      (setq unread-command-events (cons initial-key unread-command-events))
      (call-interactively #'describe-key)))

(defun god-mode--k-regular-key-string (k)
  "Return regular key string interpretation for K.

If this interpretation makes sense, then :binding is set."
  (let* ((key-string
          (if (god-mode--k-prefix k)
              (concat (god-mode--k-prefix k) " "
                      (god-mode--k-modifier k)
                      (single-key-description (god-mode--k-key k)))
            (concat (god-mode--k-modifier k)
                    (single-key-description (god-mode--k-key k)))))
         (translated-key-string (god-mode-maybe-translate k key-string))
         (key-binding (key-binding (read-kbd-macro translated-key-string t))))
    (when key-binding
      (setf (god-mode--k-prefix k) translated-key-string)
      (setf (god-mode--k-modifier k) nil)
      (setf (god-mode--k-key k) nil)
      (setf (god-mode--k-binding k) key-binding))
    translated-key-string))

(defun god-mode--k-sanitized-read (k)
  "Maybe read a keystroke for K."
  (unless (god-mode--k-key k)
    (let* ((key (read-key (god-mode--k-prefix k)))
           (sanitized-key (single-key-description key)))
      (setf (god-mode--k-key k) key)
      (setf (god-mode--k-trace k)
            (concat (god-mode--k-trace k) " " sanitized-key))
      (setf (god-mode--k-binding k) nil)
      (setf (god-mode--k-modifier k) nil))))

(defun god-mode-read-command (k)
  "Read command for K, until a command is found."
  (while (let ((binding (god-mode--k-binding k)))
           (or (not binding) (keymapp binding)))
    (god-mode--k-read-one-key k))
  (when (commandp (god-mode--k-binding k))
    (setq last-command-event (god-mode--k-last-key k)))
  (god-mode--k-binding k))

(defun god-mode--k-read-one-key (k)
  "Lookup the command for K by reading one more (combo) key."
  (interactive)
  (god-mode--k-sanitized-read k)
  (god-mode-interpret-k k)

  (let* ((key-string (god-mode--k-regular-key-string k)))
    (or (god-mode--k-binding k)
        (god-mode--maybe-shift-translate k)
        (god-mode--maybe-omit-literal-key k)
        (god-mode--maybe-help k))
    (unless (god-mode--k-binding k)
      (user-error "God: Unknown key binding for `%s'" key-string))))

(defun god-mode--maybe-shift-translate (k)
  "Maybe remove the shift key if the regular binding does not work.

Currently only trigger for first key. See info node for
 `read-key-sequence' for how that is handled in regular emacs."
  (when (not (god-mode--k-prefix k))
    (let* ((modifier (god-mode--k-modifier k))
           (shift-removed (replace-regexp-in-string "S-" "" modifier))
           alt-key-string
           key-binding)
      (when (and (not (string= modifier shift-removed))
                 (setq alt-key-string
                       (concat shift-removed
                               (single-key-description (god-mode--k-key k))))
                 (setq key-binding (key-binding (read-kbd-macro alt-key-string t)))
                 (commandp key-binding))
        (setq this-command-keys-shift-translated t)
        (setf (god-mode--k-prefix k) alt-key-string)
        (setf (god-mode--k-modifier k) nil)
        (setf (god-mode--k-key k) nil)
        (setf (god-mode--k-binding k) key-binding)
        alt-key-string))))

(defun god-mode--maybe-omit-literal-key (k)
  "Maybe interpret K as user omitted the `god-literal-key'.

When the alternative sequence is bound to something, set :binding
and :literal for K. Consume :key by setting it nil."
  (when (and god-mode-can-omit-literal-key
             (god-mode--k-prefix k))
    (let* ((alt-key-string
            (concat (god-mode--k-prefix k) " " (single-key-description (god-mode--k-key k))))
           (key-binding (key-binding (read-kbd-macro alt-key-string t))))
      (when key-binding
        (setf (god-mode--k-literal k) 't)
        (setf (god-mode--k-prefix k) alt-key-string)
        (setf (god-mode--k-modifier k) nil)
        (setf (god-mode--k-key k) nil)
        (setf (god-mode--k-binding k) key-binding)
        alt-key-string))))

(defun god-mode--maybe-help (k)
  "Maybe trigger key binding help for K.

If :key is not `help-char', then return nil."
  (let ((prefix (god-mode--k-prefix k))
        (key (god-mode--k-key k)))
    (when (and prefix
               (or (eq help-char key)
                   (memq key help-event-list)))
      (describe-bindings (read-kbd-macro prefix))
      (setf (god-mode--k-binding k) #'ignore)
      (setf (god-mode--k-prefix k) (concat prefix " " (single-key-description key)))
      (setf (god-mode--k-modifier k) nil)
      (setf (god-mode--k-key k) nil)
      k)))

(defun god-mode-interpret-k (k)
  "Interpret god-mode special keys for K.

Consumes more keys if needed."
  (let ((key (god-mode--k-key k))
        (next-modifier "")
        next-key)
    (cond
     ;; Don't check for god-literal-key with the first key
     ((and (god-mode--k-prefix k) (eq key god-literal-key))
      (setf (god-mode--k-literal k) 't)
      (setf (god-mode--k-key k) nil))
     ((god-mode--k-literal k))         ;do nothing, key is not consumed
     ((assq key god-mod-alist)
      (setf (god-mode--k-key k) nil)
      (setq next-modifier (cdr (assq key god-mod-alist))))
     (t
      (setq next-modifier (cdr (assq nil god-mod-alist)))))
    (god-mode--k-sanitized-read k)
    (setq next-key (god-mode--k-key k))
    (when (and (memq 'shift (event-modifiers next-key))
               ;; If C- is part of the modifier, S- needs to be given
               ;; in order to distinguish the uppercase from the
               ;; lowercase bindings. If C- is not in the modifier,
               ;; then emacs natively treats uppercase differently
               ;; from lowercase, and the S- modifier should not be
               ;; given
               (string-prefix-p "C-" next-modifier))
      (setq next-modifier (concat next-modifier "S-")))
    (setf (god-mode--k-modifier k) next-modifier)
    k))

(defun god-mode-maybe-translate (k key-string)
  "Translate KEY-STRING according to `god-mode-translate-alist'.

If translation was performed, also set :literal for K according
to the setting."
  (let ((translation (cdr (assoc key-string god-mode-translate-alist))))
    (if (not translation)
        key-string
      (setf (god-mode--k-literal k) (cadr translation))
      (car translation))))

(defun god-mode--k-last-key (k)
  "Return last key character for K."
  (let* ((key-string (god-mode--k-prefix k))
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
