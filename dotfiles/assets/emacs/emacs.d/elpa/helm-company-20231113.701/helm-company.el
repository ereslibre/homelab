;;; helm-company.el --- Helm interface for company-mode

;; Copyright (C) 2013 Yasuyuki Oka <yasuyk@gmail.com>

;; Author: Yasuyuki Oka <yasuyk@gmail.com>
;; Maintainer: Daniel Ralston <Sodel-the-Vociferous@users.noreply.github.com>
;; Package-Version: 20231113.701
;; Package-Revision: 4622b8235322
;; URL: https://github.com/Sodel-the-Vociferous/helm-company
;; Package-Requires: ((helm "1.5.9") (company "0.10.0"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Add the following to your Emacs init file:
;;
;; (autoload 'helm-company "helm-company") ;; Not necessary if using ELPA package
;; (eval-after-load 'company
;;   '(progn
;;      (define-key company-mode-map (kbd "C-:") 'helm-company)
;;      (define-key company-active-map (kbd "C-:") 'helm-company)))

;;; Code:

(require 'cl-lib)
(require 'helm)
(require 'helm-multi-match)
(require 'helm-files)
(require 'helm-elisp) ;; For with-helm-show-completion
(require 'company)
(require 'subr-x)

(defgroup helm-company nil
  "Helm interface for company-mode."
  :prefix "helm-company-"
  :group 'helm)

(defcustom helm-company-candidate-number-limit 300
  "Limit candidate number of `helm-company'.

Set it to nil if you don't want this limit."
  :group 'helm-company
  :type '(choice (const :tag "Disabled" nil) integer))

(defcustom helm-company-show-annotations t
  "Show annotations provided by company-backend when completing.

Annotations will be formatted in the `company-tooltip-annotation'
face."
  :group 'helm-company
  :type 'boolean )

(defcustom helm-company-show-icons t
  "Show icons provided by company-backend when completing.

Set it to `nil' if you want to hide the icons."
  :group 'helm-company
  :type 'boolean )

(defcustom helm-company-initialize-pattern-with-prefix nil
  "Use the thing-at-point as the initial helm completion pattern.

The thing-at-point is whatever partial thing you've typed that
you're trying to complete."
  :group 'helm-company
  :type 'boolean)

(defcustom helm-company-after-completion-hooks nil
  "Hook run after helm-company has inserted the selected completion candidate."
  :type 'hook
  :group 'helm-company)

(defcustom helm-company-fuzzy-match t
  "Enable fuzzy matching for Helm Company."
  :type 'boolean
  :group 'helm-company)

(defvar helm-company-help-window nil)
(defvar helm-company-candidates nil)
(defvar helm-company-backend nil)
(defvar helm-company-display-candidates-hash nil
  "A hash table.

KEYS: Candidate display strings with no properties.

VALUES: (FORMATTED-DISPLAY . COMPANY-CANDIDATE) pairs.
COMPANY-CANDIDATE is the candidate string exactly as provided by
company-backend (properties and all); FORMATTED-DISPLAY is the
formatted display string (with font-lock properties) of
COMPANY-CANDIDATE, for Helm to display.

Some completion backends use string properties to store and
retrieve annotation data. Helm strips all properties off before
completion, which may break this feature. So, the original
strings provided by company-backend are stored here, so they can
be retrieved and passed to company-backend when asking for
annotations.")

(defun helm-company-call-backend (&rest args)
  "Bridge between helm-company and company"
  (let ((company-backend helm-company-backend))
    (apply 'company-call-backend args)))

(defun helm-company-init ()
  "Prepare helm for company."
  (helm-attrset 'company-candidates company-candidates)
  (helm-attrset 'company-common company-common)
  (helm-attrset 'company-prefix company-prefix)
  (helm-attrset 'company-backend company-backend)
  (setq helm-company-help-window nil)
  (if (< (length company-candidates) 1)
      (helm-exit-minibuffer)
    (setq helm-company-backend                 company-backend
          helm-company-candidates              company-candidates
          helm-company-display-candidates-hash (helm-company--make-display-candidate-hash company-candidates))))

(defun helm-company-cleanup-post-command ()
  (helm-attrset 'company-candidates nil)
  (setq helm-company-backend             nil
        helm-company-candidates          nil
        helm-company-display-candidates-hash nil))

(defun helm-company-action-insert (candidate)
  "Insert CANDIDATE."
  ;; `company-manual-begin' keeps company from throwing an error in
  ;; `company-post-command', its post-command hook.
  (when (company-manual-begin)
    (company-finish candidate)
    (run-hooks 'helm-company-after-completion-hooks)))

(defun helm-company-action-show-document (candidate)
  "Show the documentation of the CANDIDATE."
  (interactive)
  (let* ((selection (cl-find-if (lambda (s) (string-match-p candidate s)) helm-company-candidates))
         (buffer (helm-company-call-backend 'doc-buffer selection)))
    (when buffer
      (display-buffer buffer))))

(defun helm-company-show-doc-buffer (candidate)
  "Temporarily show the documentation buffer for the CANDIDATE."
  (interactive)
  (let* ((selection (cl-find-if (lambda (s) (string-match-p candidate s)) helm-company-candidates))
         (buffer (helm-company-call-backend 'doc-buffer selection)))
    (when buffer
      (if (and helm-company-help-window
               (window-live-p helm-company-help-window))
          (with-selected-window helm-company-help-window
            (helm-company-display-document-buffer buffer))
        (setq helm-company-help-window
              (helm-company-display-document-buffer buffer))))))

(defun helm-company-find-location (candidate)
  "Find location of CANDIDATE."
  (interactive)
  (let* ((selection (cl-find-if (lambda (s) (string-match-p candidate s)) helm-company-candidates))
         (location (save-excursion (helm-company-call-backend 'location selection)))
         (pos (or (cdr location) (error "No location available")))
         (buffer (or (and (bufferp (car location)) (car location))
                     (find-file-noselect (car location) t))))
    (with-selected-window (display-buffer buffer t)
      (save-restriction
        (widen)
        (if (bufferp (car location))
            (goto-char pos)
          (goto-char (point-min))
          (forward-line (1- pos))))
      (set-window-start nil (point)))))

(defun helm-company-display-document-buffer (buffer)
  "Temporarily show the documentation BUFFER."
  (with-current-buffer buffer
    (goto-char (point-min)))
  (let ((display-buffer-alist '((".*" display-buffer-in-side-window)
                                (".*" display-buffer-reuse-window))))
    (display-buffer buffer t)))

(defmacro helm-company-run-action (&rest body)
  `(with-helm-window
     (save-selected-window
       ;; `with-helm-display-same-window' has been removed from recent helm
       ;; versions.
       (if (fboundp 'with-helm-display-same-window)
           (with-helm-display-same-window
            ,@body)
         ,@body)

       ;; For GC
       (setq helm-company--display-candidates-hash nil))))

(defun helm-company-run-show-doc-buffer ()
  "Run showing documentation action from `helm-company'."
  (interactive)
  (helm-company-run-action
   (helm-company-show-doc-buffer (helm-get-selection))))

(defun helm-company-run-show-location ()
  "Run showing location action from `helm-company'."
  (interactive)
  (helm-company-run-action
   (helm-company-find-location (helm-get-selection))))

(defun helm-company--propertize-annotation (str)
  (let ((str (concat str)))             ; Copy the string
    (put-text-property 0 (length str) 'font-lock-face 'company-tooltip-annotation
                       str)
    str))

(defun helm-company--make-display-string (candidate annotation)
  (let ((candidate (substring-no-properties candidate))
        (icon (if helm-company-show-icons
                  (funcall company-format-margin-function candidate nil)
                "")))
    (if (null annotation)
        (concat icon candidate)
      (concat icon candidate " "
              (helm-company--propertize-annotation annotation)))))

(defun helm-company--get-annotations (candidate)
  "Return the annotation (if any) supplied for a candidate by
company-backend."
  (let ((annot (company-call-backend 'annotation candidate)))
    (if (null annot)
        nil
      (helm-company--clean-string annot))))

(defun helm-company--make-display-candidate-hash (candidates)
  (let ((hash (make-hash-table :test 'equal :size 1000)))
    (cl-loop for candidate in candidates
             for annot = (helm-company--get-annotations candidate)
             for display-str = (helm-company--make-display-string candidate annot)
             for key = (substring-no-properties display-str)
             do (puthash key (cons display-str candidate) hash))
    hash))

(defun helm-company-get-display-strings ()
  (let ((sort (null helm--in-fuzzy))
        (display-strs (hash-table-keys helm-company-display-candidates-hash)))
    (if sort
        (sort display-strs #'helm-generic-sort-fn)
      display-strs)))

(defun helm-company-get-real-candidate (display-str)
  (let ((display-str (substring-no-properties display-str)))
    (cdr (gethash display-str helm-company-display-candidates-hash))))

(defun helm-company-get-formatted-display-strings (display-strs &optional _)
  (let ((display-strs (mapcar 'substring-no-properties display-strs)))
    (mapcar (lambda (display-str) (car (gethash display-str helm-company-display-candidates-hash)))
            display-strs)))

;; Taken verbatim from `company--clean-string'. I don't use that function
;; directly because it's a private function inside `company', so I can't rely on
;; it. I have copied it here.
(defun helm-company--clean-string (str)
  (replace-regexp-in-string
   "\\([^[:graph:] ]\\)\\|\\(\ufeff\\)\\|[[:multibyte:]]"
   (lambda (match)
     (cond
      ((match-beginning 1)
       ;; FIXME: Better char for 'non-printable'?
       ;; We shouldn't get any of these, but sometimes we might.
       "\u2017")
      ((match-beginning 2)
       ;; Zero-width non-breakable space.
       "")
      ((> (string-width match) 1)
       (concat
        (make-string (1- (string-width match)) ?\ufeff)
        match))
      (t match)))
   str))


(defvar helm-company-map
  (let ((keymap (make-sparse-keymap)))
    (set-keymap-parent keymap helm-map)
    (define-key keymap (kbd "M-s") 'helm-company-run-show-location)
    (define-key keymap (kbd "C-s") 'helm-company-run-show-doc-buffer)
    (delq nil keymap))
  "Keymap used in Company sources.")

(defvar helm-company-actions
  '(("Insert" . helm-company-action-insert)
    ("Show documentation (If available)" . helm-company-action-show-document)
    ("Find location (If available)" . helm-company-find-location))
  "Actions for `helm-company'.")

(defvar helm-source-company
  (helm-build-in-buffer-source "Company"
    :data (lambda ()
            (helm-company-init)
            (helm-company-get-display-strings))
            ;(list "position -> int"))
    :filtered-candidate-transformer 'helm-company-get-formatted-display-strings
    :display-to-real 'helm-company-get-real-candidate
    ;; No :cleanup. It is executed before the action, and we still need the
    ;; vars. Cleanup can be added later in a one-shot post-command-hook function
    ;; to cleanup helm-company vars and remove itself from the hook.
    :fuzzy-match helm-company-fuzzy-match
    :keymap helm-company-map
    :persistent-action 'helm-company-show-doc-buffer
    :persistent-help "Show documentation (If available)"
    :action helm-company-actions)
  ;; "Helm source definition for recent files in current project."
  )

;;;###autoload
(defun helm-company ()
  "Select `company-complete' candidates by `helm'.
It is useful to narrow candidates."
  (interactive)

  (cond ((and company-candidates
              (= 1 (length company-candidates)))
         ;; If there is exactly one company candidate, insert it without running
         ;; helm. (FIXES #20).
         (company-complete))
        (t
         (unless company-candidates
           ;; If there are no candidates yet, have company generate them for us.
           ;; Otherwise, running this function a second time would not work as
           ;; desired.
           (company-complete))
         (when company-common
           ;; Work around a quirk with company. `company-complete' inserts the common
           ;; part of all candidates into the buffer. But, it doesn't update
           ;; `company-prefix' -- and `company-prefix' is all `company-finish' replaces
           ;; in the buffer. (issue #9)
           (setq company-prefix company-common))
         (let ((initial-pattern (and helm-company-initialize-pattern-with-prefix
                                     company-prefix))
               ;; Hook to abort company completion & hide company frontend if we
               ;; keyboard-quit (C-g) out of `helm-company'.
               (helm-quit-hook (cons 'company-abort helm-quit-hook)))
           (when company-point
             ;; There SHOULD be a company-point if company-complete was run. This
             ;; is from some old code and I'm not bold enough to delete this
             ;; conditional.
             (helm :sources 'helm-source-company
                   :buffer  "*helm company*"
                   :input initial-pattern
                   :candidate-number-limit helm-company-candidate-number-limit))))))

(provide 'helm-company)

;; Local Variables:
;; coding: utf-8
;; eval: (setq byte-compile-not-obsolete-vars '(display-buffer-function))
;; eval: (checkdoc-minor-mode 1)
;; End:

;;; helm-company.el ends here
