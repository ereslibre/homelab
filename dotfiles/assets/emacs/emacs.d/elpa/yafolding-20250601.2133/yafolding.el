;;; yafolding.el --- Folding code blocks based on indentation  -*- lexical-binding:t -*-

;; Copyright (C) 2013-2017 Zeno Zeng

;; Author: Zeno Zeng <zenoofzeng@gmail.com>
;; Package-Version: 20250601.2133
;; Package-Revision: 77d36147a07d
;; Package-Requires: ((emacs "28.1"))
;; Keywords: folding

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

;; Folding code blocks based on indentation.

;;; Code:

(defgroup yafolding nil
  "Fold code blocks based on indentation level."
  :prefix "yafolding-"
  :group 'applications)

(defface yafolding-ellipsis-face '((t))
  "Face for folded blocks."
  :group 'yafolding)

(defcustom yafolding-ellipsis-content "..."
  "Text to show in place of a folded block."
  :tag "Ellipsis"
  :type 'string
  :group 'yafolding)

(defcustom yafolding-show-fringe-marks t
  "Whether to show fold markers in the fringe."
  :tag "Show fringe marks?"
  :type 'boolean
  :group 'yafolding)

(defun yafolding-get-overlays (beg end)
  "Get all overlays between BEG and END."
  (seq-filter (lambda (ov) (eq (overlay-get ov 'category) 'yafolding))
              (overlays-in beg end)))

(defun yafolding-should-ignore-current-line-p ()
  "Return t if the current line should be ignored."
  (string-match-p "^[ \t]*$"
                  (buffer-substring-no-properties
                   (line-beginning-position)
                   (line-end-position))))

(defun yafolding-get-indent-level ()
  "Get the indent level of the current line."
  (interactive)
  (if (and (yafolding-should-ignore-current-line-p)
           (< (line-number-at-pos) (line-number-at-pos (point-max))))
      (save-excursion
        (forward-line 1)
        (yafolding-get-indent-level))
    (let ((indent-level 0)
          (last-indentation (current-indentation)))
      (save-excursion
        (while (and (> (current-indentation) 0)
                    (> (line-number-at-pos) 1))
          (forward-line -1)
          (when (< (current-indentation) last-indentation)
            (setq last-indentation (current-indentation))
            (setq indent-level (+ 1 indent-level)))))
      indent-level)))

(defun yafolding-show-region (beg end)
  "Delete all yafolding overlays between BEG and END."
  (mapcar #'delete-overlay (yafolding-get-overlays beg end)))

(defun yafolding-show-all ()
  "Delete all yafolding overlays."
  (interactive)
  (yafolding-show-region (point-min) (point-max)))

(defun yafolding-hide-all (&optional indent-level)
  "Hide all elements based on INDENT-LEVEL."
  (interactive)
  (unless indent-level
    (setq indent-level (yafolding-get-indent-level)))
  (save-excursion
    (goto-char (point-min))
    (while (< (line-number-at-pos)
              (line-number-at-pos (point-max)))
      (if (and (= (yafolding-get-indent-level) indent-level)
               (not (yafolding-should-ignore-current-line-p)))
          (yafolding-hide-element))
      (forward-line 1))))

(defun yafolding-toggle-all (&optional indent-level)
  "Toggle folding of the entire file.

If given, toggle all entries that start at INDENT-LEVEL."
  (interactive)
  (unless indent-level
    (setq indent-level (yafolding-get-indent-level)))
  (if (yafolding-get-overlays (point-min) (point-max))
      (yafolding-show-all)
    (yafolding-hide-all indent-level)))

(defun yafolding-ellipsis ()
  "Return propertized ellipsis content."
  (concat " "
          (propertize yafolding-ellipsis-content 'face 'yafolding-ellipsis-face)
          " "))

(defun yafolding-hide-region (beg end)
  "Hide region between BEG and END."
  (when (> end beg)
    (yafolding-show-region beg end)
    (let ((before-string
           (concat
            (and yafolding-show-fringe-marks
                 (propertize " " 'display '(left-fringe right-triangle)))
            (yafolding-ellipsis)))
          (new-overlay (make-overlay beg end)))
      (overlay-put new-overlay 'invisible t)
      (overlay-put new-overlay 'intangible t)
      (overlay-put new-overlay 'evaporate t)
      (overlay-put new-overlay 'modification-hooks
                   (list (lambda (overlay &rest _)
                           (delete-overlay overlay))))
      (overlay-put new-overlay 'before-string before-string)
      (overlay-put new-overlay 'category 'yafolding))))

(defun yafolding-debug ()
  "Show yafolding information of the current position."
  (interactive)
  (pcase-let ((`(,beg ,end)) (yafolding-get-element-region))
    (message "indentation: %d, indent level: %d, ingore current line: %s, \
element-region: %d - %d, (L%d - L%d)"
             (current-indentation)
             (yafolding-get-indent-level)
             (yafolding-should-ignore-current-line-p)
             beg end
             (line-number-at-pos beg)
             (line-number-at-pos end))))

(defun yafolding-get-element-region ()
  "Return (BEG END) of current element."
  (let ((beg (line-end-position))
        (end (line-end-position))
        (indentation (current-indentation)))
    (save-excursion
      (forward-visible-line 1)
      (while (and (< (line-number-at-pos)
                     (line-number-at-pos (point-max)))
                  (or (> (current-indentation) indentation)
                      (yafolding-should-ignore-current-line-p)))
        (unless (yafolding-should-ignore-current-line-p)
          (setq end (line-end-position)))
        (forward-visible-line 1)))
    (list beg end)))

(defun yafolding-hide-element ()
  "Hide current element."
  (interactive)
  (apply #'yafolding-hide-region (yafolding-get-element-region)))

(defun yafolding-show-element ()
  "Show current element."
  (interactive)
  (yafolding-show-region (line-beginning-position)
                         (+ 1 (line-end-position))))

(defun yafolding-toggle-element ()
  "Toggle current element."
  (interactive)
  (if (yafolding-get-overlays (line-beginning-position)
                              (+ 1 (line-end-position)))
      (yafolding-show-element)
    (yafolding-hide-element)))

(add-hook 'isearch-mode-hook
          (lambda ()
            (mapcar (lambda (overlay)
                      (overlay-put overlay 'invisible nil))
                    (yafolding-get-overlays (point-min) (point-max)))))

(add-hook 'isearch-mode-end-hook
          (lambda ()
            (mapcar (lambda (overlay)
                      (overlay-put overlay 'invisible t))
                    (yafolding-get-overlays (point-min) (point-max)))))

(defun yafolding-go-parent-element ()
  "Go back to parent element."
  (interactive)
  (re-search-backward (concat "^.\\{,"
                              (number-to-string (- (current-indentation) 1))
                              "\\}[^ \t]+")))

(defun yafolding-hide-parent-element ()
  "Hide the parent element."
  (interactive)
  (ignore-errors
    (yafolding-go-parent-element)
    (yafolding-hide-element)))

;;;###autoload
(defvar yafolding-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<C-S-return>") #'yafolding-hide-parent-element)
    (define-key map (kbd "<C-M-return>") #'yafolding-toggle-all)
    (define-key map (kbd   "<C-return>") #'yafolding-toggle-element)
    map))

;;;###autoload
(define-minor-mode yafolding-mode
  "Toggle yafolding mode."
  :keymap yafolding-mode-map)

(provide 'yafolding)
;;; yafolding.el ends here
