;;; path-complete.el --- Basic path completion       -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Renan Fonseca

;; Author: Renan Fonseca <renanfonseca@gmail.com>
;; Keywords: convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Use elisp default path completion functions to provide path completion
;; functionality. A minor mode is provided in order to toggle path completion.

;;; Code:


(defun get-file-completions (path)
 (let ((file (file-name-nondirectory path))
       (dir (or (file-name-directory path) "")))
   (mapcar (lambda (i) (concat dir i )) (file-name-all-completions file dir))))


(defun complete-file-name ()
  (ignore-errors
    (let* ((bounds (bounds-of-thing-at-point 'filename))
           (start (car bounds))
           (end (cdr bounds))
           (thing (buffer-substring-no-properties start end))
           (collection (get-file-completions thing)))
      (when collection
        (list start end collection :exclusive 'no)))))


(define-minor-mode complete-path-mode
  "Path completion")

(defun complete-path-hook ()
  (if complete-path-mode
      (add-hook 'completion-at-point-functions 'complete-file-name 0 t)
      (remove-hook 'completion-at-point-functions 'complete-file-name t)))

(add-hook 'complete-path-mode-hook 'complete-path-hook)


(provide 'path-complete)
;;; path-complete.el ends here
