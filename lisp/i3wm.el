;;; i3wm.el --- i3wm and emacs together              -*- lexical-binding: t; -*-

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

;;

;;; Code:



(defun window-to-frame (w)
  (let ((b (window-buffer w)))
    (with-current-buffer b
      (make-frame))
    (delete-window w)))


(defun spread-into-frames ()
  (interactive)
  (let ((cf (selected-frame))
        (cw (selected-window)))
    (progn
      (dolist (w (window-list))
        (unless (equal w cw)
          (window-to-frame w)))
      (x-focus-frame cf))))


(defun i3-exec (&rest args)
  (let ((cmd (mapconcat 'identity args " ")))
    (make-process
     :name "i3-emacs-term"
     :buffer "i3-shell"
     :command `("i3-msg" "-s" ,(expand-file-name "~/.i3/i3-ipc.sock") "exec" ,cmd))))

(defun i3-terminal ()
  (interactive)
  (i3-exec "kitty" "--working-directory" default-directory))

(defun kitty-send-buffer ()
  (interactive)
  (async-shell-command (format "kitty @ --to unix:@emacs send-text \' %s \n\'"
                                (buffer-string))
                       "kitty-send-buffer")
  (window--delete (get-buffer-window "kitty-send-buffer")))

(customize-set-variable
 'display-buffer-base-action
 '(display-buffer-reuse-window (reusable-frames . 0)))



(provide 'i3wm)
;;; i3wm.el ends here
