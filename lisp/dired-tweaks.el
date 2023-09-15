;;; dired-tweaks.el -*- lexical-binding: t; -*-


(defmacro then-come-back (body)
  `(let*
      ((current-frame (selected-frame)))
      (,body)
      (select-frame-set-input-focus current-frame)))

(defun make-dired-view-frame ()
  (interactive)
  (or
   (car
    (filtered-frame-list (lambda (x) (equal
                                      "*dired-view*"
                                      (frame-parameter x 'name)))))
   (make-frame '((name . "*dired-view*")))))

(defun make-dired-view-frame-and-back ()
  (then-come-back make-dired-view-frame))

(defmacro on-dired-view-frame (&rest body)
  `(let* ((current-frame (selected-frame)))
        (make-dired-view-frame-and-back)
        (select-frame-by-name "*dired-view*")
        (,@body)
        (select-frame-set-input-focus current-frame)))


(defun dired-view-file-on-frame ()
  (interactive)
  (let* ((file (dired-get-file-for-visit)))
        (on-dired-view-frame view-file file)))

(defun dired-jump-other-frame ()
  (interactive)
  (with-selected-frame
      (make-frame)
      (dired-jump)))

(defun quit-and-close-frame ()
  (interactive)
  (+dired/quit-all)
  (delete-frame (select-frame-by-name "*dired-view*")))



(provide 'dired-tweaks)
