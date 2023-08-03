;;; pgeneric.el --- Macros for pattern matching function dispatch  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Renan Alves Fonseca

;; Author: Renan Alves Fonseca <rafonseca@small-boat>
;; Keywords: matching, extensions

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

;; The scope here is to allow function dispatch based on arguments
;; pattern matching. This is similar to generic methods but it
;; provides more flexibility.

;;; Code:


(defmacro pgeneric (name &rest body)
  (let ((def-cases  (intern (format "%s--cases" name))))
    `(progn
       (setq ,def-cases  (quote ,`((_ ,@body))) )
       (defun ,name ( &rest arglist)
	 (eval
	  (append `(pcase [,@arglist]) ,def-cases))))))


(pcase-lambda)
(ert-deftest pgeneric ()
  (pgeneric p-function-1 42)
  (should (equal '((_ 42)) p-function-1--cases ))
  (should (equal (p-function-1 1) 42))
  )


(cl-loop for i in '( a b c)
	 collect `(,backquote-unquote-symbol ,i))

(defmacro pmethod (name arglist &rest body)
  (let* ((def-cases  (intern (format "%s--cases" name)))
	 (arglist    (cl-loop for i in arglist  collect `(,backquote-unquote-symbol ,i)))
	 (pattern    `(,backquote-backquote-symbol ,(seq--into-vector arglist))))  
    `(progn
       (push (list ',pattern ',@body) ,def-cases )
       ',name )))



(pmethod p-car (h 2 a `(,c ,d) (rx (let e any)) 0) (identity h))
(pmethod p-car ('i) "i")
(pgeneric p-car 42)
;; TODO: understand how it was done
;; TODO: add tests

(p-car 9 2 8 '(0 1) "u" 0)
(pcase '(9) (`(h) h) )

(ert-deftest pmethod ()
  (pgeneric p-car nil)
  (pmethod p-car (`(,a . ,b)) a)
  (should-not (p-car 1))
  (should (equal 3 (p-car '(3 . 5))) )
  )

(provide 'pgeneric)
;;; pgeneric.el ends here
