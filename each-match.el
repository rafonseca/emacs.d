;;; each-match.el --- Loop macros for text matching  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Renan Alves Fonseca

;; Author: Renan Alves Fonseca <rafonseca@small-boat>
;; Keywords: matching, convenience

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

(require 'cl-lib)
(require 'dash)

(rx-define let (name content) (group content))

(defmacro -it-> (&rest body)
  "Thread the symbol `it' through `BODY' returning result of last expression."
  `(let*
       ,(cl-loop for stmt in  body
		 collect `(it  ,stmt))
     it
     )
  )

(defmacro erx-using-region (fun region &rest rest )
  "Call `FUN' splicing `REGION' into start end args.
Many text manipulating functions deal with start end args, while
`erx-loop' deals with region as a cons."
  `(seq-let (start end) (flatten-list ,region)
     (,fun start end ,@rest)))

(defun erx-replace-region (region content)
  "Similar to `replace-region-contents' with different signature."
  (erx-using-region replace-region-contents region (lambda () content)))

(defun erx-narrow-to-region (region)
  "Like `narrow-to-region' but with single arg.."
  (erx-using-region narrow-to-region region))

(defun erx--star-symbol (sym)
  "Returns new symbol preffixed with *"  
  (-it-> sym
	 (symbol-name it)
	 (concat "*" it)
	 (intern it)))

(defun erx--star-sym-binding (binding)
  "Auxiliary function of macro `for-each-match'."
  (cl-loop for w in binding
 	   collect `(,(star-symbol w)  (region-text ,w)))
  )

(cl-defmacro erx-for-each-match (regex  body &key binding  in)
  "This macro executes search-match against `regex' in current
buffer, narrowed to region `in' if not nil. Then, for each match,
narrows to match and executes `body'. If `regex' capture groups,
they are available inside `body' as defined by list of symbols
`binding'."
  (let ((whole-match (make-symbol "whole-match")))
    `(progn
       (message "%s" ,in)
       (when ,in (erx-narrow-to-region ,in))
       (-it->
	(cl-loop while (re-search-forward ,regex nil t)
		 collect (map-into (match-data) 'alist))
	(cl-loop for (,whole-match ,@binding) in it
		 collect
		 (save-restriction
 		   (let ,(erx--star-sym-binding binding)
		     (erx-narrow-to-region ,whole-match)
 		     ,body)))))))


(defun erx--extract-rx-bindings (rx-exp)
  "Search for (let name rx-exp) returning names in order. (group
rx-exp) is taken into account in order to maintain order."
  ;; TODO: we can possibly use pcase-let in the search loop instead.
  (-it-> rx-exp
	 (-tree-seq 'listp 'identity it);; traverse rx-expression tree
	 (cl-loop for item in it 
		  collect (pcase item
			    (`(let ,name ,rest) name)
			    (`(group ,rest) "_")))
	 (cl-loop for item in it
		  when item
		  collect (pcase item
			    ("_" nil)
			    (item item)))))


(ert-deftest erx--extract-rx-bindings ()
  (cl-loop for (params . result) in '(
				      ((: (let name (* anything))) . (name) )
				      ((: let ) . nil)
				      ((: (let n1 "a") (group "b") (let n2 "c")) . (n1 nil n2))
				      ((: (let n1 (: "a" (group "b"))) (let n2 "c")) . (n1 nil n2))				    
				      )
	   do (should (equal result (erx--extract-rx-bindings params)))))

(defun erx-rx (rx-exp)
  "Wraps `rx' to avoid eager macro expansion.
`RX-EXP' is a list like '(: ...) "
  (eval `(rx  ,rx-exp)))


(cl-defmacro erx-loop (&key rx do narrow buffer file )
  "Execute `re-search-forward' in loop against regex obtained from
`RX' in current buffer, `BUFFER' or `FILE', narrowed to region
`IN' if not nil. Then, for each match, narrows to match and
executes `BODY' with local bindings defined in `RX'. An `RX'
expression of the form (let NAME ...) binds the symbol NAME to
the region where the submatch happens and the symbol *NAME to the
contents of that region, as a string."
  (let (( binding (extract-rx-bindings rx) ))
    `(with-current-buffer ,(or buffer
			       (when file (find-file-noselect file))
			       (current-buffer))
       (beginning-of-buffer)
       (erx-for-each-match
	(erx-rx ,rx)
	,do
	:binding (,@binding)
	:in ,narrow
	))))


;; TODO: make package
;; TODO: make examples


(provide 'each-match)
;;; each-match.el ends here

;;; example 1


(erx-loop :rx '(: "[" (let heading (* nonl)) "]\n" (let settings (* (: (not "[") (* nonl) "\n"))) )
	  :file "../.pg_service.conf"
	  :do (cons *heading
		    (erx-loop :file "../.pg_service.conf"
			      :narrow settings
			      :rx '(: (let key (* nonl)) "=" (let value (* nonl)) "\n")
			      :do (cons *key *value))))


;;; example 3
(with-temp-file "test_file.env"
  (insert "VAR1=abc\nVAR2=def\n"))

(with-temp-file "test_file.script"
  (insert "result = $VAR1 + $VAR2"))

(erx-loop :file "test_file.env"
	  :rx '(: (let key (* nonl)) "=" (let value (* nonl)))
	  :do (erx-loop :file "test_file.script"
			:rx `(: "$" (let to-replace ,*key))
			:do  (erx-replace-region to-replace *value)))
