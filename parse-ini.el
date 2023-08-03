
(require 'each-match)


(defmacro parse-ini-file (filename)
  `(erx-loop :rx '(: "[" (let heading (* nonl)) "]\n" (let settings (* (: (not "[") (* nonl) "\n"))) )
	    :file ,filename
	    :do (cons *heading
		      (erx-loop :file ,filename
				:narrow settings
				:rx '(: (let key (* nonl)) "=" (let value (* nonl)) "\n")
				:do (cons *key *value)))))


(ert-deftest parse-ini-file ()
  (should (parse-ini-file "../.pg_service.conf")))

(provide 'parse-ini)
