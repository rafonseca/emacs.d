;;; db-credentials.el -*- lexical-binding: t; -*-


(require 'parse-ini)


(defconst pg-conf-xlt '(("dbname" . sql-database)
                        ("port" . sql-port)
                        ("host" . sql-server)
                        ("user" . sql-user)
                        ("password" . sql-password)
			))

(defun translate-pg-keywords (pg-service-alist)
  (cl-loop for (k . v) in pg-service-alist
	   collect (cons k  (cl-loop for (kk . vv) in v
				     collect (cons (map-elt pg-conf-xlt kk) vv) into x
				     finally return (push '(sql-product "postgresql") x)))))


(thread-last
  "~/.pg_service.conf"
  (parse-ini-file)
  (translate-pg-keywords)
  (setq sql-connection-alist))



(provide 'db-credentials)
