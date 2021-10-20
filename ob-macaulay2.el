(require 'ob)
(require 'M2)
(require 'subr-x)

(defun org-babel-execute:M2 (body params)
  "Execute a block of Macaulay2 code with org-babel.
This function is called by `org-babel-execute-src-block'"
  (let* ((processed-params (org-babel-process-params params))
	 (result-type (cdr (assq :result-type processed-params))))
    (pcase result-type
      (`output (string-trim-left (org-babel-eval M2-exe body)))
      (`value "TODO"))))

(provide 'ob-macaulay2)
