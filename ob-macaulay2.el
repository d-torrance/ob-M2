(require 'ob)
(require 'M2)

(defun org-babel-execute:M2 (body params)
    "Execute a block of Macaulay2 code with org-babel.
This function is called by `org-babel-execute-src-block'"
    (let ((temp-file (org-babel-temp-file "M2-" ".m2"))
	   (result-type (cdr (assq :result-type params))))
      (pcase result-type
	(`output (with-temp-file temp-file (insert body))
		 (org-babel-eval (concat M2-exe " --script " temp-file) ""))
	(`value "TODO"))))

(provide 'ob-macaulay2)
