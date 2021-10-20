(require 'ob)
(require 'M2)

(defun org-babel-execute:M2 (body params)
  "Execute a block of Macaulay2 code with org-babel.
This function is called by `org-babel-execute-src-block'"
  (let ((result-type (cdr (assq :result-type params))))
    (pcase result-type
      (`output (org-babel-eval M2-exe body))
      (`value "TODO"))))

(provide 'ob-macaulay2)
