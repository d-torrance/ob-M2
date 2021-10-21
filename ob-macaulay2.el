(require 'ob)
(require 'M2)
(require 'subr-x)

(defun org-babel-macaulay2-initiate-session (&optional session)
  "If there is not a current inferior-process-buffer in SESSION then create.
Return the initialized session."
  (unless (string= session "none")
    (buffer-name (M2 M2-exe session))))

(defun org-babel-execute:M2 (body params)
  "Execute a block of Macaulay2 code with org-babel.
This function is called by `org-babel-execute-src-block'"
  (let* ((processed-params (org-babel-process-params params))
	 (session (org-babel-macaulay2-initiate-session
                     (cdr (assq :session processed-params))))
	 (result-type (cdr (assq :result-type processed-params))))
    (pcase result-type
      (`output (string-trim-left (org-babel-eval M2-exe body)))
      (`value "TODO"))))

(provide 'ob-macaulay2)
