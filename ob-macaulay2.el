(require 'ob)
(require 'M2)
(require 'subr-x)

(defconst org-babel-macaulay2-eoe-output "org_babel_macaulay2_eoe")
(defconst org-babel-macaulay2-eoe-indicator
  (prin1-to-string org-babel-macaulay2-eoe-output))

(defun org-babel-macaulay2-initiate-session (&optional session)
  "If there is not a current inferior-process-buffer in SESSION then create.
Return the initialized session."
  (if (string= session "none") "none"
    (buffer-name (save-excursion (M2 M2-exe session)))))

(defun org-babel-execute:M2 (body params)
  "Execute a block of Macaulay2 code with org-babel.
This function is called by `org-babel-execute-src-block'"
  (let* ((processed-params (org-babel-process-params params))
	 (session (org-babel-macaulay2-initiate-session
                   (cdr (assq :session processed-params))))
	 (result-type (cdr (assq :result-type processed-params))))
    (pcase result-type
      (`output (if (string= session "none")
		   (string-trim-left (org-babel-eval M2-exe body))
		 (org-babel-comint-with-output
		     (session org-babel-macaulay2-eoe-output)
		   (insert (concat body "\n"
				   org-babel-macaulay2-eoe-indicator))
		   (comint-send-input nil t))))
      (`value "TODO"))))

(provide 'ob-macaulay2)
