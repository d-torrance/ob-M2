;;; ob-macaulay2.el --- Babel functions for Macaulay2

;;; Commentary:

;; Org-Babel support for evaluating Macaulay2 source code.

;;; Code:

(require 'ob)
(require 'M2)
(require 'subr-x)

(add-to-list 'org-babel-tangle-lang-exts '("macaulay2" . "m2"))
(defvar org-babel-default-header-args:macaulay2 '())

(defconst org-babel-macaulay2-eoe-output "org_babel_macaulay2_eoe")
(defconst org-babel-macaulay2-eoe-indicator
  (concat "print "
	  (prin1-to-string org-babel-macaulay2-eoe-output)))
(defconst org-babel-macaulay2-command
  (concat M2-exe " --no-prompts --silent -e 'clearEcho stdio'"))

(defun org-babel-macaulay2-initiate-session (&optional session)
  "If there is not a current inferior-process-buffer in SESSION then create.
Return the initialized session."
  (if (string= session "none") "none"
    (buffer-name
     (save-window-excursion (M2 org-babel-macaulay2-command session)))))

(defconst org-babel-macaulay2-value-output "org_babel_macaulay2_value")

(defun org-babel-macaulay2-prepare-value (body)
  (concat body "\n"
	  "print " (prin1-to-string org-babel-macaulay2-value-output) "\n"
	  "print oo"))

(defun org-babel-macaulay2-get-value (output)
  (car (last (split-string output org-babel-macaulay2-value-output))))

(defun org-babel-macaulay2-evaluate-session (session body result-type)
  "Pass BODY to the Macaulay2 process in SESSION.
If RESULT-TYPE equals `output' then return standard output as a
string.  If RESULT-TYPE equals `value' then return the value of the
last statement in BODY, as elisp."
  (let ((comint-prompt-regexp-old
	 (with-current-buffer session comint-prompt-regexp))
	(prepare-body
	 (pcase result-type
	   (`output 'identity)
	   (`value 'org-babel-macaulay2-prepare-value)))
	(process-output
	 (pcase result-type
	   (`output 'identity)
	   (`value 'org-babel-macaulay2-get-value))))
    (with-current-buffer session
      (setq-local comint-prompt-regexp "^"))
    (prog1
	(string-trim-left
	 (funcall process-output
		  (apply
		   #'concat
		   (seq-remove
		    (lambda (line)
		      (or
		       (string-match-p org-babel-macaulay2-eoe-output
				       line)
		       (string-match-p "^+ M2" line)))
		    (org-babel-comint-with-output
			(session org-babel-macaulay2-eoe-output)
		      (insert
		       (concat (funcall prepare-body body) "\n"
			       org-babel-macaulay2-eoe-indicator))
		      (comint-send-input nil t)))))
	 "[\n\r]+")
      (with-current-buffer session
	(setq-local comint-prompt-regexp
		    comint-prompt-regexp-old)))))

(defun org-babel-macaulay2-evaluate-external-process (body result-type)
  "Evaluate BODY in external Macaulay2 process.
If RESULT-TYPE equals `output' then return standard output as a
string.  If RESULT-TYPE equals `value' then return the value of the
last statement in BODY, as elisp."
  (let ((prepare-body
	 (pcase result-type
	   (`output 'identity)
	   (`value 'org-babel-macaulay2-prepare-value)))
	(process-output
	 (pcase result-type
	   (`output 'identity)
	   (`value 'org-babel-macaulay2-get-value))))
    (string-trim-left
     (funcall process-output
	      (org-babel-eval org-babel-macaulay2-command
			      (funcall prepare-body body)))
     "[\n\r]+")))

(defun org-babel-variable-assignments:M2 (params)
  "Return list of Macaulay2 statements assigning the block's variables."
  (mapcar (lambda (pair)
	    (format "%s = %s;" (car pair) (cdr pair)))
	  (org-babel--get-vars params)))

(defun org-babel-execute:M2 (body params)
  "Execute a block of Macaulay2 code with org-babel.
This function is called by `org-babel-execute-src-block'"
  (let* ((processed-params (org-babel-process-params params))
	 (session (org-babel-macaulay2-initiate-session
                   (cdr (assq :session processed-params))))
	 (result-type (cdr (assq :result-type processed-params)))
	 (full-body (org-babel-expand-body:generic
		     body params
		     (org-babel-variable-assignments:M2 params))))
    (if (string= session "none")
	(org-babel-macaulay2-evaluate-external-process
	 full-body result-type)
      (org-babel-macaulay2-evaluate-session
       session full-body result-type))))

(provide 'ob-macaulay2)

;;; ob-macaulay2.el ends here
