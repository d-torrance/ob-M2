;;; ob-macaulay2.el --- Babel functions for Macaulay2

;; Copyright (C) 2021-2023 Doug Torrance

;; Author: Doug Torrance <dtorrance@piedmont.edu>
;; Version: 0.0.1
;; URL: https://github.com/d-torrance/ob-macaulay2
;; Package-Requires: ((emacs "26.1"))

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;;; Commentary:

;; Org-Babel support for evaluating Macaulay2 source code.

;;; Code:

(require 'ob)
(require 'M2)

(add-to-list 'org-babel-tangle-lang-exts '("macaulay2" . "m2"))

(defconst ob-macaulay2-command
  (concat M2-exe " --no-prompts --silent -e 'clearEcho stdio'")
  "Name of the command for silently executing Macaulay2 code.")

(defconst ob-macaulay2-boe-output "org_babel_macaulay2_boe"
  "String to indicate that Macaulay2 output is beginning.")

(defconst ob-macaulay2-eoe-output "org_babel_macaulay2_eoe"
  "String to indicate that Macaulay2 output has completed.")

(defvar org-babel-default-header-args:M2 '()
  "Macaulay2 default header arguments")

(defun ob-macaulay2-initiate-session (session)
  "Create a Macaulay2 inferior process in SESSION, returning buffer name."
  (if (string= session "none") "none"
    (buffer-name
     (save-window-excursion (M2 ob-macaulay2-command session)))))

(defun ob-macaulay2-print-string (string)
  "Command to print STRING in Macaulay2."
  (concat "print " (prin1-to-string string) "\n"))

(defun ob-macaulay2-prepare-value (body)
  "Prepare code given by BODY to send to Macaulay process."
  (concat body "\n"
	  (ob-macaulay2-print-string ob-macaulay2-boe-output)
	  "print oo"))

(defun ob-macaulay2-get-value (output)
  "Get the value from OUTPUT."
  (car (last (split-string output ob-macaulay2-boe-output))))

(defun ob-macaulay2-evaluate-session (session body result-type)
  "Pass BODY to the Macaulay2 process in SESSION.
If RESULT-TYPE equals `output' then return standard output as a
string.  If RESULT-TYPE equals `value' then return the value of the
last statement in BODY, as elisp."
  (let ((comint-prompt-regexp-old
	 (with-current-buffer session comint-prompt-regexp))
	(prepare-body
	 (pcase result-type
	   (`output 'identity)
	   (`value 'ob-macaulay2-prepare-value)))
	(process-output
	 (pcase result-type
	   (`output 'identity)
	   (`value 'ob-macaulay2-get-value))))
    (with-current-buffer session
      (setq-local comint-prompt-regexp "^"))
    (prog1
	(funcall process-output
		 (apply
		  #'concat
		  (seq-remove
		   (lambda (line)
		     (or
		      (string-match-p (concat "^+ " (regexp-quote M2-exe))
				      line)
		      (string-match-p ob-macaulay2-eoe-output
				      line)))
		   (org-babel-comint-with-output
		       (session ob-macaulay2-eoe-output)
		     (insert
		      (concat (funcall prepare-body body) "\n"
			      (ob-macaulay2-print-string
			       ob-macaulay2-eoe-output)))
		     (comint-send-input nil t)))))
      (with-current-buffer session
	(setq-local comint-prompt-regexp
		    comint-prompt-regexp-old)))))

(defun ob-macaulay2-evaluate-external-process (body result-type)
  "Evaluate BODY in external Macaulay2 process.
If RESULT-TYPE equals `output' then return standard output as a
string.  If RESULT-TYPE equals `value' then return the value of the
last statement in BODY, as elisp."
  (let ((prepare-body
	 (pcase result-type
	   (`output 'identity)
	   (`value 'ob-macaulay2-prepare-value)))
	(process-output
	 (pcase result-type
	   (`output 'identity)
	   (`value 'ob-macaulay2-get-value))))
    (funcall process-output
	       (org-babel-eval ob-macaulay2-command
			       (funcall prepare-body body)))))

(defun org-babel-variable-assignments:M2 (params)
  "Return list of Macaulay2 statements assigning the block's variables."
  (append
   (mapcar (lambda (pair)
	    (format "%s = %s;"
		    (car pair)
		    (ob-macaulay2-var-to-macaulay2 (cdr pair))))
	   (org-babel--get-vars params))
   (list "oo = null")))

(defun ob-macaulay2-var-to-macaulay2 (var)
  "Convert an elisp value to a Macaulay2 variable."
  (if (listp var)
      (concat "{" (mapconcat #'ob-macaulay2-var-to-macaulay2 var ", ") "}")
    (format "%s" var)))

(defun org-babel-execute:M2 (body params)
  "Execute a block of Macaulay2 code in BODY using PARAMS with org-babel.
This function is called by `org-babel-execute-src-block'"
  (let* ((processed-params (org-babel-process-params params))
	 (session (ob-macaulay2-initiate-session
                   (cdr (assq :session processed-params))))
	 (result-type (cdr (assq :result-type processed-params)))
	 (full-body (org-babel-expand-body:generic
		     body params
		     (org-babel-variable-assignments:M2 params))))
    (org-babel-script-escape
     (org-trim
      (if (string= session "none")
	  (ob-macaulay2-evaluate-external-process
	   full-body result-type)
	(ob-macaulay2-evaluate-session
	 session full-body result-type))
      t))))

(provide 'ob-macaulay2)

;;; ob-macaulay2.el ends here
