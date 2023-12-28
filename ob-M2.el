;;; ob-M2.el --- Babel functions for Macaulay2

;; Copyright (C) 2021-2023 Doug Torrance

;; Author: Doug Torrance <dtorrance@piedmont.edu>
;; Version: 0.1.2
;; URL: https://github.com/d-torrance/ob-M2
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

(add-to-list 'org-babel-tangle-lang-exts '("M2" . "m2"))

(defconst ob-M2-command
  (concat M2-exe " --no-prompts --silent -e 'clearEcho stdio'")
  "Name of the command for silently executing Macaulay2 code.")

(defconst ob-M2-boe-output "org_babel_macaulay2_boe"
  "String to indicate that Macaulay2 output is beginning.")

(defconst ob-M2-eoe-output "org_babel_macaulay2_eoe"
  "String to indicate that Macaulay2 output has completed.")

(defvar org-babel-default-header-args:M2 '()
  "Macaulay2 default header arguments.")

(defun ob-M2-initiate-session (session)
  "Create a Macaulay2 inferior process in SESSION, returning buffer name."
  (if (string= session "none") "none"
    (buffer-name
     (save-window-excursion (M2 ob-M2-command session)))))

(defun ob-M2-print-string (string)
  "Command to print STRING in Macaulay2."
  (concat "print " (prin1-to-string string) "\n"))

(defun ob-M2-prepare-value (body)
  "Prepare code given by BODY to send to Macaulay process."
  (concat body "\n"
	  (ob-M2-print-string ob-M2-boe-output)
	  "print oo"))

(defun ob-M2-get-value (output)
  "Get the value from OUTPUT."
  (car (last (split-string output ob-M2-boe-output))))

(defun ob-M2-evaluate-session (session body result-type)
  "Pass BODY to the Macaulay2 process in SESSION.
If RESULT-TYPE equals `output' then return standard output as a
string.  If RESULT-TYPE equals `value' then return the value of the
last statement in BODY, as elisp."
  (let ((comint-prompt-regexp-old
	 (with-current-buffer session comint-prompt-regexp))
	(prepare-body
	 (pcase result-type
	   (`output 'identity)
	   (`value 'ob-M2-prepare-value)))
	(process-output
	 (pcase result-type
	   (`output 'identity)
	   (`value 'ob-M2-get-value))))
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
		      (string-match-p ob-M2-eoe-output
				      line)))
		   (org-babel-comint-with-output
		       (session ob-M2-eoe-output)
		     (insert
		      (concat (funcall prepare-body body) "\n"
			      (ob-M2-print-string
			       ob-M2-eoe-output)))
		     (comint-send-input nil t)))))
      (with-current-buffer session
	(setq-local comint-prompt-regexp
		    comint-prompt-regexp-old)))))

(defun ob-M2-evaluate-external-process (body result-type)
  "Evaluate BODY in external Macaulay2 process.
If RESULT-TYPE equals `output' then return standard output as a
string.  If RESULT-TYPE equals `value' then return the value of the
last statement in BODY, as elisp."
  (let ((prepare-body
	 (pcase result-type
	   (`output 'identity)
	   (`value 'ob-M2-prepare-value)))
	(process-output
	 (pcase result-type
	   (`output 'identity)
	   (`value 'ob-M2-get-value))))
    (funcall process-output
	       (org-babel-eval ob-M2-command
			       (funcall prepare-body body)))))

(defun org-babel-variable-assignments:M2 (params)
  "Return list of Macaulay2 statements assigning the variables from PARAMS."
  (append
   (mapcar (lambda (pair)
	    (format "%s = %s;"
		    (car pair)
		    (ob-M2-var-to-macaulay2 (cdr pair))))
	   (org-babel--get-vars params))
   (when (eq (cdr (assq :result-type params)) 'value)
     (list "oo = null"))))

(defun ob-M2-var-to-macaulay2 (var)
  "Convert an elisp value VAR to a Macaulay2 variable."
  (if (listp var)
      (concat "{" (mapconcat #'ob-M2-var-to-macaulay2 var ", ") "}")
    (prin1-to-string var)))

(defun org-babel-expand-body:M2 (body params)
  "Expand BODY with PARAMS for Macaulay2 code.
Graphics generation will only work if ImageMagick is installed."
  (let ((graphics-file (and (member "graphics" (assq :result-params params))
			    (org-babel-graphical-output-file params))))
    (org-babel-expand-body:generic
     body params
     (nconc (org-babel-variable-assignments:M2 params)
	    (when graphics-file
	      (list (concat
		     "show URL := url -> run(\"convert \" | first url | \" "
		     graphics-file "\")")))))))

(defun org-babel-execute:M2 (body params)
  "Execute a block of Macaulay2 code in BODY using PARAMS with org-babel.
This function is called by `org-babel-execute-src-block'"
  (let* ((processed-params (org-babel-process-params params))
	 (session (ob-M2-initiate-session
                   (cdr (assq :session processed-params))))
	 (result-type (cdr (assq :result-type processed-params)))
	 (full-body (org-babel-expand-body:M2 body params))
	 (result (org-babel-script-escape
		  (org-trim
		   (if (string= session "none")
		       (ob-M2-evaluate-external-process
			full-body result-type)
		     (ob-M2-evaluate-session
		      session full-body result-type))
		   t))))
    (org-babel-reassemble-table
     result
     (org-babel-pick-name (cdr (assq :colname-names params))
			  (cdr (assq :colnames params)))
     (org-babel-pick-name (cdr (assq :rowname-names params))
			  (cdr (assq :rownames params))))))

(provide 'ob-M2)

;;; ob-M2.el ends here
