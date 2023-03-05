;;; test-ob-M2.el --- test for ob-M2

;; Copyright (C) 2021-2023 Doug Torrance

;; Package-Requires: ((emacs "27.1"))
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;;; Commentary:

;; Tests for Org Babel support for evaluating Macaulay2 source code.

;;; Code:

(require 'ert)
(require 'ob-M2)
(require 'org-id)

(defun test-ob-M2-update-id-locations ()
  "Scan files in current directory for IDs."
  (org-id-update-id-locations
   (directory-files
    default-directory
    'full
    "^\\([^.]\\|\\.\\([^.]\\|\\..\\)\\).*\\.org$")))

(defmacro test-ob-M2-test-at-id (id &rest body)
  "Run BODY after placing the point in the headline identified by ID.
This is just `org-test-at-id' from org-test.el."
  (declare (indent 1) (debug t))
  `(let* ((id-location (org-id-find ,id))
	  (id-file (car id-location))
	  (visited-p (get-file-buffer id-file))
	  to-be-removed)
     (unwind-protect
	 (save-window-excursion
	   (save-match-data
	     (org-id-goto ,id)
	     (setq to-be-removed (current-buffer))
	     (condition-case nil
		 (progn
		   (org-show-subtree)
		   (org-show-all '(blocks)))
	       (error nil))
	     (save-restriction ,@body)))
       (unless (or visited-p (not to-be-removed))
	 (kill-buffer to-be-removed)))))

(defun test-ob-M2-test-block (n cmp expected)
  "Run code in block N and compare its output using CMP to EXPECTED."
  (test-ob-M2-test-at-id "19aeeb54-ac72-45d5-b35a-820588267e5f"
		  (org-babel-next-src-block n)
		  (should (funcall cmp expected
				   (org-babel-execute-src-block)))))

(ert-deftest test-ob-M2-hello-world ()
  (test-ob-M2-test-block 1 'string-equal "Hello, world!"))

(ert-deftest test-ob-M2-var ()
  (test-ob-M2-test-block 2 '= 7))

(ert-deftest test-ob-M2-twisted-cubic ()
  (test-ob-M2-test-block 3 'string-equal "        2                    2
ideal (z  - y*w, y*z - x*w, y  - x*z)"))

(ert-deftest test-ob-M2-list ()
  (test-ob-M2-test-block 4 'equal (list 1 3 5 7 9)))

(defun test-ob-M2-run-tests ()
  "Run each test and exit."
  (let ((org-confirm-babel-evaluate nil))
    (test-ob-M2-update-id-locations)
    (ert t)))

(provide 'test-ob-M2)

;;; test-ob-M2.el ends here
