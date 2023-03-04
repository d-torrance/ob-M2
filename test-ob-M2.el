(require 'ob-M2)
(require 'org-id)

(defconst ob-M2-test-dir
  (expand-file-name (file-name-directory (or load-file-name buffer-file-name))))

(defconst org-id-locations-file
  (expand-file-name ".test-org-id-locations" ob-M2-test-dir))

(defun ob-M2-test-update-id-locations ()
  (org-id-update-id-locations
   (directory-files
    ob-M2-test-dir 'full
    "^\\([^.]\\|\\.\\([^.]\\|\\..\\)\\).*\\.org$")))

;; from org-test.el
(defmacro org-test-at-id (id &rest body)
  "Run body after placing the point in the headline identified by ID."
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

(defun ob-M2-test-block (n cmp expected)
  "Run code in block N and compare its output using CMP to EXPECTED."
  (org-test-at-id "19aeeb54-ac72-45d5-b35a-820588267e5f"
		  (org-babel-next-src-block n)
		  (should (funcall cmp expected
				   (org-babel-execute-src-block)))))

(ert-deftest ob-M2/hello-world ()
  (ob-M2-test-block 1 'string-equal "Hello, world!"))

(ert-deftest ob-M2/var ()
  (ob-M2-test-block 2 '= 7))

(ert-deftest ob-M2/twisted-cubic ()
  (ob-M2-test-block 3 'string-equal "        2                    2
ideal (z  - y*w, y*z - x*w, y  - x*z)"))

(ert-deftest ob-M2/list ()
  (ob-M2-test-block 4 'equal (list 1 3 5 7 9)))

(defun ob-M2-test-run-all ()
  "Run all tests and exit."
  (let ((org-confirm-babel-evaluate nil))
    (ob-M2-test-update-id-locations)
    (ert-run-tests-batch-and-exit)))
