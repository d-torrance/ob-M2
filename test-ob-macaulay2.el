(require 'ob-macaulay2)
(require 'org-id)

(defconst ob-macaulay2-test-dir
  (expand-file-name (file-name-directory (or load-file-name buffer-file-name))))

(defconst org-id-locations-file
  (expand-file-name ".test-org-id-locations" ob-macaulay2-test-dir))

(defun ob-macaulay2-test-update-id-locations ()
  (org-id-update-id-locations
   (directory-files
    ob-macaulay2-test-dir 'full
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

(ert-deftest ob-macaulay2/hello-world ()
  (let ((org-confirm-babel-evaluate nil))
    (ob-macaulay2-test-update-id-locations)
    (org-test-at-id "19aeeb54-ac72-45d5-b35a-820588267e5f"
		    (org-babel-next-src-block 1)
		    (should (string-equal "Hello, world!\n"
					  (org-babel-execute-src-block))))))
