check:
	emacs -q --batch -L . -l ert -l test-ob-macaulay2.el \
		-f ert-run-tests-batch-and-exit
