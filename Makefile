check:
	emacs -q --batch -L . -l ert -l test-ob-M2.el -f test-ob-M2-run-tests
