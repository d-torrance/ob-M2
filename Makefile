check:
	emacs -q --batch -L . -l ert -l test-ob-M2.el -f ob-M2-test-run-all
