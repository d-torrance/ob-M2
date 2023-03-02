check:
	emacs -q --batch -L . -l ert -l test-ob-macaulay2.el \
		-f ob-macaulay2-test-run-all
