all:
	emacs --batch -L . -f batch-byte-compile *.el

check:
	emacs --batch -L . -l test-ob-M2.el -f test-ob-M2-run-tests
