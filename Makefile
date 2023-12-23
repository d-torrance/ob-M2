install:
	emacs --batch --eval "(require 'package)" \
		--eval "(package-install-file \"ob-M2.el\")"

check:
	emacs --batch -L . -l test-ob-M2.el -f test-ob-M2-run-tests
