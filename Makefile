VERSION := $(shell sed -n 's/^;; Version: //p' ob-M2.el)
REQUIRES := $(shell sed -n 's/^;; Package-Requires: //p' ob-M2.el)
PACKAGE = ob-M2-$(VERSION)
PACKAGE_FILES = ob-M2.el README.org COPYING barbell.png

install:
	emacs --batch --eval "(require 'package)" \
		--eval "(package-install-file \"ob-M2.el\")"

check:
	emacs --batch -L . -l test-ob-M2.el -f test-ob-M2-run-tests

dist: dist/$(PACKAGE).tar

dist/$(PACKAGE).tar: $(PACKAGE_FILES) Makefile
	mkdir -p dist/$(PACKAGE)
	cp $(PACKAGE_FILES) dist/$(PACKAGE)/
	printf '%s\n' '(define-package "ob-M2" "$(VERSION)" "Babel functions for Macaulay2" '\''$(REQUIRES))' > dist/$(PACKAGE)/ob-M2-pkg.el
	tar -cf $@ -C dist $(addprefix $(PACKAGE)/,$(PACKAGE_FILES) ob-M2-pkg.el)

clean:
	rm -rf dist

.PHONY: install check dist clean
