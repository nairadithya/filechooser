EMACS ?= emacs
PYTHON ?= python3

.PHONY: test verify-portal
test:
	$(EMACS) -Q --batch --eval '(progn (require (quote package)) (package-initialize) (when-let ((compat (getenv "COMPAT"))) (add-to-list (quote load-path) compat)))' -L . -l tests/filechooser-tests.el -f ert-run-tests-batch-and-exit

verify-portal:
	$(PYTHON) scripts/verify-portal.py
