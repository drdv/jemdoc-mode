EMACS=emacs
PACKAGE-NAME=jemdoc-mode.el
INIT_PACKAGE_EL="(progn (require 'package) (package-initialize))"

all: build

checkdoc:
	@$(EMACS) -Q --batch -f "checkdoc" ${PACKAGE-NAME}

package-lint:
	@echo "checking with package-lint.el ..."
	@$(EMACS) -Q --batch \
	--eval ${INIT_PACKAGE_EL} \
	-l "package-lint.el" \
	-f "package-lint-batch-and-exit" ${PACKAGE-NAME}
	@echo "done \n"

build: checkdoc package-lint
	$(EMACS) -Q --batch \
	--eval ${INIT_PACKAGE_EL} \
	--eval "(progn \
	           (setq byte-compile-error-on-warn t)  \
	           (batch-byte-compile))" ${PACKAGE-NAME}

clean :
	@rm -f *.elc

.PHONY:	package-lint clean build checkdoc all
