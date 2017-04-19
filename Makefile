EMACS=emacs
CASK=cask
PACKAGE-NAME=jemdoc-mode.el

# 1. make cask
# 2. make

all: build

checkdoc:
	@$(EMACS) -Q --batch -f "checkdoc" ${PACKAGE-NAME}

package-lint:
	${CASK} exec $(EMACS) -Q --batch -l "package-lint.el" \
	-f "package-lint-batch-and-exit" ${PACKAGE-NAME}

build: checkdoc package-lint
	${CASK} exec  $(EMACS) -Q --batch \
	--eval "(require 'package)" \
	--eval "(progn \
	           (setq byte-compile-error-on-warn t)  \
	           (batch-byte-compile))" ${PACKAGE-NAME}

# I need to --eval (require 'package) for package-installed-p
# which is used to check whether font-lock+ is installed

cask:
	${CASK} install

clean :
	@rm -f *.elc
	@rm -rf .cask

.PHONY:	all checkdoc package-lint build cask clean
