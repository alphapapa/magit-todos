# EMACS_VERSION should be set in your ~/.profile on your development machine
EMAKE_SHA1            := 4208a5e4e68c0e13ecd57195209bdeaf5959395f
PACKAGE_BASENAME      := magit-todos

# override defaults
PACKAGE_ARCHIVES      := gnu melpa
# PACKAGE_TESTS         := test-sample.el # normally, EMake would discover these in the test/ directory
PACKAGE_TEST_DEPS     := dash
PACKAGE_TEST_ARCHIVES := gnu melpa

### Bootstrap and convenience targets

.DEFAULT_GOAL: help

# test: test-ert test-buttercup   ## run tests
test:
lint: lint-package-lint lint-checkdoc ## run lints

emake.mk:                       ## download the emake Makefile
	curl -O 'https://raw.githubusercontent.com/vermiculus/emake.el/$(EMAKE_SHA1)/emake.mk'

include emake.mk
