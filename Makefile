## r binaries
R_BIN := R

PACKAGE_NAME := $(shell sed -n "s/Package: *\([^ ]*\)/\1/p" DESCRIPTION)
PACKAGE_VERSION := $(shell sed -n "s/Version: *\([^ ]*\)/\1/p" DESCRIPTION)
SRC := $(shell basename $(PWD))

.PHONY: clean check build

all: clean check build

roxygen: 
	cd .. ;\
	$(R_BIN) -q -e "library(\"roxygen2\")" \
				      -e "roxygenize(\"$(PACKAGE_NAME)\")"

build: roxygen
	cd .. ;\
	$(R_BIN) CMD build $(SRC)

install: build
	cd .. ;\
	$(R_BIN) CMD INSTALL $(PACKAGE_NAME)_$(PACKAGE_VERSION).tar.gz

check: build
	cd .. ;\
	$(R_BIN) CMD check $(PACKAGE_NAME)_$(PACKAGE_VERSION).tar.gz --as-cran

clean:
	cd .. ;\
	$(RM) -rf $(PACKAGE_NAME).Rcheck/ \
	$(RM) -rf $(PACKAGE_NAME)_*.tar.gz

