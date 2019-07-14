# This is a gnu makefile with several commands to build, document and test
# the package.  The actual building and installation of the package is achieved
# with the standard R commands R CMD BUOLD and R CMD INSTALL.

PKGDIR=cbsots
INSTALL_FLAGS=--no-multiarch --with-keep.source
RCHECKARG=--no-multiarch

# Package name, Version and date from DESCIPTION
PKG=$(shell grep 'Package:' $(PKGDIR)/DESCRIPTION  | cut -d " " -f 2)
PKGTAR=$(PKG)_$(shell grep 'Version' $(PKGDIR)/DESCRIPTION  | cut -d " " -f 2).tar.gz
PKGDATE=$(shell grep 'Date' $(PKGDIR)/DESCRIPTION  | cut -d " " -f 2)
TODAY=$(shell date "+%Y-%m-%d")
OSTYPE=$(shell Rscript -e "cat(.Platform[['OS.type']])")

help:
	@echo
	@echo "The following targets are available:"
	@echo "   help      - displays this help"
	@echo "   test      - run the tests"
	@echo "   covr      - check package coverage (package covr)"
	@echo "   check     - Run R CMD check $(PKGDIR)"
	@echo "   document  - run roxygen to generate Rd files and make pdf Reference manual"
	@echo "   mkpkg     - builds source package, add to drat and checks with --as-cran"
	@echo "   bin       - builds binary package in ./tmp"
	@echo "   install   - install package in .libPaths()[1]"
	@echo "   installv  - install package with vignettes in .libPaths()[1]"
	@echo "   uninstall - uninstall package from .libPaths()[1]"
	@echo "   clean     - cleans up everything"
	@echo "   flags     - display R config flags and some macros"

flags:
	@echo "OSTYPE=$(OSTYPE)"
	@echo "PKGDIR=$(PKGDIR)"
	@echo "PKG=$(PKG)"
	@echo "PKGTAR=$(PKGTAR)"
	@echo "PKGDATE=$(PKGDATE)"
	@echo "libPaths:"
	@R --no-save --quiet --slave -e '.libPaths()'

test:
	Rscript test.R

test_covr:
	Rscript test_covr.R

check: cleanx
	@echo " *** Running R CMD check ***"
	R CMD build $(PKGDIR)
	R CMD check $(RCHECKARG) $(PKGTAR)
	@rm -f  $(PKGTAR)
	@echo "Today                           : $(TODAY)"
	@echo "Checked package description date: $(PKGDATE)"
# 	@Rscript -e 'cat("Installed version date          :",packageDescription("nleqslv", fields="Date"))'
	@echo ""

cleanx:
ifneq ($(OSTYPE), windows) 
# Apple Finder rubbish
	@find . -name '.DS_Store' -delete
	@rm -f $(PKGTAR)
	@rm -fr $(PKG).Rcheck
endif

# build date of package must be at least today
# build source package for submission to CRAN
# after building do a check as CRAN does it
mkpkg: cleanx install_deps
ifeq ($(OSTYPE), windows) 
	@echo Please run mkpkg on Linux or MAC OSX
else
	R CMD build $(PKGDIR)
	R CMD check --as-cran $(RCHECKARG) $(PKGTAR)
	@cp -nv $(PKGTAR) archive
	@echo "Today                           : $(TODAY)"
	@echo "Checked package description date: $(PKGDATE)"
# 	@Rscript -e 'cat("Installed version date          :",packageDescription("nleqslv", fields="Date"))'
	@echo ""
	./drat.sh --pkg=$(PKGTAR)
endif

bin: install_deps
	-@rm -rf tmp
	mkdir tmp
	R CMD build $(PKGDIR)
	R CMD INSTALL $(INSTALL_FLAGS) -l ./tmp --build $(PKGTAR)

document: install_deps
	-@rm -f cbsots.pdf
	R -e "roxygen2::update_collate('"$(PKGDIR)"'); devtools::document('"$(PKGDIR)"')"
	R CMD Rd2pdf --batch $(PKGDIR) -o cbsots.pdf 2>cbsots.log

install: install_deps
	-@rm -rf tmp
	R CMD INSTALL $(INSTALL_FLAGS) $(PKGDIR)

installv: install_deps
	R CMD build $(PKGDIR)
	R CMD INSTALL $(INSTALL_FLAGS) $(PKGTAR)

install_deps:
	R --slave -f install_deps.R

uninstall:
	R CMD REMOVE $(PKG)

clean:
	rm -fr $(PKGDIR).Rcheck
	rm -fr tmp
	rm -f $(PKGTAR)
	rm -f $(PKGDIR).pdf
	rm -f $(PKGDIR).log
