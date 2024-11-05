######################################################################
# DIRECTORIES

HOME     := ${HOME}
CONFIG   := $(HOME)/.config
LOCAL    := $(HOME)/.local
HOME-APP := $(HOME)/Applications

# LOCAL
SRC       := $(LOCAL)/src
SHARE     := $(LOCAL)/share
BIN       := ${LOCAL}/bin
EMACS-EXE := $(BIN)/emacs

SYSTEM-INITS     := $(SRC)/System-Inits
SYSTEM-INIT-GITS := $(SYSTEM-INIT)/.git

# UTILS
EMACS-SRC            := $(SRC)/emacs
EMACS-UTILS          := $(EMACS-SRC)/lolh
DENOTE               := $(EMACS-SRC)/denote

SITE-LISP            := $(SHARE)/emacs/site-lisp
SITE-LISP-UTILS      := $(SITE-LISP)/lolh
SITE-LISP-UTILS-APP  := $(SITE-LISP-APP)/lolh

EMACS-D      := $(EMACS-SRC)/purcell/emacs.d
TEST-STARTUP := zsh $(EMACS-D)/test-startup.sh

# END
######################################################################

######################################################################
# EMACS REFERENCES, BRANCHES, TAGS

EMACS-REF-V    := emacs
EMACS-REF-V1   := emacs-30
EMACS-REF-V2   := emacs-29.4
# V|V1|V2
EMACS-REF      := $(EMACS-SRC)/$(EMACS-REF-V)
EMACS-REF-TAR  := $(EMACS-REF).tar.gz
EMACS-REF-BUILD:= $(EMACS-REF)/build

# END
######################################################################

######################################################################
# EMACS.APP

EMACS-APP       := $(HOME-APP)/Emacs.app
EMACS-APP-V     := emacs-app
EMACS-SRC-APP   := $(EMACS-SRC)/$(EMACS-APP-V)
EMACS-APP-BUILD := $(EMACS-SRC-APP)/build
EMACS-BUILD-APP := $(EMACS-APP-BUILD)/nextstep/Emacs.app
SITE-LISP-APP   := $(EMACS-BUILD-APP)/Contents/Resources/site-lisp

# END
######################################################################

######################################################################
# ORIGINS
# Savannah is the official origin
# but Emacs-Mirror is much quicker for cloning

EMACS-SAVAN      := https://git.savannah.gnu.org/git/emacs.git
EMACS-MIRROR     := https://github.com/emacs-mirror/emacs
EMACS-MIRROR-GIT := $(EMACS-MIRROR).git
EMACS-MIRROR-TAG := $(EMACS-MIRROR)/archive/refs/tags

# GITHUB REST API
EMACS-GH-API     := https://api.github.com/repos/emacs-mirror/emacs/tarball

# END
######################################################################

######################################################################
# METHODS FOR OBTAINING EMACS SOURCES

# There are two methods for obtaining source files:
# 1- git clone
# 2- curl

# The advantage of `git clone' is that it produces a directory with source
# files that can be used immediately.  The disadvante of `clone' is that
# it is slightly slower.

# The advantage of `curl' is that it produces a directory named after the
# commit it comes from.  The disadvantage of `curl' is that it takes
# more work to process (untar the tar file, then perhaps rename the directory).

# 1: clone the latest version as simply `emacs'
# this method clones the latest version of master
# the directory is simply `emacs'
# this is the simplest way to get the latest version
# may alternately add a ref at the end to name the download something other than `emac'
# e.g. `emacs-latest'
EMACS-CLONE    := git clone --depth 1 $(EMACS-MIRROR-GIT)
EMACS-CLONE-V  := git clone --depth 1 $(EMACS-MIRROR-GIT) $(EMACS-REF-V)

# 2: clone a branch or tag
# this method clones a particular branch or tag
EMACS-CLONE-BR  := git clone --depth 1 --branch $(EMACS-REF-V) $(EMACS-MIRROR-GIT) $(EMACS-REF-V)

# 3: download a tag version
# this method downloads a tag snapshot as a tar file
# when it is untarred, it is in a directory named emacs-emacs-#
# this directory can be renamed to emacs-#
EMACS-MI-TAG-TAR := curl -LO $(EMACS-MIRROR-TAG)/$(EMACS-REF-V).tar.gz

# 4. Download a ref version (branch or tag) using the GitHub REST API
# this method downloads a particular snapshot tarball (e.g. branch 30.0)
# when untarred, the name will include the latest git sha
# the tarball can then be renamed to the version downloaded
EMACS-TAR-REF   := curl -L \
		    -H "Accept: application/vnd.github+json" \
		    -H "X-GitHub-Api-Version: 2022-11-28" \
		    -o $(EMACS-REF-TAR) \
		       $(EMACS-GH-API)/$(EMACS-REF-V)

# 5. Download the latest master verson using the GitHub REST API
# this method downloads the latest snapshot tarball (master branch)
# the tarball must be untarred and then renamed
# this is more cumbersome than cloning the latest version
EMACS-LATEST := curl -L \
			-H "Accept: application/vnd.github+json" \
			-H "X-GitHub-Api-Version: 2022-11-28" \
			-O $(EMACS-GH-API)

# Not working
EMACS-SAV-TAR   := https://git.savannah.gnu.org/cgit/emacs.git
EMACS-BR-TAR    := curl -LO $(EMACS-SAV-TAR)/snapshot/$(EMACS-BR-V).tar.gz

# END
######################################################################

######################################################################
# CONFIGURE settings for EMACS
PREFIX           := --prefix=$(LOCAL)
MAILUTILS        := --with-mailutils
IMAGEMAGICK      := --with-imagemagick
NO-NS            := --disable-ns-self-contained
EMACS-CONFIG     := $(PREFIX) $(MAILUTILS) $(IMAGEMAGICK) $(NO-NS)
EMACS-CONFIG-APP := $(MAILUTILS) $(IMAGEMAGICK)
NCPU             := $(getconf _NPROCESSORS_ONLN)

# END
######################################################################

# COMMON-LISP
SRC-CL         := $(SRC)/common-lisp
CL-IMPL        := $(SRC-CL)/implementations

# SBCL
SBCL-IMPL      := $(CL-IMPL)/sbcl
# TODO: Figure out how to inclue the current version number automatically
SBCL-LATEST-V  := sbcl-2.4.3
SBCL-LATEST    := $(SBCL-IMPL)/$(SBCL-LATEST-V)
SBCL-GIT-CLONE := git clone --depth 1 https://git.code.sf.net/p/sbcl/sbcl $(SBCL-LATEST-V)
SBCL-ROOT      := INSTALL_ROOT=$(LOCAL)
SBCL-EXE       := $(BIN)/sbcl
SBCL-INIT      := $(HOME)/.sbclrc
# NEEDED TO BUILD SBCL
CCL-EXE        := $(CL-IMPL)/ccl/dx86cl64

# CCL
CCL-IMPL      := $(CL-IMPL)/ccl
CCL-LATEST-V  := ccl-1.12.2
CCL-LATEST    := $(CCL-IMPL)/$(CCL-LATEST-V)
CCL-GIT-CLONE := git clone https://github.com/Clozure/ccl.git $(CCL-IMPL)/$(CCL-LATEST-V)
CCL-BOOT      := curl -L -O https://github.com/Clozure/ccl/releases/download/v1.12.2/darwinx86.tar.gz
CCL-EXE       := $(BIN)/ccl
CCL-OS        := $(CCL-LATEST)/lisp-kernel/darwinx8664
CCL-BIN       := $(CCL-LATEST)/dx86cl64
CCL-HEAP      := $(CCL-BIN).image
CCL-MANUAL    := $(CCL-LATEST)/doc/manual
CCL-HTML      := $(CCL-MANUAL)/html
CCL-CCL-HTML  := $(CCL-MANUAL)/html/ccl.html
CCL-CSS       := $(CCL-IMPL)/ccl.css
CCL-INIT      := $(HOME)/.ccl-init.lisp

# ABCL
ABCL-IMPL      := $(CL-IMPL)/abcl
ABCL-LATEST-V  := abcl-1.9.2
ABCL-LATEST    := $(ABCL-IMPL)/$(ABCL-LATEST-V)
ABCL-GIT-CLONE := git clone --depth 1 git@github.com:armedbear/abcl.git $(ABCL-LATEST-V)
ABCL-BUILD     := $(ABCL-LATEST)/build
ABCL-EXE       := $(BIN)/abcl
ABCL-DOC       := $(ABCL-LATEST)/doc/manual/abcl.pdf
ABCL-INIT      := $(HOME)/.abclrc

# ECL
ECL-IMPL      := $(CL-IMPL)/ecl
ECL-LATEST-V  := ecl-23.9.9
ECL-LATEST    := $(ECL-IMPL)/$(ECL-LATEST-V)
ECL-TAR       := curl -O https://ecl.common-lisp.dev/static/files/release/ecl-23.9.9.tgz
ECL-BUILD     := $(ECL-LATEST)/build
ECL-EXE       := $(BIN)/ecl
ECL-INIT      := $(HOME)/.eclrc

# QUICKLISP
QUICKLISP := $(SRC-CL)/quicklisp
QL-SETUP  := $(QUICKLISP)/setup.lisp

ORIGIN    := git@wlharvey4:wlharvey4/System-Init.git


system: $(SYSTEM)

$(SYSTEM):
	mkdir -v $(SYSTEM)

system-init: $(SYSTEM-INIT)

$(SYSTEM-INIT): | $(SYSTEM)
	mkdir -v $(SYSTEM-INIT)

system-init-git: | $(SYSTEM-INIT-GIT)

$(SYSTEM-INIT-GIT): | $(SYSTEM-INIT)
	cd $(SYSTEM-INIT); \
	git clone $(ORIGIN)

#---------------------------------------------------------------------
# EMACS
# make build-emacs
# Prerequisites
# - autoconf
# - texinfo
# - gnutls
# - pkgconfig

###
# emacs
# ~/.local/bin/emacs

# NOTE: emacs-build builds the most recent branch version (e.g., 29.3)
# and installs it into ~/.local/bin as emacs and emacsclient

emacs: | emacs-exe
emacs-exe: $(EMACS-EXE)
$(EMACS-EXE): | emacs-build

emacs-build: | emacs-ref-build
emacs-ref-build: $(EMACS-REF-BUILD)
$(EMACS-REF-BUILD): | emacs-ref
	cd $(EMACS-REF) && \
	./autogen.sh && \
	mkdir -v build && \
	cd build && \
	../configure $(EMACS-CONFIG) && \
	make -j$(NCPU) && \
	make -j$(NCPU) install
	cd $(EMACS-D) && \
	$(TEST-STARTUP)

emacs-ref: $(EMACS-REF)
$(EMACS-REF): | emacs-src
	cd $(EMACS-SRC) && \
	$(EMACS-CLONE)

emacs-src: $(EMACS-SRC)
$(EMACS-SRC): | src
	@mkdir -v $(EMACS-SRC)

src: $(SRC)
$(SRC): $(LOCAL)
	@mkdir -v $(SRC)

######################################################################
# EMACS-REF-T    := $(EMACS-SRC)/$(EMACS-REF-V) => ~/.local/src/emacs/emacs-30
# EMACS-REF-TAR  := $(EMACS-REF).tar.gz

# emacs-ref-t: $(EMACS-REF-T)
# $(EMACS-REF-T):
#	make emacs-ref-tar
#	GLOBIGNORE=".:.." && \
#	cd $(EMACS-SRC) && \
#	mkdir $(EMACS-REF) && \
#	tar -xf $(EMACS-REF-TAR) && \
#	tarx=$$(tar -tf $(EMACS-REF-TAR) | head -n 1) && \
#	cd $${tarx} && \
#	for file in *; do mv "$$file" $(EMACS-REF) ; done && \
#	cd .. && rmdir $${tarx} && \
#	rm $(EMACS-REF-TAR)

# emacs-ref-tar: $(EMACS-REF-TAR)
# $(EMACS-REF-TAR): | emacs-src
#	$(EMACS-TAR-REF)

######################################################################

# Symlink utils and denote into site-lisp
# TODO: This will not work in a new system; they don't exist yet
site-lisp-utils: $(SITE-LISP-UTILS)

$(SITE-LISP-UTILS): | emacs-build
	ln -s $(EMACS-UTILS) $(SITE-LISP)
	ln -s $(DENOTE) $(SITE-LISP)

###
# Emacs.app
# ---------
# NOTE: emacs-app builds the most recent version from the ~master~ branch
# and installs it into Emacs.app, which is moved into ~/Applications

emacs-app: | $(EMACS-APP)

$(EMACS-APP): | emacs-build-app site-lisp-utils-app
	mv $(EMACS-BUILD-APP) $(HOME-APP)

emacs-build-app: | $(EMACS-BUILD-APP)

$(EMACS-BUILD-APP): | emacs-src-app
	cd $(EMACS-SRC-APP) && \
	./autogen.sh && \
	mkdir build && \
	cd build && \
	../configure $(EMACS-CONFIG-APP) && \
	make -j$(NCPU) && \
	make -j$(NCPU) install

emacs-src-app: | $(EMACS-SRC-APP)

$(EMACS-SRC-APP): | emacs-src
	cd $(EMACS-SRC) && \
	$(EMACS-CLONE)

# Symlink utils and denote into Emacs.app's site-lisp
site-lisp-utils-app: | $(SITE-LISP-UTILS-APP)

$(SITE-LISP-UTILS-APP): | emacs-src-app
	ln -s $(EMACS-UTILS) $(SITE-LISP-APP)
	ln -s $(DENOTE) $(SITE-LISP-APP)

clean-emacs-app-build:
	rm -rf $(EMACS-APP-BUILD)

clean-emacs-app:
	rm -rf $(EMACS-APP)
	rm  -rf $(EMACS-SRC-APP)

#---------------------------------------------------------------------
# SBCL
# TODO: This does not prevent a new install when there should not be one.

sbcl: sbcl-install
	@echo in sbcl

sbcl-install: $(SBCL-EXE)
	@echo in sbcl-install

$(SBCL-EXE): sbcl-build
	@echo in $$(SBCL-EXE)
	cd $(SBCL-LATEST) && \
	$(SBCL-ROOT) sh install.sh

sbcl-build: | $(SBCL-LATEST)/version.lisp-expr
	@echo in sbcl-build

$(SBCL-LATEST)/version.lisp-expr: sbcl-git-clone
	@echo in version.lisp-expr
	cd $(SBCL-LATEST) && \
	echo '"2.4.3"' > version.lisp-expr && \
	sh make.sh $(CCL-EXE)
	cd $(SBCL-LATEST)/doc/manual && make

sbcl-git-clone: | $(SBCL-LATEST)
	@echo in sbcl-git-clone

$(SBCL-LATEST):
	@echo in SBCL-LATEST
	cd $(SBCL-IMPL) && \
	$(SBCL-GIT-CLONE)


#---------------------------------------------------------------------
# CCL

ccl: ccl-git-clone ccl-build ccl-heap ccl-install

ccl-build: | $(CCL-BIN)

$(CCL-BIN):
	cd $(CCL-OS) && \
	make clean && make

ccl-heap: | $(CCL-HEAP)

$(CCL-HEAP):
	cd $(CCL-LATEST) && \
	$(CCL-BOOT) && \
	tar -xf darwinx86.tar.gz && \
	$(CCL-BIN) --batch --quiet --no-init --eval '(rebuild-ccl :clean t)'

ccl-install: $(CCL-EXE)

$(CCL-EXE):
	ln -fs $(CCL-BIN) ${BIN}/ccl

ccl-git-clone: | $(CCL-LATEST)

$(CCL-LATEST):
	$(CCL-GIT-CLONE)

ccl-doc: | $(CCL-CCL-HTML)

$(CCL-CCL-HTML):
	mkdir $(CCL-HTML)
	cp $(CCL-CSS) $(CCL-HTML)
	cd $(CCL-MANUAL) && \
	ccl --batch --quiet \
	    --eval '(ql:quickload :ccldoc)' \
	    --eval '(defparameter *d* (ccldoc:load-document "ccl:doc;manual;ccl.ccldoc"))' \
	    --eval '(ccldoc::output-html *d* "./html/ccl.html" :stylesheet "ccl.css")' \
	    --eval '(quit)'

ccl-open-doc: ccl-doc
	open $(CCL-CCL-HTML)


#---------------------------------------------------------------------
# ABCL
abcl: abcl-build

abcl-impl: | $(ABCL-IMPL)

$(ABCL-IMPL):
	mkdir $(ABCL-IMPL)

abcl-latest: | $(ABCL-LATEST)

$(ABCL-LATEST): | abcl-impl
	cd $(ABCL-IMPL) && \
	$(ABCL-GIT-CLONE)

abcl-build: | $(ABCL-BUILD)

$(ABCL-BUILD): | abcl-latest abcl-doc-make
	cd $(ABCL-LATEST) && \
	ant
	ln -fs $(ABCL-LATEST)/abcl $(ABCL-EXE)

abcl-doc-make: | $(ABCL-DOC)

$(ABCL-DOC):
	cd $(ABCL-LATEST)/doc/manual && \
	make

abcl-doc: | abcl-doc-make
	open $(ABCL-LATEST)/doc/manual/abcl.pdf


#---------------------------------------------------------------------
# ECL

ecl: ecl-exe quicklisp-init-ecl

ecl-impl: | $(ECL-IMPL)

$(ECL-IMPL):
	mkdir $(ECL-IMPL)

ecl-latest: | $(ECL-LATEST)

$(ECL-LATEST): | ecl-impl
	cd $(ECL-IMPL) && \
	$(ECL-TAR) && \
	tar -xf $(ECL-LATEST-V).tgz && \
	rm $(ECL-LATEST-V).tgz

ecl-build: | $(ECL-BUILD)

$(ECL-BUILD): | ecl-latest
	cd $(ECL-LATEST) && \
	./configure --prefix=${HOME}/.local \
		    --enable-threads=yes \
		    --enable-boehm=system \
		    --enable-gmp=system \
		    --with-gmp-prefix=/opt/local \
		    --with-libffi-prefix=/opt/local/lib \
		    --with-libgc-prefix=/opt/local/lib

ecl-exe: | $(ECL-EXE)

$(ECL-EXE): | ecl-build
	cd $(ECL-LATEST) && \
	make && make install

#---------------------------------------------------------------------
# QUICKLISP

quicklisp: | quicklisp  quicklisp-init-all

quicklisp-init-all: quicklisp-init-sbcl quicklisp-init-ccl quicklisp-init-abcl

quicklisp: | $(QUICKLISP)

$(QUICKLISP):
	@echo Downloading Quicklisp
	cd $(SRC-CL) && curl -O "https://beta.quicklisp.org/quicklisp.lisp"
	@echo Installing Quicklisp using SBCL
	sbcl --noinform --noprint --disable-debugger \
	     --load $(QUICKLISP) \
	     --eval "(quicklisp-quickstart:install :path \"$(QUICKLISP)\")" \
	     --eval '(exit)'
	@rm -v $(SRC-CL)/quicklisp.lisp

quicklisp-init-sbcl: | $(SBCL-INIT)

$(SBCL-INIT):
	sbcl --noinform --noprint --disable-debugger \
	     --load "$(QL-SETUP)" \
	     --eval '(ql:add-to-init-file)' \
	     --eval '(exit)'

quicklisp-init-ccl: | $(CCL-INIT)

$(CCL-INIT):
	@echo Installing Quicklisp for CCL
	ccl  --batch --quiet \
	     --load $(QL-SETUP) \
	     --eval '(ql:add-to-init-file)' \
	     --eval '(quit)'

quicklisp-init-abcl: | $(ABCL-INIT)

$(ABCL-INIT):
	@echo Installing Quicklisp for ABCL
	abcl --batch \
	     --load $(QL-SETUP) \
	     --eval '(ql:add-to-init-file)'

quicklisp-init-ecl: | $(ECL-INIT)

$(ECL-INIT):
	@echo Installing Quicklisp to ECL
	ecl -q \
	    --load $(QL-SETUP) \
	    --eval '(ql:add-to-init-file)' \
	    --eval '(quit)'

quicklisp-update:
	@echo Updating Quicklisp
	sbcl --noinform --noprint --disable-debugger \
	     --eval '(ql:update-client)' \
	     --eval '(ql:update-dist "quicklisp")' \
	     --eval '(exit)'

#---------------------------------------------------------------------

clear-system:
	rm -vrf $(SYSTEM)

dir: | /Users/minilolh/.local/srcx
	@echo Yes

/Users/minilolh/.local/srcx: mk-dir

mk-dir:
	@echo Making srcx

reset-emacs:
	rm -rf $(EMACS)
	rm -rf $(BIN-EMACS)
