HOME     := ${HOME}
CONFIG   := $(HOME)/.config
LOCAL    := $(HOME)/.local
HOME-APP := $(HOME)/Applications

SRC       := $(LOCAL)/src
SHARE     := $(LOCAL)/share
BIN       := ${LOCAL}/bin

SYSTEM          := $(CONFIG)/system
SYSTEM-INIT     := $(SYSTEM)/system-init
SYSTEM-INIT-GIT := $(SYSTEM-INIT)/.git


# EMACS
EMACS-SRC      := $(SRC)/emacs
EMACS-EXE      := $(BIN)/emacs
EMACS-LATEST-V := emacs-29.3
EMACS-LATEST   := $(EMACS-SRC)/$(EMACS-LATEST-V)
EMACS-BUILD    := $(EMACS-LATEST)/build
SITE-LISP      := $(SHARE)/emacs/site-lisp
EMACS-TAR      := curl -O https://git.savannah.gnu.org/cgit/emacs.git/snapshot/emacs-29.3.tar.gz

# EMACS.APP
EMACS-APP       := $(HOME-APP)/Emacs.app
EMACS-APP-V     := emacs-app
EMACS-SRC-APP   := $(EMACS-SRC)/$(EMACS-APP-V)
EMACS-APP-BUILD := $(EMACS-SRC-APP)/build
EMACS-BUILD-APP := $(EMACS-APP-BUILD)/nextstep/Emacs.app
SITE-LISP-APP   := $(EMACS-BUILD-APP)/Contents/Resources/site-lisp
EMACS-GIT       := https://git.savannah.gnu.org/git/emacs.git
EMACS-CLONE     := git clone --depth 1 $(EMACS-GIT) $(EMACS-APP-V)

# CONFIGURE settings for EMACS
PREFIX       := --prefix=$(LOCAL)
MAILUTILS    := --with-mailutils
NO-NS        := --disable-ns-self-contained
EMACS-CONFIG := $(PREFIX) $(MAILUTILS) $(NO-NS)
EMACS-CONFIG-APP := $(MAILUTILS)
NCPU         := $(getconf _NPROCESSORS_ONLN)

# UTILS
EMACS-UTILS          := $(EMACS-SRC)/utils
DENOTE               := $(EMACS-SRC)/denote
SITE-LISP-UTILS      := $(SITE-LISP)/utils
SITE-LISP-UTILS-APP  := $(SITE-LISP-APP)/utils

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
CCL-EXE := $(CL-IMPL)/ccl/dx86cl64

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
ABCL-INIT     := $(HOME)/.abclrc

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

###
# emacs
# ~/.local/bin/emacs
emacs: emacs-exe site-lisp-utils

# NOTE: emacs-build builds the most recent branch version (e.g., 29.3)
# and installs it into ~/.local/bin as emacs and emacsclient
emacs-exe: | $(EMACS-EXE)

$(EMACS-EXE): | emacs-build

emacs-build: | $(EMACS-BUILD)

$(EMACS-BUILD): | emacs-latest
	cd $(EMACS-LATEST) && \
	./autogen.sh && \
	mkdir build && \
	cd build && \
	../configure $(EMACS-CONFIG) && \
	make -j$(NCPU) && \
	make -j$(NCPU) install

emacs-latest: | $(EMACS-LATEST)

$(EMACS-LATEST): | emacs-src
	cd $(EMACS-SRC) && \
	$(EMACS-TAR) && \
	tar -xf $(EMACS-LATEST).tar.gz && \
	rm -rf $(EMACS-LATEST).tar.gz

emacs-src: | $(EMACS-SRC)

$(EMACS-SRC):
	@mkdir -v $(EMACS)

# Symlink utils and denote into site-lisp
site-lisp-utils: | $(SITE-LISP-UTILS)

$(SITE-LISP-UTILS): | emacs-build
	ln -s $(EMACS-UTILS) $(SITE-LISP)
	ln -s $(DENOTE) $(SITE-LISP)

###
# Emacs.app
# ---------
# NOTE: emacs-app builds the most recent version from the ~master~ branch
# and installs it into Emacs.app.
# However, it remains in the build/nextstep directory

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
