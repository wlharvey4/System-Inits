# -*- mode: sh; fill-column:79; -*-
# ~/.oh-my-zsh/custom/envars.zsh

# Time-stamp: <2023-03-03 08:05:12 lolh-mbp-16>

printf "  ${CYAN}sourcing envars.zsh...${CLEAR}\n"

setopt RE_MATCH_PCRE

export INFOPATH=~/.local/share/common-lisp/implementations/share/info:

export COMP=$(hostname -s)
printf "\t${GREEN}COMP = ${COMP}${CLEAR}\n"

export XDG_CONFIG_HOME=~/.config

export DLDS=${HOME}/Downloads
export DOCS=${HOME}/Documents
export DATA=${DOCS}/Data

export LO=${DATA}/LO
export CASES=${LO}/CASES
export STATES=${LO}/STATES
export WA=${STATES}/WA

export USR=/usr/local
export OPT=/opt/local
export DEV=${USR}/dev
export PROGRAMS=${DEV}/programs
export PROGRAMMING=${DEV}/programming

export GOPATH=${PROGRAMS}/go

export SYNC_ORG_TEMPLATE=${DEV}/Templates/Org/Template.org
export AWS_S3_BUCKET=pinecone-forest.com

export EMACS_APP_PREFIX=$(find ~/Applications -name Emacs.app -maxdepth 3)
export EMACS_MACOS=${EMACS_APP_PREFIX}/Contents/MacOS
export EMACS_APP=${EMACS_MACOS}/Emacs
export EMACS_APP_BIN=${EMACS_MACOS}/bin
export EMACS_APP_CLIENT=${EMACS_APP_BIN}/emacsclient

export EMACS_TERM=emacs
export EMACS_TERM_CLIENT=emacsclient

# export EMACS_HOME=$(
#     ( [[ -f $HOME/.emacs ]] || [[ -f $HOME/.emacs.el ]] ) && { echo $HOME; return; } ||
#     [[ -d $HOME/.emacs.d ]] && { echo $HOME/.emacs.d; return; } ||
#     [[ -d $HOME/.config/emacs ]] && { echo $HOME/.config/emacs; return; } ||
#     echo error
#)
export EMACS_INIT_DIR=$($EMACS_TERM -Q --batch --eval '
       (princ (directory-file-name (expand-file-name user-emacs-directory)))')

export EMACS_INIT_FILE=$(
    case $EMACS_INIT_DIR in

	($HOME)
	    [[ -f $EMACS_INIT_DIR/.emacs ]]    && { echo ".emacs"; return; }    ||
	    [[ -f $EMACS_INIT_DIR/.emacs.el ]] && { echo ".emacs.el"; return; } ||
		    echo "ERROR: no .emacs"                                 ;;

	($HOME/.emacs.d)
	    [[ -f $EMACS_INIT_DIR/init ]]      && { echo "init"; return; }      ||
	    [[ -f $EMACS_INIT_DIR/init.el ]]   && { echo "init.el"; return; }   ||
	    [[ -f $EMACS_INIT_DIR/emacs ]]     && { echo "emacs"; return; }     ||
	    [[ -f $EMACS_INIT_DIR/emacs.el ]]  && { echo "emacs.el"; return; }  ||
		   echo "ERROR: no .emacs"                                  ;;

        ($HOME/.config/emacs)
	    [[ -f $EMACS_INIT_DIR/init ]]      && { echo "init"; return; }      ||
	    [[ -f $EMACS_INIT_DIR/init.el ]]   && { echo "init.el"; return; }   ||
		   echo "ERROR: no init.el"                                 ;;

     esac
)

export EMACS_INIT=${EMACS_INIT_DIR}/${EMACS_INIT_FILE}
export EMACS_WIDTH=250

# Preferred editor for local and remote sessions
export EDITOR=${EMACS_TERM_CLIENT}

# if [[ -n $SSH_CONNECTION ]]; then
#   export EDITOR='$EMACS_TERM_CLIENT -a ""'
# else
#   export EDITOR='$EMACS_APP_CLIENT "--server=guiserver"'
# fi

# CLOZURE COMMON LISP CCL
export CCL_DEFAULT_DIRECTORY=$USR/ccl/ccl-1.12/ccl-1.12.1
export CCL_KERNEL=dx86cl64

# KAWA SCHEME
export KAWA_HOME=/usr/local/kawa/kawa-3.1.1

# OPENJDK
# openjdk11 has the following notes:
#   If you have more than one JDK installed you can make openjdk11 the default
#   by adding the following line to your shell profile:
#
#export JAVA_HOME=/Library/Java/JavaVirtualMachines/openjdk11/Contents/Home
export JAVA_HOME=/Library/Java/JavaVirtualMachines/openjdk11-temurin/Contents/Home

# GIT
export GIT_EDITOR=$EDITOR

export INFOPATH="\
/opt/local/share/info:\
/usr/local/dev/share/info:\
/usr/local/dev/emacsdev/share/info:\
/usr/share/info:\
/usr/local/share/info"

printf "  ${CYAN}envars.zsh done${CLEAR}\n"
