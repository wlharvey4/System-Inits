# ~/.oh-my-zsh/custom/envvars.zsh -*- mode:sh; -*-
# Time-stamp: <2024-01-30 03:21:50 minilolh>

export INFOPATH=/opt/local/share/info:/usr/local/share/info:

export CLOUDDOCS=$HOME/Library/Mobile\ Documents/com\~apple\~CloudDocs
export LH_GOOGLEDOCS=$HOME/Library/CloudStorage/GoogleDrive-lincoln@ccvlop.org
export LH_MY_DRIVE=${LH_GOOGLEDOCS}/My\ Drive
export LH_MY_DRIVE_2023=${LH_MY_DRIVE}/2023\ UD\ Case\ Prep/Lincoln\ Harvey
export LH_MY_DRIVE_2024=${LH_MY_DRIVE}/2024\ UD\ Case\ Prep/Lincoln\ Harvey

export ANT_HOME=/Users/minilolh/.local/share/java/ant/apache-ant-1.10.13
export JAVA_HOME=/Library/Java/JavaVirtualMachines/temurin-17.jdk/Contents/Home

# WORK
export WORK=$HOME/.local/work
export WORKFIN=${WORK}/workfin

# LEDGER/HLEDGER
export LEDGER=${WORKFIN}/ledger
export LEDGER_FILE=${LEDGER}/zero.ledger
export HLEDGER=${WORKFIN}/hledger
#export LEDGER_FILE=${HLEDGER}/hledger.journal

#COMMON LISP
export ROSWELL_HOME=$HOME/.local/share/common-lisp/roswell

# Preferred editor for local and remote sessions
if [[ -n $SSH_CONNECTION ]]; then
  export EDITOR='emacsclient -s termserver'
else
  export EDITOR='emacsclient -s guiserver'
fi

export GUISERVER="STOPPED"
export TERMSERVER="STOPPED"

export EMAIL="lincoln@ccvlp.org"
