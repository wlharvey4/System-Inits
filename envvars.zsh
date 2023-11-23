# ~/.oh-my-zsh/custom/envvars.zsh -*- mode:sh; -*-
# Time-stamp: <2023-11-22 21:06:02 minilolh>

export CLOUDDOCS=$HOME/Library/Mobile\ Documents/com\~apple\~CloudDocs

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

export EMAIL="lincoln@ccvlp.org"
