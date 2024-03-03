# ~/.oh-my-zsh/custom/envvars.zsh -*- mode:sh; -*-
# Time-stamp: <2024-02-24 19:47:30 lolh-mbp-16>

export INFOPATH=/opt/local/share/info:/usr/local/share/info:

export CLOUDDOCS=$HOME/Library/Mobile\ Documents/com\~apple\~CloudDocs

# These point to the same place; first one is shorter, so use it??
export GOOGLE_DRIVE=$HOME/Google\ Drive/My\ Drive
export LH_GOOGLE_DRIVE=$HOME/Library/CloudStorage/GoogleDrive-lincoln@ccvlop.org/My\ Drive

export GOOGLE_DRIVE_2022=${GOOGLE_DRIVE}/2022\ UD\ Case\ Prep.gdrive/Lincoln\ Harvey
export GOOGLE_DRIVE_2023=${GOOGLE_DRIVE}/2023\ UD\ Case\ Prep.gdrive/Lincoln\ Harvey
export GOOGLE_DRIVE_2024=${GOOGLE_DRIVE}/2024\ UD\ Case\ Prep/Lincoln\ Harvey

export LH_GOOGLE_DRIVE_2022=${LH_GOOGLE_DRIVE}/2022\ UD\ Case\ Prep.gdrive/Lincoln\ Harvey
export LH_GOOGLE_DRIVE_2023=${LH_GOOGLE_DRIVE}/2023\ UD\ Case\ Prep.gdrive/Lincoln\ Harvey
export LH_GOOGLE_DRIVE_2024=${LH_GOOGLE_DRIVE}/2024\ UD\ Case\ Prep/Lincoln\ Harvey

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
