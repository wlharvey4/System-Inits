# ~/.oh-my-zsh/custom/envvars.zsh -*- mode:sh; -*-
# Time-stamp: <2024-03-02 21:38:03 lolh-mbp-16>

## DOCUMENTATION PATHS
export INFOPATH=/opt/local/share/info:/usr/local/share/info:
export MANPATH=/opt/local/share/man:/usr/local/share/man

## ICLOUD CLOUDDOCS
export CLOUDDOCS=$HOME/Library/Mobile\ Documents/com\~apple\~CloudDocs
export CD_DOCUMENTS=${CLOUDDOCS}/Documents
export CD_DOWNLOADS=${CLOUDDOCS}/Downloads/
export CD_DATA=${CD_DOCUMENTS}/Data

## GOOGLE DRIVE My Drive (CloudStorage)

# GOOGLE DRIVE
export GOOGLE_DRIVE=https://drive.google.com/drive
export MY_DRIVE=${GOOGLE_DRIVE}/my-drive
export SHARED_DRIVES=${GOOGLE_DRIVE}/shared-drives
export SHARED_DRIVES_RTC=${GOOGLE_DRIVE}/folders/0AOM9lzbnNfCBUk9PVA

# The Full Path to My Drive
export LH_MY_DRIVE=$HOME/Library/CloudStorage/GoogleDrive-lincoln@ccvlp.org/My\ Drive
export LH_MY_PUBLIC_DOCS=${LH_MY_DRIVE}/Public\ Documents/Lincoln\ Harvey
export LH_MY_PRIVATE_DOCS=${LH_GOOGLE_DRIVE}/Lincoln-Private
# Open Cases
export LH_MY_CASES_2022=${LH_MY_DRIVE}/2022\ UD\ Case\ Prep/Lincoln\ Harvey\ 2022
export LH_MY_CASES_2023=${LH_MY_DRIVE}/2023\ UD\ Case\ Prep/Lincoln\ Harvey\ 2023
export LH_MY_CASES_2024=${LH_MY_DRIVE}/2024\ UD\ Case\ Prep/Lincoln\ Harvey\ 2024
# Closed Cases
export LH_MY_CLOSED_CASES_2022=${LH_MY_CASES_2022}/00_2022_Closed_Cases
export LH_MY_CLOSED_CASES_2023=${LH_MY_CASES_2023}/00_2023_Closed_Cases
export LH_MY_CLOSED_CASES_2024=${LH_MY_CASES_2024}/00_2024_Closed_Cases

# A shortcut to My Drive
export GOOGLE_DRIVE=$HOME/Google\ Drive/My\ Drive
export GOOGLE_DRIVE_2022=${GOOGLE_DRIVE}/2022\ UD\ Case\ Prep/Lincoln\ Harvey\ 2022
export GOOGLE_DRIVE_2023=${GOOGLE_DRIVE}/2023\ UD\ Case\ Prep/Lincoln\ Harvey\ 2023
export GOOGLE_DRIVE_2024=${GOOGLE_DRIVE}/2024\ UD\ Case\ Prep/Lincoln\ Harvey\ 2024

export GD_CLOSED=00_2023_Closed_Cases

## JAVA TEMURIN-<VER>
# https://wolfpaulus.com/java-on-macos-monterey/
# Java is placed into $HOME/Library/Java/JavaVirtualMachines/temurin-<ver>/
# JAVA_HOME is produced by running /usr/libexec/java_home
export JAVA_HOME=$(/usr/libexec/java_home)
export ANT_HOME=/Users/minilolh/.local/share/java/ant/apache-ant-1.10.14

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
