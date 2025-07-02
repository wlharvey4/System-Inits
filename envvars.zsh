# ~/.oh-my-zsh/custom/envvars.zsh -*- mode:sh; -*-
# Time-stamp: <2025-07-01 23:16:50 lolh-mbp-16>

printf "${CYAN}sourcing envars.zsh...${CLEAR}		"

## DOCUMENTATION PATHS
export INFOPATH=/opt/local/share/info:/usr/local/share/info:
export MANPATH=/opt/local/share/man:/usr/local/share/man

## ICLOUD CLOUDDOCS
#export CLOUDDOCS=$HOME/Library/Mobile\ Documents/com\~apple\~CloudDocs
#export CD_DOCUMENTS=${CLOUDDOCS}/Documents
#export CD_DOWNLOADS=${CLOUDDOCS}/Downloads/
#export CD_DATA=${CD_DOCUMENTS}/Data




# GOOGLE DRIVE
export GOOGLE_DRIVE=$HOME/Library/CloudStorage/GoogleDrive-lincoln@ccvlp.org
export GD=$HOME/Google\ Drive

# MY DRIVE
export MY_DRIVE=${GOOGLE_DRIVE}/My\ Drive
export MY_GD=${GD}/My\ Drive

# SHARED DRIVES
export SHARED_DRIVES=${GOOGLE_DRIVE}/Shared\ drives
export SH_GD=${GD}/Shared\ drives

# RTC DRIVES
export RTC=${SHARED_DRIVES}/Right\ to\ Counsel
export JT_HJP_RTC=${SHARED_DRIVES}/Joint\ HJP\ +\ RTC
export ALL_STAFF=${SHARED_DRIVES}/All\ Staff

# My Open Cases
export RTC_2022=${RTC}/2022\ UD\ Case\ Prep/Lincoln\ Harvey\ 2022
export RTC_2023=${RTC}/2023\ UD\ Case\ Prep/Lincoln\ Harvey\ 2023
export RTC_2024=${RTC}/2024\ UD\ Case\ Prep/Lincoln\ Harvey\ 2024
export RTC_2025=${RTC}/2025\ UD\ Case\ Prep/Lincoln\ Harvey\ 2025
# My Closed Cases
export RTC_2022_CLOSED=${RTC_2022}/00_Closed\ Cases
export RTC_2023_CLOSED=${RTC_2023}/00_Closed\ Cases
export RTC_2024_CLOSED=${RTC_2024}/00_Closed\ Cases
export RTC_2025_CLOSED=${RTC_2025}/00_Closed\ Cases

# Public Documents
export PUBLIC_DOCS=${MY_DRIVE}/Public\ Documents.gdrive
# Prive Documents
export PRIVATE_DOCS=${MY_DRIVE}/Lincoln-Private

export HTTP_GOOGLE_DRIVE=https://drive.google.com/drive




## JAVA TEMURIN-<VER>
# https://wolfpaulus.com/java-on-macos-monterey/
# Java is placed into $HOME/Library/Java/JavaVirtualMachines/temurin-<ver>/
# JAVA_HOME is produced by running /usr/libexec/java_home
export JAVA_HOME=$(/usr/libexec/java_home)
export ANT_HOME=/Users/minilolh/.local/src/java/ant/apache-ant-1.10.14

# WORK
export WORK=$HOME/.local/work
export WORKFIN=${WORK}/workfin

# LEDGER/HLEDGER
export LEDGER=${WORKFIN}/ledger
export LEDGER_FILE=${LEDGER}/zero.ledger
export HLEDGER=${WORKFIN}/hledger
#export LEDGER_FILE=${HLEDGER}/hledger.journal

#COMMON LISP
export SBCL_HOME=$HOME/.local/src/common-lisp/implementations/lib/sbcl
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

printf "${CYAN}done sourcing envars.zsh${CLEAR}\n"
