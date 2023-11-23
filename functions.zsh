# ~/.oh-my-zsh/custom/functions.zsh -*- mode:sh; -*-
# Time-stamp: <2023-11-22 21:05:39 minilolh>

guiserver () {
    GUISERVER=$(emacs --batch --eval='
    (progn
      (require (quote server))
      (princ
        (car (member "guiserver"
           (directory-files server-socket-dir nil
             directory-files-no-dot-files-regexp)))))')

    if [[ ${GUISERVER} == "guiserver" ]];
    then
	print "The guiserver is running."
    else
	emacs&
	print "Started the guiserver."
    fi
}

guiserver-kill () {
    emacsclient -s guiserver -e '(kill-emacs)'
    print "Killed the guiserver."
}

termserver () {
    TERMSERVER=$(emacs --batch --eval='
    (progn
      (require (quote server))
      (princ
        (car (member "termserver"
           (directory-files server-socket-dir nil
             directory-files-no-dot-files-regexp)))))')

    if [[ $TERMSERVER == "termserver" ]];
    then
	print "The termserver is running."
    else
	emacs -nw --daemon=termserver
	print "Started the termserver."
    fi
}

termserver-kill () {
    emacsclient -s termserver -e '(kill-emacs)'
    print "Killed the termserver."
}

emacs-servers-start () {
    guiserver
    termserver
}

emacs-servers-kill () {
    guiserver-kill
    termserver-kill
}

# This shutdown function kills everything completely no-questions-asked dead.
shutdown () {
    emacs-servers-kill
    tmux kill-server
    exec pkill -lf iTerm2
}

emacs-servers-help () {
    print "guiserver
termserver
guiserver-kill
termserver-kill
emacs-servers-start
emacs-servers-kill
shutdown
ect
ecg"
}

alias ect='emacsclient -s termserver'
alias ecg='emacsclient -s guiserver'
alias mkd='mkdocs serve'
alias mkdkill='pkill -lf mkdocs\ serve'

zbkp () {

    USAGE="zbkp -x # | -c # | -u #"
    OPTARG=
    # printf "Number of positional parameters: %d\n" $#

    # SYNTAX: getopts optstring name
    # optstring syntax: ":c:u:x:"
    # If a letter is followed by a ‘:’, that option requires an argument
    # A leading ‘:’ in optstring causes getopts to store the letter of any invalid option in OPTARG
    # and to set 'name' to ‘?’ for an unknown option
    # and to ‘:’ when a required argument is missing
    # 'name' (e.g., opt): Each time it is invoked, getopts places the option letter it finds in the shell parameter name
    # OPTIND: The index of the next arg is stored in OPTIND
    # OPTARG: The option argument, if any, is stored in OPTARG

    # c :> tar -cf tarfile files
    # u :> tar -uf tarfile files
    # x :> tar -xf tarfile -C dir

    getopts ":c:u:x:" opt
#   printf "arg: %s\n" $opt
#   printf "OPTIND: %d\n" $OPTIND
#   printf "OPTARG: %s\n" $OPTARG

    # First, check for less than 2 positional arguments.
    # Second check for a missing required option argument.
    # opt will be set to ':' for a missing required argument.
    # Third, check for an invalid option.  OPTARG will have an invalid letter stored in it.
    # opt will be set to '?' for an unknown option
    # Then execute the first option and argument combination, ignoring all others.

    # Check for less than 2 options; exit with error and usage statement.
    if [[ $# -lt 2 ]]; then
        printf "ERROR: Usage: %s\n" $USAGE
        return -1
    fi

    # Check for invalid option, e.g., something other than c, u, x
    if [[ $opt == "?" ]]; then
        printf "ERROR: Unknown option: %s\n" $OPTARG
        printf "Usage: %s\n" $USAGE
        return -1
    fi

    # Check for a missing required argument
    # Because the number of positional arguments is checked, this will never trigger.
    if [[ $opt == ":" ]]; then
        printf "ERROR: Missing a required argument, such as '13'"
        printf "Usage: %s\n" $USAGE
        return -1
    fi

    case $opt in
        c) printf "executing tar -c %s\n" $OPTARG ;;
        u) printf "executing tar -u %s\n" $OPTARG ;;
        x) printf "executing tar -x %s\n" $OPTARG ;;
        *) printf "Should never get here." ;;
    esac
}

emacs-servers-start
