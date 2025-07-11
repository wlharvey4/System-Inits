# ~/.oh-my-zsh/custom/functions.zsh -*- mode:sh; -*-
# Time-stamp: <2024-11-17 23:52:53 lolh-mbp-16>

printf "${PURPLE}sourcing functions.zsh...${CLEAR}	"

guiserver () {
#    if [[ $(emacs -Q -batch --eval \
#                  '(progn (require (quote server)) (setq server-name "guiserver") (princ (server-running-p)))') == "t" ]];
    if [[ $GUISERVER == "RUNNING" ]];
    then
        echo "Guiserver is running";
    else
        if [[ $GUISERVER == "STOPPED" ]];
        then
            GUISERVER="RUNNING"
           echo "Starting Guiserver"
            ~/.local/bin/emacs&
         else
            echo Error: Guiserver is $GUISERVER
        fi
    fi
}

guiserver-kill () {
    if [[ GUISERVER="RUNNING" ]];
    then
        echo "Killing Guiserver"
        pkill -lf /.local/bin/emacs
        GUISERVER="STOPPED"
    else
        echo "Guiserver is not running"
    fi
}


termserver () {
    if [[ $TERMSERVER == "RUNNING" ]];
    then
        echo "Termserver is running."
    else
        TERMSERVER="RUNNING"
        echo "Startng Termserver"
        emacs -nw --daemon=termserver;
    fi
}

termserver-kill () {
    if [[ $TERMSERVER == "RUNNING" ]]
    then
        echo "Killing Termserver"
        pkill -lf termserver;
        TERMSERVER=STOPPED
    else
        echo "Termserver is not running."
    fi
}

tmuxserver () {
    if [[ -z $TMUX ]];
    then
        echo "Starting tmux"
        tmux a -t Home
    else
        echo "tmux is running"
    fi
}

startup () {
    termserver
    guiserver
}

shutdown () {
    pkill -lf emacs
    tmux kill-server
    exec pkill -lf iTerm
}

# startup () {
#     termserver
#     guiserver
#     tmux
#  }

# server-status () {
#     unset ss
#     ss=$(emacsclient -s $1 -e "(print \"$1\")") 2>/dev/null
#     printf "%s" ${ss:="nil"}
# }

# server-start () {
#     while getopts ":a:s:t:" name
#     do
#         case $name in
#             ("a") ss=$(server-status $OPTARG);
#                   if [[ $ss != \"$OPTARG\" ]]
#                   then
#                       ~/Applications/Emacs.app/Contents/MacOS/Emacs --eval '(require (quote server))' \
#                                                                     --eval "(setq server-name \"$OPTARG\")" \
#                                                                     --eval '(server-start)' \
#                           &
#                   fi
#                   printf "%s is running.\n" $OPTARG;
#                   ;;
#             ("s") ss=$(server-status $OPTARG);
#                   if [[ $ss != \"$OPTARG\" ]]
#                   then
#                       emacs --eval '(require (quote server))' \
#                             --eval "(setq server-name \"$OPTARG\")" \
#                             --eval '(server-start)' \
#                           &
#                   fi
#                   printf "%s is running.\n" $OPTARG;
#                   ;;
#             ("t") printf "opt: %s  arg: %s\n" $name $OPTARG; echo server termserver;
#                             ss=$(server-status $OPTARG);
#                             if [[ $ss != \"$OPTARG\" ]]
#                             then
#                                 emacs -nw --daemon=$OPTARG
#                             fi
#                             printf "%s is running.\n" $OPTARG;
#                             ;;
#             (":") printf "opt: %s  arg: %s\n" $name $OPTARG; echo missing argument ;;
#             ("?") printf "opt: %s  arg: %s\n" $name $OPTARG; echo wrong option ;;
#         esac
#     done
#     echo Done
# }

# guiserver () {
#     # if emacsclient -qs guiserver --eval '(prin1 "The guiserver is running.")' 2>/dev/null
#     if [[ ${GUISERVER} = "RUNNING" ]]; then
#         print "The guiserver is running."
#     else
#         # emacs should be in $PATH
# 	emacs&
#        	print "Started the guiserver."
#         GUISERVER="RUNNING"
#     fi
# }

# guiserver-stop () {
#     if [[ ${GUISERVER} = "RUNNING" ]]; then
#         emacsclient -s guiserver -e '(kill-emacs)'
#         GUISERVER="STOPPED"
#         print "Killed the guiserver."
#     else
#         :
#     fi
# }

# termserver () {
#     # if emacsclient -qs termserver --eval '(prin1 "The termserver is running.")' 2>/dev/null 
#     if [[ ${TERMSERVER} = "RUNNING" ]]; then
#         print "The termserver is running."
#     else
#         # emacs should be in $PATH
# 	emacs -nw --no-desktop --daemon=termserver
#         print "Started the termserver."
#         TERMSERVER="RUNNING"
#     fi
# }

# termserver-stop () {
#     if [[ $TERMSERVER="RUNNING" ]]; then
#         emacsclient -s termserver -e '(kill-emacs)'
#         TERMSERVER="STOPPED"
#         print "Killed the termserver."
#     else
#         :
#     fi
# }

# emacs-servers-stop () {
#     guiserver-stop
#     termserver-stop
# }

# # This shutdown function kills everything completely no-questions-asked dead.
# shutdown () {
#     emacs-servers-stop
#     tmux kill-server
#     exec pkill -lf iTerm
# }

# emacs-servers-help () {
#     print "guiserver
# termserver
# guiserver-stop
# termserver-stop
# emacs-servers-stop
# startup
# shutdown
# ect
# ecg"
# }

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

# 2023-12-04
ali () {
    # alisma -a $srcfile $destfile
    # Creates an alias of $srcfile (from Goodgle Drive) to $destfile (in local file).
    # Due to a limitation with iTerm not being able to open the Google Drive,
    # Terminal must be opened from a Finder window opened at the source,
    # such as "23-2-03006-06 Case Name", and then this command, `ali` is run
    # from that Terminal window.
    # TODO: This can be expanded to copy all files in the directory structure.

    SOURCE=$(basename $PWD) # the case name, e.g. 23-2-03006-06 Plaintiff v. Defendant
    FULL_SOURCE="$PWD"/Court\ File # The directory holding the pleadings
    FULL_DEST="$CCVLP_DATA"/"$SOURCE"/Court\ File # The directory for the new aliases
    #print "FULL_SOURCE is $FULL_SOURCE"
    #print "FULL_DEST is $FULL_DEST"

    mkdir -vp "${FULL_DEST}"

    for srcfile in ${FULL_SOURCE}/*.pdf; do
        destfile="${FULL_DEST}/$(basename $srcfile)"
        #print "$srcfile"
        #print "${destfile}\n"
        if ! [[ -f $destfile ]]; then
            #print "Creating ${destfile}."
            alisma -a "${srcfile}" "${destfile}"
        #else
            #print "${destfile} already exists; skipping."
        fi
    done
}

to () {

    cd "$@"
}

# alias toccvlp='to $CCVLP'
# alias tolcnotes='to $LCNOTES'
# alias tocases='to $CASES'
# alias tolsnotes='to $LSNOTES'
# alias pdftk-java='java -jar $HOME/.local/bin/pdftk-all.jar'

printf "${PURPLE}done sourcing functions.zsh${CLEAR}\n"
