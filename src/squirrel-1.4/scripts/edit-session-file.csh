#! /bin/csh -f
#
# edit-session-file
#
#    Allow a session file to be edited, then parse it for editing
#    mistakes.
#
# Usage:
#    edit-session-file fof_in fof_out
#

onintr abort
if (! $?SQUIRREL_LOG) setenv SQUIRREL_LOG /dev/null

# Usage
if ( $#argv != 2 ) then
	echo Usage: edit-session-file fof_in fof_out
	exit 2
endif

if ( ! -f $1 ) then
	echo edit-session-file: file of file names $1 does not exist. | tee -a "$SQUIRREL_LOG"
	exit 1
endif
if ( -e $2 ) then
	echo exit-session-file: file $2 exists. | tee -a "$SQUIRREL_LOG"
	exit 1
endif

#
# Where the wild things are
#
set PARSE_EDIT = $SQUIRREL/scripts/parse-session-file.awk


#
# Temporary files
#
set sess_edit = _session.$$
set parse_out = _parse.$$
set spacer = _spacer.$$

#
# Make a copy
#
/bin/cp $1 ${sess_edit}

#
# Allow the user to edit the file
#
$EDITOR ${sess_edit}

#
# Create a spacer - DO NOT CHANGE THIS!!!!
#
cat << EOF > ${spacer}
##PASS-2##
EOF

#
# Parse the edited file for errors
#
awk -f $PARSE_EDIT $1 ${spacer} ${sess_edit} > ${parse_out}

#
# Report errors
#
echo "" | tee -a "$SQUIRREL_LOG"
echo Parsing edited file... | tee -a "$SQUIRREL_LOG"
grep -v '^OUT>' ${parse_out} | tee -a "$SQUIRREL_LOG"

#
# Gather clean lines
#
awk '/^OUT>/ {print substr($0,length($1)+2);}' ${parse_out} > $2

#
# Tidy up
#
/bin/rm -f ${sess_edit} ${parse_out} ${spacer}



exit 0


abort:
#
# interruption not normal exit
#

exit 1
