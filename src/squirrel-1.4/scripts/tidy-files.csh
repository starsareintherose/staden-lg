#! /bin/csh -f
#
# tidy-files
#     Remove processed (not abandoned) files from transfer directory
#
# Usage:
#     tidy-files session-file
#

onintr abort
if (! $?SQUIRREL_LOG) setenv SQUIRREL_LOG /dev/null

#
# Check usage
#
if ( $#argv != 1 ) then
	echo "Usage: tidy-files session-file | tee -a "$SQUIRREL_LOG"
	exit 2
endif

if ( ! -e $1 ) then
	echo tidy-files: Session log file does not exist | tee -a "$SQUIRREL_LOG"
	exit 1
endif

#
# Programs and scripts and things
#

#
# Temporary files
#
set includes = _incl.$$
set commands = _comm.$$
/bin/rm -f $includes $commands

echo "" | tee -a "$SQUIRREL_LOG"
echo Tidying up transfer directory | tee -a "$SQUIRREL_LOG"

#
# Remove unneeded files in directory
#
awk 'BEGIN {FS=":";} /^#/ {next;} /Abandon/ {next;} {print $1;}' $1 > $includes
sed 's/^/\/bin\/rm -f "/;s/$/"/' $includes > $commands
source $commands

/bin/rm -f $1
/bin/rm _*


#
# Remove temporary files
#
/bin/rm -f $includes $commands



exit 0

abort:
#
# interruption not normal exit
#

exit 1
