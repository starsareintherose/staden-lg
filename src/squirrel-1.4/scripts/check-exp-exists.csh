#! /bin/csh -f
#
# check-exp-exists
#   Checks that experiments don't already exist in project
#   directory
#
#   Usage:
#      check-exp-exists project-directory session_file out_fofn
#

onintr abort
if (! $?SQUIRREL_LOG) setenv SQUIRREL_LOG /dev/null

#
# Check usage
#
if ( $#argv != 3 ) then
	echo "Usage: check-exp-exists project-directory session_file out_fofn" | tee -a "$SQUIRREL_LOG"
	exit 2
endif


if ( ! -d $1 ) then
	echo "check-exp-exists: Project directory $1 does not exist" | tee -a "$SQUIRREL_LOG"
	exit 1
endif

if ( ! -e $2 ) then
	echo "check-exp-exists: Session file $2 does not exist" | tee -a "$SQUIRREL_LOG"
	exit 1
endif

if ( -e $3 ) then
	echo "check-exp-exists: File of file names $3 already exists" | tee -a "$SQUIRREL_LOG"
	exit 1
endif

#
# Save typing if need to change
#
set FAILURES = $1/Failures
set EXPFILES = $1/Expfiles

#
# Determine experiment file list to process this time
#
set temp_fofn = _temp_fofn
if ( -e ${temp_fofn} ) /bin/rm -f ${temp_fofn}
awk 'BEGIN {FS = ":";} /^#/ {next;} {print $2;}' $2 > ${temp_fofn}


echo "" | tee -a "$SQUIRREL_LOG"
echo "Checking files don't alreay exist in $1" | tee -a "$SQUIRREL_LOG"
touch $3
foreach expname ( `cat ${temp_fofn}` )

	set file = `awk 'BEGIN {FS=":";} $2 == "'$expname'" { s = $1; } END { print s;}' $2`

	# details for log
	echo "**" $expname from $file | tee -a "$SQUIRREL_LOG"

	# Check expname doesn't already exist
	if ( -e $1/$expname || -e ${EXPFILES}/$expname || -e ${FAILURES}/$expname ) then
		echo Abandoned: ${expname}: Experiment already processed | tee -a "$SQUIRREL_LOG"
		goto skip
	endif

	echo $expname >> $3
	
skip:
end

#
# Remove temporary files
#
/bin/rm -f ${temp_fofn}

exit 0

abort:
#
# interruption not normal exit
#

exit 1
