#! /bin/csh -f
#
# transfer-files
#   Copies files specified by a file of file names from the current
#   directory to the project_directory, making file format changes
#   where necessary.
#
#   When running with dap experiment files are converted to xdap files.
#
#   Usage:
#      transfer-files project-directory in_fofn out_fofn
#

onintr abort
if (! $?SQUIRREL_LOG) setenv SQUIRREL_LOG /dev/null

#
# Check usage
#
if ( $#argv != 3 ) then
	echo "Usage: transfer-files project-directory in_fofn out_fofn" | tee -a "$SQUIRREL_LOG"
	exit 2
endif


if ( ! -d $1 ) then
	echo "transfer-files: Project directory $1 does not exist" | tee -a "$SQUIRREL_LOG"
	exit 1
endif

if ( ! -e $2 ) then
	echo "transfer-files: File of file names $2 does not exist" | tee -a "$SQUIRREL_LOG"
	exit 1
endif

if ( -e $1/$3 ) then
	echo "transfer-files: File of file names $1/$3 already exists" | tee -a "$SQUIRREL_LOG"
	exit 1
endif

set CONVERT_EXP_FILE = $SQUIRREL/scripts/exp2dap.awk

#
# Save typing if need to change
#
set FAILURES = $1/Failures
set EXPFILES = $1/Expfiles

# Repository for experiment files and failures
if (! -e ${EXPFILES}) mkdir ${EXPFILES}
if (! -e ${FAILURES}) mkdir ${FAILURES}

echo "" | tee -a "$SQUIRREL_LOG"
echo "Transferring files from $cwd to $1" | tee -a "$SQUIRREL_LOG"
touch $1/$3
foreach file ( `cat $2` )

	# Check file exists in transfer directory
	if (! -e $file ) then
		# check it is processed
		if ( ! -e ${EXPFILES}/$file ) then
			echo Abandoned: ${file}: Problems transferring | tee -a "$SQUIRREL_LOG"
			goto skip
		endif

		echo transfer-files: warning - $file already transferred
		#		set PS = `awk '/^PS/ {$1="";line = substr($0,2);} END {print line;}' ${EXPFILES}/$file`
		#		goto recover
		goto skip
	endif

	# Ensure files don't already exist
	/bin/rm -f $1/$file ${EXPFILES}/$file ${FAILURES}/$file

	# Determine directory for sequence files and trace files
	set PS = `awk '/^PS/ {$1="";line = substr($0,2);} END {print line;}' $file`

	if ( "$PS" == "" ) then
		set dir = "$1"		
	else
		if ( $PS[1] == "Matches" ) then
			set dir = "$1"		
		else
			set dir = "${FAILURES}"
		endif
	endif


	# Convert experiment file
	awk -f $CONVERT_EXP_FILE $file > $dir/$file
	if ( $status ) then
		echo Abandoned: ${file}: Converting experiment file 
		/bin/rm -f $dir/$file
		goto skip
	endif

	# Transfer Local Trace file
	set LN = `awk '/^LN/ {$1="";line = substr($0,2);} END {print line;}' $file`
	if ( "$LN" != "" ) then
		/bin/cp $LN $dir
		if ( $status ) then
			echo Abandoned: ${file}: Transferring trace file 
			/bin/rm -f $dir/$LN
			goto skip
		endif
	endif

	# Copy experiment file
	# This should be the last operation before removal of files
	/bin/cp $file ${EXPFILES}
	if ( $status ) then
		echo "transfer-files.csh: failed to copy experiment file"
	endif

	# Get rid of files
	/bin/rm -f $file $LN

recover:
	# If not marked for failure add to file of file names
	if ( "$PS" == "" ) then
		echo $file >> $1/$3
	else
		if ( $PS[1] == "Matches" ) then
			# have a match with a vector... add to screen file
			set failf = "$1"/Screen.$PS[4]
			if ( ! -e "$failf" ) touch "$failf"
			echo $file >> "$failf"
		endif
	endif

skip:
end

exit 0

abort:
#
# interruption not normal exit
#

exit 1
