#! /bin/csh -f
#
# ABI-exp-files
#   Creates an experiment file for each trace file in the current
#   directory, and a file of filenames for them.
#
#   Usage:
#      ABI-exp-files session-file fofn_in fofn_out
#

onintr abort
if (! $?SQUIRREL_LOG) setenv SQUIRREL_LOG /dev/null

# Usage
if ( $#argv != 3 ) then
	echo "Usage: ABI-exp-files session-file fofn_in fofn_out" | tee -a "$SQUIRREL_LOG"
	exit 2
endif

set sessfile = $1
if (! -e $sessfile) then
	echo "ABI-exp-files: Session file does not exist" | tee -a "$SQUIRREL_LOG"
	exit 1
endif

if ( ! -e $2 ) then
	echo "ABI-exp-files: File of file names does not exist" | tee -a "$SQUIRREL_LOG"
	exit 1
endif

set filenames = $3
if ( -e $filenames) /bin/rm -f $filenames
touch $filenames

# Directories
if ( $?SQUIRREL_SUB ) then
	set TABLES_DIR   = $SQUIRREL/tables-$SQUIRREL_SUB
else
	set TABLES_DIR   = $SQUIRREL/tables
endif

# Programs and scripts
set GET_ABI_DATE  = $SQUIRREL/scripts/ABI-gel-date.awk
set MAKE_SCF_FILE = $SQUIRREL/bin/makeSCF

# Process each file
echo "" | tee -a "$SQUIRREL_LOG"
echo "Creating experiment file for:" | tee -a "$SQUIRREL_LOG"
foreach expname ( `cat $2` )

	# specify file names
	set file = `awk 'BEGIN {FS=":";} $2 == "'$expname'" { s = $1; } END { print s;}' ${sessfile}`
	set expfile = $expname
	set tracefile = ${expfile}SCF

	# log
	echo $expname | tee -a "$SQUIRREL_LOG"

	# ensure the experiment doesn't already exist in this directory
	if ( -e $expfile ) /bin/rm -f $expfile

	# create experiment file
	touch $expfile

	# include obvious information
	echo "ID   $expname" >> $expfile
	echo "EN   $expname" >> $expfile
	echo "MN   $file" >> $expfile
	echo "MT   ABI" >> $expfile

	# Make standard chromatogram file
	$MAKE_SCF_FILE -ABI "$file" -output $tracefile
	if ( $status ) then
		echo Discarded: ${expfile}: Failed to create SCF trace file | tee -a "$SQUIRREL_LOG"
		/bin/rm -f $tracefile
		goto skip
	endif
	echo "LN   $tracefile" >> $expfile
	echo "LT   SCF" >> $expfile
	
	# get date information
	# YUK! next to useless
	#set DT = `echo $date | awk -f $GET_ABI_DATE`
	#if ( $DT == "" ) set DT = `date +%d-%h-19%y`
	set DT = `date +%d-%h-19%y`
	echo "DT   $DT" >> $expfile
	# YUK! next to useless
	# echo MC "ABI-$machine" >> $expfile
	echo "CC   Dir: $cwd" >> $expfile

	# update file of file names
	echo $expfile >> $filenames

# skip to here on error
skip:

end

exit 0


abort:
#
# interruption - not successful completion
#

exit 1
