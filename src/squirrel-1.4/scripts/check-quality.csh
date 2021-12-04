#! /bin/csh -f
#
# check-quality
#   Check a file of file name for quality
#
#   Usage:
#      create-exp-files fofn_in fofn_out
#

onintr abort
if (! $?SQUIRREL_LOG) setenv SQUIRREL_LOG /dev/null

# Usage
if ( $#argv != 2 ) then
	echo "Usage: check-quality fofn_in fofn_out" | tee -a "$SQUIRREL_LOG"
	exit 2
endif
if ( ! -e $1 ) then
	echo check-quality: file of file names $1 does not exist | tee -a "$SQUIRREL_LOG"
	exit 1
endif
if ( -e $2 ) then
	echo check-quality: output file of file names $1 already exists | tee -a "$SQUIRREL_LOG"
	exit 1
endif

# Programs and scripts
set CHECK_QUALITY = $SQUIRREL/scripts/quality-check.awk

#
touch $2

# Process each file
echo "" | tee -a "$SQUIRREL_LOG"
echo "Checking for quality:" | tee -a "$SQUIRREL_LOG"
foreach file ( `cat $1` )

	# Do a quality check
	set qc = `awk -f $CHECK_QUALITY $file`
	if ($qc != "0") then
		echo Failed: ${file}: Trace quality | tee -a "$SQUIRREL_LOG"
		echo "PS   Trace quality" >> ${file}
		goto skip
	endif

	# update file of file names
	echo $file >> $2

# skip to here on error
skip:

end


exit 0


abort:
#
# interruption - not successful completion
#

exit 1
