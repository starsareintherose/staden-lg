#! /bin/csh -f
#
# ABI-sess-file
#   Creates a session file for the files in the current working directory
#
#   Usage:
#      ABI-sess-file session-file
#
# 22-Jan-93
#     Also picks out files of format "Sample [0-9][0-9]{,.[0-9]}"

onintr abort
if (! $?SQUIRREL_LOG) setenv SQUIRREL_LOG /dev/null

# Usage
if ( $#argv != 1 ) then
	echo "Usage: ABI-sess-file session-file" | tee -a "$SQUIRREL_LOG"
	exit 2
endif

set filenames = $1
if (-e $filenames) /bin/rm -f $filenames
touch $filenames

# Programs and scripts
set GET_ABI_NAME  = $SQUIRREL/bin/getABISampleName

#
# Temporary files
#
set file_list = $1

#
# Create session file header
#
if ( -e ${file_list} ) /bin/rm -f ${file_list}
touch ${file_list}
head -7 "$SQUIRREL_LOG" | sed 's/^/# /' >> ${file_list}
cat << EOF >> ${file_list}
#
# You may...
#  + delete lines you don't wish to be processed
#  + change the experiment name
# But DON'T...
#  - create additional lines
#  - introduce white space
#
EOF

#
# Build a list of files in this directory and experiment names
#
echo "" | tee -a "$SQUIRREL_LOG"
echo Determining samples to be processed | tee -a "$SQUIRREL_LOG"
foreach file ( Sample*[0-9][0-9]{,.[0-9]} )
	echo -n $file "- " | tee -a "$SQUIRREL_LOG"

	# tidy up as we go - delete Seq files
	/bin/rm -f "$file.Seq"

	# get sample name
	set abiname = `$GET_ABI_NAME "$file"`
	if ( $status ) then
		echo "" | tee -a "$SQUIRREL_LOG"
		echo Discarded: : Cannot determine sample name for $file | tee -a "$SQUIRREL_LOG"
		goto skip
	endif

	# beautify it
	set expname = `echo $abiname | sed 's/ /_/g'`
	echo $expname | tee -a "$SQUIRREL_LOG"

	echo $file":"$expname >> ${file_list}

skip:
end

exit 0


abort:
#
# interruption - not successful completion
#

exit 1
