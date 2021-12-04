#! /bin/csh -f
#
# screen-against-vector
#
#   Screen files against sequences
#
# Usage:
#   screen-against-vector in_fofn out_fofn
#
# NOTE:
#   This script:
#   1) determines a list of files to screen against
#   2) extracts the clipped sequences from the experiment files
#   3) screens each sequence against the files in (1)
#
#

onintr abort
if (! $?SQUIRREL_LOG) setenv SQUIRREL_LOG /dev/null

#
# Check usage
#
if ( $#argv != 2 ) then
	echo Usage: screen-against-vector in_fofn out_fofn | tee -a "$SQUIRREL_LOG"
	exit 2
endif

if ( ! -f $1 ) then
	echo screen-against-vector: file of file names $1 does not exist | tee -a "$SQUIRREL_LOG"
	exit 1
endif

if ( -e $2 ) then
	echo screen-against-vector: output file of file names $2 already exists | tee -a "$SQUIRREL_LOG"
	exit 1
endif

#
# Scripts and programs
#
set EXPAND = $SQUIRREL/scripts/staden-env.csh
set CONVERT = $SQUIRREL/scripts/exp2dap.awk

#
# Temporary files
#
set in = _in_$$
set out = _out_$$
set hits = _hits_$$
set misses = _misses_$$
set tmpdir = _tmpdir_$$

#
# Determine all sequences to screen against
#
set expfiles = `cat $1`
set f = `awk '/^SS/{$1="";print substr($0,2);}' $expfiles | sort -u`

#
# Set up for dap/screen against vector
#
sort $1 >! $in

if ( $#f != 0 ) then
	if ( -d $tmpdir ) /bin/rm -fr $tmpdir
	mkdir $tmpdir
	foreach file ( $expfiles )
		awk -f $CONVERT $file > $tmpdir/$file
	end
endif

set VECTOR_SCORE = 25
foreach vector ( $f )

	echo Screening against $vector | tee -a "$SQUIRREL_LOG"

	if (`wc -l $in | awk '{print $1;}'` == 0) goto skip

	#
	set xvec = $SQUIRREL/seqs/$vector
	if (! -f $xvec) then
		echo vector file $xvec does not exist...skipping | tee -a "$SQUIRREL_LOG"
		goto skip
	endif
	# this is a hack to make bap happy
	set xvec = SQUIRREL/seqs/$vector

	# determine files to screen against
	grep -l $vector `cat $in` >! $hits
	comm -23 $in $hits >! $misses

	/bin/rm -f $out
pushd $tmpdir > /dev/null
bap << EOF
3
!

18
y
../$hits
../$out
$xvec
$VECTOR_SCORE

!
EOF


	#
	# Check exit status
	#
	if ( $status ) then
		echo screen-against-vector: Error running bap/screen against vector | tee -a "$SQUIRREL_LOG"
		exit 1
	endif

	popd > /dev/null
	echo "" | tee -a "$SQUIRREL_LOG"

	#
	# Print out Failed this pass
	#
	tr -d ' ' < $out >! $in ; /bin/mv -f $in $out
	foreach file ( `comm -23 $hits $out` )
		echo Failed: ${file}: Matches screening sequence $vector | tee -a "$SQUIRREL_LOG"
		echo "PS   Matches screening sequence $vector" >> ${file}
	end

	#
	# Determine sequences passed to next round
	#
	sort $misses $out >! $in

skip:
end


# transfer successful passes to output file
/bin/cp $in $2

#
# Remove temporary files
#
/bin/rm -f $in $out $hits $misses
/bin/rm -fr $tmpdir

exit 0

abort:
#
# interruption not normal exit
#

exit 1
