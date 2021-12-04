#! /bin/csh -f
#
# embellish-exp-files
#   Fill in the details of an experiment file once the machine specific bits
#   have been done.
#
#   Usage:
#      embellish-exp-files fofn_in fofn_out
#

onintr abort
if (! $?SQUIRREL_LOG) setenv SQUIRREL_LOG /dev/null

# Usage
if ( $#argv != 2 ) then
	echo Usage: embellish-exp-files fofn_in fofn_out | tee -a "$SQUIRREL_LOG"
	exit 2
endif
if ( ! -e $1 ) then
	echo embellish-exp-files: File of file names $1 does not exist | tee -a "$SQUIRREL_LOG"
	exit 1
endif
if ( -e $2 ) then
	echo embellish-exp-files: Output file of file names $2 already exists | tee -a "$SQUIRREL_LOG"
	exit 1
endif

# create output file of file names
touch $2

# Directories
if ( $?SQUIRREL_SUB ) then
	set TABLES_DIR   = $SQUIRREL/tables-$SQUIRREL_SUB
else
	set TABLES_DIR   = $SQUIRREL/tables
endif

# Programs and scripts
set GET_MACH_SEQ  = $SQUIRREL/bin/expGetSeq
set GET_CLIP_SEQ  = $SQUIRREL/scripts/clip-seq.awk
set GET_LOOK_UP   = $TABLES_DIR/lookup.csh
set MAKE_SCF_FILE = $SQUIRREL/bin/makeSCF





# Process each file
echo "" | tee -a "$SQUIRREL_LOG"
echo "Embellishing experiment file for:" | tee -a "$SQUIRREL_LOG"
foreach file ( `cat $1` )

	echo $file | tee -a "$SQUIRREL_LOG"

	if ( ! -e $file ) then
		echo Abandoned: ${file}: Experiment file missing | tee -a "$SQUIRREL_LOG"
		goto skip
	endif

	# check not embellished previously (look for OP)
	set operator = `awk '/^OP/ {$1="";line = substr($0,2);} END {print line;}' $file`
	if ( $operator != "" ) then
		# This file has been embellished already - clean it
		set tempfile = _temp.exp
		/bin/mv -f $file $tempfile
		sed '/^OP/,$d' $tempfile > $file
	endif

	# beautify it
	set expname = `awk '/^EN/ {$1="";line = substr($0,2);} END {print line;}' $file`
	if ( $expname == "" ) set expname = $file

	# specify file names
	set expfile = $expname
	set tracefile = `awk '/^LN/ {$1="";line = substr($0,2);} END {print line;}' $file`
	if ( $tracefile == "" ) set tracefile = ${expfile}SCF

	# include obvious information
	# OP must always be the first embellished record
	echo "OP   `whoami`" >> $expfile

	# gather information from lookup files

	# information on subclones/templates
	set template = `$GET_LOOK_UP $expname $TABLES_DIR/subclones.lookup`
	if ( $#template == 0 ) then
		echo Abandoned: ${expfile}: No template information for $expname | tee -a "$SQUIRREL_LOG"
		goto skip
	endif

	# information on clones and cloning vectors
	set clone = `$GET_LOOK_UP $template[2] $TABLES_DIR/clone-types.lookup`
	if ( $#clone == 0 ) then
		echo Abandoned: ${expfile}: No clone information for $template[2] | tee -a "$SQUIRREL_LOG"
		goto skip
	endif

	# information on sequencing clone sites
	set subclone = `$GET_LOOK_UP $template[5] $TABLES_DIR/seqclone-sites.lookup`
	if ( $#subclone == 0 ) then
		echo Abandoned: ${expfile}: No cloning site information for $template[5] | tee -a "$SQUIRREL_LOG"
		goto skip
	endif

	# information on sequencing vector sequence files
	set scfile = `$GET_LOOK_UP $template[5] $TABLES_DIR/vector-seqfiles.lookup`
	if ( $#scfile == 0 ) then
		echo Abandoned: ${expfile}: No sequence file for sequencing vector $template[5] | tee -a "$SQUIRREL_LOG"
		goto skip
	endif
	if ( ! -e $SQUIRREL/seqs/$scfile[2] ) then
		echo Abandoned: ${expfile}: Sequencing vector file does not exist | tee -a "$SQUIRREL_LOG"
		goto skip
	endif

	# information on cloning vector sequence files
	set cfile = `$GET_LOOK_UP $clone[2] $TABLES_DIR/vector-seqfiles.lookup`
	if ( $#cfile == 0 ) then
		echo Abandoned: ${expfile}: No sequence file for cloning vector $clone[2] | tee -a "$SQUIRREL_LOG"
		goto skip
	endif
	if ( ! -e $SQUIRREL/seqs/$cfile[2] ) then
		echo Abandoned: ${expfile}: Cloning vector file does not exist | tee -a "$SQUIRREL_LOG"
		goto skip
	endif

	# include info derived from lookups
	echo "TN   $expname:r" >> $expfile
	echo "SV   $template[5]" >> $expfile
	echo "SF   $SQUIRREL/seqs/$scfile[2]" >> $expfile
	echo "SI   $template[3]" >> $expfile
	echo "SC   $subclone[2]" >> $expfile
	echo "FM   $template[4]" >> $expfile

	# assume Universal primer U1
	#echo PN U1 >> $expfile

	# determine whether forwards or reverse read
	if ( $expname =~ *.[rR]* ) then
		# these experiments are reverse reads
		echo "DR   -" >> $expfile
		echo "SP   $subclone[4]" >> $expfile
	else
		# otherwise assume forward read
		echo "DR   +" >> $expfile
		echo "SP   $subclone[3]" >> $expfile
	endif

	# determine strands
	if ( $expname =~ *.[rRfF]* ) then
		# these experiments are double stranded
		echo "ST   2" >> $expfile
	else
		# otherwise assume single stranded
		echo "ST   1" >> $expfile
	endif

	# include cloning vector information
	echo "CN   $template[2]" >> $expfile
	echo "CV   $clone[2]" >> $expfile
	echo "CF   $SQUIRREL/seqs/$cfile[2]" >> $expfile


	# determine lists of screens
	$GET_LOOK_UP $template[2] $TABLES_DIR/screens.lookup | awk '{$1="";print  "SS   " substr($0,2);}' >> $expfile


	# retrieve the sequence
	set temp_file = $expfile.temp.$$
	$GET_MACH_SEQ -SCF "$tracefile" -output $temp_file
	if ( $status ) then
		echo Discarded: ${expfile}: Error reading sequence from trace | tee -a "$SQUIRREL_LOG"
		/bin/rm -f $temp_file
		goto skip
	endif

	# include sequence in experiment file
	cat $temp_file >> $expfile
	/bin/rm -f $temp_file

	# Determine poor quality parts
	awk -f $GET_CLIP_SEQ $expfile >> $expfile

	# update file of file names
	echo $expfile >> $2

# skip to here on error
skip:

end


exit 0


abort:
#
# interruption - not successful completion
#

exit 1
