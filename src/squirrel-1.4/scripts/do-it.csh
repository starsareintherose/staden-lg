#! /bin/csh -f
#
# do_it
#     Process a batch work of readings
# 
# Usage:
#     do_it transfer_directory project
#

onintr abort
if (! $?SQUIRREL_LOG) setenv SQUIRREL_LOG /dev/null


# Usage
if ( $#argv != 2 ) then
	echo "Usage: do-it transfer_directory project" | tee -a "$SQUIRREL_LOG"
	exit 2
endif


set PROJECT = $2
set transfer_directory = "$1"

if ( ! -d "$transfer_directory" ) then
	echo "do-it: Bad folder machine or date" | tee -a "$SQUIRREL_LOG"
	exit 1
endif



# Directories
if ( $?SQUIRREL_SUB ) then
	set TABLES_DIR   = $SQUIRREL/tables-$SQUIRREL_SUB
else
	set TABLES_DIR   = $SQUIRREL/tables
endif


# Programs and scripts
set ABI_SESS_FILE    = $SQUIRREL/scripts/ABI-sess-file.csh
set EDIT_FILES       = $SQUIRREL/scripts/edit-session-file.csh
set CHECK_FILES      = $SQUIRREL/scripts/check-exp-exists.csh
set ABI_EXP_FILES    = $SQUIRREL/scripts/ABI-exp-files.csh
set BUILD_EXP_FILES  = $SQUIRREL/scripts/embellish-exp-files.csh
set QUALITY_CHECK    = $SQUIRREL/scripts/check-quality.csh
set VECTOR_EXCISE    = $SQUIRREL/scripts/excise-vector.csh
set TRANSFER_FILES   = $SQUIRREL/scripts/transfer-files.csh
set ASSEMBLE_FILES   = $SQUIRREL/scripts/assemble-files.csh
set SCREEN_FILES     = $SQUIRREL/scripts/screen-against-vector.csh
set BACKUP_DIR       = $SQUIRREL/scripts/tidy-files.csh
set LOG_TO_SUMMARY   = $SQUIRREL/scripts/log-to-summary.awk

#
# Gather information about this project
#
set GET_LOOK_UP  = $TABLES_DIR/lookup.csh
set db_info = `$GET_LOOK_UP $PROJECT $TABLES_DIR/databases.lookup`
if ( $#db_info == 0 ) then
	echo do-it: no database information for $PROJECT
	exit 2
endif
# Check things look ok
if ( ! -e ~$db_info[6]/$db_info[2]/$db_info[3].SQ$db_info[4] ) then
	echo do-it: Cannot find database ~$db_info[6]/$db_info[2]/$db_info[3] version $db_info[4]
	exit 2
endif
# Check owner ok
if ( $db_info[6] != $USER ) then
	echo do-it: User $USER not registered to use database ~$db_info[6]/$db_info[2]/$db_info[3] version $db_info[4]
	exit 2
endif


#
# Go to where all the action is
#
cd "$transfer_directory"


#
# Determine which pass this is
#
set pass_file = .squirrel
if (! -e ${pass_file}) then
	set last_pass  = 0
	set last_phase = completed
	touch ${pass_file}
else
	set last_pass  = `tail -1 ${pass_file} | awk '/^Pass/ {pass = $2;} END {print pass;}'`
	set last_phase = `tail -1 ${pass_file} | awk '/^Pass/ {phase = $4;} END {print phase;}'`
endif
@ pass_count = $last_pass + 1

set resume = 0

if ( "$last_phase" != "completed" ) then

	switch ( "$last_phase" )
	case started:
		set phase = "log file creation"; breaksw
	case session:
		set phase = "session file creation"; breaksw
	case edited:
		set phase = "session file editing"; breaksw
	case checked:
		set phase = "checking experiment files don't already exist"; breaksw
	case created:
		set phase = "experiment files creation"; breaksw
	case embellished:
		set phase = "experiment files embellishment"; breaksw
	case quality:
		set phase = "quality checking"; breaksw
	case excised:
		set phase = "vector removal"; breaksw
	case screened:
		set phase = "screen against vector"; breaksw
	case transferred:
		set phase = "file transfer"; breaksw
	case assembled:
		set phase = "sequence assembly"; breaksw
	case reported:
		set phase = "reporting"; breaksw
	case archived:
		set phase = "backing up files"; breaksw
	default:
		set phase = "***problem***" ; breaksw
	endsw

	echo "***"
	echo "*** WARNING\!\!\!"
	echo "***"
	echo "*** SQUIRREL did not successfully complete the last time it was"
	echo "*** run on this directory"
	echo "***"
	echo ""
	echo "Last phase successfully completed: $phase"
repeat:
	echo -n "Do you want to resume the previous pass? "
	set reply = $<
	switch ( $reply )
	case [yY]*:
		set pass_count = $last_pass
		set resume = 1;
		breaksw
	case [nN]*:
		set last_phase = "completed"
		breaksw
	default:
		echo "please reply yes or no"
		goto repeat
	endsw
	echo ""
	echo ""

endif


#
# START
#
setenv SQUIRREL_LOG "$transfer_directory"/log.Pass.${pass_count}


#
# Files and directories
#
# WARNING : the following line makes assumptions about directory names
#
# YUK! YUK! YUK! the following line is aweful
set temp = ( $1 )
set session =  $temp[2]$temp[3].${pass_count}
set sessfile =  sess.$session
set sessfile2 =  sess.$session.2
set filenames = fn.$session
set filenames2 = fn.$session.2
set filenames3 = fn.$session.3
set filenames4 = fn.$session.4

#
# PHASE 0: Start log file
#
if ( $last_phase == "completed" ) then
	if ( -e "$SQUIRREL_LOG" ) /bin/rm -f "$SQUIRREL_LOG"
	touch "$SQUIRREL_LOG"

	# log this phase
	set last_phase = started
	echo Pass ${pass_count} : ${last_phase} `date` >> ${pass_file}
endif

#
# Add messages to log
#
if ( $resume == 0) then
	echo SQUIRREL Version 1.4 | tee -a "$SQUIRREL_LOG"
	echo "" | tee -a "$SQUIRREL_LOG"
	echo Processing "$transfer_directory" | tee -a "$SQUIRREL_LOG"
	echo Project $PROJECT | tee -a "$SQUIRREL_LOG"
	echo Started at `date` | tee -a "$SQUIRREL_LOG"
        echo Pass ${pass_count} | tee -a "$SQUIRREL_LOG"
else
	echo Resumed at `date` | tee -a "$SQUIRREL_LOG"
        echo Pass ${pass_count} - Phase $phase | tee -a "$SQUIRREL_LOG"
endif

#
# Create session file files
#
if ( $last_phase == "started" ) then
	/bin/rm -f $sessfile
	$ABI_SESS_FILE $sessfile
	if ($status != 0) then
		echo "do-it: Error while creating session file" | tee -a "$SQUIRREL_LOG"
		exit 1
	endif

	# log this phase
	set last_phase = session
	echo Pass ${pass_count} : ${last_phase} `date` >> ${pass_file}
endif

#
# Allow the user to edit the session file to suit themselves
#
if ( $last_phase == "session" ) then
	/bin/rm -f $sessfile2
	$EDIT_FILES $sessfile $sessfile2
	if ( $status ) then
		echo "do-it: Error while editing session file" | tee -a "$SQUIRREL_LOG"
		exit 1
	endif
	/bin/mv $sessfile2 $sessfile

	# log this phase
	set last_phase = edited
	echo Pass ${pass_count} : ${last_phase} `date` >> ${pass_file}
endif

#
# Filter out experiments already processed
#
if ( $last_phase == "edited" ) then
	/bin/rm -f $filenames
	$CHECK_FILES ~$db_info[6]/$db_info[2] $sessfile $filenames
	if ( $status ) then
		echo "do-it: Error while checking for previous processing" | tee -a "$SQUIRREL_LOG"
		exit 1
	endif

	# log this phase
	set last_phase = checked
	echo Pass ${pass_count} : ${last_phase} `date` >> ${pass_file}
endif

#
# Create experiment files from session file
#
if ( $last_phase == "checked" ) then
	if ( `wc $filenames | awk '{print $2;}'` > 0 ) then
		/bin/rm -f $filenames2
		$ABI_EXP_FILES $sessfile $filenames $filenames2
		if ($status != 0) then
			echo "do-it: Error while creating experiment files" | tee -a "$SQUIRREL_LOG"
			exit 2
		endif
		/bin/mv -f $filenames2 $filenames
	else
		echo do-it: Skipping creation of experiment files | tee -a "$SQUIRREL_LOG"
	endif

	# log this phase
	set last_phase = created
	echo Pass ${pass_count} : ${last_phase} `date` >> ${pass_file}
endif

#
# Fill out the details on the experiment files
#
if ( $last_phase == "created" ) then
	if ( `wc $filenames | awk '{print $2;}'` > 0 ) then
		/bin/rm -f $filenames2
		$BUILD_EXP_FILES $filenames $filenames2
		if ($status != 0) then
			echo "do-it: Error while building experiment files" | tee -a "$SQUIRREL_LOG"
			exit 2
		endif
		/bin/mv $filenames2 $filenames
	else
		echo do-it: Skipping building of experiment files | tee -a "$SQUIRREL_LOG"
	endif

	# log this phase
	set last_phase = embellished
	echo Pass ${pass_count} : ${last_phase} `date` >> ${pass_file}
endif


#
# Remove poor quality reads
#
if ( $last_phase == "embellished") then
	if ( `wc $filenames | awk '{print $2;}'` > 0 ) then
		/bin/rm -f $filenames2
		$QUALITY_CHECK $filenames $filenames2
		if ($status != 0) then
			echo "do-it: Error while checking quality of reads" | tee -a "$SQUIRREL_LOG"
			exit 2
		endif
	else
		echo do-it: Skipping quality check of files | tee -a "$SQUIRREL_LOG"
		/bin/cp $filenames $filenames2
	endif

	# log this phase
	set last_phase = quality
	echo Pass ${pass_count} : ${last_phase} `date` >> ${pass_file}
endif

#
# Remove vector sequence
#
if ( $last_phase == "quality" ) then
	if ( `wc $filenames2 | awk '{print $2;}'` > 0 ) then
		/bin/rm -f $filenames3
		$VECTOR_EXCISE $filenames2 $filenames3
		if ($status != 0) then
			echo "do-it: Error while excising vectors from files" | tee -a "$SQUIRREL_LOG"
			exit 2
		endif
	else
		echo do-it: Skipping excising of vector from files | tee -a "$SQUIRREL_LOG"
		/bin/cp $filenames2 $filenames3
	endif

	# log this phase
	set last_phase = excised
	echo Pass ${pass_count} : ${last_phase} `date` >> ${pass_file}
endif






#
# Screen against vector
#
if ( $last_phase == "excised" ) then
	if ( `wc $filenames3 | awk '{print $2;}'` > 0 ) then
		/bin/rm -f $filenames4
		$SCREEN_FILES $filenames3 $filenames4
		if ($status != 0) then
			echo "do-it: Error while screening files" | tee -a "$SQUIRREL_LOG"
			exit 2
		endif
	else
		echo do-it: Skipping screening of vector from files | tee -a "$SQUIRREL_LOG"
		/bin/cp $filenames3 $filenames4
	endif

	# log this phase
	set last_phase = screened
	echo Pass ${pass_count} : ${last_phase} `date` >> ${pass_file}
endif






#
# Transfer files - some reformating and converting may occur
# NOTE: The second $filenames is in ~$db_info[6]/$db_info[2] directory
#
if ( $last_phase == screened ) then
	if ( `wc $filenames | awk '{print $2;}'` > 0 ) then
		/bin/rm -f ~$db_info[6]/$db_info[2]/$filenames 
		$TRANSFER_FILES ~$db_info[6]/$db_info[2] $filenames $filenames 
		if ($status != 0) then
			echo "do-it: Error while transferring files" | tee -a "$SQUIRREL_LOG"
			exit 2
		endif
	else
		echo do-it: Skipping transfer of files | tee -a "$SQUIRREL_LOG"
	endif

	# log this phase
	set last_phase = transferred
	echo Pass ${pass_count} : ${last_phase} `date` >> ${pass_file}
endif



#
# Do the assembly
#
if ( $last_phase == transferred ) then
	pushd ~$db_info[6]/$db_info[2]
	if ( `wc $filenames | awk '{print $2;}'` > 0 ) then
		$ASSEMBLE_FILES $PROJECT $filenames
		if ($status != 0) then
			echo "do-it: Error while assembling files into database" | tee -a "$SQUIRREL_LOG"
			exit 2
		endif
	else
		echo do-it: Skipping assembly of files | tee -a "$SQUIRREL_LOG"
	endif
	popd

	# log this phase
	set last_phase = assembled
	echo Pass ${pass_count} : ${last_phase} `date` >> ${pass_file}
endif


#
# Generate log to tell us what has happened with all the files this pass
#
if ( $last_phase == assembled ) then
	pushd ~$db_info[6]/$db_info[2]
	if (! -e Logs) mkdir Logs
	set session_log = log.$session
	awk -f $LOG_TO_SUMMARY "$SQUIRREL_LOG" > Logs/$session_log
	/bin/cp Logs/$session_log "$transfer_directory"
	lpr Logs/$session_log
	popd

	# log this phase
	set last_phase = reported
	echo Pass ${pass_count} : ${last_phase} `date` >> ${pass_file}
endif


#
# Back up
#
if ( $last_phase == reported ) then
	$BACKUP_DIR $session_log
	if ($status != 0) then
		echo "do-it: Error while tidying up files in transfer directory" | tee -a "$SQUIRREL_LOG"
		exit 2
	endif

	# log this phase
	set last_phase = archived
	echo Pass ${pass_count} : ${last_phase} `date` >> ${pass_file}
endif


#
# Remove temporary files
#
/bin/rm -f $sessfile $sessfile2 $filenames $filenames2 $filenames3 $filenames4


#
# Completed successfully
#

# log this phase
set last_phase = completed
echo Pass ${pass_count} : ${last_phase} `date` >> ${pass_file}

echo Completed at `date` | tee -a "$SQUIRREL_LOG"

exit 0


abort:
#
# Interruption not normal termination
#

if ( $?SQUIRREL_LOG ) then
    echo Aborted at `date` | tee -a "$SQUIRREL_LOG"
else
    echo Aborted at `date`
endif

exit 1
