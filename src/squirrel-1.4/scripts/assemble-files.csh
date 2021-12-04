#! /bin/csh -f
#
# assemble-files
#
#   Script to assemble data into a database using a file of file names.
#   Output from the initial assembly is directed to a file that is
#   deleted, hence nothing appears on the screen. The failed files are
#   then recompared with the output directed to another file. This
#   file is printed then both output files are deleted.
# 
# Usage:
#   assemble-files project file_of_filenames
#
# 27-May-1992 Set consensus cutoff percentage to 51 for assembly
# 15-Oct-1992 PROJECT was not set to information in databases table!
#	      Rather - it was set to $1 ("project" name)
# 22-Oct-1992 Should delete busy file for working version
# 05-Jan-1993 Check copying to working version is successful
# 13-Jan-1993 FAILURES_PASS1 is the important one when reporting
#             errors, not FAILURES_PASS2
# 18-May-1993 **** this script will only work with bap version 12.1 or greater ****
#	      Bap auto assembly now prompts for "Hide alignments?" (after "Permit entry?")
#	      We should respond "Y" first pass, "N" the second
#

onintr abort
if (! $?SQUIRREL_LOG) setenv SQUIRREL_LOG /dev/null

if ( $#argv != 2 ) then
	echo "Usage: assemble-files project file_of_filenames" | tee -a "$SQUIRREL_LOG"
	exit 2
endif

if  ( ! -f $2 ) then
	echo "assemble-files: File of file names not found" | tee -a "$SQUIRREL_LOG"
	exit 1
endif

#
# Commands used in this script
#
set RM = "/bin/rm -f"
set CP = "/bin/cp"
set MV = "/bin/mv"




# Directories
if ( $?SQUIRREL_SUB ) then
	set TABLES_DIR   = $SQUIRREL/tables-$SQUIRREL_SUB
else
	set TABLES_DIR   = $SQUIRREL/tables
endif




#
# Gather project infomation from lookup
#
set GET_LOOK_UP  = $TABLES_DIR/lookup.csh
set db_info = `$GET_LOOK_UP "$1" $TABLES_DIR/databases.lookup`
if ( $#db_info == 0 ) then
	echo assemble-files: No database information for $1 | tee -a "$SQUIRREL_LOG"
	exit 1
endif

echo "" | tee -a "$SQUIRREL_LOG"
echo Assembling data from file of filenames $2 into database $1 | tee -a "$SQUIRREL_LOG"
echo "" | tee -a "$SQUIRREL_LOG"

#
# database versions
#
set PROJECT = $db_info[3]
set V_CURRENT  = $db_info[4]
set V_PREVIOUS = "1"
set V_WORKING  = "X"

#
# Check for database's existence
#
if (! -e ${PROJECT}.SQ${V_CURRENT}) then
	echo assemble-files: Cannot find database ${PROJECT} version ${V_CURRENT} | tee -a "$SQUIRREL_LOG"
	exit 1
endif

#
# Check database isn't already busy
#
if (-e ${PROJECT}_BUSY${V_CURRENT}) then
	echo "assemble-files: Version ${V_CURRENT} of the database ${PROJECT} is currently being used" | tee -a "$SQUIRREL_LOG"
	echo "                Processing has been suspended - Please try again later" | tee -a "$SQUIRREL_LOG" 
	echo "                (running squirrel again will allow you to resume from this point)" | tee -a "$SQUIRREL_LOG" 
	exit 1
endif


#
# Delete old working version
#
$RM ${PROJECT}.RL${V_WORKING}
$RM ${PROJECT}.SQ${V_WORKING}
$RM ${PROJECT}.AR${V_WORKING}
$RM ${PROJECT}.CC${V_WORKING}
$RM ${PROJECT}.TG${V_WORKING}
$RM ${PROJECT}_BUSY${V_WORKING}

#
# Make a copy of the current version
#
# YUK! should check that each copy succeeds
$CP ${PROJECT}.RL${V_CURRENT} ${PROJECT}.RL${V_WORKING}
if ( $status ) then
	echo assemble-files: Error copying database file ${PROJECT}.RL${V_CURRENT} | tee -a "$SQUIRREL_LOG"
	exit 1
endif
$CP ${PROJECT}.SQ${V_CURRENT} ${PROJECT}.SQ${V_WORKING}
if ( $status ) then
	echo assemble-files: Error copying database file ${PROJECT}.SQ${V_CURRENT} | tee -a "$SQUIRREL_LOG"
	exit 1
endif
$CP ${PROJECT}.AR${V_CURRENT} ${PROJECT}.AR${V_WORKING}
if ( $status ) then
	echo assemble-files: Error copying database file ${PROJECT}.AR${V_CURRENT} | tee -a "$SQUIRREL_LOG"
	exit 1
endif
$CP ${PROJECT}.CC${V_CURRENT} ${PROJECT}.CC${V_WORKING}
if ( $status ) then
	echo assemble-files: Error copying database file ${PROJECT}.CC${V_CURRENT} | tee -a "$SQUIRREL_LOG"
	exit 1
endif
$CP ${PROJECT}.TG${V_CURRENT} ${PROJECT}.TG${V_WORKING}
if ( $status ) then
	echo assemble-files: Error copying database file ${PROJECT}.TG${V_CURRENT} | tee -a "$SQUIRREL_LOG"
	exit 1
endif

#
# Temporary files for assembly
#
set FAILURES_PASS1 = _fail.$$
set FAILURES_PASS2 = _fail2.$$
set ASSEMBLE_LOG = _show.$$
set REPORT = _show2.$$

#
# assembly parameters
#
set DEFAULT = ""
set CUT_OFF = 51
set VECTOR_SCORE = 20
set ASSEMBLY_SCORE = 20
set MAX_READ_PADS = 25
set MAX_CONTIG_PADS = 25
set MAX_MISMATCH = 8

#
# Determine assembly program
# ?? Why do this? Why not assume $db_info[5]
#
switch ( $db_info[5] )
case "bap":
	#set ASSEMBLE = /nfs/al/pubseq/pubseq/inhouse/sun-nematode/bin/bap
	set ASSEMBLE = bap
	breaksw
case "dap":
	echo assemble-files: Squirrel does not supoport database type $db_info[5] | tee -a "$SQUIRREL_LOG"
	exit 1
	##set ASSEMBLE = /nfs/al/pubseq/pubseq/inhouse/sun-nematode/bin/dap
	#set ASSEMBLE = dap
	#breaksw
deault:
	echo assemble-files: Unknown database type $db_info[5] | tee -a "$SQUIRREL_LOG"
	exit 1
endsw

#
# ASSEMBLE
#
# ---------------------------------ASSEMBLE---------------------------
$ASSEMBLE << EOF
3
y
${PROJECT}
${V_WORKING}

27
${DEFAULT}
${CUT_OFF}
${DEFAULT}
${DEFAULT}
${DEFAULT}

20
Y
y
Y
$2
${FAILURES_PASS1}
1
Y
${ASSEMBLY_SCORE}
${MAX_READ_PADS}
${MAX_CONTIG_PADS}
${MAX_MISMATCH}

7
1
${ASSEMBLE_LOG}

20
y
n
y
${FAILURES_PASS1}
${FAILURES_PASS2}
1
y
${ASSEMBLY_SCORE}
${MAX_READ_PADS}
${MAX_CONTIG_PADS}
${MAX_MISMATCH}

25
!

7
y

!
EOF
# =================================ASSEMBLE===========================

#
# Check for successful completion
#
if ( $status ) then
	echo assemble-files: Error during assembly | tee -a "$SQUIRREL_LOG"
	$RM ${PROJECT}.RL${V_WORKING}
	$RM ${PROJECT}.SQ${V_WORKING}
	$RM ${PROJECT}.AR${V_WORKING}
	$RM ${PROJECT}.CC${V_WORKING}
	$RM ${PROJECT}.TG${V_WORKING}
	exit 1
endif

#
# Switch databases 
#
# old copy to save copy
$RM ${PROJECT}.RL${V_PREVIOUS}
$RM ${PROJECT}.SQ${V_PREVIOUS}
$RM ${PROJECT}.AR${V_PREVIOUS}
$RM ${PROJECT}.CC${V_PREVIOUS}
$RM ${PROJECT}.TG${V_PREVIOUS}
# keep an old copy of the current version
$MV ${PROJECT}.RL${V_CURRENT} ${PROJECT}.RL${V_PREVIOUS}
$MV ${PROJECT}.SQ${V_CURRENT} ${PROJECT}.SQ${V_PREVIOUS}
$MV ${PROJECT}.AR${V_CURRENT} ${PROJECT}.AR${V_PREVIOUS}
$MV ${PROJECT}.CC${V_CURRENT} ${PROJECT}.CC${V_PREVIOUS}
$MV ${PROJECT}.TG${V_CURRENT} ${PROJECT}.TG${V_PREVIOUS}
# make the new version current
$MV ${PROJECT}.RL${V_WORKING} ${PROJECT}.RL${V_CURRENT}
$MV ${PROJECT}.SQ${V_WORKING} ${PROJECT}.SQ${V_CURRENT}
$MV ${PROJECT}.AR${V_WORKING} ${PROJECT}.AR${V_CURRENT}
$MV ${PROJECT}.CC${V_WORKING} ${PROJECT}.CC${V_CURRENT}
$MV ${PROJECT}.TG${V_WORKING} ${PROJECT}.TG${V_CURRENT}

#
# Determine assembly failures and successes
#
set temp_in = _temp_in.$$
set temp_out = _temp_out.$$
set temp_out2 = _temp_out2.$$
tr -d ' ' < $2 | sort > ${temp_in}
# no stripping of space needes as done by awk
sort ${FAILURES_PASS1} > ${temp_out}
# Strip out error codes
awk '{print $1;}' ${temp_out} > ${temp_out2}
set expfiles = `cat ${temp_out2}`
set errcodes = `awk '{if (NF==2)print $2;else print -1;}' ${temp_out}`

while ( $#expfiles )

	set expname = $expfiles[1]
	set expfile = Expfiles/$expfiles[1]
	set errcode = $errcodes[1]

	switch(${errcode})
	case 0:
		set reason = " : file not found"
		breaksw;
	case 1:
		set reason = " : read too short"
		breaksw;
	case 2:
		set reason = " : failed to align and not entered"
		breaksw;
	case 3:
		set reason = " : failed on entry"
		breaksw;
	case 4:
		set reason = " : failed to align but entered"
		breaksw;
	default:
		set reason = "" #Unknown
		breaksw;
	endsw

	if ( ${errcode} == 4 ) then
		# This actually assembled
		echo OK: ${expname}: Assembled $reason | tee -a "$SQUIRREL_LOG"
	else
		echo Rejected: ${expname}: Did not assemble $reason | tee -a "$SQUIRREL_LOG"
		if (-e ${expfile}) echo "PS   Did not assemble $reason" >> ${expfile}
	endif

	shift expfiles
	shift errcodes
end

#
# Determine passes
#
foreach expfile (`comm -23 ${temp_in} ${temp_out2}`)
	echo OK: ${expfile}: Assembled | tee -a "$SQUIRREL_LOG"
end

#
# Remove temporary files
#
$RM ${temp_in} ${temp_out} ${temp_out2}

#
# Produce a report
#
cat ${FAILURES_PASS1} >> ${PROJECT}.FAILS
echo Assembly for project ${PROJECT} on `date` >> ${REPORT}
echo In directory `pwd` >> ${REPORT}
echo Input file of file names $2 >> ${REPORT}
echo Parameters used >> ${REPORT}
echo Minimum initial match = ${ASSEMBLY_SCORE} >> ${REPORT}
echo Maximum pads in each reading = ${MAX_READ_PADS} >> ${REPORT}
echo Maximum pads in contig = ${MAX_CONTIG_PADS} >> ${REPORT}
echo Maximum percent mismatch = ${MAX_MISMATCH} >> ${REPORT}

echo "List of entry failures ">> ${REPORT}
cat ${FAILURES_PASS1} >> ${REPORT}

echo "Alignments for entry failures " >> ${REPORT}
cat ${ASSEMBLE_LOG} >> ${REPORT}

lpr ${REPORT}


#
# Remove temporary files
#
$RM ${FAILURES_PASS1} ${FAILURES_PASS2} ${ASSEMBLE_LOG} ${REPORT}

exit 0



abort:

#
# interruption - not successful conclusion
#

exit 1
