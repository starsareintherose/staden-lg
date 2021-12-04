#! /bin/csh -f
#
# vepe-fails
#    Parse a vepe failure file and report errors
#
# Usage:
#	vepe-fails vepe_fail_file vepe_task
#
# where vepe_task is
#    1 Mark sequencing vector
#    2 Mark cloning vector
#

onintr abort
if (! $?SQUIRREL_LOG) setenv SQUIRREL_LOG /dev/null

#
# Check usage
#
if ( $#argv != 2 ) then
	echo "Usage: vepe-fails vepe_fail_file vepe_task" | tee -a "$SQUIRREL_LOG"
	exit 2
endif
if ( ! -f $1 ) then
	echo vepe-fails: failure file does not exist | tee -a "$SQUIRREL_LOG"
	exit 1
endif



#
# Strip out failures and reasons
#
set expfiles = `awk '{print $1;}' $1`
set errcodes = `awk '{if (NF==2)print $2;else print -1;}' $1`

while ( $#expfiles )

	set expname = $expfiles[1]
	set expfile = $expfiles[1]
	set errcode = $errcodes[1]

	switch(${errcode})
	case 1:
		set reason = "Couldn't open experiment file"
		breaksw;
	case 2:
		set reason = "No reading"
		breaksw;
	case 3:
		set reason = "Reading too short for vep"
		breaksw;
	case 4:
		set reason = "No vector file name"
		breaksw;
	case 5:
		set reason = "No cloning site"
		breaksw;
	case 6:
		set reason = "No priming site"
		breaksw;
	case 7:
		set reason = "Couldn't open vector file"
		breaksw;
	case 8:
		set reason = "Error writing to experiment file"
		breaksw;
	case 9:
		switch ( $2 )
		case 1:
			set reason = "Completely sequencing vector"
			breaksw
		case 2:
			set reason = "Completely cloning vector"
			breaksw
		default:
			set reason = "Completely vector"
			breaksw
		endsw
		breaksw;
	default:
		set reason = "Reason unknown" #Unknown
		breaksw;
	endsw

	if ( ${errcode} == 3  || ${errcode} == 9 ) then
		# vep failures
		echo Failed: ${expname}: $reason | tee -a "$SQUIRREL_LOG"
		if (-e ${expfile}) echo "PS   $reason" >> ${expfile}
	else
		# vep abandons
		echo Abandoned: ${expname}: Vep failed : $reason | tee -a "$SQUIRREL_LOG"
	endif


	# Cycle
	shift expfiles
	shift errcodes
end





exit 0


abort:
#
# interruption not normal exit
#

exit 1
