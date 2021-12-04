#! /bin/csh -f
#
# excise_vector
#    Removes sequencing and cloning vector from sequences
#
# Usage:
#	excise-vector in_fofn out_fofn
#
# 5-June-1992
#    vepe now writes fails to a fail file
#

onintr abort
if (! $?SQUIRREL_LOG) setenv SQUIRREL_LOG /dev/null

#
# Check usage
#
if ( $#argv != 2 ) then
	echo "Usage: excise-vector in_fofn out_fofn" | tee -a "$SQUIRREL_LOG"
	exit 2
endif
if ( ! -f $1 ) then
	echo excise-vector: file of file names $1 does not exist. | tee -a "$SQUIRREL_LOG"
	exit 1
endif
if ( -e $2 ) then
	echo excise-vector: file $2 exists. | tee -a "$SQUIRREL_LOG"
	exit 1
endif

#
# Where the wild things are
#
set EXCISE_VECTOR = $SQUIRREL/bin/vepe
set VEPE_FAILS    = $SQUIRREL/scripts/vepe-fails.csh
#
# Temporary files
#
set temp_fofn = _vep_temp_fofn.$$
set fails1    = _vep_fails1.$$
set fails2    = _vep_fails2.$$
/bin/rm -f ${temp_fofn} ${fails1} ${fails2}

#
# Vep options, responses
#
set DEFAULT = ""

#************************************************************
# First pass - excise sequencing vector
#************************************************************
echo "" | tee -a "$SQUIRREL_LOG"
echo "Excising sequencing vector" | tee -a "$SQUIRREL_LOG"

${EXCISE_VECTOR} << EOF
1
$1
${temp_fofn}
${fails1}
${DEFAULT}
${DEFAULT}
${DEFAULT}
EOF
if ( $status ) then
	echo "excise_vector: Abnormal termination" | tee -a "$SQUIRREL_LOG"
	exit 1
endif

#
# Report failures
#
$VEPE_FAILS $fails1 1
if ( $status ) then
	echo "excise_vector: Abnormal termination" | tee -a "$SQUIRREL_LOG"
	exit 1
endif













#************************************************************
# Second pass - excise cloning vector
#************************************************************
echo "" | tee -a "$SQUIRREL_LOG"
echo "Excising cloning vector" | tee -a "$SQUIRREL_LOG"

${EXCISE_VECTOR} << EOF
2
${temp_fofn}
$2
${fails2}
${DEFAULT}
${DEFAULT}
${DEFAULT}
EOF
if ( $status ) then
	echo "excise_vector: Abnormal termination" | tee -a "$SQUIRREL_LOG"
	exit 1
endif

#
# Report failures
#
$VEPE_FAILS $fails2 2
if ( $status ) then
	echo "excise_vector: Abnormal termination" | tee -a "$SQUIRREL_LOG"
	exit 1
endif


#
# Tidy up
#
/bin/rm -f ${temp_fofn}
/bin/rm -f ${fails1} ${fails2}

exit 0


abort:
#
# interruption not normal exit
#

exit 1
