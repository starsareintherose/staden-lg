#! /bin/csh -f
#
# stealdata.csh
#
#    Borrow data and files from a neighbouring cosmid database
#
#
# Change log:
#
#   v1.2 Handle dap and bap databases
#   v1.3 Provide linking option
#   v1.4 Under no circumstances link sequence files
#   v1.5 Bug in option "l". Now works when databases have same name
#   v1.6 To work with new version of bap
#   v1.7 Variable report now set correctly
#   v1.8 Front and back end version
#

onintr abort

if ( $#argv != 8 ) then
	echo Usage: stealdata.csh ... (8 args)
	exit 1
endif

set project       = $1
set version       = $2
set db_type       = $3
set other_dir     = $4
set other_project = $5
set other_version = $6
set other_db_type = $7
set operation     = $8


#
# other - DON'T TOUCH
#
set trace_suffixes = ALF,RES,SCF

#
# check defaults
#
if (! -e ${project}.SQ${version}) then
	echo Project ${project} version ${version} does not exist
	exit 1
endif

if (! -d ${other_dir}) then
	echo Directory ${other_dir} does not exist
	exit 1
endif

if (! -e ${other_dir}/${other_project}.SQ${other_version}) then
	echo Project ${other_dir}/${other_project} version ${other_version} does not exist
	exit 1
endif

#
# Report file
#
set report = $cwd/_steal.rep.${project}
set count = 0
while ( -e $report )
	# echo Report file ${report} already exists...
	@ count ++
	set report = $cwd/_steal.rep.${project}.${count}
end

touch ${report}

#
# Output details of stealdata run
#
cat << EOF | tee -a ${report}
Stealdata v1.6
Borrow data from neighbouring cosmid database

project = ${project}
version = ${version}
db_type = ${db_type}
other_dir = ${other_dir}
other_project = ${other_project}
other_version = ${other_version}
other_db_type = ${other_db_type}
operation = ${operation}
EOF

switch ( ${operation} )
case "r":
	echo "    ( report only )" | tee -a ${report}
	breaksw
case "s":
	echo "    ( copy sequences only, report )" | tee -a ${report}
	breaksw
case "a":
	echo "    ( copy sequences and traces, report )" | tee -a ${report}
	breaksw
case "l":
	echo "    ( copy sequences and link traces, report )" | tee -a ${report}
	breaksw
default:
	echo "    ( unknown - report only )" | tee -a ${report}
	set operation = "r"
	breaksw
endsw
echo "" | tee -a ${report}

#
# create a temporary directory
#
echo Creating a temporary directory | tee -a ${report}
set temp_dir = _steal.dir.${other_project}
set count = 0
while (-e ${temp_dir})
	echo Temporary directory ${temp_dir} already exists | tee -a ${report}
	@ count ++
	set temp_dir = _steal.dir.${other_project}.${count}
end

set home_dir = `pwd`
mkdir ${temp_dir}

#
#consensus for database
#
set consensus = _steal.seq.${project}
set count = 0
while ( -e $consensus )
	echo Consensus file ${consensus} already exists | tee -a ${report}
	@ count ++
	set consensus = _steal.seq.${project}.${count}
end

#
# determine consensus
#
echo Determining the consensus for project ${project} version ${version} | tee -a ${report}
#
# FUDGE!!!
# The third question in option 8 (Calculate consensus)
# has changed in the latest version of bap.
# Was:
#     ? Make another consensus (y/n) (y) 
# Now is:
#     ? Staden format (y/n) (y) =
#
# Set fudge to reflect the appropriate choice
#
if ( ${db_type} == "bap" ) then
	set fudge = y
else
	set fudge = n
endif
${db_type} << EOF > /dev/null

y
${project}
${version}

8
${consensus}
y
${fudge}

!
EOF
if ( $status ) then
	echo stealdata: failed to make consensus | tee -a ${report}
	goto abort
endif


#
# move consensus to temp_dir
#
mv ${consensus} ${temp_dir}
if ( $status ) then
	echo stealdata: failed to move consensus to temp_dir | tee -a ${report}
	goto abort
endif


#
# copy other database to this directory
#
echo Copying project ${other_project} version ${other_version} | tee -a ${report}
cd ${temp_dir}
cp -i \
	${other_dir}/${other_project}.SQ${other_version} \
	${other_dir}/${other_project}.AR${other_version} \
	${other_dir}/${other_project}.RL${other_version} \
	${other_dir}/${other_project}.TG${other_version} \
	${other_dir}/${other_project}.CC${other_version} \
	.
if ( $status ) then
	echo stealdata: failed to copy other database | tee -a ${report}
	goto abort
endif


#
# file of file names in other database
#
set other_files = ${other_project}.all
set other_useless = ${other_project}.useless
set other_useful = ${other_project}.useful
set other_new = ${other_project}.fn.$$

#
# extract gel readings
#
set option_extract = 32
#
# screen against vector
#
set option_screen  = 18
#
echo Extracting gel readings from ${other_project} and | tee -a ${report}
echo "  screening against consensus from ${project}" | tee -a ${report}
${other_db_type} << EOF > /dev/null

y
${other_project}
${other_version}

${option_extract}
${other_files}
n

${option_screen}
y
${other_files}
${other_useless}
${consensus}
30

!
EOF
if ( $status ) then
	echo stealdata: failed to extact or screen readings | tee -a ${report}
	goto abort
endif

#
# determine useful files in other project
#
echo Determining useful gel readings | tee -a ${report}
sort ${other_files} | tr -d ' ' > ${other_files}.nice
sort ${other_useless} | tr -d ' ' > ${other_useless}.nice
comm -23 ${other_files}.nice ${other_useless}.nice > ${other_useful}

#
# create report header
#
echo "" | tee -a ${report}
echo Useful gel readings: | tee -a ${report}

#
# determine new files 
#
touch ${other_new}
set useful = 0
foreach x ( `cat ${other_useful}` )
	if ( -e ../$x ) then
		echo $x | tee -a ${report}
	else
		echo $x "- NEW" | tee -a ${report}
		echo $x >> ${other_new}
		@ useful ++
	endif
end

if ( $useful == 0 ) then
	echo No new gel readings | tee -a ${report}
	goto done	
endif

#
# transfer sequences
#
if (${operation} == "s") then
	echo "" | tee -a ${report}
	echo "Transfering sequence files: " | tee -a ${report}
	foreach x ( `cat ${other_new}` )
		cp -i ${other_dir}/${x} ..
		echo ${other_dir}/${x} | tee -a ${report}
	end
	# file of file names
	mv ${other_new} ..
	echo "" | tee -a ${report}
	echo File of file names is ${other_new} | tee -a ${report}
endif

#
# transfer sequences and traces
#
if (${operation} == "a") then
	echo "" | tee -a ${report}
	echo "Transfering sequence and trace files: " | tee -a ${report}
	foreach x ( `cat ${other_new}` )
		# copy sequence file
		cp -i ${other_dir}/${x} ..
		echo ${other_dir}/${x} | tee -a ${report}
		foreach file ( ${other_dir}/${x}{${trace_suffixes}} )
			if (-e $file) then
				cp -i $file ..
				echo $file | tee -a ${report}
			endif
		end
	end
	# file of file names
	mv ${other_new} ..
	echo "" | tee -a ${report}
	echo File of file names is ${other_new} | tee -a ${report}
endif

#
# transfer sequences and link traces
#
if (${operation} == "l") then
	echo "" | tee -a ${report}
	echo "Transfering sequence and trace files: " | tee -a ${report}
	set files = `cat ${other_new}`
	pushd .. >& /dev/null
	foreach x ( $files )
		# copy sequence file
		cp -i ${other_dir}/${x} .
		echo ${other_dir}/${x} | tee -a ${report}
		foreach file ( ${other_dir}/${x}{${trace_suffixes}} )
			# link to traces
			if (-e $file) then
				ln -s $file
				echo $file | tee -a ${report}
			endif
		end
	end
	popd >& /dev/null
	# file of file names
	mv ${other_new} ..
	echo "" | tee -a ${report}
	echo File of file names is ${other_new} | tee -a ${report}
endif

done:
#
# cleanup
#
echo Cleaning up | tee -a ${report}
cd ..
/bin/rm -r ${temp_dir}

#
# Finish up
#
echo "" | tee -a ${report}
echo "Completed" | tee -a ${report}

#
# mail report
#
echo Mailing you a report
mail `whoami` < ${report}


exit 0


#
#
#
abort:
	echo stealdata aborted `date` | tee -a ${report}
	if ( $?home_dir ) then
		cd $home_dir
		echo Cleaning up | tee -a ${report}
		/bin/rm -r ${temp_dir}
	endif

	exit 1
