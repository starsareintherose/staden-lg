#! /bin/sh
# shell script to be run on cele to back up a folder and transfer to project
# first argument specifies working directory second gives date
cd ${1}
# now move up one directory
# target directory is current dir, useful to save name for tar
tardir=`echo ${1} |  sed 's/\// /g' | awk '{print $NF;}'`
cd ..

# next line needed to prevent mt from rewinding at the end of each operation
# May not need this now because now mt -f /dev/nrst0 -CWL
unset TAPE

echo -n "load tape - how many data sets are already on this tape? "
read nmark
while test ! "$nmark"
  do
    echo -n "You must give a number of previous files. How many? "
    read nmark
  done

mt -f /dev/nrst0 fsf $nmark
if test ! $? = 0
  then
    echo "tape positioning failed - is tape loaded? or file number correct?"
    exit 1
  fi
# tar with rewind after done
tar cvef /dev/rst0 $tardir		 # temp modification
if test ! $? = 0
  then
    echo "tape archive failed - perhaps tape is write protected"
    exit 1
  fi

# now move back down
cd $tardir 
echo "Give project name to move data to (blank for no move) "
echo -n "and give path name starting from your home directory:"
read project

while test "$project"
  do 
    if test -d $HOME/$project
      then
	for i in `cat ${2}fn` ;
	   do
		mv -i $i $HOME/$project ;
		mv -i $i"ALF" $HOME/$project
	done
	mv -i ${2}fn $HOME/$project ;
#       rm -r Results${2} # temp change -cwl
	exit 0
      else
        echo -n "Project directory not found - please retype: "
	read project
      fi
  done






