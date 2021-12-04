#
# log-to-summary.awk
#
#   Generate a summary from a squirrel log file
#
# Possible usage:
#   awk -f log-to-summary.awk logfile > summary
#
# Changes:
#  22-Oct-92 There are now defaults for readings lost in processing
#
BEGIN {
	count = 0;
}

/^\*\*/ {
	# Format: ** expfile from trace
	if (reverse[$2] == "") {
	    count++;
	    t = substr($0,length($2)+10);
	    sample[count] = $2;
	    trace[count] = t;
	    reverse[$2] = count;
            # defaults
	    status[count] = "Abandoned";
	    comment[count] = "lost in processing";

	}
}

/^Discarded:/ || /^Failed:/ || /^Abandoned:/ || /^OK:/ || /^Rejected:/ {
	# Format: Failed: expfile: reason
	#   -or-  Succeeded: expfile: comment
	if (length($2) > 1) {
		expfile = substr($2,1,length($2)-1);
		NN = reverse[expfile];
		status[NN] = substr($1,1,length($1)-1);
		comment[NN] = substr($0,length($1)+length($2)+3);
	}
}

#Miscellaneous lines
/^SQUIRREL/ || /^Processing/ || /^Project/ || /^Started at/ || /^Completed at/ || /^Aborted at/ || /^Pass/ || /^Resumed/ {
	print "#", $0;
}

END {
	print "#"; 
	maxTrace = 0; maxSample = 0; maxSTATUS = 0;
	for (i = 1 ; i <= count ; i++ ) {
		if (sample[i] != "") {
			if (maxTRACE < length(trace[i]))
			    maxTRACE = length(trace[i]);
			if (maxSAMPLE < length(sample[i]))
			    maxSAMPLE = length(sample[i]);
			if (maxSTATUS < length(status[i]))
			    maxSTATUS = length(status[i]);
		}
	}
	fmt = "%-" maxTRACE+1 "s  %-" maxSAMPLE "s  %-" maxSTATUS "s  %s\n";
	for (i = 1 ; i <= count ; i++ ) {
		if (sample[i] != "") {
			printf(fmt,trace[i] ":",sample[i],status[i],comment[i]);
		}
	}
}
	
