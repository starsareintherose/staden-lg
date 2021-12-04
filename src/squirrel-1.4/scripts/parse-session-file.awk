#
# Parse edited session file
#
# Input:
#
#      (original session file)
#      ##PASS-2##
#      (edited session file)
#
# Output:
#      (annotated edited session file)
#      OUT> passed line...
#
#

BEGIN {
	FS = ":";
	pass = 1;
	count = 0;
	l = 0;
}

/^##PASS-2##/ {
	pass = 2;
	next;
}

pass == 2 { l++; printf("%3d %s\n",l,$0); }

/^#/ { next; }

pass == 1 {
	pass1sample[$1] = 1;
	next;
}

pass == 2 {

	# must be two fields
	if (NF != 2) {
		print "ERROR: wrong number of fields >>", $0;
		next;
	}

	# must be non null
	if (length($1)==0) {
		print "ERROR: sample missing >>",$0;
		next;
	}
	if (length($2)==0) {
		print "ERROR: experiment missing >>",$0;
		next;
	}

	# $1 should be in original file
	if (pass1sample[$1] != 1) {
		print "ERROR: sample", $1, "was not in original file"
		next;
	}

	# $1 should not be duplicated
	sample[$1]++;
	if (sample[$1] > 1) {
		print "ERROR: sample", $1, "appears more than once in edited file"
		line[sample2line[$1]] = "";
		next;
	}

	# $2 should look ok
	for(i=1;i<length($2);i++) {
		if(index("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789. ",substr($2,i,1))==0) {
			print "ERROR: experiment", $2, "contains invalid characters"
			next;
		}
	}

	# $2 should not be duplicated
	expname[$2]++;
	if (expname[$2] > 1) {
		print "ERROR: experiment", $2, "appears more than once in edited file"
		line[experiment2line[$2]] = "";
		next;
	}

	line[count] = $0;
	sample2line[$1] = count;
	experiment2line[$2] = count;
	count ++;
}

END {
	for (i=0; i<count; i++) if (line[i]!="") print "OUT>",line[i];
}
