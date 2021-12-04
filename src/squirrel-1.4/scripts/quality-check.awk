#
# Determine if the experiment file should be rejected because of
# poor quality. Ideally this would be done by looking at trace
# quality, but this will have to do in the meantime
#
# Method:
#    Determine the extents of the good data, by looking at the
#    experiment file entries. Default is to use LL..RR
#    Reject if the good data is less that MINLEN
#    Reject if the good data has greater than CRAP (%) ambiguity content
#
# Outputs:
#    0 - passes quality check
#    1 - fails length constraint
#    2 - fails ambiguity content constraint
#
# Written by Simon Dear, 27 March 1992
#

BEGIN {
	LL = 50;
	RR = 350;
	MINLEN = 100;
	CRAP = 4.0;
}

$1 == "SQ" {
	# Gather sequence
	seq = "";
	getline;
	while ( $0 != "//") {
		# sequence may be broken up by white space
		for (frag=1;frag<=NF;frag++) seq = seq $frag;
		getline;
	}
	seqlen = length(seq);
}

{
	# Record entry lines
	type = $1;
	$1 = "";
	entry[type] = substr($0,2);
}


END {
	if ( entry["QL"] != "" ) LL = entry["QL"] + 0;
	if ( entry["QR"] != "" ) RR = entry["QR"] + 0;
	if ( LL > seqlen ) LL = seqlen;
	if ( RR > seqlen ) RR = seqlen;
	good_len = RR - LL + 1;

	# Check length constraint
	if ( good_len < MINLEN ) {
		print 1;
	} else {
		# Check ambiguity content restraint
		
		count = 0;
		for (i = LL; i <= RR; i++) {
			
			if ( index("ACGTacgt",substr(seq,i,1)) == 0 ) count++;

		}

		if ( count * 100 / good_len > CRAP )
			print 2;
		else
			print 0;
	}
}
