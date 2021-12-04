#
# Convert experiment file format to a format compatible with dap
#
# It's pretty clutzy and nasty as information is lost in the conversion
# process. By it'll do until a version of "gap" is written that accepts
# files in the nice format.
#
# Written by Simon Dear, 10 February 1992
#
# Changes:
# 3-Nov-92 Added code for creation of initial tags
#

BEGIN {
	# set createTags to "YES" if you want them
	createTags = "YES";
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
	# Determine left and right end cuts
	# Take all left and right ends of excluded sequences
	n = 0;
	if ( entry["CS"] != "" ) {
		# special variables because we will need them later
		CS = entry["CS"];
		CSl = substr(CS,1,index(CS,".")-1)+0;
		CSr = substr(CS,index(CS,".")+2)+0;
		n++; pos[n] = CSl; type[n] = +1;
		n++; pos[n] = CSr; type[n] = -1;
	}
	if ( entry["SR"] != "" ) {
		l = entry["SR"]+0;
		r = seqlen+1;
		n++; pos[n] = l; type[n] = +1;
		n++; pos[n] = r; type[n] = -1;
	}
	if ( entry["QL"] != "" ) {
		l = 0;
		r = entry["QL"]+0;
		n++; pos[n] = l; type[n] = +1;
		n++; pos[n] = r; type[n] = -1;
	}
	if ( entry["QR"] != "" ) {
		l = entry["QR"]+0;
		r = seqlen+1;
		n++; pos[n] = l; type[n] = +1;
		n++; pos[n] = r; type[n] = -1;
	}
	if ( entry["SL"] != "" ) {
		l = 0;
		r = entry["SL"]+0.0;
		n++; pos[n] = l; type[n] = +1;
		n++; pos[n] = r; type[n] = -1;
	}

	# Sort them
	for (i = 2; i <= n; i++ ) {
		for (j=i; j>1 && pos[j-1] > pos[j]; j--) {
			#swap pos[j-1] and pos[j];
			t=pos[j-1]; pos[j-1]=pos[j]; pos[j]=t;
			t=type[j-1]; type[j-1]=type[j]; type[j]=t;
		}
	}

	# Set defaults
	pos[0] = 0; pos[n+1] = seqlen+1;

	# Deduce region not marked
	# Select longest unmarked region of sequence
	inn = 0;
	max_width = 0;
	for (i = 1; i <= n+1; i++ ) {
		if (inn == 0) {
			if (max_width < pos[i]-pos[i-1] ) {
				l_cut = pos[i-1];
				r_cut = pos[i];
				max_width = pos[i]-pos[i-1];
			}

		}
		inn += type[i];
	}

	# Write dap file header
	ulen = r_cut - l_cut - 1;
	printf(";%6d%6d%6d%-4s%-18s\n",seqlen,l_cut,ulen,entry["LT"],entry["LN"]);

	# Write left cutoffs (if any)
	if(l_cut>0) {
		str = substr(seq,1,l_cut);
		strlen = l_cut;
		bits = 48;
		for (i = 1; i <= strlen; i+=bits) {
			printf (";<%s\n",substr(str,i,bits));
		}
	}

	# Write right cutoffs (if any)
	if(r_cut<=seqlen) {
		str = substr(seq,r_cut);
		strlen = length(str);
		bits = 48;
		for (i = 1; i <= strlen; i+=bits) {
			printf (";>%s\n",substr(str,i,bits));
		}
	}

	if (createTags == "YES") {
		# Write initial tags here
		# IGNS
		if (entry["SR"]+0 <= seqlen && ulen > 5) {
			printf (";;IGNS %6d %6d %s\n",ulen-4,5,entry["SV"]);
		}
		# IGNC
		if (entry["CS"] != "" && ulen > 5) {
			# determine which end to put tag
			if (CSr <= l_cut) {
				# at the left
				printf (";;IGNC %6d %6d %s\n",1,5,entry["CV"]);
			} else {
				# at the right
				printf (";;IGNC %6d %6d %s\n",ulen-4,5,entry["CV"]);
			}
		}
	}

	# Write remaining sequence (if any!)
	if (l_cut+1 != r_cut) {
		str = substr(seq,l_cut+1,r_cut-l_cut-1);
		strlen = length(str);
		bits = 50;
		for (i = 1; i <= strlen; i+=bits) {
			printf ("%s\n",substr(str,i,bits));
		}
	}
}

