#
# Clip sequence on basis of sequence content -
#
#	(xdap version)
#
# Rule: everything is good quality up to where NN uncertainties in
#       MM window, in the base range LL..RR
#
# Outputs new experiment file with extra clip line to stdout
#
# St.Louis use NN=2, MM=5, LL=200, RR=sequence-length
#

BEGIN {
	# Up to where NN uncertainties in MM window starting at base SS
	NN = 2;
	MM = 5;
	LL = 200;
	RR = 400;
}


/;</ { lseq = lseq substr($0,3); next; }
/;>/ { rseq = rseq substr($0,3); next; }
/;/  { header = $0; next; }
     { mseq = mseq $0; next; }

END {
	seq = lseq mseq rseq;

	seqlen = length(seq);
	lcut = length(lseq);

	if (RR > seqlen) RR = seqlen;
	rcut = lcut + length(mseq) + 1;
	#	rcut = RR+1;
	if ( NN>0 ) {
		score = 0;
		for(i=LL-MM+1; i<=RR-MM+1; i++) {
			if (i>=MM) {
				if (index("ACGTacgt",substr(seq,i,1)) == 0) score--
			}
			if (i+MM-1<=RR) {
				if (index("ACGTacgt",substr(seq,i+MM-1,1)) == 0) score++;
			}
			if (score >= NN) {
				rcut = i;
				break;
			}
		}
	}

	# Write dap file header
	if (length(header)==0) {
		printf(";%6d%6d%6d%-4s%-18s\n",seqlen,lcut,rcut-lcut-1,"PLN","");
	} else {
		printf(";%6d%6d%6d%s\n",seqlen,lcut,rcut-lcut-1,substr(header,20));
	}

	# Write left cutoffs (if any)
	if(lcut>0) {
		str = substr(seq,1,lcut);
		strlen = lcut;
		bits = 48;
		for (i = 1; i <= strlen; i+=bits) {
			printf (";<%s\n",substr(str,i,bits));
		}
	}

	# Write right cutoffs (if any)
	if(rcut<=seqlen) {
		str = substr(seq,rcut);
		strlen = length(str);
		bits = 48;
		for (i = 1; i <= strlen; i+=bits) {
			printf (";>%s\n",substr(str,i,bits));
		}
	}

	# Write remaining sequence (if any!)
	if (lcut+1 != rcut) {
		str = substr(seq,lcut+1,rcut-lcut-1);
		strlen = length(str);
		bits = 50;
		for (i = 1; i <= strlen; i+=bits) {
			printf ("%s\n",substr(str,i,bits));
		}
	}

}
