#
# File: clip-seq.awk
# Version: 1.0
# Created: 17 November 1992
#
# Author: Simon Dear
#         MRC Laboratory of Molecular Biology
#	  Hills Road
#	  Cambridge CB2 2QH
#	  United Kingdom
#
# Description:
#
# Clip poor quality sequence at the left (5') and right (3') ends
# on basis of sequence content. Program acts as a filter for xdap
# sequence files.
#
# Rule:
#
# At the 3' end: A window of (MM) bases is slid down the sequence,
# starting from base position (SR) until there are (NN) Ns within the
# window, or until the window reaches position (RR), whichever happens
# first. The 3' extent of the good data is the set to be (KR) bases
# upsteam of this position.
# 
# At the 5' end: This essentially the same as for the 3' end. The window
# is slid back along the sequence starting at base position (SL), until
# there are sufficient Ns or until the window reaches position (LL),
# whichever happens first. There is no further adjustment of the 5'
# extent of good data.
#
# St.Louis use:  NN=2, MM=5, SL=1, LL=1, SR=200, RR=sequence_length, KR=0
# Cambridge use: NN=2, MM=5, SL=100, LL=1, SR=200, RR=450, KR=50
#

BEGIN {
	# Up to where NN uncertainties in window of MM bases
	NN = 2;
	MM = 5;
	SL = 100
	SR = 200
	LL = 1;
	RR = 450;
	KR = 50;
	# don't change
	QL = -1;
	QR = -1;
}


/;/ && NR==1 { header = $0; next; } #first line is a header
/;</ { lseq = lseq substr($0,3); next; }
/;>/ { rseq = rseq substr($0,3); next; }
/;/ { if (extras=="") extras = $0; else extras = extras "\n" $0; next; } # gather extras
     { mseq = mseq $0; next; }

END {
	seq = lseq mseq rseq;
	# fudge QL and QR
	QL = length(lseq);
	QR = length(lseq) + length(mseq) + 1;


	seqlen = length(seq);
	if (RR > seqlen) RR = seqlen;
	if (LL < 1) LL = 1
	lcut = LL - 1;
	rcut = RR+1;
	if ( NN>0 ) {
		#
		# clip to the right
		#
		score = 0;
		# calculate default score
		for(j=SR-1;j<SR+MM-1 && j<=seqlen ; j++)
			if(j>0 && index("ACGTacgt",substr(seq,j,1)) == 0)
				score++;
		# loop through
		for(j=SR; j<=RR; j++) {
			if (j>1 && index("ACGTacgt",substr(seq,j-1,1)) == 0)
				score--;
			if (j+MM-1<=seqlen && index("ACGTacgt",substr(seq,j+MM-1,1)) == 0)
				score++;
			if (score >= NN) {
				rcut = j;
				break;
			}
		}

		#
		# clip to the left
		#
		score = 0;
		# calculate default score
		for(j=SL+1;j>SL-MM+1 && j>0 ; j--)
			if(j<=seqlen && index("ACGTacgt",substr(seq,j,1)) == 0)
				score++;
		# loop through
		for(j=SL; j>=LL; j--) {
			if (j<seqlen && index("ACGTacgt",substr(seq,j+1,1)) == 0)
				score--;
			if (j-MM+1>0 && index("ACGTacgt",substr(seq,j-MM+1,1)) == 0)
				score++;
			if (score >= NN) {
				lcut = j;
				break;
			}
		}
	}

	# knock back the 3' end
	rcut = rcut - KR;
	if (rcut <= lcut) rcut = lcut + 1;

	# Determine old values
	if (QR<0) QR = seqlen+1;
	if (QL<0) QL = 0;
	# Constrain new values to old values
	if (lcut >= QR) lcut = QR - 1;
	if (rcut <= QL) rcut = QL + 1;
	# Adjust if there are changes
	if (lcut < QL) lcut = QL;
	if (rcut > QR) rcut = QR;

	#
	# Print out the filtered file
	#

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

	# Write extras here
	if (extras!="") print extras;


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
