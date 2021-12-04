#
# Clip poor quality sequence at the left (5') and right (3') ends
# on basis of sequence content - filter version
#
# Rule:
#	To the right of base number SR and to the left of base SL
#       everything is good quality before where there are NN
#	uncertainties in MM window, in the base range LL..RR 
#       At the right hand end only, ALWAYS knock back KR bases
#
# St.Louis use NN=2, MM=5, SL=1, LL=1, SR=200, RR=sequence_length, KR=0
#
# Outputs just the new clip line in experiment file format
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
}

/^\/\// {
	slines = 0;
}

slines==1 {
	# sequence may be broken up by white space
	for (frag=1;frag<=NF;frag++) seq = seq $frag;
}
	

slines==0 && $1 == "SQ" {
	slines = 1;
	seq = "";
}

{
	print;
}

END {
	lseq = length(seq);
	if (RR > lseq) RR = lseq;
	if (LL < 1) LL = 1
	lcut = LL - 1;
	rcut = RR+1;
	if ( NN>0 ) {
		#
		# clip to the right
		#
		score = 0;
		# calculate default score
		for(j=SR-1;j<SR+MM-1 && j<=lseq ; j++)
			if(j>0 && index("ACGTacgt",substr(seq,j,1)) == 0)
				score++;
		# loop through
		for(j=SR; j<=RR; j++) {
			if (j>1 && index("ACGTacgt",substr(seq,j-1,1)) == 0)
				score--;
			if (j+MM-1<=lseq && index("ACGTacgt",substr(seq,j+MM-1,1)) == 0)
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
			if(j<=lseq && index("ACGTacgt",substr(seq,j,1)) == 0)
				score++;
		# loop through
		for(j=SL; j>=LL; j--) {
			if (j<lseq && index("ACGTacgt",substr(seq,j+1,1)) == 0)
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

	# output
	if (lcut >= 1)    print "QL   " lcut;
	if (rcut <= lseq) print "QR   " rcut;
}
