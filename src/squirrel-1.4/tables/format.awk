BEGIN {
    entries_per_page = 50;
    PP = 0;
}
{
    if ( (NR % entries_per_page) == 1 ) {
	if (PP)	print ".bp";
	PP++;
	print "Page - " PP;
	#
	print ".sp 3";
	print ".TS";
	print "box, tab (%);";
	print "c s   s   s   s";
	print " c | c | c | c | c | c";
	print " c | c | c | c | c | c";
	print " l | l | l | l | l | l.";
	print "Cosmid Subclone Libraries";
	print "=";
	print "Microtitre%Cosmid%Size Range%Fragmentation%Vector%Comments" ;
	print "Dish Number%%(bases)%Method%";
	print "=";
    }

    count = 0;
    if (NF > 5) {
	line = $0;
	s = index(line,"(");
	t = index(line,")");
	while (s!=0 && t!=0) {
	    comment = substr(line,s+1,t-s-1);
	    line = substr(line,t+1);
	    if (count==0) {
		print $1 "%" $2 "%" $3 "%" $4 "%" $5 "%" comment;
	    } else {
		print "%%%%%" comment;
	    }
	    count++;
	    s = index(line,"(");
	    t = index(line,")");
	}
	if (count>1) print "%%%%%_";
    }
    if (count == 0)
	print $1 "%" $2 "%" $3 "%" $4 "%" $5 "%";

    if ( (NR % entries_per_page) == 0 ) {
	print ".TE";
    }
}

END {
    if ( (NR % entries_per_page) != 0 ) {
	# fill up page
	for(i=NR; i % entries_per_page; i++) print "%%%%%";
	print ".TE";
    }
}
