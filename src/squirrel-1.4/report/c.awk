{
	class = $2;
	$1 = "" ; $2 = "" ;
	reason = substr($0,3);
	count++;
	ccount[class]++;
	rcount[class reason]++;
	reazon[class reason] = reason;
	klass[class reason] = class;
}

END {
	for (c in ccount) {
		if (c=="") {
			print "unknown:"
		} else {
			print c ":"
		}
		for ( cr in rcount ) {
			if (klass[cr] == c) {
				if (reazon[cr] == "") reazon[cr]="unknown";
				printf "   %-64s%4d\n",reazon[cr],rcount[cr];
			}
		}
		printf "   %-64s%4d\n","TOTAL", ccount[c];
		print "";
	}
	printf "%-64s   %4d\n", "TOTAL PROCESSED",count;
}
