BEGIN { dir = "unknown"; pass = "unknown" }
/^# Processing/ {
	dir = substr($0,14);
}

/^# Pass/ {
	pass = $3;
}

/^#/ {next;}

{ 
	colon = index($0,":");
        sample = substr($0,1,colon - 1);
	rest = substr($0,colon+1);
	print dir, sample, ":", pass, ":", rest; }
