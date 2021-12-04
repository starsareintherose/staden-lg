BEGIN { FS = ":"; }
{
	if (lastkey != $1 && lastkey != "") print last;
	lastkey = $1;
	last = $0;
}
END { print last; }
