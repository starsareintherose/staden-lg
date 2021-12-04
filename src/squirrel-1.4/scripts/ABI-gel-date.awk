#
# A little program to convert dates of the format MM_DD_YY
# into the format DD-Mon-YEAR
#
# EG 7_9_92 -> 9-Jul-1992
#
BEGIN {FS="_"; month = "Unknown"; }
$1==1  {month = "January";}
$1==2  {month = "February";}
$1==3  {month = "March";}
$1==4  {month = "April";}
$1==5  {month = "May";}
$1==6  {month = "June";}
$1==7  {month = "July";}
$1==8  {month = "August";}
$1==9  {month = "September";}
$1==10 {month = "October";}
$1==11 {month = "November";}
$1==12 {month = "December";}
{ 
	if (month != "Unknown" && NF == 3) {
		print $2 "-" substr(month,1,3) "-19" substr($3,1,2);
	}
}
