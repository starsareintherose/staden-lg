.TH ted 1L "July 1991" "MRC LMB" "LOCAL"
.SH NAME
ted \- trace editor
.SH SYNOPSIS
.B ted
[(
.B -ABI\||\|-ALF\||\|-plain
)
.I tracefilename
[
.B -baseNum
.I number
]
.B [
.B -mag
.I number
( 1 to 100 )
] 
.B [
.B -bottom
.I number
(1(true) or 0(false))
.B ]
.B [
.B -astring
.I nucleotide-string
]] 
.B [
.B -enzyme
.I  5' cutting sequence
] 
.B [
.B -raw
.I  filename
(to be placed at head of xdap compatible .seq file)
.B ]
[
.B -output
.I outputfilename
]

.SH DESCRIPTION
.B ted
is a simple prototype editor for traces produced from automatic
sequencing machines. It allows the traces (from the ABI
or ALF sequencing machines) produced to be
displayed along with the machines interpretation of these into
bases and an initially identical sequence which can be edited
by the user. A cutoff region can be defined at both ends. The
edited and clipped list of bases can then be written out.
.LP
When initially run,
.B ted
displays the trace file
.I tracefilename
(if given) of the specified format centered on the base number
.I baseNum
(if given). If no file is provided,
.B ted
initially displays nothing.
.LP
The display consists of
the control panel and the synchronized view of the base position
information, original and edited sequence data, 
and graphical representation of the trace (with each nucleotide's trace
being represented by a different color).  The control
panel allows the user to read in new trace files (in either
bottom or top strand orientation)
as well as to search for a string of nucleotides or a certain base position.
The information button brings up signal strength and average spacing for
ABI files.
Scroll bars allow the user to adjust the magnification of or scroll through
the sequence and trace data.  The user may also choose to change the vertical
magnification of the trace data.  Further, sequence on the head (vector)
or tail (uncertain data) of the sequence may be ``cutoff'' 
using the adjust left and right cutoff buttons. Bases can be inserted, 
deleted, or replaced as with
any ordinary word-processor in the sequence data window. Finally, the
sequence may be written to an ascii file using the output button on
the control panel.  The output filename is specified in a dialogue, 
but a default value of inputfilename.seq is provided or the default value
can be given with the
.I outputfilename
argument.
.LP
A simple help system is provided.
.SH FILES
.PD 0
.TP 20
.B ted.help
Text provided in the help window.
.TP
.B /usr/lib/X11/app-defaults/Xted
Default application resources.
.SH ENVIRONMENT
.TP 20
.SB XFILESEARCHPATH
Specifies the locations where
.B ted.help
is sought.
If this is not defined,
.B ted.help
must be in the
.B /usr/lib/X11/app-defaults
directory.
.SH AUTHORS
Tim Gleeson, LaDeana Hillier, Simon Dear.
