.TH staden 1L "November 1991" "MRC LMB" "LOCAL"
.SH NAME
staden, xstaden \- sequence analysis suite
.SH DESCRIPTION
.I staden
is a suite of programs for sequence analysis. Currently available are
.I mep,
.I nip,
.I pip,
.I sap,
.I sip,
.I nipl,
.I pipl,
.I and sipl.
These all run under the SUN X11
.I xterm
Tektronics terminal emulator, but also work with the VT640 terminal
and the VersaTermPro and MS-Kermit emulators if they login to a SUN.
.PP
.I xstaden
is the same set of programs, named
.I xmep,
.I xnip,
.I xpip,
.I xsap,
.I xdap,
and
.I xsip,
which run directly under X providing a convenient user interface,
including resizable output and pull-down menus. All these programs
accept the standard X arguments. The library searching programs
nipl, pipl and sipl are only available in xterm form.
.PP
Sequence library access is provided for the format as distributed
on CDROM by EMBL. The CDROM contains the EMBL nucleotide library and
the SWISSPROT protein library. The libraries can be left on the
CDROM or transferred to hard disk.
.PP
The programs also provide an interface to the PROSITE protein motif
library.
.PP
Some initialisation is required in order to use the package. csh users
should insert the following in their .login files:
.IP
 setenv STADENROOT /home/BioSW/staden
.IP
 source $STADENROOT/staden.login
.LP
Users of the Bourne shell, sh, should insert the following in
their .profile:
.IP
 STADENROOT=/home/BioSW/staden
.IP
 export STADENROOT
.IP
 . $STADENROOT/staden.profile
.LP
These initialisations will alter your shell's search path so
that it can find the program binaries, and other files that are
required.
.SH ENVIRONMENT
The following environment variables may be set in the
user's \fI .login\fP or \fI .profile\fP file:
.TP 20
.BI STADENROOT= /home/BioSW/staden
This must be set in the user's initialisation.
.TP 20
.BI SEQEDT= editor
Set the editor to be used by the package.  The default is
\fIemacs\fP.
.SH FILES
.PD 0
.TP 30
$STADENROOT/staden.login
csh initialisation
.TP 30
$STADENROOT/staden.profile
sh initialisation
.TP 30
$STADENROOT/tables
Tables used by the programs
.TP 30
$STADENROOT/help
Helpfiles used by the programs, documentation of the user interface
and of each of the programs.
.TP 30
$STADENROOT/tables/SEQUENCELIBRARIES
Defines the sequence libraries available, their file descriptors
and the prompts to appear on the users screen.
.SH AUTHOR
Rodger Staden, MRC Laboratory of Molecular Biology, Hills Rd., Cambridge,
CB2 2QH, UK.
.SH BUGS
.PP
When using the xterm programs and in graphics input mode,
a carriage return should not be
entered on its own but should be preceded by some other character,
such as SPACE, COMMA or K. If a carriage return is entered on its
own, some garbage will (relatively) harmelssly appear on the plot.
.PP
General comments on the package can be sent to
\fI<rs@uk.ac.cam.mrc-lmb>\fP 
