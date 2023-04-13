                       General Information
                   (Not for the faint hearted)

			30 September 1992


0. Introduction
---------------

This document contains information on the following subjects:

   1. Installing the Staden Package on SPARCstations and DECstations
   2. Installing the Staden Package on Other Machines
   3. A Quick Guide to What's on the Release Tape
   4. Overview of Data Flow During Sequence Assembly
   5. Acknowledgements



1. Installing the Staden Package on SPARCstations and DECstations
-----------------------------------------------------------------

We are endeavouring to make the installation of the Staden Package as
quick and as easy as possible. In this current release we provide
statically linked sparc and mips executables as well as all sources.

To install the package:

1) Create a new directory for the software. You may have to log on as
superuser to do this.

	% mkdir -p /home/BioSW/staden

2) Place the distribution tape in the drive and down load the package:

   -sun-
	% tar xvf /dev/rst0
	...system messages...

   -dec- 
	% tar xvf /dev/rmt0h
	...system messages...

3) Users of the C Shell should add the following to his/her .login
file:

	setenv STADENROOT /home/BioSW/staden
	source $STADENROOT/staden.login

Users of the Bourne shell should add the following to their .profile
file: 

	STADENROOT=/home/BioSW/staden
	export STADENROOT
	. $STADENROOT/staden.profile


4) When the user next logs onto the work station the required
initialisation will automatically be performed, and the programs in
the Staden package can be run. Refer to the help/*.MEM files for
information on the various program. (eg help on xdap is in
help/DAP.MEM)


2. Installing the Staden Package on Other Machines
--------------------------------------------------

This is a little more difficult as you will need to remake all the
executables. Your system configuration may also mean that some changes
will need to be made, though hopefully only to makefiles. We provide
a script to aid installation (we hope!), but you may prefer to make
all the components manually.

To remake the Staden package you will require the following:
	1) A Fortran77 compiler
	2) An ANSI C compiler
	3) X11 Release 4, including the Athena Widget libraries.

Start by following step 1 through 3 above, to unload the sources and
perform initialisations. Read the rest of this document and the other
help files. Look at the make files. Follow your nose!

If you have any problems or successes porting our software to other
platforms we would love to hear from you. We would also appreciate
receiving your general comments on the package.

Rodger Staden (principle author)
  phone: +44 223 402389  email: rs@mrc-lmba.cam.ac.uk
  post: MRC Laboratory of Molecular Biology, Hills Road, Cambridge CB2 2QH, U.K.
Simon Dear:    
  phone: +44 223 402266  email: sd@mrc-lmba.cam.ac.uk
  post: MRC Laboratory of Molecular Biology, Hills Road, Cambridge CB2 2QH, U.K.
James Bonfield:
  phome: +44 223 402499  email: jkb@mrc-lmba.cam.ac.uk
  post: MRC Laboratory of Molecular Biology, Hills Road, Cambridge CB2 2QH, U.K.



3. A Quick Guide to What's on the Release Tape
----------------------------------------------

The directory structure on this tape is very important. Once set up, the Staden
package expects things to be in a predefined place. The root directory
of the structure is referred to by the environment variable
STADENROOT. Below this there should be at least the following:

1) bin/
All executable files and scripts should be in this directory.
$STADENROOT/bin is added to the search path by the script staden.login
(or staden.profile if you are using the Bourne Shell). Though you are
not forced to keep programs here, we find it is the simplest place to
keep them.

2) help/
All on-line help files are in this directory. Files of the form *.MEM
or *.mem are formatted ascii files and can be printed for personal
reference. The script staden.login sets up many environment variables
that refer to files in this directory, as well as modifying
XFILESEARCHPATH, which is used by X programs.

3) manl/
Local manual pages for ted and the staden package are in this directory. The
environment variable MANPATH is modified in staden.login to search
here too.

4) staden.login and staden.profile
These two files are scripts to set up environment variables required
by the Staden package. C Shell users should source staden.login from
their .login file, and Bourne Shell users should "source" staden.profile
from their .profile directory. See "Installing the Staden Package on
SPARCstations and DECstations", Part 3.

5) tables/
Configuration files for the Staden package are in this directory.
Various environment variables are set in staden.login to refer to
files in this directory.

Also of use are the following:

doc/           - Miscellaneous documentation.
userdata/      - Sample databases
src/           - program sources
ReleaseNotes   - Notes on this and future releases
Staden_install - Installation script
SequenceLibraries - Notes on the use and installation of sequence libraries


Program Sources
---------------

All the program sources are found in the directories in $STADENROOT/src:

0) Misc/
Sources for a library of useful routines used by the staden package.
** Should be made before the programs in staden/ **

1) staden/
Sources for the Staden suite: mep, xmep, nip, xnip, nipl, pip, xpip,
pipl, sap (now superseded by dap), xsap (now superceded by xdap), sip,
xsip, sipl, dap, xdap, splitp1, splitp2, splitp3, gip and convert_project.

2) ted/
Sources for the trace display and sequence editing program ted.

3) abi/
Sample scripts and programs for handling ABI 373A data files.

4) alf/
Sample scripts and programs for handling Pharmacia A.L.F. data files.

Each directory has appropriate makefiles and README files.



4. Overview of Data Flow During Sequence Assembly
-------------------------------------------------

During a sequence assembly project the data can enter the sequence
assembly program from various routes (See Figure below).


       
             Fluorescent Based
             Sequencing Machine
                Chromatogram                      Autoradiogram

	 ABI 373A     	Pharmacia A.L.F.                |  
             |                 |                        |
             |                 |                        |
             |             alfsplit                     |
             |                 |                        |
             +--------+--------+                        |
                      |                                 |
                      |                                 |
                     ted                              (gip)
                      |                                 |
                      +----------------+----------------+
                                       |
                                       |
                                     xdap


                 Figure 1: Data Flow Through The Staden Suite


The Pharmacia A.L.F. data files in their original format consist of
one file for the (up to 10) samples that were on the gel. The program
alfsplit divides the file up so that each sample is in a file of
its own. From then on each gel reading can be handled individually.
Whether these files can be transferred back to the Compaq for
reprocessing is unknown.

All data from fluorescent based sequencing machines must pass through
the trace editing program ted. Ted allows data vector sequence at the
5' end and unreliable data at the 3' end to be clipped. The sequence
can be edited if desired, though we should stress that this is NOT
RECOMMENDED when used in conjunction with xdap. Ted translates all
Pharmacia A.L.F. uncertainty codes to a hyphen ("-") and outputs the
clipped sequence, along with additional information on the position
and content of cutoffs, to a file.

People wanting to use xdap with ABI and Pharmacia files, but who have
written their own trace clipping software should be aware that xdap
requires information to be passed in the sequence file so that
traces can be displayed. You may want to modify your software to be
compatible with our file format. The file consists of four parts:

	1) Cut off information (Optional).
	Format is ";%6d%6d%6d%-4s%-16s", where
	field 1 = total number of bases called
	      2 = number of bases in the clipped sequence at the 5' end
	      3 = number of bases in the sequence in this file
	      4 = type of trace file.
	          "ALF " - Pharmacia A.L.F.
		  "ABI " - ABI 373A
		  "SCF " - SCF
		  "PLN " - Text only
	      5 = name of trace file.

	2) Content of the clipped sequence at the 5' end (Optional).
	The sequence can extend over several lines. Each line must
	begin with ";<" and should be less than 80 characters in
	length.

	3) Content of the clipped sequence at the 3' end (Optional).
	The sequence can extend over several lines. Each line must
	begin with ";>" and should be less than 80 characters in
	length.

	4) Initial tags for the sequence (Optional)
	Format is: ";;%4s %6d %6d %s\n", where
	field 1 = type of tag to be created (see $STADTABL/TAGDB)
	      2 = position of tag
	      3 = length of tag
	      4 = annotation for tag (optional)
	This feature is only available in the program xbap, which
	at the time of writing is not yet being distributed with
	the package.

	5) The sequence, which can extend over several lines. Each
	line should be less than 80 characters in length.

Here is a sample file:

;   660    55   450ABI a21d12.s1RES
;<AGCTTGCATGCCTGCAGGTCGACTCTAGAGGATCCCCCGGTTCCTTCTGG
;<ATATC
;>-GATAAGCTGATTTG-TTT-CCATTATGGC-GGTTTGAGCCTC-G-GGTC
;>GACCACTCGGTGTGCCAGGAAGGGGTCTGAAATTGAATGGGTTATCACTA
;>GGCGACGTTT--TTTTCAAATTCCGGGCTAAATTTTACGGC-GGA-CGGT
;>TCCG-
;;COMM      1     10 M13mp18 subclone
CAAGACATTTTGAAATACTTGGAATACTGAATCCAAGATGTGGAACATTA
GACATATCCGTGTGCTCAACAATCGACATTTGATCCACTGATGAAAATGT
TCTTCGTTTAGAATTTCTCATAGCATCAGCCACTTTTGCATAATACTCGA
TTGAAGGTTCATGGAAAAAGCTGCGTAGAAGGCATGTCATTGTGCTTACG
AGCCATTTCGGATATCTTGTGAATTTAGCAGGAAGTTCTGTAACTGGTTG
GAATTCAAATATATCAGTTCTTCTTCCTGGATCTCGTCCTTTTTGCACTA
AAACCATTGCGATTGCATCCGGATTCTGAGTAAGAGCCACTACAGCTTTA
TGATACAGGCTCTTGTTATTCCTTTCGTGCTCGAATGGGAACTTTCCAGT
GGCACAAAAATATAGTGTACATCCCAGAGCCCATAGATCACATGTTCCGA



5. Acknowledgements

We would like to thank Applied Biosystems, Inc. and Pharmacia LKB
Biotechnology for their cooperation in agreeing to our routines
accessing the data files of their fluorescent sequencing machines.

373A sequence data file formats are the exclusive property of Applied
Biosystems, Inc.

ALF sequence data file formats are the exclusive property of Pharmacia
LKB Biotechnology, Inc.

