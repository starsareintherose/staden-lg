#
# Setup file for Staden software running on a Sun
# This setup for Bourne shell (sh) users
#
# This file should be source'd from your .profile
# assuming the environmental variable STADENROOT has been set up
# to point to the root directory for the staden software
#
# e.g.
# STADENROOT=/home/BioSW/staden; export STADENROOT
# . $STADENROOT/staden.setup
#
#
#echo 'Setting up the Staden software environment...'

PATH=$PATH:$STADENROOT/bin;	export PATH

STADTABL=$STADENROOT/tables;	export STADTABL
STADUSER=$STADENROOT/userdata;	export STADUSER
STADHELP=$STADENROOT/help;	export STADHELP


#
# files for nip
#
ECRIBS=$STADTABL/PERCPTRON.WTS;	export ECRIBS
EUKRIBS=$STADTABL/RIBSEUKAR.WTS;export EUKRIBS
INTRONS=$STADTABL/INTRON.WTS;	export INTRONS
ECPROMS=$STADTABL/PROKPROMS.WTS;export ECPROMS
RENZYM4=$STADTABL/RENZYM.4;	export RENZYM4
RENZYM6=$STADTABL/RENZYM.6;	export RENZYM6
RENZYMAL=$STADTABL/RENZYM.ALL;	export RENZYMAL
HELPNIP=$STADHELP/nip_help;	export HELPNIP
NIPHELP=$STADHELP/NIP.HLP;	export NIPHELP
NIPHPNT=$STADHELP/NIP.PNT;	export NIPHPNT
NIPMARG=$STADTABL/NIP.MRG;	export NIPMARG

#
# files for sip
#
HELPSIP=$STADHELP/sip_help;	export HELPSIP
SIPHELP=$STADHELP/SIP.HLP;	export SIPHELP
SIPHPNT=$STADHELP/SIP.PNT;	export SIPHPNT
SIPMARG=$STADTABL/SIP.MRG;	export SIPMARG
PROTMAT=$STADTABL/PROTMAT.DAT;	export PROTMAT

#
# files for sap
#
HELPSAP=$STADHELP/sap_help;	export HELPSAP
SAPHELP=$STADHELP/SAP.HLP;	export SAPHELP
SAPHPNT=$STADHELP/SAP.PNT;	export SAPHPNT
SAPMARG=$STADTABL/SAP.MRG;	export SAPMARG

#
# files for bap
#
HELPBAP=$STADHELP/bap_help;	export HELPBAP
BAPHELP=$STADHELP/BAP.HLP;	export BAPHELP
BAPHPNT=$STADHELP/BAP.PNT;	export BAPHPNT
BAPMARG=$STADTABL/BAP.MRG;	export BAPMARG

#
# files for dap
#
HELPDAP=$STADHELP/dap_help;	export HELPDAP
DAPHELP=$STADHELP/DAP.HLP;	export DAPHELP
DAPHPNT=$STADHELP/DAP.PNT;	export DAPHPNT
DAPMARG=$STADTABL/DAP.MRG;	export DAPMARG
TAGDB=$STADTABL/TAGDB;          export TAGDB

#
# files for pip
#
PIPMARG=$STADTABL/PIP.MRG;	export PIPMARG
PROTGRP=$STADTABL/PROTEIN.GRP;	export PROTGRP
PROTALL=$STADTABL/PROTEIN.ALL;	export PROTALL
ROBSON=$STADTABL/ROBSON.WTS;	export ROBSON
HELPPIP=$STADHELP/pip_help;	export HELPPIP
PIPHELP=$STADHELP/PIP.HLP;	export PIPHELP
PIPHPNT=$STADHELP/PIP.PNT;	export PIPHPNT

#
# files for mep
#
MEPMARG=$STADTABL/MEP.MRG;	export MEPMARG
HELPMEP=$STADHELP/mep_help;	export HELPMEP
MEPHELP=$STADHELP/MEP.HLP;	export MEPHELP
MEPHPNT=$STADHELP/MEP.PNT;	export MEPHPNT

#
# files for rep
#
ALUNAMES=$STADTABL/alus/files;	export ALUNAMES


#
# miscellaneous files
#
GIPMEM=$STADHELP/gip_help;	export GIPMEM
HELPSTADEN=$STADHELP/staden_help;export HELPSTADEN

#
# vector sequences
#
M13MP18_VECTOR=$STADUSER/m13mp18.vec; export M13MP18_VECTOR
PUC18_VECTOR=$STADUSER/puc18.vec; export PUC18_VECTOR
BLUE_VECTOR=$STADUSER/blue.vec; export BLUE_VECTOR



#
# Sequence databases (currently embl cdrom format only)
#

# The file SEQUENCELIBRARIES lists the available libraries
# and the *DESCRP files contain the names of each libraries files
# 
SEQUENCELIBRARIES=$STADTABL/SEQUENCELIBRARIES; export SEQUENCELIBRARIES
EMBLLIBDESCRP=$STADTABL/EMBLLIBDESCRP; export EMBLLIBDESCRP
SWISSLIBDESCRP=$STADTABL/SWISSLIBDESCRP; export SWISSLIBDESCRP


#
# Help X11 find where things are
#
XFILESEARCHPATH=$STADHELP/%N%S:$STADTABL/%N%S:${XFILESEARCHPATH-/usr/lib/X11/%T/%N%S}
export XFILESEARCHPATH

#
# Find manual pages
#
MANPATH=${MANPATH-/usr/man}:$STADENROOT; export MANPATH

#
# For gip, specify default digitizer port
#
DIGITIZER=/dev/ttyb; export DIGITIZER

#
# Sequence databases
#

. $STADTABL/libraries.config.sh
