#
# Setup file for Staden software running on a Sun
# This setup for c-shell (csh) users
#
# This file should be source'd from your .login
# assuming the environmental variable STADENROOT has been set up
# to point to the root directory for the staden software
#
# e.g.
# setenv STADENROOT /home/BioSW/staden
# source $STADENROOT/staden.login
#
#
#echo 'Setting up the Staden software environment...'

set path = ($path $STADENROOT/bin)

setenv STADTABL	$STADENROOT/tables
setenv STADUSER	$STADENROOT/userdata
setenv STADHELP	$STADENROOT/help

# The default is 'vi' but this causes the X versions to hang
setenv SEQEDT 'xterm -e vi'

#
# files for nip
#
setenv ECRIBS	$STADTABL/PERCPTRON.WTS
setenv EUKRIBS	$STADTABL/RIBSEUKAR.WTS
setenv INTRONS	$STADTABL/INTRON.WTS
setenv ECPROMS	$STADTABL/PROKPROMS.WTS
setenv RENZYM4	$STADTABL/RENZYM.4
setenv RENZYM6	$STADTABL/RENZYM.6
setenv RENZYMAL	$STADTABL/RENZYM.ALL
setenv HELPNIP	$STADHELP/nip_help
setenv NIPHELP	$STADHELP/NIP.HLP
setenv NIPHPNT	$STADHELP/NIP.PNT         
setenv NIPMARG	$STADTABL/NIP.MRG


# files for nipf

setenv NIPFMARG	$STADTABL/NIPF.MRG


#
# files for sip
#
setenv HELPSIP	$STADHELP/sip_help
setenv SIPHELP	$STADHELP/SIP.HLP         
setenv SIPHPNT	$STADHELP/SIP.PNT         
setenv SIPMARG	$STADTABL/SIP.MRG       
setenv PROTMAT  $STADTABL/PROTMAT.DAT   

#
# files for sap
#
setenv HELPSAP	$STADHELP/sap_help
setenv SAPHELP	$STADHELP/SAP.HLP         
setenv SAPHPNT	$STADHELP/SAP.PNT
setenv SAPMARG	$STADTABL/SAP.MRG

#
# files for bap
#
setenv HELPBAP	$STADHELP/bap_help
setenv BAPHELP	$STADHELP/BAP.HLP         
setenv BAPHPNT	$STADHELP/BAP.PNT
setenv BAPMARG	$STADTABL/BAP.MRG

#
# files for dap
#
setenv HELPDAP	$STADHELP/dap_help
setenv DAPHELP	$STADHELP/DAP.HLP         
setenv DAPHPNT	$STADHELP/DAP.PNT
setenv DAPMARG	$STADTABL/DAP.MRG
setenv TAGDB    $STADTABL/TAGDB

#
# files for pip
#
setenv PIPMARG	$STADTABL/PIP.MRG       
setenv PROTGRP	$STADTABL/PROTEIN.GRP   
setenv PROTALL	$STADTABL/PROTEIN.ALL   
setenv ROBSON	$STADTABL/ROBSON.WTS    
setenv HELPPIP	$STADHELP/pip_help
setenv PIPHELP	$STADHELP/PIP.HLP         
setenv PIPHPNT	$STADHELP/PIP.PNT

#
# files for mep
#
setenv MEPMARG $STADTABL/MEP.MRG
setenv HELPMEP $STADHELP/mep_help
setenv MEPHELP $STADHELP/MEP.HLP
setenv MEPHPNT $STADHELP/MEP.PNT

#
# files for rep
#
setenv ALUNAMES $STADTABL/alus/files



#
# miscellaneous files
#
setenv GIPMEM		$STADHELP/gip_help
setenv HELPSTADEN	$STADHELP/staden_help
#
# Typing "staden" will list all the program names
#      
alias staden 'cat $STADHELP/stadenp_help'
#
# vector sequences
#
setenv M13MP18_VECTOR   $STADUSER/m13mp18.vec
setenv PUC18_VECTOR     $STADUSER/puc18.vec
setenv BLUE_VECTOR      $STADUSER/blue.vec

setenv PROSITE           /pubseq/pubseq/seqlibs/prosite
setenv PROSITENAMES      /pubseq/pubseq/seqlibs/prosite/prosite.nam




#
# Help X11 find where things are
#
if ( $?XFILESEARCHPATH ) then
    setenv XFILESEARCHPATH ${STADHELP}/%N%S:${STADTABL}/%N%S:${XFILESEARCHPATH}
else
    setenv XFILESEARCHPATH ${STADHELP}/%N%S:${STADTABL}/%N%S:/usr/lib/X11/%T/%N%S
endif

#
# Find manual pages
#
if ( $?MANPATH ) then
    setenv MANPATH ${MANPATH}:${STADENROOT}
else
    setenv MANPATH /usr/man:${STADENROOT}
endif

#
# For gip, specify default digitizer port
#
setenv DIGITIZER /dev/ttyb


#
# Sequence databases
#

source $STADTABL/libraries.config.csh


