#
# Make file for help files - this requires gmake on some systems.
#
PROGS = bap       dap       gip       mep         nip      \
	nipf      pip       sap       sip        #mem

HELPS = bap_help  dap_help  gip_help  mep_help    nip_help \
	nipf_help pip_help  sap_help  sip_help   #mem_help

MENUS = bap_menu  dap_menu  gip_menu  mep_menu    nip_menu \
	nipf_menu pip_menu  sap_menu  sip_menu   #mem_menu

all:	$(PROGS)

DOIT = rm -f $@_help $@_menu; ./runoff $?

bap:	BAP.RNO
	$(DOIT)

dap:	DAP.RNO
	$(DOIT)

gip:	GIP.RNO
	$(DOIT)

#mem:	MEM.RNO
#	$(DOIT)

mep:	MEP.RNO
	$(DOIT)

nip:	NIP.RNO
	$(DOIT)

nipf:	NIPF.RNO
	$(DOIT)

pip:	PIP.RNO
	$(DOIT)

sap:	SAP.RNO
	$(DOIT)

sip:	SIP.RNO
	$(DOIT)

clean:
	rm -f $(HELPS) $(MENUS)
