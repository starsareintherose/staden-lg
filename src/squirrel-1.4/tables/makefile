#
# Make look up tables for files
#
TABLES= \
	databases.lookup \
	seqclone-sites.lookup \
	subclones.lookup \
	vector-seqfiles.lookup \
	screens.lookup


%.lookup : %
	@echo ""
	@echo Building $<.lookup
	@rm -f $<.lookup
	@./create.lookup $< $<.lookup

all: $(TABLES)

clean:
	rm -f $(TABLES)

