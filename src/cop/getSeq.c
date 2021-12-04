#include <stdio.h> /* IMPORT: stderr */
#include "seq.h"
#include "seqIOABI.h"
#include "seqIOALF.h"
#include "seqIOSCF.h"
#include "seqIOPlain.h"

extern void *malloc(size_t s);

Seq getSeq(char *filename, char *type)
{
    Seq seq;

    if (is_SCF(filename)) {
        seq = readSeqSCF(filename);
    } else {
	if (strcmp(type, "ABI")   == 0)
	    seq = readSeqABI(filename);
	else if (strcmp(type, "ALF")   == 0)
	    seq = readSeqALF(filename);
	else if (strcmp(type, "SCF")   == 0)
	    seq = readSeqSCF(filename);
	else
	    seq = readSeqPlain(filename);
    }

    if (seq == NULLSeq ) {
	fprintf(stderr,"Error reading %s trace file %s\n",type,filename);
	return NULL;
    }

    return seq;
}

char *getSequence(Seq seq)
{
    char *bases;

    if (seq == NULLSeq ) {
	return NULL;
    }

    bases = malloc(seq->NorigBases+1);
    strncpy(bases,seq->base,seq->NorigBases);
    bases[seq->NorigBases] = '\0';

    return bases;
}
