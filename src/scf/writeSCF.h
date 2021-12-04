#ifndef _writeSCF_h
#define _writeSCF_h

#include "scf.h"      /* IMPORT: scf structures */
#include "seq.h"      /* IMPORT: Seq, BasesAndTraces, NULLSeq,
			         newSeq, freeSeq */

extern Boolean writeSeqSCF(Seq seq, char *fn);

#endif /* _writeSCF_h */
