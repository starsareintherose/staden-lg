/* 
  Title:       seqIOSCF
  
  File: 	 seqIOSCF.c
  Purpose:	 IO of Standard Chromatogram Format sequences
  Last update: Tue Nov 10 1992
  
  Change log:
  4 Feb 1992,  Now draft proposal version 2
  20 Feb 1992, Grab info from comment lines
  19 Aug 1992, If SCF file has clip information, don't clip automatically
  10 Nov 1992  SCF comments now stored in seq data structure
  
*/





/* ---- Imports ---- */

#include "scf.h"      /* SCF structures */
#include "seq.h"      /* IMPORT: Seq, BasesAndTraces, NULLSeq,
			 newSeq, freeSeq */
#include "seqIOEdit.h" /* IMPORT: writeEdSeq, 
			  readEdSEq */

#include <ctype.h>
#include <stdio.h>    /* IMPORT: fopen, fclose, fseek, ftell, fgetc,
			 EOF */
#include <stdlib.h>
#include "mach-io.h"


/* ---- Internal functions ---- */


static int read_scf_header(FILE *fp, Header *h)
{
    int i;

    if (be_read_int_4(fp,&h->magic_number)==False) return (False);
    if (be_read_int_4(fp,&h->samples)==False) return (False);
    if (be_read_int_4(fp,&h->samples_offset)==False) return (False);
    if (be_read_int_4(fp,&h->bases)==False) return (False);
    if (be_read_int_4(fp,&h->bases_left_clip)==False) return (False);
    if (be_read_int_4(fp,&h->bases_right_clip)==False) return (False);
    if (be_read_int_4(fp,&h->bases_offset)==False) return (False);
    if (be_read_int_4(fp,&h->comments_size)==False) return (False);
    if (be_read_int_4(fp,&h->comments_offset)==False) return (False);
    if (fread(&h->version[0],sizeof(h->version),1,fp)!=1) return (False);
    if (be_read_int_4(fp,&h->sample_size)==False) return (False);
    if (be_read_int_4(fp,&h->code_set)==False) return (False);
    for (i=0;i<20;i++)
	if (be_read_int_4(fp,&h->spare[i])==False) return (False);
    
    return (True);
}


static int read_scf_sample1(FILE *fp, Samples1 *s)
{
    if (be_read_int_1(fp,&s->sample_A)==False) return (False);
    if (be_read_int_1(fp,&s->sample_C)==False) return (False);
    if (be_read_int_1(fp,&s->sample_G)==False) return (False);
    if (be_read_int_1(fp,&s->sample_T)==False) return (False);
    
    return (True);
}


static int read_scf_sample2(FILE *fp, Samples2 *s)
{
    if (be_read_int_2(fp,&s->sample_A)==False) return (False);
    if (be_read_int_2(fp,&s->sample_C)==False) return (False);
    if (be_read_int_2(fp,&s->sample_G)==False) return (False);
    if (be_read_int_2(fp,&s->sample_T)==False) return (False);
    
    return (True);
}





static int read_scf_base(FILE *fp, Bases *b)
{
    if (be_read_int_4(fp,&b->peak_index)==False) return (False);
    if (be_read_int_1(fp,&b->prob_A)==False) return (False);
    if (be_read_int_1(fp,&b->prob_C)==False) return (False);
    if (be_read_int_1(fp,&b->prob_G)==False) return (False);
    if (be_read_int_1(fp,&b->prob_T)==False) return (False);
    if (be_read_int_1(fp,(byte *)&b->base)==False) return (False);
    if (be_read_int_1(fp,&b->spare[0])==False) return (False);
    if (be_read_int_1(fp,&b->spare[1])==False) return (False);
    if (be_read_int_1(fp,&b->spare[2])==False) return (False);
    
    return (True);
}



static float parse_version(char version[])
/*
 *
 */
{
    char v[5];
    strncpy(v,version,4);v[4]='\0';
    if (strspn(v,"0123456789. ")!=4) return 0.0;
    return atof(v);

}



/* ---- Exports ---- */

int is_SCF(char *fn)
/*
 * Check to see if file with name `fn' is in SCF format
 * 
 * Returns:
 * 1 - is SCF format
 * 0 - not SCF format
 */
{
    FILE *fp;
    uint_4 magic;
    int ok;
    
    if ( (fp=fopen(fn,"rb")) == NULL) {
	ok = 0;
    } else {
	if ( be_read_int_4(fp, &magic) != 1 ) {
	    ok = 0;
	} else {
	    ok = (magic==SCF_MAGIC);
	}
	fclose(fp);
    }
    
    return ok;
}








Seq readSeqSCF(char *fn)
/*
 * Read the SCF format sequence with name `fn' into `seq'.
 * If bottom is True, we read the sequence in in reverse and
 * complemented A NULL result indicates failure.
 */
{   Seq seq = NULLSeq;
    FILE *fp;
    Header header;
    int i;
    
    /* Open for: reading, binary */
    if ((fp = fopen(fn, "rb")) == NULL) return(NULLSeq);
    
    /* Read header */
    if (fseek(fp, (off_t)0, 0) != 0) {
	fclose(fp);
	return(NULLSeq);
    }
    if (read_scf_header(fp,&header)==False) {
	fclose(fp);
	return(NULLSeq);
    }
    
    /* Allocate the sequence */
    if ((seq = newSeq(BasesAndTraces, header.samples, header.bases)) == NULLSeq)
	{   fclose(fp);
	    return(NULLSeq);
	}
    
    
    seq->mode         = BasesAndTraces;
    seq->format       = SCFFormat;
    seq->dirty        = False;
    seq->maxTraceVal  = 0;
    seq->NPoints      = header.samples;
    seq->NorigBases   = header.bases;
    seq->NedBases     = header.bases;
    seq->leftCutoff   = header.bases_left_clip;
    seq->rightCutoff  = header.bases_right_clip;
    seq->bottom       = False;
    
    /* set precision if necessary */
    if (parse_version(header.version)<VERSION) {
	/* old 8-bit format */
	header.sample_size = 1;
    }
    
    /* read sample points */
    if (fseek(fp, (off_t)(header.samples_offset), 0) != 0) {
	fclose(fp);
	return(NULLSeq);
    }
    for(i=0;i<seq->NPoints;i++) {
	switch ( header.sample_size ) {
	case 2:
	    {
		Samples2 sample;
		if (read_scf_sample2(fp,&sample)==False) {
		    fclose(fp);
		    return(NULLSeq);
		}
		seq->traceA[i] = sample.sample_A;
		seq->traceC[i] = sample.sample_C;
		seq->traceG[i] = sample.sample_G;
		seq->traceT[i] = sample.sample_T;
		break;
	    }
	default:
	    {
		Samples1 sample;
		if (read_scf_sample1(fp,&sample)==False) {
		    fclose(fp);
		    return(NULLSeq);
		}
		seq->traceA[i] = sample.sample_A;
		seq->traceC[i] = sample.sample_C;
		seq->traceG[i] = sample.sample_G;
		seq->traceT[i] = sample.sample_T;
		break;
	    }
	}
	if (seq->maxTraceVal < seq->traceA[i]) seq->maxTraceVal = seq->traceA[i];
	if (seq->maxTraceVal < seq->traceC[i]) seq->maxTraceVal = seq->traceC[i];
	if (seq->maxTraceVal < seq->traceG[i]) seq->maxTraceVal = seq->traceG[i];
	if (seq->maxTraceVal < seq->traceT[i]) seq->maxTraceVal = seq->traceT[i];
    }
    
    
    /* Read bases */
    if (fseek(fp, (off_t)(header.bases_offset), 0) != 0) {
	fclose(fp);
	return(NULLSeq);
    }
    for(i=0;i<seq->NorigBases;i++) {
	Bases base;
	if (read_scf_base(fp,&base)==False) {
	    fclose(fp);
	    return(NULLSeq);
	}
	seq->base[i] = base.base;
	seq->basePos[i] = base.peak_index;
    }
    
    
    /* Read selected comment information  */
    {
	char *comments;
	
	comments = (char *) malloc(header.comments_size+1);
	
	if (fseek(fp,(off_t)(header.comments_offset), 0) != 0) {
	    free(comments);
	    fclose(fp);
	    return(NULLSeq);
	}
	if (fread(comments,header.comments_size,1,fp)==0) {
	    free(comments);
	    fclose(fp);
	    return(NULLSeq);
	}
	comments[header.comments_size] = '\0';
	
	seq->info = comments;
	
    }
    
    
    
    /* Finished with the file */
    fclose(fp);
    
    return(seq);
}
