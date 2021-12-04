/* 
    Title:       writeSCF

    File: 	 writeSCF.c
    Purpose:	 Output of Standard Chromatogram Format sequences
    Last update: 23 November 1992

    Change log:
    4 Feb 1992, Now draft proposal version 2
    23 Nov 92,  SCF 2.0 + LaDeana's changes
           
*/


/*
** makeSCF version number
*/
#define VERSION_NO "2.00"

/* ---- Imports ---- */


#include "scf.h"      /* IMPORT: scf structures */
#include "seq.h"      /* IMPORT: Seq, BasesAndTraces, NULLSeq,
			         newSeq, freeSeq */


#include "mach-io.h"  /* IMPORT: be_write_int_1, be_write_int_2, be_write_int_4 */

#include <ctype.h>
#include <stdio.h>    /* IMPORT: fopen, fclose, fseek, ftell, fgetc,
		                 EOF */







/* ---- Internal functions ---- */




static int write_scf_header(FILE *fp, Header *h)
{
    int i;

    if (be_write_int_4(fp,&h->magic_number)==False) return (False);
    if (be_write_int_4(fp,&h->samples)==False) return (False);
    if (be_write_int_4(fp,&h->samples_offset)==False) return (False);
    if (be_write_int_4(fp,&h->bases)==False) return (False);
    if (be_write_int_4(fp,&h->bases_left_clip)==False) return (False);
    if (be_write_int_4(fp,&h->bases_right_clip)==False) return (False);
    if (be_write_int_4(fp,&h->bases_offset)==False) return (False);
    if (be_write_int_4(fp,&h->comments_size)==False) return (False);
    if (be_write_int_4(fp,&h->comments_offset)==False) return (False);
    if (fwrite(h->version,sizeof(h->version),1,fp)!=1) return (False);
    if (be_write_int_4(fp,&h->sample_size)==False) return (False);
    if (be_write_int_4(fp,&h->code_set)==False) return (False);
    for (i=0;i<20;i++)
	if (be_write_int_4(fp,&h->spare[i])==False) return (False);

    return (True);
}


static int write_scf_sample1(FILE *fp, Samples1 *s)
{
    if (be_write_int_1(fp,&s->sample_A)==False) return (False);
    if (be_write_int_1(fp,&s->sample_C)==False) return (False);
    if (be_write_int_1(fp,&s->sample_G)==False) return (False);
    if (be_write_int_1(fp,&s->sample_T)==False) return (False);

    return (True);
}


static int write_scf_sample2(FILE *fp, Samples2 *s)
{
    if (be_write_int_2(fp,&s->sample_A)==False) return (False);
    if (be_write_int_2(fp,&s->sample_C)==False) return (False);
    if (be_write_int_2(fp,&s->sample_G)==False) return (False);
    if (be_write_int_2(fp,&s->sample_T)==False) return (False);

    return (True);
}





static int write_scf_base(FILE *fp, Bases *b)
{
    if (be_write_int_4(fp,&b->peak_index)==False) return (False);
    if (be_write_int_1(fp,&b->prob_A)==False) return (False);
    if (be_write_int_1(fp,&b->prob_C)==False) return (False);
    if (be_write_int_1(fp,&b->prob_G)==False) return (False);
    if (be_write_int_1(fp,&b->prob_T)==False) return (False);
    if (be_write_int_1(fp,(uint_1 *)&b->base)==False) return (False);
    if (be_write_int_1(fp,&b->spare[0])==False) return (False);
    if (be_write_int_1(fp,&b->spare[1])==False) return (False);
    if (be_write_int_1(fp,&b->spare[2])==False) return (False);

    return (True);
}



static int write_scf_comment(FILE *fp, Comments *c, size_t l)
{
    if (fwrite(c, l, 1, fp) !=1) return (False);
    return (True);

}




/* ---- Exports ---- */








Boolean writeSeqSCF(Seq seq, char *fn)
/*
** Write Seq out as a .scf file
*/
{

    FILE *fp;
    Header header;
    Bases base;
    Comments comments[1024];
    Comments default_comments[] = "conversion_program = makeSCF " VERSION_NO "\n";
    char *src;


    if ((fp = fopen(fn,"wb"))==NULL) 
	return (False);
    else {
	int i;
	int prec; /* precision to use */

	/* source dependant switches */
	prec = 1;
	switch (seq->format) {
	case ABIFormat:
	    src = "ABI 373A";
	    break;
	case ALFFormat:
	    src = "Pharmacia A.L.F.";
	    prec = 2;
	    break;
	case SCFFormat:
	    src = "SCF";
	    if (seq->maxTraceVal > 255) prec=2; /* retain precision */
	    break;
	default:
	    src = "Unknown";
	    break;
	}

	if (seq->info==NULL) {
	    sprintf(comments,"%ssource = %s\n",
		    default_comments,
		    src);
	} else {
	    sprintf(comments,"%s\n%ssource = %s\n",
		    seq->info,
		    default_comments,
		    src);
	}
	
	header.magic_number = SCF_MAGIC;
	header.samples = seq->NPoints;
	header.samples_offset = (uint_4)sizeof(Header);
	header.bases = seq->NorigBases;
	header.bases_left_clip = seq->leftCutoff;
	header.bases_right_clip = seq->rightCutoff;
	header.bases_offset = (uint_4)(header.samples_offset + header.samples * ((prec==2)?sizeof(Samples2):sizeof(Samples1)));
	header.comments_size = (uint_4)strlen(comments)+1;
	header.comments_offset = (uint_4)(header.bases_offset + header.bases * sizeof(Bases));
	strncpy(header.version, VERSION_NO,4);
	header.sample_size = prec;
	header.code_set = CSET_DEFAULT;

	for(i=0;i<20;i++) header.spare[i]=0;
	if (write_scf_header(fp, &header)==False) return (False);


	switch(prec) {
	case 2:
	    {
		Samples2 sample;
		
		for(i=0; i<header.samples; i++) {
		    sample.sample_A = seq->traceA[i];
		    sample.sample_C = seq->traceC[i];
		    sample.sample_G = seq->traceG[i];
		    sample.sample_T = seq->traceT[i];
		    if (write_scf_sample2(fp, &sample)==False) return(False);
		}
		break;
	    }
	default:
	    {
		Samples1 sample;
		
		for(i=0; i<header.samples; i++) {
#define scale(P,M) (  (byte) ((float)(P)*255.0/(float)(M))   )
		    sample.sample_A = scale(seq->traceA[i],seq->maxTraceVal);
		    sample.sample_C = scale(seq->traceC[i],seq->maxTraceVal);
		    sample.sample_G = scale(seq->traceG[i],seq->maxTraceVal);
		    sample.sample_T = scale(seq->traceT[i],seq->maxTraceVal);
		    if (write_scf_sample1(fp, &sample)==False) return(False);
		}
		break;
	    }
	}


	for(i=0; i<header.bases; i++) {
	    base.peak_index = seq->basePos[i];
	    base.base = seq->base[i];
	    base.spare[0] = base.spare[1] = base.spare[2] = 0;
	    base.prob_A = base.prob_C = base.prob_G = base.prob_T = 0;
	    switch(base.base) {
	    case 'A' : case 'a':
		base.prob_A = 1; break;
	    case 'C' : case 'c':
		base.prob_C = 1; break;
	    case 'G' : case 'g':
		base.prob_G = 1; break;
	    case 'T' : case 't':
		base.prob_A = 1; break;
	    default:
		base.prob_A = base.prob_C = base.prob_G = base.prob_T = 1;
	    }
	    if (write_scf_base(fp,&base)==False) return (False);

	}

	if (write_scf_comment(fp,comments,(size_t)header.comments_size)==False) return (False);


	fclose(fp);
    }
    
    return (True);
}
