/* 
  Title:       seqIOABI
  
  File: 	 seqIOABI.c
  Purpose:	 IO of ABI sequences
  Last update: Tue Nov 10 1992
  
  Change log:
  27/11/90 SD     writeSeqABI() outputs header to sequence file:
  format: ;{noOfBases}{leftCutOff}{basesWritten}{type}{tra
  cefile}
  eg:     ;   867    45    383ABI a09b7.s1RES
  28.11.90 SD  put undesirables under STLOUIS compilation flag
  11.12.90 SD  new static function tail to find file name in path name
  02.01.91 SD  Merged with St.L version
  15.01.91 SD  New include added (opp.h)
  30.07.91 SD  Those ole FWO_ field blues
  17.09.91 LFW changed STLOUIS compilation flag to SAVE_EDITS
  and AUTO_CLIP
  25.10.91 SD  Machine independant I/O...removed BIGENDIAN flag
  10.11.92 SD  FWO_ and S/N% interpretation. Comments for information
  window.
  
  */


/*
  This module should be regarded as part of `seq' since it is
  privy to the internal structure of `Seq'.
  
  At the moment, the `maxTraceVal' of the sequence is hardwired
  as 1200, but I suspect it is encoded somewhere in the ABI file
  structure, or if not, we should calculate it as we read the
  traces in.
  
  The ABI results file is controlled by an index found towards
  the end --- this is pointed to by a longword found at `IndexPO'.
  The index consists of a number of entries, each of which is
  four character label followed by 6 long words. The first of these
  long words holds a simple count (starting at 1) for those cases
  where there are multiple entries with the same label. Entries should
  be found by label (and count), rather than their index position,
  because entries can be ommited or new ones added. This happens when
  ABI changes the version of their software and also depending
  on whether the data was analysed or unalaysed. We do, however,
  make assumptions about the relative order of entries.
  
  Ideally we would have a separate module which provides a number
  of functions to extract the data we are interested in, keeping
  the ABI format well wrapped up and out of harms way.
  
  Note that we are relying on the endian-ness of the machine being
  appropriate so we can just read long words in as integers. This
  should be recoded to deal with running on different endians.
  
  All references to the seq->bottom, the bottom strand of the
  sequence, were added by lfw
  */




/* ---- Imports ---- */

#include "seq.h"      /* IMPORT: Seq, BasesAndTraces, NULLSeq,
			 newSeq, freeSeq */
#include "seqIOABI.h"
#include "seqIOEdit.h" /* IMPORT: writeEdSeq, 
			  readEdSEq */

#include "fpoint.h"    /* IMPORT: int_to_float */
#include <ctype.h>
#include <stdio.h>    /* IMPORT: fopen, fclose, fseek, ftell, fgetc,
			 EOF */
#include <stdlib.h>

#include "mach-io.h" /* IMPORT: machine independant io library */
#include "dialogues.h"

/* ---- Constants ---- */

#define BasesPerLine 50 /* For output formatting */


/*
  The index is located towards the end of the ABI trace file.
  It's location is given by a longword at a fixed place.
  */
#define IndexPO ((off_t)26)

#define IndexEntryLength 28


/*
  Here are some labels we will be looking for, four chars packed
  into a long word.
  */
#define DataEntryLabel    ((int_4) ((((('D'<<8)+'A')<<8)+'T')<<8)+'A')
#define BaseEntryLabel    ((int_4) ((((('P'<<8)+'B')<<8)+'A')<<8)+'S')
#define BasePosEntryLabel ((int_4) ((((('P'<<8)+'L')<<8)+'O')<<8)+'C')
#define SpacingEntryLabel ((int_4) ((((('S'<<8)+'P')<<8)+'A')<<8)+'C')
#define SignalEntryLabel  ((int_4) ((((('S'<<8)+'/')<<8)+'N')<<8)+'%')
#define FWO_Label         ((int_4) ((((('F'<<8)+'W')<<8)+'O')<<8)+'_')
#define MCHNLabel         ((int_4) ((((('M'<<8)+'C')<<8)+'H')<<8)+'N')
#define PDMFLabel         ((int_4) ((((('P'<<8)+'D')<<8)+'M')<<8)+'F')
#define SMPLLabel         ((int_4) ((((('S'<<8)+'M')<<8)+'P')<<8)+'L')
#define PPOSLabel         ((int_4) ((((('P'<<8)+'P')<<8)+'O')<<8)+'S')




/* ---- Internal functions ---- */



static Boolean getIndexEntryLW(FILE *fp, off_t indexO,
			       uint_4 label, uint_4 count, int lw,
			       uint_4 *val)
/*
  From the ABI results file connected to `fp' whose index starts
  at byte offset `indexO', return in `val' the `lw'th long word
  from the `count'th entry labelled `label'.
  The result indicates success.
  */
{   off_t entryNum=-1;
    int i;
    uint_4 entryLabel, entryLw1;
    
    do
	{   entryNum++;
	    if (fseek(fp, indexO+(entryNum*IndexEntryLength), 0) != 0) return(-1);
	    if (!be_read_int_4(fp, &entryLabel)) return(False);
	    if (!be_read_int_4(fp, &entryLw1))   return(False);
	} while (!(entryLabel == label && entryLw1 == count));
    
    for(i=2; i<=lw; i++)
	if (!be_read_int_4(fp, val)) return(-1);
    
    return(True);
}




/* ---- Exports ---- */




Seq readSeqABI(char *fn)
/*
  Read the ABI format sequence with name `fn' into `seq'.
  All printing characters (as defined by ANSII C `isprint')
  are accepted, but `N's are translated to `-'s.  If bottom 
  is True, we read the sequence in in reverse and complemented 
  A NULL result indicates failure.
  
  In this module all references to the bottom strand were 
  inserted by lfw.
  */
{   Seq seq = NULLSeq;
    uint_4 fwo_; /* base -> lane mapping */
    FILE *fp;
    uint_4 numPoints, numBases;
    uint_4 signalO;

    uint_4 indexO;   /* File offset where the index is */
    uint_4 baseO;    /* File offset where the bases are stored */
    uint_4 basePosO; /* File offset where the base positions are stored */
    uint_4 dataCO;   /* File offset where the C trace is stored */
    uint_4 dataAO;   /* File offset where the A trace is stored */
    uint_4 dataGO;   /* File offset where the G trace is stored */
    uint_4 dataTO;   /* File offset where the T trace is stored */
    uint_4 MCHN_O;   /* File offset where the machine name is stored */
    uint_4 PDMF_O;   /* File offset where the dye primer guff is stored */
    /*
      uint_4 SMPL_O;   /. File offset where the sample name is stored ./
    */
    
    int i;
    
    
    if ((fp = fopen(fn, "rb")) == NULL) return(NULLSeq);
    
    
    /* Get the index offset */
    if ((fseek(fp, IndexPO, 0) != 0) || (!be_read_int_4(fp, &indexO)))
	{   fclose(fp);
	    return(NULLSeq);
	}
    
    /* Get the number of points */
    if (!getIndexEntryLW(fp,(off_t)indexO,DataEntryLabel,9,3,&numPoints))
	{   fclose(fp);
	    return(NULLSeq);
	}
    
    /* Get the number of bases */
    if (!getIndexEntryLW(fp,(off_t)indexO,BaseEntryLabel,1,3,&numBases))
	{   fclose(fp);
	    return(NULLSeq);
	}
    
    /* Allocate the sequence */
    if ((seq = newSeq(BasesAndTraces, numPoints, numBases)) == NULLSeq)
	{   fclose(fp);
	    return(NULLSeq);
	}
    
    seq->mode        = BasesAndTraces;
    seq->format      = ABIFormat;
    seq->dirty       = False;
    seq->maxTraceVal = 1200;
    seq->NPoints     = numPoints;
    seq->NorigBases  = numBases;
    seq->NedBases    = numBases;
    seq->bottom      = False;
    
    
    /*************************************************************
     *
     *************************************************************/
    
    /*
     ** The order of the DATA fields is determined by the field FWO_
     ** Juggle around with data pointers to get it right
     */
    {
	uint_4 *dataxO[4];
	
	dataxO[0] = &dataCO;
	dataxO[1] = &dataAO;
	dataxO[2] = &dataGO;
	dataxO[3] = &dataTO;
	
	/* Get the Freak World Out (FWO?) field ... */
	if (!getIndexEntryLW(fp,(off_t)indexO,FWO_Label,1,5,&fwo_))
	    {   fclose(fp);
		return(NULLSeq);
	    }
#define baseIndex(B) ((B)=='C'?0:(B)=='A'?1:(B)=='G'?2:3)
	/*Get the positions of the four traces */
	if (! ( getIndexEntryLW(fp,(off_t)indexO,DataEntryLabel,9 ,5,dataxO[baseIndex((char)(fwo_>>24&255))]) &&
	       getIndexEntryLW(fp,(off_t)indexO,DataEntryLabel,10,5,dataxO[baseIndex((char)(fwo_>>16&255))]) &&
	       getIndexEntryLW(fp,(off_t)indexO,DataEntryLabel,11,5,dataxO[baseIndex((char)(fwo_>>8&255))]) &&
	       getIndexEntryLW(fp,(off_t)indexO,DataEntryLabel,12,5,dataxO[baseIndex((char)(fwo_&255))])
	       )
	    )    
	    {   fclose(fp);
		freeSeq(seq);
		return(NULLSeq);
	    }
	
    }
    
    
    /* Read in the C trace */
    if (fseek(fp, (off_t)dataCO, 0) != 0) {fclose(fp);freeSeq(seq);return(NULLSeq);}
    for (i=0;i<(seq->NPoints);i++)
	{   if (!be_read_int_2(fp, &(seq->traceC[i])))
		{fclose(fp);freeSeq(seq);return(NULLSeq);}
	}
    
    /* Read in the A trace */
    if (fseek(fp, (off_t)dataAO, 0) != 0) {fclose(fp);freeSeq(seq);return(NULLSeq);}
    for (i=0;i<(seq->NPoints);i++)
	{   if (!be_read_int_2(fp, &seq->traceA[i]))
		{fclose(fp);freeSeq(seq);return(NULLSeq);}
	}
    
    /* Read in the G trace */
    if (fseek(fp, (off_t)dataGO, 0) != 0) {fclose(fp);freeSeq(seq);return(NULLSeq);}
    for (i=0;i<(seq->NPoints);i++)
	{   if (!be_read_int_2(fp, &seq->traceG[i]))
		{fclose(fp);freeSeq(seq);return(NULLSeq);}
	}
    
    
    /* Read in the T trace */
    if (fseek(fp, (off_t)dataTO, 0) != 0) {fclose(fp);freeSeq(seq);return(NULLSeq);}
    for (i=0;i<(seq->NPoints);i++)
	{   if (!be_read_int_2(fp, &seq->traceT[i]))
		{fclose(fp);freeSeq(seq);return(NULLSeq);}
	}
    
    
    
    /* Read in the bases */
    if (! (getIndexEntryLW(fp,(off_t)indexO,BaseEntryLabel,1,5,&baseO) &&
           (fseek(fp, (off_t)baseO, 0) == 0)
	   )
	)
	{   fclose(fp);
	    freeSeq(seq);
	    return(NULLSeq);
	}
    for (i=0;i<(seq->NorigBases);i++)
	{   int ch;
	    
	    if ((ch = fgetc(fp)) == EOF)
		{   fclose(fp);
		    freeSeq(seq);
		    return(NULLSeq);
		}
	    seq->base[i] = (ch == 'N') ? '-' : ch;
	}
    
    
    
    
    /* Read in the base positions */
    if (!(getIndexEntryLW(fp,(off_t)indexO,BasePosEntryLabel,1,5,&basePosO) &&
          (fseek(fp, (off_t)basePosO, 0) == 0)
	  )
	)
	{    fclose(fp);
	     freeSeq(seq);
	     return(NULLSeq);
	 }
    
    for (i=0;i<(seq->NorigBases);i++)
	{   if (!be_read_int_2(fp, (uint_2 *)&seq->basePos[i]))
		{ fclose(fp); freeSeq(seq); return(NULLSeq); }
	}
    
    
    /*************************************************************
     * Gather useful information
     *************************************************************/
    {
	char comment[1024];
	char line[128];
	int4 spacing;
	int4 ppos;
	
	*comment = '\0';
	
	
	/* Get Signal Strength Offset */
	if (getIndexEntryLW(fp,(off_t)indexO,SignalEntryLabel,1,5,&signalO)) {
	    int2 C,A,G,T;
	    int2 *base[4];
	    base[0] = &C;
	    base[1] = &A;
	    base[2] = &G;
	    base[3] = &T;
	    if (fseek(fp, (off_t)signalO, 0) >= 0 &&
		be_read_int_2(fp, (uint_2 *)base[baseIndex((char)(fwo_>>24&255))]) &&
		be_read_int_2(fp, (uint_2 *)base[baseIndex((char)(fwo_>>16&255))]) &&
		be_read_int_2(fp, (uint_2 *)base[baseIndex((char)(fwo_>>8&255))]) &&
		be_read_int_2(fp, (uint_2 *)base[baseIndex((char)(fwo_&255))])) {
		sprintf(line,"avg_signal_strength = C:%d A:%d G:%d T:%d\n",C,A,G,T);
		strcat(comment,line);
	    }
	}

	/* Get the spacing.. it's a float but don't worry yet */
	if (getIndexEntryLW(fp,(off_t)indexO,SpacingEntryLabel,1,5,(uint_4*)&spacing)) {
	    sprintf(line,"avg_spacing = %6.2f\n",int_to_float(spacing));
	    strcat(comment,line);
	}

	
	/* Get primer position */
	if (getIndexEntryLW(fp,(off_t)indexO,PPOSLabel,1,5,(uint_4 *)&ppos)) {
	    sprintf(line,"primer_position = %d\n",(ppos>>16)); /* ppos stores in MBShort of pointer */
	    strcat(comment,line);
	}

	/* Get Machine Name Offset */
	if (getIndexEntryLW(fp,(off_t)indexO,MCHNLabel,1,5,&MCHN_O)) {
	    if (fseek(fp, (off_t)MCHN_O, 0) >= 0) {
		unsigned char l;
		char buffer[256];
		/* first byte is a length */
		fread(&l,sizeof(char),1,fp);
		fread(buffer,l,1,fp);
		sprintf(line,"machine_name = %.*s\n",l,buffer);
		strcat(comment,line);
	    }
	}
	
	/* Get Dye Primer Offset */
	if (getIndexEntryLW(fp,(off_t)indexO,PDMFLabel,1,5,&PDMF_O)) {
	    if (fseek(fp, (off_t)PDMF_O, 0) >= 0) {
		unsigned char l;
		char buffer[256];
		/* first byte is a length */
		fread(&l,sizeof(char),1,fp);
		fread(buffer,l,1,fp);
		sprintf(line,"dye_primer = %.*s\n",l,buffer);
		strcat(comment,line);
	    }
	}

	/* Get Sample Name Offset */
	/*
         * The code here needs to be a bit more clever
	 * When sample name length is less than (or equal?) four, the
	 * string is packed into the SMPL_O field
	if (getIndexEntryLW(fp,(off_t)indexO,SMPLLabel,1,5,&SMPL_O)) {
	    if (fseek(fp, (off_t)SMPL_O, 0) >= 0) {
		unsigned char l;
		char buffer[256];
		/. first byte is a length ./
		fread(&l,sizeof(char),1,fp);
		fread(buffer,l,1,fp);
		sprintf(line,"sample_name = %.*s\n",l,buffer);
		strcat(comment,line);
	    }
	}
	*/

	
	/* dumplicate string and set info */
	{
	    char *s = (char *)malloc(strlen(comment)+1);
	    strcpy(s,comment);
	    seq->info = s;
	}
    }
    
    /* Finished with the file */
    fclose(fp);
    
    return(seq);
}
