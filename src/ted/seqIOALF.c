/* 
  Title:       seqIOALF
  
  File: 	 seqIOALF.c
  Purpose:	 IO of ALF sequences
  Last update: Tue Nov 10 1992
  */

/*
  Change Log :- 
  14.01.91 SD
  when complimenting the sequence with an odd number of bases,
  the middle base position was not adjusted.
  15.01.91 SD  Put StLouis stuff on compilation flag
  15.01.91 SD  New include file (opp.h)
  02.08.91 SD  Changes the mapping of uncertainty codes so that we
  now only generate A C G T and -
  Previously... bug in interpreting ALF integer fields.
  We now treat them as unsigned.
  17.09.91 LFW changed STLOUIS compilation flag to SAVE_EDITS
  and AUTO_CLIP
  25.10.91 SD  Machine independant I/O...removed BIGENDIAN flag
  25.11.91 SD There was a hard limit (of 1024) for allocation of
  space for number of bases, yet program would 
  read in more if there were any, causing nasties to happen.
  
  */

/* RMD I made substantial changes to this file 12/28/90 so as to
   read sequence data more freely (necessary when reading data from
   multiple trace files).
   The affected area is indicated by comments starting RMD, like
   this one.
   */

/* This file was adapted by LFW from seqIOABI.c.
   
   At the moment, the `maxTraceVal' of the sequence is hardwired
   as 1200.  This fudge worked for the ABI, but is not really good
   for the ALF.  To keep compatibility we keep 1200 as a max and
   scale to fit (search for scaleFactor).  This needs work.
   
   The ALF results file is a concatenation of many files with an
   index structure at the beginning, consisting of a 512 byte
   block that we ignore, followed by 128 byte blocks describing
   each file.  All files, including the header region, are rounded 
   up to a multiple of 512 bytes long.  
   The getIndexEntry routines identify the 128 byte index component
   of interest by matching 4 chars of its ASCII label, then extract
   the field of choice from that entry.
   
   Note that the SUN and PC are of opposite endian-ness, so that
   we have to provide special routines to read words and longwords
   from the results file.  Luckily the floating point numbers are
   written out in ASCII.
   
   All references to the seq->bottom, the bottom strand of the
   sequence, were added by lfw.
   */


/* ---- Imports ---- */

#include "seq.h"      /* IMPORT: Seq, BasesAndTraces, NULLSeq,
			 newSeq, freeSeq */
/*#include "seqIOABI.h"*/
#include "seqIOEdit.h" /* IMPORT: writeEdSeq, 
			  readEdSEq */


#include <ctype.h>
#include <stdio.h>    /* IMPORT: fopen, fclose, fseek, ftell, fgetc,
			 EOF */
#include "mach-io.h"

/* ---- Constants ---- */

#define BasesPerLine 50 /* For output formatting */

#define IndexEntryLength ((off_t)128)


/*
  Here are some labels we will be looking for, four chars packed
  into a long word.
  */
#define EntryLabel        ((uint_4) ((((('A'<<8)+'L')<<8)+'F')<<8)+' ')
#define BaseEntryLabel    ((uint_4) ((((('S'<<8)+'e')<<8)+'q')<<8)+'u')
#define DataEntryLabel    ((uint_4) ((((('P'<<8)+'r')<<8)+'o')<<8)+'c')


/* ---- Internal functions ---- */

static Boolean getIndexEntryLW(FILE *fp, off_t indexO,
			       uint_4 label, int lw,
			       uint_4 *val)
/*
  From the ALF results file connected to `fp' whose index starts
  at byte offset `indexO', return in `val' the `lw'th long word
  from the entry labelled `label'.
  The result indicates success.
  */
{   off_t entryNum=-1;
    int i;
    uint_4 entryLabel;
    
    do
	{   entryNum++;
	    if (fseek(fp, indexO+(entryNum*IndexEntryLength), 0) != 0) 
		return(-1);
	    
	    if (!be_read_int_4(fp, &entryLabel)) return(False);
	} while (!(entryLabel == label));
    
    for(i=2; i<lw; i++)
	if (!be_read_int_4(fp, val))  return(-1);
    
    
    /* when i = lw read in the 4 bytes backwards */
    if (!le_read_int_4(fp,val)) return(-1);
    
    return(True);
}

static Boolean getIndexEntryW(FILE *fp, off_t indexO,
			      uint_4 label, int lw,
			      uint_2 *val)
/*
  From the ALF results file connected to `fp' whose index starts
  at byte offset `indexO', return in `val' the `lw'th  word (int2)
  from the entry labelled `label'.
  The result indicates success.
  */
{   off_t entryNum=-1;
    int i;
    uint_4 entryLabel;
    uint_4 jval;
    
    do
	{   entryNum++;
	    if (fseek(fp, indexO+(entryNum*IndexEntryLength), 0) != 0) return(-1);
	    if (!be_read_int_4(fp, &entryLabel)) return(False);
	} while (!(entryLabel == label));
    
    
    for(i=2; i<lw; i++)
	if (!be_read_int_4(fp, &jval)) return(-1);
    if (!le_read_int_2(fp, val)) return(-1);
    
    return(True);
}




/* ---- Exports ---- */




Seq readSeqALF(char *fn)
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
    int i;
    FILE *fp;
    uint_4 numPoints;
    
    uint_4 data_size;
    uint_4 dataO;
    uint_4 header_size=396; /* size of the header of the processed data
			     section */
    uint_2 actBaseDataSize; /* actual number of bytes of data of information
			     containing the base and basePos information */
    int num_points; /* keeps track of the actual number of points, rather
		       than the early guess of numPoints */
    
    /* RMD 12/28/90 changed scaleFactor from 7 to 20 -
       should read this from file, or calculate it */
    
    off_t indexO;   /* File offset where the index is */
    uint_4 baseO;    /* File offset where the bases are stored */
    
    
    if ((fp = fopen(fn, "rb")) == NULL) return(NULLSeq);
    
    
    /* RMD lots of changes below here until end of data reading section
       Some are cosmetic - I introduced the label abort: at the end of
       the routine and sent all error stuff there.  
       getIndexEntry calls in front of where they were needed, and made
       There is a substantive change to the inner loop of the sequence
       reading section.  This now uses fscanf - much less rigid than the
       previous scheme.  Note that it reads bp as a float.  This is because
       it is a float in multiple trace data files! (bizarre Pharmacia
       programming!).
       */
    
    
    /* find offsets and length of sequence and data segments */
    
    indexO = 522;      /* indexO is the offset of the index */
    /* or I could look for the first label, starting 'ALF'
       if I used 512 then none of the entries are on long 
       word boundaries */
    
    if (! ( getIndexEntryLW(fp,indexO,BaseEntryLabel,12,&baseO)  ))
	/* offset in file of first base of sequence */
	goto abort ;
    
    if (! ( getIndexEntryW(fp,indexO,BaseEntryLabel,10,&actBaseDataSize)  ))
	/* actual size of region containing this data */
	goto abort ;
    
    if (! ( getIndexEntryLW(fp,indexO,DataEntryLabel,12,&dataO)))
	/* offset in file to start of processed data segment - there 
	   is then a header of size header_size (currently 396) */
	goto abort ;
    
    if (! ( getIndexEntryLW(fp,indexO,DataEntryLabel,10,&data_size)  ))
	/* actual size of region containing this data */
	goto abort ;
    
    numPoints = (int)((data_size - header_size)/ 8); 
    /* because each trace value is stored in a 2 byte
       integer, thus to store A C G T information
       it takes 8 bytes.  So subtract off the header and
       divide by 8 */
    
    /* RMD make enough space for 1024 bases - hard limit */
#define BASELIMIT 1024
    /* Allocate the sequence */
    if ((seq = newSeq(BasesAndTraces, numPoints, BASELIMIT)) == NULLSeq)
	{   fclose(fp);
	    return(NULLSeq);
	}
    
    seq->mode        = BasesAndTraces;
    seq->format      = ALFFormat;
    seq->dirty       = False;
    seq->maxTraceVal = 0;
    seq->NPoints     = numPoints;
    seq->bottom	     = False;
    
    /* RMD set ->NorigBases and ->NedBases after reading them in */
    
    /* read in the sequence */
    
    if (!(fseek(fp, (off_t)baseO, 0) == 0))
	goto abort ;
    
    {	/* new locals introduced by LFW and/or RMD for the ALF */
	int numBases;	/* number of nucleotides read in */
	float bp ;
	char ch;
	
	
	for (numBases = 0 ; ftell(fp) < baseO+(unsigned short)actBaseDataSize  && numBases<BASELIMIT; ) {
	    char line[200];
	    fgets(line,(int)sizeof(line),fp);
	    sscanf (line,"%c %*d %f", &ch, &bp) ;
	    
	    /* we convert ch to Staden format here */
	    switch (ch) {
	    case 'A':
	    case 'C':
	    case 'G':
	    case 'T':
		break;
	    default:
		if (isupper(ch))
		    ch = '-';
		else
		    ch = '\0';
	    }
	    
	    if (ch) {
		seq->base[numBases]    = ch;
		seq->basePos[numBases] = bp;
		++numBases ;
		
	    }
	}
	
	seq->NorigBases  = numBases;
	seq->NedBases    = numBases;
    }
    
    /* read in the traces , stored in 2 byte integers in records in the
       order A C G T A C G T A C G T ...*/
    
    if (fseek(fp, (off_t)(dataO+header_size), 0) != 0) 
	goto abort ;
    
    num_points = 0;
    
    for (i=0;i<(seq->NPoints);i++)
	{   if (!le_read_int_2(fp, &(seq->traceA[i])))
		{fclose(fp);freeSeq(seq);return(NULLSeq);}
	    if (seq->maxTraceVal < seq->traceA[i]) seq->maxTraceVal = seq->traceA[i];
	    if (!le_read_int_2(fp, &(seq->traceC[i])))
		{fclose(fp);freeSeq(seq);return(NULLSeq);}
	    if (seq->maxTraceVal < seq->traceC[i]) seq->maxTraceVal = seq->traceC[i];
	    if (!le_read_int_2(fp, &(seq->traceG[i])))
		{fclose(fp);freeSeq(seq);return(NULLSeq);}
	    if (seq->maxTraceVal < seq->traceG[i]) seq->maxTraceVal = seq->traceG[i];
	    if (!le_read_int_2(fp, &(seq->traceT[i])))
		{fclose(fp);freeSeq(seq);return(NULLSeq);}
	    if (seq->maxTraceVal < seq->traceT[i]) seq->maxTraceVal = seq->traceT[i];
	    
	    if (seq->traceA[i]==0 && seq->traceT[i]==0 && seq->traceC[i]==0 && seq->traceG[i]==0 && i > (numPoints-64)) break;
	    
	    num_points++;
	}
    
    
    /* Finished with the file */
    fclose(fp);
    
    return(seq);
    
 abort:
    fclose(fp);
    freeSeq(seq);
    return(NULLSeq);
}
