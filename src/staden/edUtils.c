/*
    Title: 	 edUtils

    File: 	 edUtils.c
    Purpose:	 C language utilities for contig editor
    Last update: 5 April 1993
*/


/*
  9/7/91  SD Added Trace Title field to call manageTrace
  26/7/91 SD Tightened up editing options:
             1. Allow only insertion and deletion of asterisks from consensus
	        This means no replacement. This CANNOT be overridden
	     2. Do not allow edits when cursor is not visible on the screen
  28/7/91 SD Use FILE_NAME_LENGTH for trace file name length in showTrace
  1/10/91 SD Removed static from calculateConsensusLength
             Removed LLINOL and LLINOR from call to dojoin_

  28/4/92 SD General Speedup
  20/8/92 SD save state variable for returning what happened during edit/join
  01/03/93 SD New dump contig command
  05/04/93 JKB Check for editorState (disable edits after 'Leave Editor').

*/


#define CHKPNT(W,S) \
    { XSync(XtDisplay(W),False); fprintf(stderr,"CHKPNT: %s\n",S); }


/* ---- Includes ---- */

#include <stdio.h>
#include <ctype.h>
#include <stdlib.h>
#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/Xaw/Text.h>
#include <X11/Xaw/AsciiSrc.h>
#include <X11/Xlib.h>   /* IMPORT: XBell */
#include "Sheet.h"

#include "edUtils.h"
#include "contigEditor.h"
#include "tman_main.h"
#include "main.h"
#include "select.h"
#include "tagUtils.h"
#include "undo.h"
#include "extend.h"
#include "fortran.h"
#include "fort.h"
#include "misc.h"


/* ------ External variables ------- */
extern int_f save_state;
extern enum States {StateDown=0,StateUp} editorState;

/*
** Static variables
** defining the state of the contig editor
*/
#define MAXEDSTATES 2
static int EdStructCount = 0;
static EdStruct edstate[MAXEDSTATES] = {
    {
	/* oldMax             */ 0,
	/* DB_flags           */ DB_ACCESS_UPDATE |
				 DB_DELAYED_READ |
				 DB_DATA_TYPE_DNA,
	/* DB_gelCount        */ 0,
	/* DB_contigNum       */ 0,
	/* DBlist             */ NULL,
	/* DBorder            */ NULL,
	/* DB                 */ NULL,
	/* displayPos         */ 1,
	/* displayWidth       */ DEFAULT_DISPLAY_WIDTH,
	/* displayHeight      */ 0,
	/* cursorPos          */ 1,
	/* cursorSeq          */ 0,
	/* rulerDisplayed     */ 1,
	/* consensusDisplayed */ 1,
	/* fontWidth          */ 0,
	/* fontHeight         */ 0,
        /* edWid              */ NULL,
	/* namesWid           */ NULL,
	/* sequencesWid       */ NULL,
	/* scrollButtonsWid   */ NULL,
	/* sliderWid          */ NULL,
	/* displayedConsensus */ "",
	/* select_made        */ 0,
	/* select_seq         */ 0,
	/* select_start_pos   */ 0,
	/* select_end_pos     */ 0,
	/* select_tag         */ NULL,
	/* reveal_cutoffs     */ 0,
	/* showDifferences    */ 0
    },
    {
	/* oldMax             */ 0,
	/* DB_flags           */ DB_ACCESS_UPDATE |
				 DB_DELAYED_READ |
				 DB_DATA_TYPE_DNA,
	/* DB_gelCount        */ 0,
	/* DB_contigNum       */ 0,
	/* DBlist             */ NULL,
	/* DBorder            */ NULL,
	/* DB                 */ NULL,
	/* displayPos         */ 1,
	/* displayWidth       */ DEFAULT_DISPLAY_WIDTH,
	/* displayHeight      */ 0,
	/* cursorPos          */ 1,
	/* cursorSeq          */ 0,
	/* rulerDisplayed     */ 1,
	/* consensusDisplayed */ 1,
	/* fontWidth          */ 0,
	/* fontHeight         */ 0,
        /* edWid              */ NULL,
	/* namesWid           */ NULL,
	/* sequencesWid       */ NULL,
	/* scrollButtonsWid   */ NULL,
	/* sliderWid          */ NULL,
	/* displayedConsensus */ "",
	/* select_made        */ 0,
	/* select_seq         */ 0,
	/* select_start_pos   */ 0,
	/* select_end_pos     */ 0,
	/* select_tag         */ NULL,
	/* reveal_cutoffs     */ 0,
	/* showDifferences    */ 0
    }
};

static void bell()
/*
** Make a silly beep
*/
{
    XBell (GetDisplay(),100);
}



void calculateConsensusLength(EdStruct *xx)
/*
** Calculate dynamic consensus length
*/
{
    int sequenceEnd,i;

    DBsetLength(xx,0,0);
    for (i=1; i <= xx->DB_gelCount; i++) {
        sequenceEnd = DBgetRelPos(xx,i) + DBgetLength(xx,i) -1;
	if (DBgetLength(xx,0) < sequenceEnd) DBsetLength(xx,0,sequenceEnd);
    }
}

EdStructPtr getFreeEdStruct()
/*
** Get the next free EdStruct
*/
{
    if (EdStructCount == MAXEDSTATES)
	return NULL;
    else
	return &edstate[EdStructCount++];
}

EdStructPtr intToEdStruct(int i)
/*
** Given an integer,
** return the corresponding edit structure
*/
{
    return &edstate[i];
}

EdStructPtr widgetToEdStruct(Widget w)
/*
** Given a widget,
** return the corresponding edit structure
*/
{
    int i;
    if (w==NULL)
	return NULL;
    else
	for (i=0;i<EdStructCount;i++)
	    if (w==edstate[i].edWid)
		return &edstate[i];
    return NULL;
}

/* ---- Private Functions ---- */

/************************************************/




char *DBgetSeq(EdStruct *xx, int seq)
/*
** Force reading in the sequence for seq
*/
{
    int i;

    /* already in memory? */
    if (!seq || DBgetFlags(xx,seq) & DB_FLAG_SEQ_IN_MEMORY)
	return DB_Seq(xx,seq);

    /* allocate memory */
    if ((DBsetSeq(xx,seq,(char *) malloc((int)*saveState.maxgel)))==NULL)
	return NULL;

    /* force reading */
    i = DBgetNumber(xx,seq);
    readw_(saveState.idevw,&i,DB_Seq(xx,seq),saveState.maxgel,*saveState.maxgel);

    /* mark as read */
    DBsetFlags(xx,seq,DBgetFlags(xx,seq)|DB_FLAG_SEQ_IN_MEMORY);

    return DB_Seq(xx,seq);
}





tagStruct *DBgetTags (EdStruct *xx, int seq)
/*
** Force tags into memory
*/
{
    int i;

    /* already in memory? */
    if (!seq || DBgetFlags(xx,seq) & DB_FLAG_TAG_IN_MEMORY)
	return (tagStruct *) DB_Tags(xx,seq);

    /* read in tag list */
    i = DBgetNumber(xx,seq);
    DBsetTags(xx,seq,readTagList(i));

    /* mark as read */
    DBsetFlags(xx,seq,DBgetFlags(xx,seq)|DB_FLAG_TAG_IN_MEMORY);

    return (tagStruct *) DB_Tags(xx,seq);
}



char *DBgetName(EdStruct *xx, int seq)
/*
** Force reading in the sequence for seq
*/
{
    int i;
    char buf[NAMELEN+1];

    /* already in memory? */
    if (!seq || DBgetFlags(xx,seq) & DB_FLAG_NAME_IN_MEMORY)
	return DB_Name(xx,seq);

    /* allocate memory */
    if ((DBsetName(xx,seq,(char *) malloc(sizeof(char)*(NAMELEN+1))))==NULL)
	return NULL;

    /* force reading */
    i = DBgetNumber(xx,seq);
    readn_(saveState.idevn,&i,buf,DB_NAMELEN); buf[10]='\0';
    sprintf(DB_Name(xx,seq),"%*d %-*s",
	    DB_GELNOLEN,(DBgetComp(xx,seq)==COMPLEMENTED)?-DBgetNumber(xx,seq):DBgetNumber(xx,seq),
	    DB_NAMELEN, buf);

    /* mark as read */
    DBsetFlags(xx,seq,DBgetFlags(xx,seq)|DB_FLAG_NAME_IN_MEMORY);

    return DB_Name(xx,seq);
}




int initialiseDB(
	EdStruct *xx,
	int_f *idevr,	/* unit number for relationships */
	int_f *idevw,	/* unit number for working versions of sequences */
	int_f *idevn,	/* unit number for sequence names */
	int_f *relpg,	/* relative positions of gels in sequences */
	int_f *lngthg,	/* lengths of sequences */
	int_f *lnbr,	/* left neighbours */
	int_f *rnbr,	/* right neighbours */
	int_f *maxgel,	/* maximum length of gel */
	int_f *idbsiz,	/* size of database */
	int_f *llino	/* left-most gel in contig */
	)
/*
** Create an internal database and
** read all relevant data into it
*/
{
    int i,c;

    /*
    ** Reset things
    */
    xx->showDifferences = 0;

    /*
    ** determine gel number
    */
    {
	int dummy,numContigs;
	int firstGel;

	readr_(idevr,idbsiz,&dummy,&numContigs,&dummy,&dummy);
	xx->DB_contigNum = (int)*idbsiz-numContigs;
	readr_(idevr,&xx->DB_contigNum,&dummy,&dummy,&firstGel,&dummy);
	while (xx->DB_contigNum < (int)*idbsiz && firstGel!=(int)*llino) {
	    xx->DB_contigNum++;
	    readr_(idevr,&xx->DB_contigNum,&dummy,&dummy,&firstGel,&dummy);
	}

    }

    /*
    ** count number of gels in contig
    */
    for (xx->DB_gelCount=1,i=(int) *llino;
         xx->DB_gelCount<(int)*idbsiz && (int)rnbr[i-1];
         xx->DB_gelCount++,i=(int)rnbr[i-1]);

    if ((xx->DB = (DBStruct *) calloc (xx->DB_gelCount+1, sizeof(DBStruct)))==NULL)
	goto disaster;
    if ((xx->DBlist = (int *)  calloc (xx->DB_gelCount+1, sizeof(int) ))==NULL)
	goto disaster;
    if ((xx->DBorder = (int *) calloc (xx->DB_gelCount+1, sizeof(int) ))==NULL)
	goto disaster;

    /*
    ** read information into local database
    */
    for (c=1,i=(int) *llino;
         c<(int)*idbsiz && i;
         c++,i=(int)rnbr[i-1]) {

        DBsetRelPos(xx,c,relpg[i-1]);
	DBsetLength(xx,c,abs(lngthg[i-1]));
	DBsetComp(xx,c,(lngthg[i-1]<0)?-1:1);
	DBsetNumber(xx,c,i);
        DBsetFlags(xx,c,DB_FLAG_NONE);

	xx->DBorder[c] = c;

	if (xx->DB_flags & DB_STORAGE_INTERNAL) {
	    if (DBgetSeq(xx,c)==NULL) goto disaster;
	    if (DBgetName(xx,c)==NULL) goto disaster;
	    (void)DBgetTags(xx,c);
	}

    }
    /*
    ** Set up consensus
    */
    {
        DBsetRelPos(xx,0,1);
	DBsetComp(xx,0,UNCOMPLEMENTED);
	calculateConsensusLength(xx);
        if ((DBsetSeq(xx,0,(char *) malloc(MAX_DISPLAY_WIDTH)))==NULL)
	    goto disaster;
	if ((DBsetName(xx,0,(char *) malloc(sizeof(char)*(NAMELEN+1))))==NULL)
	    goto disaster;
	sprintf(DB_Name(xx,0),"%*s %-*s",
                  DB_GELNOLEN," ",
                  DB_NAMELEN, "CONSENSUS");
        xx->DBorder[0] = 0;
    }

    return 0;

disaster:

    freeDB(xx);
    return 1;
}

void freeDB(EdStruct *xx)
/*
** Free an internal database
*/
{
    int i;

    if (xx->DB != NULL)
        for (i=0; i <= xx->DB_gelCount; i++) {
	    free(DB_Name(xx,i));
            free(DB_Seq(xx,i));
	    destroyTagList(DB_Tags(xx,i));
        }

    free(xx->DB);
    free(xx->DBlist);
    free(xx->DBorder);

    xx->DB = NULL;
    xx->DBlist = NULL;
    xx->DBorder = NULL;
}


void saveDB(
	EdStruct *xx,
	int_f *idevr,	/* unit number for relationships */
	int_f *idevw,	/* unit number for working versions of sequences */
	int_f *idevn,	/* unit number for sequence names */
	int_f *relpg,	/* relative positions of gels in sequences */
	int_f *lngthg,	/* lengths of sequences */
	int_f *lnbr,	/* left neighbours */
	int_f *rnbr,	/* right neighbours */
	int_f *maxgel	/* maximum length of gel */
    )
/*
** Save an internal database
*/
{
    int i;
    int N,leftN,rightN;
    int flag;

    for (i=1; i<=xx->DB_gelCount; i++) {
	/*
	** update relationships
	*/
	N = DBgetNumber(xx,xx->DBorder[i]);
	relpg[N-1]  = DBgetRelPos(xx,xx->DBorder[i]);
	lngthg[N-1] = (DBgetComp(xx,xx->DBorder[i])==COMPLEMENTED)?-DBgetLength(xx,xx->DBorder[i]):DBgetLength(xx,xx->DBorder[i]);
	if (i==1)
	    leftN = 0;
	else
	    leftN = DBgetNumber(xx,xx->DBorder[i-1]);
	if (i==xx->DB_gelCount)
	    rightN = 0;
	else
	    rightN = DBgetNumber(xx,xx->DBorder[i+1]);
	lnbr[N-1] = leftN;
	rnbr[N-1] = rightN;
	writer_(idevr,&N,&relpg[N-1],&lngthg[N-1],&lnbr[N-1],&rnbr[N-1]);

	flag = DBgetFlags(xx,xx->DBorder[i]);
	/*
	** update working versions
	*/
	if (flag&(DB_FLAG_SEQ_IN_MEMORY|DB_FLAG_SEQ_MODIFIED))
	    writew_(idevw,&N,DB_Seq(xx,xx->DBorder[i]),maxgel,*maxgel);
	/*
	** update tag list
	*/
	if (flag&(DB_FLAG_TAG_IN_MEMORY|DB_FLAG_TAG_MODIFIED)) {
            writeTagList(xx, xx->DBorder[i]);
            destroyTagList(DB_Tags(xx,xx->DBorder[i]));
            DBsetTags(xx,xx->DBorder[i],readTagList(DBgetNumber(xx,xx->DBorder[i])));
        }

        /*
        ** Preserve only ``sequence in memory flag''
        */
	DBsetFlags(xx,xx->DBorder[i],
		   flag&(DB_FLAG_SEQ_IN_MEMORY ||
			 DB_FLAG_NAME_IN_MEMORY ||
			 DB_FLAG_TAG_IN_MEMORY));

    }

    /*
    ** update contig relationships
    */
    calculateConsensusLength(xx);
    relpg[xx->DB_contigNum-1]  = DBgetLength(xx,0);
    lngthg[xx->DB_contigNum-1] = 0;
    lnbr[xx->DB_contigNum-1] = DBgetNumber(xx,xx->DBorder[1]);
    rnbr[xx->DB_contigNum-1] = DBgetNumber(xx,xx->DBorder[xx->DB_gelCount]);
    writer_(idevr,
	    &xx->DB_contigNum,
	    &relpg[xx->DB_contigNum-1],
	    &lngthg[xx->DB_contigNum-1],
	    &lnbr[xx->DB_contigNum-1],
	    &rnbr[xx->DB_contigNum-1]
	    );
    flushl_(idevr);
    flushl_(idevw);
    flushl_(idevn); /* not needed really */
    flushl_(&devils_.idevt);
    flushl_(&devils_.idevc);
    
    cleanUpStack(&bucketStack);

    /* update save state */
    if(xx == intToEdStruct(0))
	save_state |= (int_f)1;
    else
	save_state |= (int_f)2;


}

void joinDB(
	int_f *idevr,	/* unit number for relationships */
	int_f *idevw,	/* unit number for working versions of sequences */
	int_f *idevn,	/* unit number for sequence names */
	int_f *relpg,	/* relative positions of gels in sequences */
	int_f *lngthg,	/* lengths of sequences */
	int_f *lnbr,	/* left neighbours */
	int_f *rnbr,	/* right neighbours */
	int_f *maxgel,	/* maximum length of gel */
	int_f *llinol,
	int_f *lnconl,
	int_f *llinor,
	int_f *lnconr,
	int_f *ngels,
	int_f *nconts,
	int_f *idbsiz
    )
{
    int_f relx;
    int i;

    /*
    ** Save an internal databases
    */
    for (i=0;i<2;i++)
        saveDB(
	    intToEdStruct(i),
	    idevr,
	    idevw,
	    idevn,
	    relpg,
	    lngthg,
	    lnbr,
	    rnbr,
	    maxgel
        );
    relx = editorLockedPos(1/*force*/);
    if (relx<0) {
	relx = 1-relx;
        dojoin_(relpg,lngthg,lnbr,rnbr,ngels,nconts,lnconl,lnconr,
                idbsiz,idevr,idevw,&relx);
    } else {
	relx = 1+relx;
        dojoin_(relpg,lngthg,lnbr,rnbr,ngels,nconts,lnconr,lnconl,
                idbsiz,idevr,idevw,&relx);
    }

    save_state |= (int_f)4;

}


static int linesInRegion(EdStruct *xx, int pos, int width)
/*
** Return number of sequences on screen
*/
{
    int i, count;

    for (i=1,count=0 ;
	 i<=xx->DB_gelCount && DBgetRelPos(xx,xx->DBorder[i])<(pos+width) ;
	 i++) {
        if (DBgetRelPos(xx,xx->DBorder[i])+DBgetLength(xx,xx->DBorder[i])>pos &&
	    DBgetLength(xx,xx->DBorder[i])) count++;
    }
    count += xx->consensusDisplayed;

    return count;

}


static int linesOnScreen (EdStruct *xx, int pos, int width)
/*
** Return number of sequences on screen
*/
{
    int i, count;

    for (i=1,count=0 ; i<=xx->DB_gelCount ; i++) {
	int relPos, length;
	int len_lcut, len_rcut;

	if (xx->reveal_cutoffs) {
	    len_lcut = lenLCut(xx,xx->DBorder[i]);
	    len_rcut = lenRCut(xx,xx->DBorder[i]);
	} else
	    len_lcut = len_rcut = 0;
	relPos = DBgetRelPos(xx,xx->DBorder[i]) - len_lcut;
	length = DBgetLength(xx,xx->DBorder[i]) + len_lcut + len_rcut;
        if (relPos < pos+width && relPos+length>pos)
	    count++;
    }
    count += xx->consensusDisplayed;

    return count;
}



#ifdef notdef
static int maxLinesOnScreen_x (EdStruct *xx, int width)
/*
** Given width, return maximum number of lines on screen
*/
{
    int i,max,linesAtX,X;

    max = linesOnScreen(xx,1,width);
    for (i=1 ; i <= xx->DB_gelCount ; i++) {
        X=DBgetRelPos(xx,i)-width+1;
	if (X<=1) continue;
	linesAtX=linesOnScreen(xx,X,width);
	if (linesAtX>max) max = linesAtX;
    }
    max += xx->consensusDisplayed;

    return max;
}
#endif

int *sequencesInRegion(EdStruct *xx,int pos, int width)
/*
** Return a pointer to list of sequences in region of contig
*/
{
    int i, count;

    for (i=1,count=0 ; i<=xx->DB_gelCount && DBgetRelPos(xx,xx->DBorder[i])<(pos+width) ; i++) {
        if (DBgetRelPos(xx,xx->DBorder[i])+DBgetLength(xx,xx->DBorder[i])>pos && DBgetLength(xx,xx->DBorder[i]))
	    xx->DBlist[count++]=xx->DBorder[i];
    }
    if (xx->consensusDisplayed) xx->DBlist[count++] = 0;

    return xx->DBlist;

}


int *sequencesOnScreen(EdStruct *xx,int pos, int width)
/*
** Return a pointer to list of sequences on screen
*/
{
    int i, count;

    for (i=1,count=0 ; i<=xx->DB_gelCount ; i++) {
	int relPos, length;
	int len_lcut, len_rcut;

	if (xx->reveal_cutoffs) {
	    len_lcut = lenLCut(xx,xx->DBorder[i]);
	    len_rcut = lenRCut(xx,xx->DBorder[i]);
	} else
	    len_lcut = len_rcut = 0;
	relPos = DBgetRelPos(xx,xx->DBorder[i]) - len_lcut;
	length = DBgetLength(xx,xx->DBorder[i]) + len_lcut + len_rcut;
        if (relPos < pos+width && relPos+length>pos)
	    xx->DBlist[count++]=xx->DBorder[i];
    }
    if (xx->consensusDisplayed) xx->DBlist[count++] = 0;

    return xx->DBlist;

}

void DBgetSequence(EdStruct *xx, int seq, int pos, int width, char *str)
/*
** get part of a sequence from its `pos' base for `width' bases
** Bases number from 0?
*/
{
    char *src;
    int length = DBgetLength(xx,seq);
    int i;

    src = DBgetSeq(xx,seq);

    /* Lefthand cut off */
    if (pos<0) {
	i = (width<-pos)?width:-pos;
	getLCut(xx,seq, -pos, i, str);
    } else
	i=0;
    
    /*copy sequence*/
    for (;
	 i<width && (pos+i)<length;
	 i++) {
	str[i]=src[pos+i]; 
    }
    
    /* Righthand cut off */
    if (i<width) {
	getRCut(xx,seq, pos+i-length, width-i, &str[i]);
    }
    
    str[width]='\0';

}



void DBcalcConsensus (EdStruct *xx,int pos, int width, char *str, int mode)
/*
** calculate the consensus for position `pos' in contig,
** for `width' characters
*/
{
    /*
    ** When calculating the consensus,
    **   handle DNA differently from protein
    ** switch (xx->DB_flags & DB_DATA_TYPE) {
    **     case DB_DATA_TYPE_DNA:     do dna stuff
    **     case DB_DATA_TYPE_PROTEIN: do protein stuff
    **     default : do dna stuff???
    ** }
    ** BASES = 6 for dna, 26 for protein (specified by fortran variable IDM?)
    */
    int_f scores[MAX_DISPLAY_WIDTH][BASES];
    int_f index,score;
    int_f i,j,k;
    int_f seven=BASES;
    int jlow,klow,jhigh /* ,khigh */;
    int chunk; /* do in chucks of MAX_DISPLAY_WIDTH */
    int cwidth;

    for (chunk=0; chunk<width; chunk+=MAX_DISPLAY_WIDTH) {
	cwidth = width - chunk;
	if (cwidth>MAX_DISPLAY_WIDTH) cwidth=MAX_DISPLAY_WIDTH;
	/*now do for pos+chunk to pos+chunk+cwidth*/

	for (i=0;i<MAX_DISPLAY_WIDTH;i++)
	    for (j=0;j<BASES;j++) scores[i][j] = 0;

	for (i=1 ;
	    i<=xx->DB_gelCount &&
	    DBgetRelPos(xx,xx->DBorder[i])<(pos+chunk+cwidth) ;
	    i++)
	{
	    if (DBgetRelPos(xx,xx->DBorder[i]) +
		DBgetLength(xx,xx->DBorder[i]) > (pos+chunk) &&
		DBgetLength(xx,xx->DBorder[i]) &&
		(mode == BOTH_STRANDS ||
		 mode == DBgetComp(xx,xx->DBorder[i])))
	    {
		if (DBgetRelPos(xx,xx->DBorder[i]) < (pos+chunk))
		{
		    klow = (pos+chunk) - DBgetRelPos(xx,xx->DBorder[i]);
		    jlow = 0;
		}
		else
		{
		    klow = 0;
		    jlow = DBgetRelPos(xx,xx->DBorder[i]) - (pos+chunk);
		}

		if (DBgetRelPos(xx,xx->DBorder[i]) +
		    DBgetLength(xx,xx->DBorder[i]) > pos+chunk+cwidth)
		{
		    /*khigh = klow + cwidth;*/
		    jhigh = cwidth;
		}
		else
		{
		    /*khigh = DBgetLength(xx,xx->DBorder[i]);*/
		    jhigh = jlow + DBgetLength(xx,xx->DBorder[i]) - klow;
		}

		{
		    char s[MAX_DISPLAY_WIDTH+1];
		    DBgetSequence(xx,xx->DBorder[i],klow,jhigh-jlow,s);
		    for (j=jlow,k=0; j<jhigh; j++,k++)
		    {
			if (*saveState.idm == 26) {
			    pcon1_(&s[k],&scores[j][0]);
			} else {
			    index = indexs_(&s[k],&score);
			    scores[j][index-1] += score;
			    /* No longer needed
			    scores[j][BASES-1] += score;
			    */
			}
		    }
		}

	    }
	}

	for (i=0;i<cwidth;i++){
	    if (*saveState.idm == 26) 
		munotp_(&str[chunk+i],(int_fl)1,&scores[i][0]);
	    else
		gtconc_(&str[chunk+i],(int_fl)1,&scores[i][0],&seven,&pcCut);
	}
    }
    str[width]='\0';
    
}



int positionInContig(EdStruct *xx, int seq, int pos)
/*
** returns relative position in a sequence as an 
** absolute position in the contig
*/
{
    return DBgetRelPos(xx,seq) + pos - 1;
}

static int onScreen (EdStruct *xx, int seq, int pos)
/*
** returns true if base in `seq' at position `pos' is currently
** being displayed on screen 
*/
{
    int posInContig;

    posInContig = positionInContig(xx,seq,pos);
    return (posInContig >= xx->displayPos &&
            posInContig < xx->displayPos + xx->displayWidth );
}


#ifdef notdef
static void getJoinedData(EdStruct *xx, int *leftPos, int* rightPos)
/*
** get information about relative positions of two joined contigs
*/
{
    int offset = editorLockedPos(0/*don't force recalculation*/);
    EdStruct *otherxx;

    otherxx = intToEdStruct(0);
    if (otherxx==xx) {
	otherxx = intToEdStruct(1);
        *leftPos = min(1,1-offset);
	*rightPos = max(DBgetLength(xx,0),DBgetLength(otherxx,0)-offset);
    } else {
        *leftPos = min(1,1+offset);
	*rightPos = max(DBgetLength(xx,0),DBgetLength(otherxx,0)+offset);
    }
}
#endif /*notdef*/

#ifdef notdef
static int getJoinedLength(EdStruct *xx)
/*
** Get the combined length of the two joined contigs
*/
{
    int leftPos, rightPos;
    getJoinedData(xx,&leftPos,&rightPos);
    return rightPos - leftPos;
}

static int getJoinedLeftPos(EdStruct *xx)
/*
** Get the position (relative to xx) of the leftmost base in
** the joined contig
*/
{
    int leftPos, rightPos;
    getJoinedData(xx,&leftPos,&rightPos);
    return leftPos;
}

static int getJoinedRightPos(EdStruct *xx)
/*
** Get the position (relative to xx) of the rightmost base in
** the joined contig
*/
{
    int leftPos, rightPos;
    getJoinedData(xx,&leftPos,&rightPos);
    return rightPos;
}
#endif

static void extents(EdStruct *xx, int *left, int *right)
/*
** Get maximum extents of sequence, taking into account cutoffs.
*/
{

    if (xx->reveal_cutoffs) {
	int eleft, eright;
	int i;

	eleft = eright = 0;
	
	for (i=1; i<=xx->DB_gelCount ; i++) {
	    int thisleft,thisright;

	    thisleft = DBgetRelPos(xx,xx->DBorder[i]) - lenLCut(xx,xx->DBorder[i]);
	    thisright =  DBgetRelPos(xx,xx->DBorder[i]) + DBgetLength(xx,xx->DBorder[i]) + lenRCut(xx,xx->DBorder[i]) - 1;

	    if (eleft > thisleft)
		eleft = thisleft;
	    if (eright < thisright)
		eright = thisright;
	}

	*left = eleft;
	*right = eright;

    } else {
	*left = 1;
	*right = DBgetLength(xx,0);
    }

}




static void joinedExtents(EdStruct *xx, int *leftPos, int* rightPos)
/*
** get information about relative positions of two joined contigs
*/
{
    int offset = editorLockedPos(0/*don't force recalculation*/);
    EdStruct *otherxx;
    int left,right;
    int otherleft,otherright;

    otherxx = intToEdStruct(0);
    extents(xx,&left,&right);
    if (otherxx==xx) {
	otherxx = intToEdStruct(1);
	extents(otherxx,&otherleft,&otherright);
        *leftPos = min(left,otherleft-offset);
	*rightPos = max(right,otherright-offset);
    } else {
	extents(otherxx,&otherleft,&otherright);
        *leftPos = min(left,otherleft+offset);
	*rightPos = max(right,otherright+offset);
    }
}






static void getExtents(EdStruct *xx, int *left, int *right)
{
    if (inJoinMode() && editorLocked())
	joinedExtents(xx,left,right);
    else {
	extents(xx,left,right);
	if (inJoinMode()  && !editorLocked()) {
	    *right += xx->displayWidth - 2;
	    *left  -= xx->displayWidth - 1;
	}
    }


}



/***************************************************/
/**/
/**/





static void setSliderPos (EdStruct *xx, int pos)
/*
** set slider as a position in contig
*/
{
    float percentSet;
    float percentShown;
    int left, right;
    getExtents(xx,&left,&right);
    percentSet = (float)(pos-left)/(float)(right-left+1+1-xx->displayWidth);
    percentShown = (float)xx->displayWidth/(float)(right-left+1+1);

    XawScrollbarSetThumb(xx->sliderWid,percentSet,percentShown);
}






static void incDisplayPosP (EdStruct *xx, int distance)
/*
** Increase the leftmost base position on the screen by a symbolic amount
*/
{
    int oldPos = xx->displayPos;
    int left,right;
    switch (distance) {
        case D_screen     : xx->displayPos += xx->displayWidth; break;
        case D_halfScreen : xx->displayPos += xx->displayWidth/2; break;
        case D_character  : xx->displayPos += 1; break;
    }
    getExtents(xx,&left,&right);
    if (xx->displayPos > right + 2 - xx->displayWidth)
	xx->displayPos = right + 2 - xx->displayWidth;
    if (oldPos!=xx->displayPos)
        redisplaySequences (xx,xx->namesWid, xx->sequencesWid, xx->displayPos, xx->displayWidth);
}








static void decDisplayPosP (EdStruct *xx, int distance)
/*
** Decrease the leftmost base position on the screen by a symbolic ammount
*/
{
    int oldPos = xx->displayPos;
    int left,right;
    switch (distance) {
        case D_screen     : xx->displayPos -= xx->displayWidth; break;
        case D_halfScreen : xx->displayPos -= xx->displayWidth/2; break;
        case D_character  : xx->displayPos -= 1; break;
    }
    getExtents(xx,&left,&right);
    if (xx->displayPos < left)
	xx->displayPos = left;
    if (oldPos!=xx->displayPos)
        redisplaySequences (xx,xx->namesWid, xx->sequencesWid, xx->displayPos, xx->displayWidth);
}









static void setDisplayPosPercentP (EdStruct *xx, float percent)
/*
** Set the leftmost base position on the screen given a percentage
*/
{
    int oldPos = xx->displayPos;
    int left,right;
    getExtents(xx,&left,&right);
    xx->displayPos  = (int)((float) (right-left+1+1-xx->displayWidth) * percent) + left;
    if (oldPos!=xx->displayPos)
        redisplaySequences (xx,xx->namesWid, xx->sequencesWid, xx->displayPos, xx->displayWidth);
}








static void positionCursor(EdStruct *xx, Widget w, int seq, int pos)
/*
** position the cursor int sequence seq at position pos
** and indicate cursor if on screen
*/
{
    if (onScreen(xx,seq,pos)) {
        int screenRow,screenColumn;
        int *seqList;

	screenColumn = positionInContig(xx,seq,pos) - xx->displayPos;
        seqList = sequencesOnScreen(xx,xx->displayPos, xx->displayWidth);
	for(screenRow=0;
	    screenRow<xx->displayHeight && seqList[screenRow] != seq;
	    screenRow++);
	XawSheetDisplayCursor(w,True);
	XawSheetPositionCursor(w,screenColumn,screenRow+xx->rulerDisplayed);
    } else
	XawSheetDisplayCursor(w,False);
}

static void redisplayDisagreement()
/*
** Recalculate and redisplay all disagreements
*/
{
    char spare[MAX_DISPLAY_WIDTH];
    EdStruct *xx[2];
    int i;

    if (inJoinMode()) {
        xx[0] = intToEdStruct(0);
        xx[1] = intToEdStruct(1);
        for (i=0;i<xx[0]->displayWidth;i++)
	    spare[i]=(xx[0]->displayedConsensus[i] ==
	              xx[1]->displayedConsensus[i])?' ':'!';
	XawSheetPutText(disagreeWid,0,0,xx[0]->displayWidth,spare);
    }
}


void redisplaySequences (EdStruct *xx, Widget namesWid, Widget sequencesWid,
                      int pos, int width)
/*
** Redisplay the whole sequence display
*/
{
    Arg args[10];
    int nargs;
    int *seqList;

    setSliderPos(xx,pos);

    /*
    ** Set Up Text Window sizes
    */
    xx->displayHeight = linesOnScreen(xx,pos,width);
    seqList = sequencesOnScreen(xx,pos, width);

    /*
    ** Update names list
    */
    nargs = 0;
    XtSetArg(args[nargs], XtNrows, xx->displayHeight+xx->rulerDisplayed); nargs++;
    XtSetValues(namesWid, args, nargs);
    {
	int i;
	XawSheetInk splodge[NAMELEN];
	for (i=0 ; i < NAMELEN ; i++) splodge[i].sh = sh_inverse;
	for (i=0 ; i < xx->displayHeight ; i++ ) {
	    if (DBgetFlags(xx,seqList[i]) & DB_FLAG_SELECTED)
		XawSheetPutJazzyText(namesWid,0,(i+xx->rulerDisplayed),NAMELEN,DBgetName(xx,seqList[i]),splodge);
	    else
		XawSheetPutText(namesWid,0,(i+xx->rulerDisplayed),NAMELEN,DBgetName(xx,seqList[i]));
	}
    }

    /*
    ** Update sequence list
    */
    nargs = 0;
    XtSetArg(args[nargs], XtNrows, xx->displayHeight+xx->rulerDisplayed); nargs++;
    XtSetValues(sequencesWid, args, nargs);
    {
	int i;
	char spare[MAX_DISPLAY_WIDTH];
	if (xx->rulerDisplayed) {
	    char *k;
	    int j,lower,times;
	    lower = (pos - pos%10);
	    times = width/10 + 2;
	    for (j=0,k=spare;j<times;j++,k+=10,lower+=10) sprintf(k,"%10d",lower);
	    XawSheetPutText(sequencesWid,0,0,width,&spare[9+pos%10]);
	}
	
	DBcalcConsensus(xx,pos,width,xx->displayedConsensus,BOTH_STRANDS);
	for (i=0 ; i < xx->displayHeight ; i++ ) {
	    char * ptr;
	    XawSheetInk splodge[MAX_DISPLAY_WIDTH];
	    
	    if (seqList[i]==0){
		ptr      = xx->displayedConsensus;
	    }else{
		DBgetSequence(xx,seqList[i],pos-DBgetRelPos(xx,seqList[i]),width,spare);
		ptr      = spare;
	    }
	    if (xx->showDifferences) {
		int j;
		for (j=0;j<width;j++) if (spare[j]==xx->displayedConsensus[j])
		    spare[j]='.';
	    }
	    getTagSplodge(xx,seqList[i],pos-DBgetRelPos(xx,seqList[i]),width,splodge);
	    XawSheetPutJazzyText(sequencesWid,0,(i+xx->rulerDisplayed),width,ptr,splodge);
	}
	
    }

    positionCursor(xx,sequencesWid,xx->cursorSeq,xx->cursorPos);
    redisplaySelection(xx);

}




static void dumpSequence(EdStruct *xx, int seq, int pos, int width, char *str)
/*
** get part of a sequence from its `pos' base for `width' bases
** Bases number from 0?
*/
{
    char *src;
    int length = DBgetLength(xx,seq);
    int i,j;

    src = DBgetSeq(xx,seq);

    /* Lefthand cut off */
    if (pos<0) {
	i = (width<-pos)?width:-pos;
	getLCut(xx,seq, -pos, i, str);
	for(j=0;j<i;j++) if (isupper(str[j])) str[j] = tolower(str[j]);
    } else
	i=0;
    
    /*copy sequence*/
    for (;
	 i<width && (pos+i)<length;
	 i++) {
	str[i]=src[pos+i]; 
    }
    
    /* Righthand cut off */
    if (i<width) {
	getRCut(xx,seq, pos+i-length, width-i, &str[i]);
	for(j=i;j<width;j++) if (isupper(str[j])) str[j] = tolower(str[j]);
    }
    
    str[width]='\0';

}

void dumpLine(EdStruct *xx, FILE *fp, int pos, int width)
/*
** Print out a section
*/
{
    int *seqList;
    int i;
    char spare[MAX_DISPLAY_WIDTH];
    char consensus[MAX_DISPLAY_WIDTH];
    int displayHeight;

    displayHeight = linesOnScreen(xx,pos,width);
    seqList = sequencesOnScreen(xx,pos, width);


    if (xx->rulerDisplayed) {
	char *k;
	int j,lower,times;
	lower = (pos - pos%10);
	times = width/10 + 2;
	for (j=0,k=spare;j<times;j++,k+=10,lower+=10) sprintf(k,"%10d",lower);
	fprintf(fp,"%*.*s   %*.*s\n",
		NAMELEN,NAMELEN," ",
		width,width,&spare[9+pos%10]);
    }
    DBcalcConsensus(xx,pos,width,consensus,BOTH_STRANDS);
    
    for (i=0 ; i < displayHeight ; i++ ) {
	char * ptr;
	
	if (DBgetFlags(xx,seqList[i]) & DB_FLAG_SELECTED)
	    fprintf(fp,"%*.*s * ",NAMELEN,NAMELEN, DBgetName(xx,seqList[i]));
	else
	    fprintf(fp,"%*.*s   ",NAMELEN,NAMELEN, DBgetName(xx,seqList[i]));
	
	if (seqList[i]==0){
	    ptr = consensus;
	}else{
	    dumpSequence(xx,seqList[i],pos-DBgetRelPos(xx,seqList[i]),width,spare);
	    ptr = spare;
	}
	if (xx->showDifferences) {
	    int j;
	    for (j=0;j<width;j++) if (spare[j]==consensus[j])
		spare[j]='.';
	}
	fprintf(fp,"%*.*s\n",width,width,ptr);
    }
    
    fprintf(fp,"\n");

}


void dumpRegion(EdStruct *xx, FILE *fp, int start, int end, int width)
{
    for(;start<=end;start+=width)
	dumpLine(xx, fp, start, (end-start+1<width)?end-start+1:width);
}






void dumpContig(EdStruct *xx)
{
    int left,right;

    static int i = 0;
    char fn[1024];
    FILE *fp;

    i++;
    sprintf(fn,"dump.%d.%d",getpid(),i);

    if ( (fp = fopen(fn,"w")) != NULL ) {
	extents(xx, &left, &right);
	bell();
	dumpRegion(xx,fp,left,right,60);
	bell();
	fclose(fp);
    }
}



int createEdDisplay(EdStruct *xx, Widget namesWid, Widget sequencesWid, int seq, int pos)
/*
** Create the initial sequence display
*/
{
    int i;

    /*
    ** Initial position on screen
    */
    xx->cursorSeq = 0;
    xx->cursorPos = pos;

    for (i=1; i<=xx->DB_gelCount; i++) {
	if (DBgetNumber(xx,i) == seq) {
	    xx->cursorSeq = i;
	    xx->cursorPos = pos;
	    break;
	}
    }

    /*
    ** Display.
    ** set xx->displayPos to force repositioning of cursor
    */
    xx->displayPos = positionInContig(xx,xx->cursorSeq,xx->cursorPos) +
	2*xx->displayWidth;
    redisplayWithCursor(xx);
    return 0;
}


void incDisplayPos (EdStruct *xx, int distance)
/*
** Increase the leftmost base position on the screen by a symbolic ammount
*/
{
    if (editorLocked()) {
	incDisplayPosP(intToEdStruct(0), distance);
	incDisplayPosP(intToEdStruct(1), distance);
    } else
	incDisplayPosP(xx, distance);

    redisplayDisagreement();
}


void decDisplayPos (EdStruct *xx, int distance)
/*
** Decrease the leftmost base position on the screen by a symbolic ammount
*/
{
    if (editorLocked()) {
	decDisplayPosP(intToEdStruct(0), distance);
	decDisplayPosP(intToEdStruct(1), distance);
    } else
	decDisplayPosP(xx, distance);

    redisplayDisagreement();
}





void setDisplayPosPercent (EdStruct *xx, float percent)
/*
** Set the leftmost base position on the screen given a percentage
*/
{
    if (editorLocked()) {
	setDisplayPosPercentP(intToEdStruct(0),percent);
	setDisplayPosPercentP(intToEdStruct(1),percent);
    } else
	setDisplayPosPercentP(xx,percent);

    redisplayDisagreement();
}

void setDisplayPos(EdStruct *xx, int pos)
/*
** centralise pos on screen
*/
{
    if (pos==1) pos = 2; /*if pos==1 no redraw is done. So force*/

    if (editorLocked()) {
        int offset = editorLockedPos(0/*don't force recalculation*/);
	EdStruct *otherxx;
	otherxx = intToEdStruct(0);
	if (otherxx == xx) {
	    otherxx = intToEdStruct(1);
	    otherxx->displayPos = pos + offset;
	} else {
	    otherxx->displayPos = pos - offset;
	}
    }

    xx->displayPos = pos;
    decDisplayPos(xx,D_halfScreen);

}

static void showCursor(EdStruct *xx, int seq, int pos)
/*
** ensure that the cursor is visible on the screen
*/
{
    if (onScreen(xx,seq,pos))
        positionCursor(xx,xx->sequencesWid,seq,pos);
    else {
	setDisplayPos(xx,positionInContig(xx,seq,pos));
/*
	if (xx->displayPos > positionInContig(xx,seq,pos))
	    decDisplayPos(xx,D_halfScreen);
	else
	    incDisplayPos(xx,D_halfScreen);
	xx->displayPos = positionInContig(xx,seq,pos) - (xx->displayWidth/2);
	if (xx->displayPos < 1) xx->displayPos = 1;
	if (xx->displayPos > (DBgetLength(xx,0)+2 - xx->displayWidth))
	    xx->displayPos = (DBgetLength(xx,0)+2 - xx->displayWidth);
	redisplaySequences (xx,xx->namesWid, xx->sequencesWid, xx->displayPos, xx->displayWidth);
*/
    }
}

void redisplayWithCursor(EdStruct *xx)
/*
** Redisplay screen, ensuring cursor display
*/
{
    if (onScreen(xx,xx->cursorSeq,xx->cursorPos)) {
	redisplaySequences (xx,xx->namesWid, xx->sequencesWid, xx->displayPos, xx->displayWidth);
	redisplayDisagreement();
    } else
	showCursor(xx,xx->cursorSeq,xx->cursorPos);
}

void caretRight (Widget widget, XEvent *event, String *params,
		 Cardinal *num_params)
/*
** Move cursor right
*/
{
    EdStruct *xx = widgetToEdStruct(XtParent(widget));

    if (xx->cursorPos<=DBgetLength(xx,xx->cursorSeq)) {
	xx->cursorPos++;
        showCursor(xx,xx->cursorSeq, xx->cursorPos);
    } else
	bell();

}


void caretLeft (Widget widget, XEvent *event, String *params,
		 Cardinal *num_params)
/*
** Move cursor left
*/
{
    EdStruct *xx = widgetToEdStruct(XtParent(widget));


    if (xx->cursorPos>1) {
	xx->cursorPos--;
        showCursor(xx,xx->cursorSeq, xx->cursorPos);
    } else
	bell();

}

void caretDown (Widget widget, XEvent *event, String *params,
		 Cardinal *num_params)
/*
** Move cursor down,
** cycle if necessary
*/
{
    EdStruct *xx = widgetToEdStruct(XtParent(widget));

    int *seqList,seqCount;
    int posInContig;
    int i;

    posInContig = positionInContig(xx,xx->cursorSeq,xx->cursorPos);
    seqList = sequencesInRegion(xx,posInContig-1,2);
    seqCount = linesInRegion(xx,posInContig-1,2);
    for(i=0;
	i<seqCount && seqList[i]!=xx->cursorSeq;
	i++);

    i++;
    if (i == seqCount) i = 0;

    xx->cursorSeq = seqList[i];
    xx->cursorPos = posInContig - DBgetRelPos(xx,xx->cursorSeq) + 1;
    if (xx->cursorPos<1)
	xx->cursorPos = 1;
    else
	if (xx->cursorPos > DBgetLength(xx,xx->cursorSeq)+1)
	    xx->cursorPos = DBgetLength(xx,xx->cursorSeq)+1;
    showCursor(xx,xx->cursorSeq, xx->cursorPos);
    
}

void caretUp (Widget widget, XEvent *event, String *params,
		 Cardinal *num_params)
/*
** Move cursor up,
** cycle if necessary
*/
{
    EdStruct *xx = widgetToEdStruct(XtParent(widget));

    int *seqList,seqCount;
    int posInContig;
    int i;

    posInContig = positionInContig(xx,xx->cursorSeq,xx->cursorPos);
    seqList = sequencesInRegion(xx,posInContig-1,2);
    seqCount = linesInRegion(xx,posInContig-1,2);
    for(i=0;
	i<seqCount && seqList[i]!=xx->cursorSeq;
	i++);

    if (i==0) i = seqCount;
    i--;

    xx->cursorSeq = seqList[i];
    xx->cursorPos = posInContig - DBgetRelPos(xx,xx->cursorSeq) + 1;
    if (xx->cursorPos<1)
	xx->cursorPos = 1;
    else
	if (xx->cursorPos > DBgetLength(xx,xx->cursorSeq)+1)
	    xx->cursorPos = DBgetLength(xx,xx->cursorSeq)+1;
    showCursor(xx,xx->cursorSeq, xx->cursorPos);
    
}


static void deleteBase (EdStruct *xx, int seq, int pos)
/*
** delete a single base in a sequence
** and update the screen if necessary
*/
{
    if (seq && pos) {
	int j;
	char *s;
	char deletedBase;
	s=DBgetSeq(xx,seq);
	deletedBase = s[pos-1];
        for (j = pos; j < DBgetLength(xx,seq); j++)
            s[j-1] = s[j];
	DBsetLength(xx,seq,DBgetLength(xx,seq)-1);
	DBsetFlags(xx,seq,DBgetFlags(xx,seq)|DB_FLAG_SEQ_MODIFIED|DB_FLAG_REL_MODIFIED);
	tagDeleteBase(xx,seq,pos,&deletedBase);
        selectDeleteBase(xx, seq, pos);
    }
    else if (seq) {
	/*
	** deletion at position 0 - shift left sequence
	*/
	int i,j;	

        if (DBgetRelPos(xx,seq)==1) {
	    for (i=1; i <= xx->DB_gelCount; i++) {
		if (seq == xx->DBorder[i]) {
		    for (j=i;j>1;j--)
			xx->DBorder[j] = xx->DBorder[j-1];
		    xx->DBorder[1] = seq;
		} else
		    DBsetRelPos(xx,xx->DBorder[i],DBgetRelPos(xx,xx->DBorder[i])+1);
	    }
	    DBsetFlags(xx,seq,DBgetFlags(xx,seq)|DB_FLAG_REL_MODIFIED);

	} else {
	    DBsetRelPos(xx,seq,DBgetRelPos(xx,seq)-1);
	    DBsetFlags(xx,seq,DBgetFlags(xx,seq)|DB_FLAG_REL_MODIFIED);

	    for (i=1; seq != xx->DBorder[i] && i <= xx->DB_gelCount; i++);
	    for (j=i; DBgetRelPos(xx,xx->DBorder[j-1]) > DBgetRelPos(xx,seq) ;j--)
		xx->DBorder[j] = xx->DBorder[j-1];
	    xx->DBorder[j] = seq;
	}

    }

}

void deleteKey (Widget widget, XEvent *event, String *params,
		Cardinal *num_params)
/*
** delete a character
*/
{
    EdStruct *xx = widgetToEdStruct(XtParent(widget));

    int i;
    char ch;

    if (!(xx->DB_flags & DB_ACCESS_UPDATE)) return;
    if (! onScreen(xx, xx->cursorSeq, xx->cursorPos)) {
	/*
	** When cursor is currently displayed on the screen,
	** give a warning tone, and recentre screen on cursor
	*/
	showCursor(xx,xx->cursorSeq, xx->cursorPos);
	bell();
    } else if (positionInContig(xx,xx->cursorSeq,xx->cursorPos)==1 && !xx->cursorSeq) {
	bell();
    } else if (xx->cursorSeq) {
	if (editModeIsSuperman()) {
	    if (xx->cursorPos==1)
		ch = ' ';
	    else {
	        char *s;
	        s = DBgetSeq(xx,xx->cursorSeq);
	        ch = s[xx->cursorPos-2];
	    }

	    recordEdit(&bucketStack,xx,undoDelete,xx->cursorSeq,xx->cursorPos-1,ch,0);

	    deleteBase (xx,xx->cursorSeq, xx->cursorPos-1);

	    calculateConsensusLength(xx);

	    if (xx->cursorPos>1) xx->cursorPos--;
	    redisplayWithCursor(xx);

	} else bell();
    } else {
        char consensus[2];
	DBcalcConsensus(xx,xx->cursorPos-1,1,consensus,BOTH_STRANDS);
	if (*consensus != '*') {
	    /*
	    ** Only allow deletion from consensus when it's an asterisk
	    */
	    bell();
	} else {
	    recordEdit(&bucketStack,xx,undoMark,0,xx->cursorPos,'\0',0);
            for (i=1; i<=xx->DB_gelCount; i++) {
                if (DBgetRelPos(xx,i) + DBgetLength(xx,i) <= xx->cursorPos) ;
		else if (DBgetRelPos(xx,i) >= xx->cursorPos) {
		    recordEdit(&bucketStack,xx,undoDelete,i,0,' ',0);
		    deleteBase (xx,i, 0);
		}
		else {
	            char *s;
		    s=DBgetSeq(xx,i);
	            ch = s[xx->cursorPos-DBgetRelPos(xx,i)-1];
		    recordEdit(&bucketStack,xx,undoDelete,i,xx->cursorPos-DBgetRelPos(xx,i),ch,0);
		    deleteBase (xx,i, xx->cursorPos-DBgetRelPos(xx,i));
		}

            }
	    recordEdit(&bucketStack,xx,undoMark,0,0,'\0',0);

	    DBsetLength(xx,0,DBgetLength(xx,0)-1);
	    redisplaySequences (xx,xx->namesWid, xx->sequencesWid, xx->displayPos, xx->displayWidth);
	    redisplayDisagreement();
	    xx->cursorPos--;
	    redisplayWithCursor(xx);
	}
    }
    
}

static void insertBase (EdStruct *xx, int seq, int pos, char base)
/*
** insert a single base in a sequence
** and update the screen if necessary
*/
{
    if (seq && base!=' ') {
	int j;
	char *s;
	int len = DBgetLength(xx,seq);
	/* don't let user overrun maximum gel length */
	if (len<(int)*saveState.maxgel) {
	    s=DBgetSeq(xx,seq);
	    for (j = len+1; j > pos; j--)
		s[j-1] = s[j-2];
	    s[pos-1] = base;
	    DBsetLength(xx,seq,len+1);
	    DBsetFlags(xx,seq,DBgetFlags(xx,seq)|DB_FLAG_SEQ_MODIFIED|DB_FLAG_REL_MODIFIED);
	    tagInsertBase(xx,seq,pos,&base);
	    selectInsertBase(xx, seq, pos);
	}
    }
    else if (seq) {
	/*
	** space inserted at start: shift sequence right
	*/
	int i,j;	

	if (DBgetRelPos(xx,seq)==1)
	    for (i=1; i<xx->DB_gelCount && DBgetRelPos(xx,xx->DBorder[i+1])==1;i++);
	else
	    i=0;
        if (i==1) {
	    for (i=2; i <= xx->DB_gelCount; i++) {
		DBsetRelPos(xx,xx->DBorder[i],DBgetRelPos(xx,xx->DBorder[i])-1);
	    }
	    DBsetFlags(xx,seq,DBgetFlags(xx,seq)|DB_FLAG_REL_MODIFIED);

	} else {
	    DBsetRelPos(xx,seq,DBgetRelPos(xx,seq)+1);
	    DBsetFlags(xx,seq,DBgetFlags(xx,seq)|DB_FLAG_REL_MODIFIED);

	    for (i=1; seq != xx->DBorder[i] && i <= xx->DB_gelCount; i++);
	    for (j=i; j < xx->DB_gelCount && DBgetRelPos(xx,xx->DBorder[j+1]) < DBgetRelPos(xx,seq) ; j++)
		xx->DBorder[j] = xx->DBorder[j+1];
	    xx->DBorder[j] = seq;
	}

    }

}

static void insertChar (EdStruct *xx,int seq, int pos, char key)
/*
** insert a character
*/
{
    int i;

    /* Insert the base to the right of the caret */
    /* perform a check to see that sequences aren't too long */
    if (seq) {
	/* don't let user overrun maximum gel length */
	if (editModeIsSuperman() &&
	    DBgetLength(xx,seq)<(int)*saveState.maxgel) {

	    recordEdit(&bucketStack,xx,undoInsert,seq,pos,key,0);

	    insertBase(xx, seq, pos, key);

	    calculateConsensusLength(xx);

	    if (key!=' ') xx->cursorPos = ++pos;
	    redisplayWithCursor(xx);
	} else bell();
    } else if (key == '*') {
	/*
	** Only allow insertion of asterisks from consensus
	*/
	recordEdit(&bucketStack,xx,undoMark,0,pos,'\0',0);
	for (i=1; i<=xx->DB_gelCount; i++) {
	    int len = DBgetLength(xx,i);
	    int relPos = DBgetRelPos(xx,i);
	    if (relPos + len < pos ||
		len>=(int)*saveState.maxgel) ;
	    else if (relPos > pos) {
		recordEdit(&bucketStack,xx,undoInsert,i,0,key,0);
		insertBase(xx, i,0,' ');
	    }
	    else if (len) {
		recordEdit(&bucketStack,xx,undoInsert,i, pos-relPos+1,key,0);
	        insertBase(xx, i, pos-relPos+1, key);
	    }
	}
	recordEdit(&bucketStack,xx,undoMark,0,0,'\0',0);

	DBsetLength(xx,0,DBgetLength(xx,0)+1);
 
	xx->cursorPos = ++pos;
	redisplayWithCursor(xx);
    } else {
	bell();
    }
}

static void replaceBase (EdStruct *xx, int seq, int pos, char base)
/*
** replace a single base in a sequence
** and update the screen if necessary
*/
{
    if (seq) {
	char *s;
	char deletedBase;
	s=DBgetSeq(xx,seq);
	deletedBase=s[pos-1];
        s[pos-1] = base;
	DBsetFlags(xx,seq,DBgetFlags(xx,seq)|DB_FLAG_SEQ_MODIFIED);
	tagReplaceBase(xx,seq,pos,&deletedBase, &base);
    }

}

static void replaceChar (EdStruct *xx, int seq, int pos, char key)
/*
** replace a character
*/
{
    /* int i,j; */
    char ch;

    /*
    ** Don't allow replacing past end of sequence
    */
    if (DBgetLength(xx,seq) < pos) {
	bell();
	return;
    }
    if (seq) {
	char *s;
	s=DBgetSeq(xx,seq);

	ch = s[pos-1];
	recordEdit(&bucketStack,xx,undoReplace,seq,pos,ch,0);

	replaceBase(xx,seq, pos, key);

	xx->cursorPos = ++pos;
	redisplayWithCursor(xx);

    } else {
	bell();
	/*
	** Don't allow replacements on consensus at ANY stage
	**
	** recordEdit(&bucketStack,xx,undoMark,0,pos,'\0',0);
	** for (i=1; i<=xx->DB_gelCount; i++) {
	**     if (DBgetRelPos(xx,i) + DBgetLength(xx,i) <= pos) ;
	**     else if (DBgetRelPos(xx,i) > pos) ;
	**     else if (DBgetLength(xx,i)) {
	** 	char *s;
	** 	s=DBgetSeq(xx,i);
	** 	ch = s[pos-DBgetRelPos(xx,i)];
	** 	recordEdit(&bucketStack,xx,undoReplace,i,pos-DBgetRelPos(xx,i)+1,ch,0);
	**         replaceBase(xx,i, pos-DBgetRelPos(xx,i)+1, key);
	**     }
	** }
	** recordEdit(&bucketStack,xx,undoMark,0,0,'\0',0);
	**
	**
	** xx->cursorPos = ++pos;
        ** redisplayWithCursor(xx);
	*/
    }
}

static int validKey(char key)
{
    static char validKeys[] = "CcTtAaGg1234DVBHKLMNRY5678-*";
    if (*saveState.idm == 26)
	return (key!=' ');
    else
	return (strchr(validKeys,key) != NULL);
}

void keyPress (Widget widget, XEvent *event, String *params,
	       Cardinal *num_params)
/*
** Handle a key press
*/
{
    EdStruct *xx = widgetToEdStruct(XtParent(widget));

    char keys[8];
    char key;
    KeySym keySym;
    int charCount;
    XComposeStatus compose;
    /*
    ** validkeys is different when database is for DNA and when
    ** database is for protein
    **
    ** switch (xx->DB_flags & DB_DATA_TYPE) {
    **     case DB_DATA_TYPE_DNA:     do dna stuff
    **     case DB_DATA_TYPE_PROTEIN: do protein stuff
    **     default : do dna stuff???
    ** }
    ** BASES = 6 for dna, 26 for protein (specified by fortran variable IDM?)
    static char validKeys[] = "CcTtAaGg1234DVBHKLMNRY5678-*";
    */

    if (!(xx->DB_flags & DB_ACCESS_UPDATE)) return;

    charCount = XLookupString((XKeyEvent *)event, keys,
			      (int)sizeof(keys), &keySym,&compose);
    key = keys[0];
    if (charCount==1) {
	if (! onScreen(xx, xx->cursorSeq, xx->cursorPos)) {
	    /*
	    ** When cursor is currently displayed on the screen,
	    ** give a warning tone, and recentre screen on cursor
	    */
	    showCursor(xx,xx->cursorSeq, xx->cursorPos);
	    bell();
	} else if (validKey(key)) {
	    if (editModeIsInsert())
	        insertChar (xx,xx->cursorSeq, xx->cursorPos, key);
	    else
	        replaceChar (xx,xx->cursorSeq, xx->cursorPos, key);
        }
	else {
	    if (xx->cursorSeq && xx->cursorPos==1 && key == ' ' && editModeIsInsert())
	        insertChar (xx,xx->cursorSeq, xx->cursorPos, key);
	    else bell();
	}
    }
}

void buttonDown (Widget widget, XEvent *event, String *params,
		 Cardinal *num_params)
/*
** An attempt has been made to reposition the cursor
*/
{
    EdStruct *xx = widgetToEdStruct(XtParent(widget));
#define left_margin 4
#define top_margin 4

    int x,y;
    int *seqList;

    if (editorState == StateDown) return;

    if (widget != xx->sequencesWid) return;

    x = (event->xbutton.x-left_margin) / xx->fontWidth;
    y = (event->xbutton.y-top_margin) / xx->fontHeight - xx->rulerDisplayed;


    if (y>=0 && y < xx->displayHeight && x>=0 && x < xx->displayWidth) {
        seqList = sequencesOnScreen(xx,xx->displayPos,xx->displayWidth);
        xx->cursorSeq = seqList[y];
        xx->cursorPos = xx->displayPos - DBgetRelPos(xx,xx->cursorSeq) + x + 1;
	if (xx->cursorPos<1)
	    xx->cursorPos = 1;
	else
	    if (xx->cursorPos > DBgetLength(xx,xx->cursorSeq)+1)
		xx->cursorPos = DBgetLength(xx,xx->cursorSeq)+1;
        positionCursor(xx,widget,xx->cursorSeq,xx->cursorPos);
    }

}

static void undoLastEdit (EdStruct *xx, int command, int seq, int pos, char ch, int t)
/*
** Undo a command
*/
{
    switch (command) {
	case undoDelete:
	    insertBase(xx, seq,pos,ch);
	    break;
	case undoInsert:
	    deleteBase(xx,seq,(ch==' ')?0:pos);
	    break;
	case undoReplace:
	    replaceBase(xx,seq,pos,ch);
	    break;
	case undoExtend:
	    unextend(xx,seq,ch);
	    break;
	case undoUnextend:
	    undo_unextend(xx,seq,ch,t);
	    break;
	default:
	    break;
	}
}

void undoLastCommand()
/*
** Undo last keypress that modified contig
*/
{
    EdStruct *xx;
    int command;
    int seq;
    int pos;
    char ch;
    int t;

    retrieveEdit(&bucketStack,&xx,&command,&seq,&pos,&ch,&t);
    if (command == undoNullCommand) {
	bell();
	return;
    }
    else if (command == undoMark) {

        retrieveEdit(&bucketStack,&xx,&command,&seq,&pos,&ch,&t);
	while (command != undoMark) {
	    undoLastEdit(xx,command,seq,pos,ch,t);
            retrieveEdit(&bucketStack,&xx,&command,&seq,&pos,&ch,&t);
	}

        xx->cursorSeq = 0;
        xx->cursorPos = pos;

    } else {

	undoLastEdit(xx,command,seq,pos,ch,t);

        xx->cursorSeq = seq;
        xx->cursorPos = pos+(command==undoDelete);

    }

    calculateConsensusLength(xx);
    redisplayWithCursor(xx);
}

static int showTrace(EdStruct *xx, int seq, int pos, int baseSpacing)
{

    char fileName[200];
    tagStruct *t;
    /*
    int traceNo;
    */
    int t_len;        /* number of bases in trace */
    int t_lcut;       /* left cut off of usable data */
    int t_ulen;       /* length of usable data */
    char t_type[5];   /* type of trace */
    char t_fname[FILE_NAME_LENGTH+1]; /* file name of trace */
    char *rawData;
    int baseNum;

    t = (tagStruct *) DBgetTags(xx,seq);

    force_comment(t);

    if (! t->newcommentlen) return 1;
    sscanf(t->newcomment,"%6d%6d%6d%*s",&t_len,&t_lcut,&t_ulen);
    strncpy(t_type,&t->newcomment[18],4);
    strncpy(t_fname,&t->newcomment[22],FILE_NAME_LENGTH);

    t_type[4] = '\0';
    {   /* convert fortran string to c string */
        int i;
        for (i=FILE_NAME_LENGTH-1;i>=0 && (!t_fname[i] || isspace(t_fname[i]));i--) ;
        t_fname[++i] = '\0';
    }

    /* skip if no raw data file for trace */
    if (t_fname[0] == '\0') return 1;

    /* check trace file exists */
    if ( ! file_exists(t_fname) ) {
	char *s;
        /* try in rawData */
	/*
	** get environment details
	*/
	rawData = (char *)getenv ("RAWDATA");
        if (rawData == NULL) return 1;
	/*
        sprintf (fileName,"%s/%s",rawData,t_fname);
	*/
	if ((s=findfile(t_fname,rawData))==NULL) return 1;
	strcpy (fileName,s);
    } else
        /* ok */
        strcpy (fileName,t_fname);

    if (DBgetComp(xx,seq) == UNCOMPLEMENTED) {
        baseNum = origpos(xx, seq, pos) + t_lcut;
	manageTrace(t_type, fileName, baseNum, t_lcut, t_ulen, /*not complemented*/0, baseSpacing, DBgetName(xx,seq));
    } else {
        baseNum = t_len - t_lcut - origpos(xx, seq, normalisePos(xx,seq,pos,1));
	manageTrace(t_type, fileName, baseNum, t_lcut, t_ulen, /*complemented*/1, baseSpacing, DBgetName(xx,seq));
    }


    return 0;
}

void invokeTrace (Widget widget, XEvent *event, String *params,
		  Cardinal *num_params)
{
    EdStruct *xx = widgetToEdStruct(XtParent(widget));
    int baseSpacing = xx->fontWidth * 2;

    if (xx->cursorSeq) {
	showTrace(
	    xx,
	    xx->cursorSeq,
	    xx->cursorPos,
	    baseSpacing);
    } else {
	int *seqList;
	int i;
        seqList = sequencesOnScreen(xx,xx->cursorPos,1);
	for (i=0; i<xx->displayHeight && seqList[i]; i++)
	    showTrace(
		xx,
		seqList[i],
		xx->cursorPos-DBgetRelPos(xx,seqList[i])+1,
		baseSpacing);

    }
}

void countDisagreements(int *overlapLength, int *wingeCount)
{
    int left0,right0;
    int left1/*,right1*/;
    int length0,length1;
    int offset = editorLockedPos(1/*force recalculation*/);
    int i;
    EdStruct *xx[2];
    char *ol0,*ol1;

    xx[0] = intToEdStruct(0);
    xx[1] = intToEdStruct(1);

    if (offset < 0) {
	left0 = 1-offset;
	left1 = 1;
    } else {
	left0 = 1;
	left1 = 1+offset;
    }
    length0 = DBgetLength(xx[0],0);
    length1 = DBgetLength(xx[1],0);
    if (offset+length0 < length1) {
	right0 = length0;
	/*
	right1 = length0-offset;
	*/
    } else {
	right0 = length1-offset;
	/*
	right1 = length1;
	*/
    }
    *overlapLength = right0 - left0+1;
    *wingeCount  = 0;

    if (*overlapLength > 0) {
	ol0 = (char *) malloc(*overlapLength+1);
	ol1 = (char *) malloc(*overlapLength+1);
	DBcalcConsensus(xx[0],left0,*overlapLength,ol0,BOTH_STRANDS);
	DBcalcConsensus(xx[1],left1,*overlapLength,ol1,BOTH_STRANDS);
	for (i=0;i<*overlapLength;i++) if(ol0[i]!=ol1[i])(*wingeCount)++;
	free(ol0);
	free(ol1);
    }
}


void selectRead (Widget widget, XEvent *event, String *params,
		 Cardinal *num_params)
/*
** A gel reading name has been selected - toggle highlighting
*/
{
    EdStruct *xx = widgetToEdStruct(XtParent(widget));
#define left_margin 4
#define top_margin 4

    int y;
    int *seqList;

    if (editorState == StateDown) return;

    if (widget != xx->namesWid) return;

    y = (event->xbutton.y-top_margin) / xx->fontHeight - xx->rulerDisplayed;

    if (y>=0 && y < xx->displayHeight) {
	int flag;
        seqList = sequencesOnScreen(xx,xx->displayPos,xx->displayWidth);
	flag = DBgetFlags(xx,seqList[y]);
	flag ^= DB_FLAG_SELECTED;
	DBsetFlags(xx,seqList[y],flag);
        redisplaySequences (xx,xx->namesWid, xx->sequencesWid, xx->displayPos, xx->displayWidth);
    }

}




/*************************************************************
**
** Fast accessing routines... utilising binary search
**
*************************************************************/

int posToIndex(EdStruct *xx, int pos)
/*
** Find the first sequence that starts at or to the right of a
** given position
*/
{
    int Min, Max, Mid;

    /* binary search */
    /* Min, Max, Mid refer to pairs of numbers: ie MAX --> [MAX-1],[MAX] */
    Min = 1;
    Max = xx->DB_gelCount + 1;
    
    do {
	int r1,r2;

	Mid = (Max+Min)/2;

	/* compare */
	r1 = (Mid==1)?(pos-1):DBgetRelPos(xx,xx->DBorder[Mid-1]);
	r2 = (Mid==xx->DB_gelCount+1)?(pos+1):DBgetRelPos(xx,xx->DBorder[Mid]);

	if (r1 < pos && r2 >= pos) 
	    return (Mid==xx->DB_gelCount+1)?(0):Mid;

	if (r1 < pos)
	    Min = Mid+1;
	else
	    Max = Mid-1;

    } while (Max>=Min);

    return 0;

}


int posToSeq(EdStruct *xx, int pos)
/*
** Find the first sequence that starts at or to the right of a
** given position
*/
{
    int ind;

    ind = posToIndex(xx,pos);
    if (ind)
	return xx->DBorder[ind];
    else
	return 0;

}


int seqToIndex(EdStruct *xx, int seq)
/*
**
*/
{
    int i;
    int ind;

    ind = posToIndex(xx,DBgetRelPos(xx,seq));

    if (ind) {
	for (i=ind; i<=xx->DB_gelCount && xx->DBorder[i]!=seq;i++) ;

	if (i<=xx->DB_gelCount) return i;
    }

    return 0;
	
}




