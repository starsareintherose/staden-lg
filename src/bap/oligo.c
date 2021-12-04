/*
 * File: oligo.c
 * Version:
 *
 * Author: Simon Dear
 *         MRC Laboratory of Molecular Biology
 *	   Hills Road
 *	   Cambridge CB2 2QH
 *	   United Kingdom
 *
 * Description: oligo selection module
 *
 * Created: 1991
 * Updated: 6 November 1992
 *	
 * 6 November 1992
 *	Changes for distribution
 */
#include <stdio.h>
#include <stdlib.h> /* getenv & malloc */
#include <string.h>

#include <X11/StringDefs.h>
#include <X11/Intrinsic.h>
#include <X11/Shell.h>
#include <X11/Xaw/Form.h>
#include <X11/Xaw/Label.h>
#include <X11/Xaw/Box.h>
#include <X11/Xaw/Toggle.h>
#include <X11/Xaw/Text.h>
#include <X11/Xaw/Command.h>
#include <X11/Xaw/MenuButton.h>
#include <X11/Xaw/SimpleMenu.h>
#include <X11/Xaw/SmeBSB.h>

#include "edUtils.h"
#include "myparams.h"
#include "defn.h"
#include "struct.h"
#include "tagUtils.h"
#include "oligo.h"
#include "misc.h"
#include "oligocom.h"

/*
 * Compilation modes:
 *
 * NEMATODE - Make some features switchable by the environment variable SUBCLONES
 * VERBOSENESS - Allow for varying degress of verbose output, including debugging information
 */
#define NEMATODE
#define VERBOSENESS

/* nematode - the drink when you're not having a drink */
int nematode;


#include "subclone.h"

/*
 * Useful #defines
 */
#define FORWARDS  True
#define BACKWARDS False
extern void messagef(char *format, ...);

/*
 * Widgets and things
 */
static EdStruct *thisxx;   /* the current EdStruct */
static int up = 0;
static Widget oldFogieWid;
static Widget oligoWid = NULL;
static Widget form;
static Widget label;

static Widget bbox,    strand;
static int strand_state = FORWARDS;

static Widget cbox,    change;
static Widget dbox,    find, next;
static Widget template;
static Widget ebox,    ok, quit;

/*
 * Current state of selection
 */
static int p;                          /* cursor position for selection */
static int l,r;                        /* position of left and right ends of selection region */
static int num_oligos;                 /* number of oligo selected last time */
static int curr_oligo;                 /* number of current oligo being considered */
static Boolean oligo_sense;            /* status of sense buttons when find-oligo button pressed */
static int template_index;             /* gel number of current selected template */
static char template_name[DB_NAMELEN+1]; /* gel name of current selected template */
static char *consensus = NULL; 	       /* consensus for region used to select oligo*/

/*
 * Parameters for template selection
 */
#ifdef VERBOSENESS
static int verbose = 1;			/* verbose output is required */
char verbosity[10];                     /* space for string form of verbose */
#define verbose_debug (verbose==2 || verbose==3)
#define verbose_panic (verbose==3)
#endif /*VERBOSENESS*/

static int fwd_width = 40;		/* how far ahead search window should stretch */
static int bkwd_width = 40;		/* how far back search window should stretch */

static int def_insert_size = 1000;            /* How far from start of template oligo can be */
static char filter[100] = "\\.[sSfFrR]1[^a-z]"; /* filter out templates from gel names */
static char clonelib[100]; /* library of subclone information: initialised in initialise()*/
static int ave_read_len = 400;         /* average read length */


/*
 * A few necessary forward declarations
 */
static int findOligos(EdStruct *xx, int sense);
static void nextOligo(EdStruct *xx, int oligo, int sense);
static int create_new_oligo_tag(EdStruct *xx, int oligo, int pos, int len, int sense);
static void destroy_oligo_popup();
static void destroy_temporary_tag(EdStruct *xx);
static void display_template_details();


/*********************************************************************************/
/*
 * Start of code proper
 */


/*
 * Callback routines
 */


static void set_strand_state(Widget w, int strand_state)
{
    Arg args[2];
    Cardinal nargs;

    nargs = 0;
    XtSetArg(args[nargs], XtNlabel, (strand_state==FORWARDS)?"------>":"<------"); nargs++;
    XtSetValues(w,args,nargs);
    
}


static void strandCallback(Widget w, XtPointer client_data, XtPointer call_data)
/*
 * Change strand
 */
{
    strand_state = (strand_state == FORWARDS)?BACKWARDS:FORWARDS;
    set_strand_state(w,strand_state);

    XtCallCallbacks(find, XtNcallback, (XtPointer) NULL);
}






/*
 * Move data[] outside scope of following function - non ANSI to perform
 * aggregate definitions inside a function
 */
Field_entry data_1[] = {
    {"Search window bases ahead",  (char *)&fwd_width,    t_int,  sizeof(fwd_width)},
    {"Search window bases back",   (char *)&bkwd_width,   t_int,  sizeof(bkwd_width)},
    {"Default insert size",        (char *)&def_insert_size,   t_int,  sizeof(def_insert_size)},
    {"Average read length",        (char *)&ave_read_len, t_int,  sizeof(ave_read_len)},
    {"Filter to select templates", (char *)filter,        t_char, sizeof(filter)},
    {"Cosmid subclone library",    (char *)clonelib,      t_char, sizeof(clonelib)},
#ifdef VERBOSENESS
    {"Verbose output?",            (char *)verbosity,     t_char, sizeof(verbosity)}
#endif /*VERBOSENESS*/
};

static void changeMineCallback(Widget w, XtPointer client_data, XtPointer call_data)
/*
 * Change oligo selection parameters
 */
{
#ifdef VERBOSENESS
    if (verbose_debug) message("Change my parameters\n");

    /* Verbose output can be "yes", "no", "debug", or "panic"!!!! */
    switch (verbose) {
    case 0: strcpy(verbosity,"No"); break;
    case 1: strcpy(verbosity,"Yes"); break;
    case 2: strcpy(verbosity,"Debug"); break;
    case 3: strcpy(verbosity,"Panic"); break;
    }
#endif /*VERBOSENESS*/

    change_params((Widget)w,NULL,data_1,nematode?XtNumber(data_1):4);

#ifdef VERBOSENESS
    switch ( verbosity[0] ) {
    case 'p': case 'P':
	verbose = 3;
	break;
    case 'd': case 'D':
	verbose = 2;
	break;
    case 'y': case 'Y':
	verbose = 1;
	break;
    default:
	verbose = 0;
	break;
    }
#endif /*VERBOSENESS*/

    XtCallCallbacks(find, XtNcallback, (XtPointer) NULL);
}





static void changeParamsCallback(Widget w, XtPointer client_data, XtPointer call_data)
/*
 * Change oligo selection parameters
 */
{
#ifdef VERBOSENESS
    if (verbose_debug) message("Change selection parameters\n");
#endif /*VERBOSENESS*/
    osp_change_parameters(w,&prm,0/*params*/);

    XtCallCallbacks(find, XtNcallback, (XtPointer) NULL);
}





static void changeWeightsCallback(Widget w, XtPointer client_data, XtPointer call_data)
/*
 * Change oligo selection parameters
 */
{
#ifdef VERBOSENESS
    if (verbose_debug) message("Change selection weights\n");
#endif /*VERBOSENESS*/
    osp_change_parameters(w,&prm,1/*weights*/);

    XtCallCallbacks(find, XtNcallback, (XtPointer) NULL);
}






static void informationCallback(Widget w, XtPointer client_data, XtPointer call_data)
/*
 * Change oligo selection parameters
 */
{
#ifdef VERBOSENESS
    if (verbose_debug) message("Display information\n");
#endif /*VERBOSENESS*/
    messagef("%s\n",score_info);

}






static void findCallback(Widget w, XtPointer client_data, XtPointer call_data)
/*
 * Search for oligo
 */
{
#ifdef VERBOSENESS
    if (verbose_debug) message("Find oligos\n");
#endif /*VERBOSENESS*/

    oligo_sense = strand_state;

    (void) findOligos(thisxx,oligo_sense);

    /*
     * Reveal buttons depending on result
     */
    XawFormDoLayout(form,False);
    if (num_oligos>0) {
	XtManageChild(ok);
	XtManageChild(template);
    } else {
	XtUnmanageChild(ok);
	XtUnmanageChild(template);
    }
    if (num_oligos > 1)
	XtManageChild(next);
    else
	XtUnmanageChild(next);
    XawFormDoLayout(form,True);

    curr_oligo = 0;

    if (num_oligos>0)
	nextOligo(thisxx,curr_oligo,oligo_sense);
    else
	destroy_temporary_tag(thisxx);
}





static void nextCallback(Widget w, XtPointer client_data, XtPointer call_data)
/*
 * Search for oligo
 */
{
#ifdef VERBOSENESS
    if (verbose_debug) message("Select next oligo\n");
#endif /*VERBOSENESS*/
    curr_oligo++;
    if (curr_oligo+1 == num_oligos) {
	XawFormDoLayout(form,False);
	XtUnmanageChild(next);
	XawFormDoLayout(form,True);
    }

    nextOligo(thisxx,curr_oligo,oligo_sense);
}






static void okCallback(Widget w, XtPointer client_data, XtPointer call_data)
/*
 * Select current oligo,
 * Create a tag for it and everything else...
 */
{
    int i = curr_oligo;


    /*
     * To create a new oligo a valid template must have been specified
     */
    if (!template_index) {
	message("A valid template has not been specified\n");
	return;
    }
    if (oligo_sense == BACKWARDS) {
	(void) create_new_oligo_tag(thisxx,i,
				    r-OSP_RESULTS[i].end_position,
				    OSP_RESULTS[i].end_position-OSP_RESULTS[i].start_position+1,
				    oligo_sense);
    } else {
	(void) create_new_oligo_tag(thisxx,i,
				    l+OSP_RESULTS[i].start_position,
				    OSP_RESULTS[i].end_position-OSP_RESULTS[i].start_position+1,
				    oligo_sense);
    }

    up = 0;

    destroy_oligo_popup();

}





static void quitCallback(Widget w, XtPointer client_data, XtPointer call_data)
/*
 * Leave without selecting an oligo
 */
{
    up = 0;

    destroy_oligo_popup();
}





/*****************************************************************************************/

static void initialise()
/*
 * Initialise miscellaneous variables
 */
{
    char *subclones;

    /* clonelib = subclones:$SUBCLONES */
    if (is_file("subclones"))
	strcpy(clonelib,"subclones");
    else {
	subclones = (char *) getenv("SUBCLONES");
	if (subclones == NULL)
	    clonelib[0] = '\0';
	else if (is_file(subclones))
	    strcpy(clonelib,subclones);
	else
	    clonelib[0] = '\0';
    }
#ifdef NEMATODE
    nematode=(getenv("SUBCLONES")!=NULL);
#else /*NEMATODE*/
    nematode=1;
#endif /*NEMATODE*/


}



static void destroy_temporary_tag(EdStruct *xx)
/*
 * Flag temporary tag as deleted
 */
{
    if (DBgetTags(xx,0) != NULL) {
	_delete_tag(xx, 0/*consensus*/, DBgetTags(xx,0));
    }
}





static void create_temporary_tag(EdStruct *xx,int pos, int len)
/*
 * Create a temporary tag in the consensus to show position of oligo under consideration
 */
{
    char *defComment = "*** Temporary Annotation ***\n";

    tagStruct *tempTag;
    tempTag = DBgetTags(xx,0);

    /*
     * Create a new tag and insert it with comment into bimbo falix
     */
    if (tempTag == NULL) {
	tempTag = newTag();
	strncpy(tempTag->tagrec.type.c,"OLIG",4);
	tempTag->newcomment = (char *)malloc(strlen(defComment)+1);
	strcpy(tempTag->newcomment,defComment);
	tempTag->flags =
	    TAG_INSERTED |
            TAG_LENGTH_CHANGED |
	    TAG_POSITION_CHANGED |
	    TAG_TYPE_CHANGED |
	    TAG_COMMENT_IN_MEMORY;
	insertTag(xx,0/*consensus*/,tempTag);
    }

    tempTag->tagrec.position = pos;
    tempTag->tagrec.length = len;

    /*
     * Jiggle about if tag is off screen
     */
    if (xx->displayPos > pos ||	xx->displayPos + xx->displayWidth < pos + len ) 
	xx->displayPos = (pos+pos+len-xx->displayWidth)/2;

    redisplaySequences (xx,
			xx->namesWid,
			xx->sequencesWid,
			xx->displayPos,
			xx->displayWidth);

}




static int gel_ok(EdStruct *xx, int pos, int len, int seq)
/*
 * If gel overlaps region in contig,
 * return true
 */
{
    return (pos >= DBgetRelPos(xx,seq) && pos+len <= DBgetRelPos(xx,seq)+DBgetLength(xx,seq));
}





static int find_gel_for_oligo(EdStruct *xx, int pos, int len, int sense, int trySeq)
/*
 * Find a gel number for this oligo. Try
 * (A) gel trySeq first, (if valid)
 * (B) then any in the correct sense,
 * (C) otherwise any at all??
 * The position here is a position in the contig
 */
{
    int i;
#ifdef VERBOSENESS
    if (verbose_debug) messagef("Trying to find gel for oligo: pos=%d, len=%d, sense=%d\n",pos,len,sense);
#endif /*VERBOSENESS*/

    /**A**/
    if (trySeq > 0) {
#ifdef VERBOSENESS
	if (verbose_debug) messagef("find_gel_for_oligo: Trying gel %s (%d)...\n",DBgetName(xx,trySeq),trySeq);
#endif /*VERBOSENESS*/
	if ( gel_ok(xx,pos,len,trySeq) ) {
#ifdef VERBOSENESS
	    if (verbose_debug) messagef("Using gel %s (%d)...\n",DBgetName(xx,trySeq),trySeq);
#endif /*VERBOSENESS*/
	    return trySeq;
	}
    }

    /**B**/
#ifdef VERBOSENESS
    if (verbose_debug) message("find_gel_for_oligo: Trying gels in correct sense\n");
#endif /*VERBOSENESS*/
    for (i=1; i<=xx->DB_gelCount ; i++) {
	if (DBgetComp(xx,i) == COMPLEMENTED && sense == BACKWARDS ||
	    DBgetComp(xx,i) == UNCOMPLEMENTED && sense == FORWARDS) {
	    if (gel_ok(xx,pos,len,i)) {
#ifdef VERBOSENESS
		if (verbose_debug) messagef("Using gel %s (%d)...\n",DBgetName(xx,trySeq),trySeq);
#endif /*VERBOSENESS*/
		return i;
	    }
	}
    }

    /**C**/
#ifdef VERBOSENESS
    if (verbose_debug) message("find_gel_for_oligo: Trying any gel\n");
#endif /*VERBOSENESS*/
    for (i=1; i<=xx->DB_gelCount ; i++) {
	    if (gel_ok(xx,pos,len,i)) {
#ifdef VERBOSENESS
		if (verbose_debug) messagef("Using gel %s (%d)...\n",DBgetName(xx,trySeq),trySeq);
#endif /*VERBOSENESS*/
		return i;
	    }
    }

#ifdef VERBOSENESS
    if (verbose_debug) message("find_gel_for_oligo: Failed to find any suitable gel\n");
#endif /*VERBOSENESS*/
    return 0;

}	 





static char *generate_oligo_comment(int oligo)
{
    char s[200];
    char seq[100];
    char *c;

    int pos,len;
#ifdef VERBOSENESS
    if (verbose_debug) message("creating comment for oligo:\n");
#endif /*VERBOSENESS*/
    pos = OSP_RESULTS[oligo].start_position;
    len = OSP_RESULTS[oligo].end_position - pos + 1;
    strncpy(seq,&consensus[pos],len);
    seq[len]='\0';

    
    sprintf(s,"serial#=\ntemplate=%s\nsequence=%s\nflags=\n",template_name,seq);
#ifdef VERBOSENESS
    if (verbose_debug) messagef("(%s)\n",s);
#endif /*VERBOSENESS*/

    c = TAG_MALLOC(strlen(s)+1);
    strcpy(c,s);
    return c;
}





static int create_new_oligo_tag(EdStruct *xx, int oligo, int pos, int len, int sense)
/*
 * This routine creates a new oligo tag, prior to leaving the
 * oligo selection window
 */
{
    tagStruct *new_oligo;
    int seq;

    seq = find_gel_for_oligo(xx,pos,len,sense,template_index);

    if (! seq) {
	messagef("NO SUITABLE GEL FOR THIS OLIGO TAG POSITION %d LENGTH %d\n",pos,len);
	return 1;
    }

    /*
     * Create a new tag and insert it with comment into bimbo falix
     */
    new_oligo = newTag();
    strncpy(new_oligo->tagrec.type.c,"OLIG",4);
    new_oligo->tagrec.position = normalisePos(xx,seq,pos-DBgetRelPos(xx,seq)+1,len);
    new_oligo->tagrec.length = len;
    new_oligo->flags = TAG_INSERTED | TAG_LENGTH_CHANGED | TAG_POSITION_CHANGED | TAG_TYPE_CHANGED;
    insertTag(xx,seq,new_oligo);

    new_oligo->flags |= TAG_COMMENT_IN_MEMORY;
    new_oligo->newcomment = generate_oligo_comment(oligo);

    /*set modified tag flag */
    DBsetFlags(xx,seq,DBgetFlags(xx,seq)|DB_FLAG_TAG_MODIFIED);

    return 0;
}





int oligo_comp_pos(const void *pa, const void *pb)
/*
 * we need to sort the oligos according to how far they are from the
 * cursor position (p)
 */
{
    const OSP_Results *a = (OSP_Results *)pa;
    const OSP_Results *b = (OSP_Results *)pb;
#define absval(A) ( ( (A)<0 ) ? (-(A)) : (A) )
    return absval((a->start_position+a->end_position)/2 - p) >
	absval((b->start_position+b->end_position)/2 - p);

}







static int findOligos(EdStruct *xx, int sense)
/*
 * Find suitable oligos using OSP with current parameter settings.
 * Return ok status
 */
{
    int seq = xx->cursorSeq;
    int pos = xx->cursorPos;
    int position; /* in contig */
    int contigLength; /* length of contig */
    int consensusLength;
    int ok; /* return status from osp_analyse */
    int i;

#ifdef VERBOSENESS
    if (verbose_debug) message("Finding oligos...\n");
#endif /*VERBOSENESS*/

    position =  positionInContig(xx,seq,pos);
    contigLength = DBgetLength(xx,0/*consensus*/);




    /*
     * Conceptually we select off consensus.
     * Determine consensus around point...
     *    the oligo will be selected from this region
     */
    if (sense == FORWARDS) {
	l = (position > bkwd_width)
	    ? position - bkwd_width : 1;
	r = (position + fwd_width < contigLength)
	    ? position + fwd_width : contigLength;
    } else {
	l = (position > fwd_width)
	    ? position - fwd_width : 1;
	r = (position + bkwd_width < contigLength)
	    ? position + bkwd_width : contigLength;
    }
    p = bkwd_width; /* save position of cursor in consensus fragment */

    consensusLength = (r-l)+1;

    /* allocate space for consensus */
    if (consensus != NULL) free(consensus);
    consensus = (char *) malloc ( consensusLength );

    DBcalcConsensus (xx,l, consensusLength, consensus, BOTH_STRANDS);
    if (sense == BACKWARDS) {
	/* we need to complement the consensus */
	/*
	 * Use Rodger's routines in subs89.f
	 */
	sqcom_(consensus, &consensusLength, consensusLength);
	sqrev_(consensus, &consensusLength, consensusLength);
    }

#ifdef VERBOSENESS
    if (verbose_debug) messagef("Cursor position = %d\n",pos);
    if (verbose_debug) messagef("Seqence = %s (%d)\n",DBgetName(xx,seq),seq);
    if (verbose_debug) messagef("Consensus for region %d-%d = (%s)\n",l,r,consensus);
    if (verbose_debug) messagef("Sense = %s\n", (sense==FORWARDS)?"forward":"reverse");
#endif /*VERBOSENESS*/

    /*
     * A cludge to get around bug in osp_analyse
     */
    for (i=0;i<MAX_NUM_OLIGOS; i++) OSP_RESULTS[i].score = 0.0;

    ok=osp_analyse(OSP_RESULTS,consensus,&prm,screens,score_info);

#ifdef VERBOSENESS
    if (verbose) messagef("osp_analyse returned with status %d\n", ok);
#endif /*VERBOSENESS*/

    /*
     * Determine number of suitable oligos
     */
    if (ok)
	for (num_oligos=0; OSP_RESULTS[num_oligos].score>0.0; num_oligos++);
    else
	num_oligos=0;
#ifdef VERBOSENESS
    if (verbose) messagef("%d suitable oligos found\n",num_oligos);
#endif /*VERBOSENESS*/

    /* sort? */
    if (num_oligos>1) {
	/* yes - sort by position relative to cursor */
	qsort(OSP_RESULTS,num_oligos,sizeof(OSP_Results),oligo_comp_pos);
    }

    return (ok);
}





static int check_sense(EdStruct *xx, int i/*template number*/, int sense)
/*
 * templates that are in the wrong sense should fail
 * returns 0 - ok,   1 - wrong sense
 */
{
    return (DBgetComp(xx,i) == COMPLEMENTED && sense == FORWARDS ||
	    DBgetComp(xx,i) == UNCOMPLEMENTED && sense == BACKWARDS);
}




static int check_5prime(EdStruct *xx, int i/*template number*/, int sense, int pos, int len)
/*
 * templates that have their 5' end after the oligo position are not usable
 */
{
    int relpos = DBgetRelPos(xx,i);
    int length = DBgetLength(xx,i);

    if (sense == FORWARDS)
	return (relpos > pos);
    else
	return (relpos+length < pos + len);

}









static int check_template_suitability(EdStruct *xx, int i, int sense, int pos, int len)
/*
 *
 */
{
    int relpos = DBgetRelPos(xx,i);
    int length = DBgetLength(xx,i);
    int near_dist;
    CloneInfo info;
    char mtdname[4];

    /*
     * Get mtd name from gel reading name
     */
    strncpy(mtdname, DBgetGelName(xx,i), 3);
    mtdname[3] = '\0';

    /*
     * Get size information from subclones file
     */
    if (read_subclone_info(clonelib,mtdname,&info))
	near_dist = def_insert_size;
    else
	near_dist = info.range_from;


    /* reject ones that are not near the required interval */
    if (sense == FORWARDS)
	return (pos - relpos > (near_dist-ave_read_len));
    else
	return (relpos+length-pos-len > (near_dist-ave_read_len));

}









static int check_template_for_oligo(EdStruct *xx,int pos, int len, int sense, int i/*template no*/)
/*
 * The template number here is actually the number of an existing gel for
 * that template
 *
 * The template:
 *   1. must be in the appropriate sense.
 *   2. must exists in our template library.
 *   3. must be found "near" the interval required.
 *   4. need not have a sequenced gel over the interval required,
 *	but must be past the 5' end.
 */
{
#define reject_wrong_sense    1
#define reject_mapped_after   2
#define reject_not_close      3
#define reject_not_in_library 4

    /* reject ones that have a sense reverse to the one required */
    if (check_sense(xx,i,sense)) {
#ifdef VERBOSENESS
	if (verbose_panic) messagef(" %s rejected because it is in the wrong sense\n",DBgetName(xx,i));
#endif /*VERBOSENESS*/
	return reject_wrong_sense;
    }


    /* reject ones with 5' end after our oligo position */
    if (check_5prime(xx,i,sense,pos,len)){
#ifdef VERBOSENESS
	if (verbose_panic) messagef(" %s rejected because template starts after oligo primer\n",DBgetName(xx,i));
#endif /*VERBOSENESS*/
	return reject_mapped_after;
    }


    /*
     * Check suitability of template according to position
     */
    if (check_template_suitability(xx,i,sense,pos,len)){
#ifdef VERBOSENESS
	if (verbose_panic) messagef(" %s rejected because template isn't near oligo primer position\n",DBgetName(xx,i));
#endif /*VERBOSENESS*/
	return reject_not_close;
    }


    /*
     * check that template exists in our template library
     */
    if (0 /* ?? */) {
#ifdef VERBOSENESS
	if (verbose_panic) messagef(" %s rejected because template not in template library\n",DBgetName(xx,i));
#endif /*VERBOSENESS*/
	return reject_not_in_library;
    }

#ifdef VERBOSENESS
    if (verbose_debug) messagef(" %s selected\n",DBgetName(xx,i));
#endif /*VERBOSENESS*/

    return 0;
}







static int filter_template(char *template_name)
/*
 * We are interested in templates rather than simply template gel sequences
 *	Reject all but *.s1 *.f1 on this basis
 *	We are lucky in that we have a rigid nomenclature for templates
 *	Other people will do things differently.
 */
{
    char *error_message;

    error_message = (char *) re_comp(filter);
    if ( error_message != NULL ) {
	messagef("  Error using re_comp: %s\n",error_message);
	return 0;
    }
    return (re_exec(template_name) == 1);
}








static int *find_templates_for_oligo (EdStruct *xx, int pos, int len, int sense)
/*
 * Once an oligo has been found for the consensus at position pos, length len,
 * search for a suitable template within the contig.
 *
 * Use existing gel readings as a basis to find a suitable template.
 * As there can be several readings from one template, filter out only the
 * initial reads for consideration
 */
{
    static int *templateList = NULL;
    int i;
    int count;

    if (templateList == NULL)
	templateList = (int *) malloc ( xx->DB_gelCount * sizeof(int));

#ifdef VERBOSENESS
    if (verbose_debug) message("Finding template for oligo:\n");
    if (verbose_debug) messagef("position = %d, length = %d, forward sense=%d\n",pos,len,sense);
#endif /*VERBOSENESS*/

    count = 0;
    if (sense==BACKWARDS) {
	for(i=1;i<=xx->DB_gelCount;i++) {
	    char *name;
	    name = DBgetGelName(xx,i);

	    /* only nematode extensions */
	    if (nematode && ! filter_template(name)) {
#ifdef VERBOSENESS
		if (verbose_debug) messagef(" %s rejected because template doesn't match filter\n",DBgetName(xx,i));
#endif /*VERBOSENESS*/
		continue;
	    }
	    
	    
	    if (! check_template_for_oligo(xx,pos,len,sense,i))
		templateList[count++] = i;
	    
	}
    } else {
	int ind;
	ind=posToIndex(xx,pos); /* optimise a bit */
	if(!ind) ind=xx->DB_gelCount;
	for(;ind>0;ind--) {
	    char *name;
	    i = xx->DBorder[ind];
	    name = DBgetGelName(xx,i);
	    
	    if (nematode && ! filter_template(name)) {
#ifdef VERBOSENESS
		if (verbose_debug) messagef(" %s rejected because template doesn't match filter\n",DBgetName(xx,i));
#endif /*VERBOSENESS*/
		continue;
	    }
	    
	    
	    if (! check_template_for_oligo(xx,pos,len,sense,i))
		templateList[count++] = i;
	    
	}
    }
    
    templateList[count] = 0;

    return templateList;
}







#ifdef OBSELETE
static void trim_suffix(char * name)
/*
 * Remove tail from first dot onwards
 */
{
    char *suffix;

    /* truncate at suffix */
    suffix = strchr(name,'.');
    if (suffix != NULL)
	*suffix = '\0';
}
#endif /*OBSELETE*/







static void get_template_name(char *name, EdStruct *xx, int i)
/*
 * Get the template name for gel number 'i'
 */
{
    (void) strcpy(name, DBgetGelName(xx,i) );
#ifdef OBSELETE
    trim_suffix(name);
#endif /*OBSELETE*/
}








#ifdef OBSELETE
static int score_reading_quality(EdStruct *xx, int seq)
/*
 * Returns a score for the quality of the read, higher the better.
 * Ideally it would look at the traces, but for now
 * will assume length of read is a good meter for this
 */
{
    return DBgetLength(xx,seq);
}
#endif /*OBSELETE*/



static int score_template(EdStruct *xx, int seq)
/*
 * Score this template
 */
{
#ifdef OBSELETE
    return score_reading_quality(xx,seq);
#else /*OBSELETE*/
    return 1; /* this should force the first one to be chosen */
#endif /*OBSELETE*/
}







static void set_default_template(EdStruct *xx, int *templateList)
/*
 * Pick a default template from the list of available templates
 */
{
    template_index = 0;
    template_name[0] = '\0';
    if (templateList[0]) {
	int i;
	int score, high_score;
	high_score = 0;
	/* search */
	for (i=0; templateList[i]; i++) {
	    score = score_template(xx, templateList[i]);
	    if (high_score < score) {
		high_score = score;
		template_index = templateList[i];
	    }
	}
	/* set template name */
	get_template_name(template_name, xx, template_index);
    }

}





#ifdef OBSELETE
static int strcmp_ignorecase(char *a, char *b)
{
    for ( ; tolower(*a) == tolower(*b); a++, b++)
	if (*a == '\0') return 0;
    return tolower(*a) - tolower(*b);
}
#endif



static int strncmp_ignorecase(char *a, char *b, int n)
{
    for ( ; n && tolower(*a) == tolower(*b); a++, b++, n--)
	if (*a == '\0') return 0;
    if (!n)
	return 0;
    else
	return tolower(*a) - tolower(*b);
}





static void check_template_name(EdStruct *xx, char *template_name, int pos, int len, int sense)
/*
 * Check the template name is valid
 *   + that it exists
 *   + that is in the correct sense etc etc
 * If it's not valid, set template_index to be 0
 */
{
    int i;
    int found_index;
    int template_len;
    int found;
    char this[DB_NAMELEN];

    strcpy(this,template_name);
#ifdef OBSELETE
    trim_suffix(this);
#endif /*OBSELETE*/

    /*
     * Check template_name exists
     */
    found = 0;
    template_len= strlen(template_name);

    for (i=1; i<=xx->DB_gelCount ; i++) {
	char *name;

	name = DBgetGelName(xx,i);

	if (nematode && ! filter_template(name))
	    continue;

	if (strncmp_ignorecase(template_name, DBgetGelName(xx,i), template_len) == 0) {
	    found++;
	    found_index = i;
#ifdef VERBOSENESS
	    if (verbose_debug) messagef("%s matches %s\n",template_name,DBgetName(xx,i));
#endif /*VERBOSENESS*/
	}

    }


    template_index = 0;

    if (! found)
	messagef("template %s not found\n", template_name);
    else {
	if (found > 1) {
#ifdef VERBOSENESS
	    if (verbose_debug) messagef("template %s found, but is not unique\n",
					template_name);
#endif /*VERBOSENESS*/
	} else {
	    if (check_sense(xx, found_index, sense)) {
		messagef("template %s in the wrong sense\n", template_name);
		return;
	    }

	    if (check_5prime(xx,found_index,sense,pos,len)) {
		messagef("template %s starts after oligo position\n", template_name);
		return;
	    }

	    template_index = found_index;
	}
    }


}







/*
 * Move data[] outside scope of following function - non ANSI to perform
 * aggregate definitions inside a function
 */
Field_entry data_2[] = {
    {"Template",  (char *)template_name,  t_char,  sizeof(template_name)},
};

static void menuSelectCallback(Widget w, XtPointer p_i, XtPointer junk)
/*
 * A template has been selected off the menu
 * Deal with it
 */
{
    int i = (int) p_i;

    /*
     * i is either -1 (other) or the gel number corresponding to a template
     */
    if (i<0) {
#ifdef VERBOSENESS
	if (verbose_debug) message("Menu: Other selected\n");
#endif /*VERBOSENESS*/

	template_name[0]='\0';

	change_params((Widget)oligoWid,"Please specify...",data_2,
		      XtNumber(data_2));
	template_index = 0;
	if (oligo_sense == BACKWARDS) {
	    check_template_name(thisxx, template_name,
				r-OSP_RESULTS[curr_oligo].end_position,
				OSP_RESULTS[curr_oligo].end_position-OSP_RESULTS[curr_oligo].start_position+1,
				oligo_sense);
	} else {
	    check_template_name(thisxx, template_name,
				l+OSP_RESULTS[curr_oligo].start_position,
				OSP_RESULTS[curr_oligo].end_position-OSP_RESULTS[curr_oligo].start_position+1,
				oligo_sense);
	}

    } else {
	template_index = i;
	get_template_name(template_name,thisxx,i);
#ifdef VERBOSENESS
	if (verbose_debug) messagef("Menu: %s selected\n", template_name);
#endif /*VERBOSENESS*/
    }

    display_template_details();

}






static void create_template_menu(EdStruct *xx, int *list)
/*
 * Create new list of available templates
 */
{
    static Widget menuWid = NULL;
    int i;
    Widget entryWid;
    Arg args[2];
    Cardinal nargs;

    if (menuWid != NULL)
	XtDestroyWidget(menuWid);


    /*
     * Create the menu parent widget
     */
    menuWid = XtCreatePopupShell("templateMenu", simpleMenuWidgetClass, template,
				 NULL, 0);

    /*
        Put the individual items in.
	When selected, each entry will generate a callback with
	its associated number.
    */
    for (i = 0; list[i]; i++)
    {
	char name[DB_NAMELEN];

#ifdef VERBOSENESS
	if (verbose_debug) messagef("Creating menu for %s\n",DBgetName(xx,list[i]));
#endif /*VERBOSENESS*/

	/*
	 * Prepare clone name
	 */
	get_template_name(name, xx, list[i]);

	nargs = 0;
	XtSetArg(args[nargs], XtNlabel, name); nargs++;
	entryWid = XtCreateManagedWidget("entry", smeBSBObjectClass,
					 menuWid, args, nargs);
	XtAddCallback(entryWid, XtNcallback, menuSelectCallback,
		      (XtPointer) list[i]);
    }
    nargs = 0;
    XtSetArg(args[nargs], XtNlabel, "Other"); nargs++;
    entryWid = XtCreateManagedWidget("entry", smeBSBObjectClass,
				     menuWid, args, nargs);
    XtAddCallback(entryWid, XtNcallback, menuSelectCallback,
		  (XtPointer) -1);

}






static void display_template_details()
/*
 * Display importand template details
 */
{
    if (template_index > 0) {
	messagef("Template: %s\n", template_name);
    } else {
	message("Template: none chosen\n");
    }

}





static void display_oligo_details(EdStruct *xx, int sense, int oligo)
/*
 * Display important oligo details
 *   a. sequence
 *   b. position, length
 *   c. osp score, gc, tm
 */
{
    char seq[100];
    int pos,len;
    int i;
    char *a;
    int consensus_start, consensus_end;

    pos = OSP_RESULTS[oligo].start_position;
    len = OSP_RESULTS[oligo].end_position - pos + 1;

    /*
     * Get consensus sequence with context
     */
    a = seq;
    for (i=3;i;i--) *a++ = '.';
    for (i=(pos>2)?2:pos;i;i--)
	*a++ = tolower(consensus[pos-i]);
    strncpy(a,&consensus[pos],len); a+=len;
    for (i=0;i<2&&consensus[pos+len+i];i++)
	*a++ = tolower(consensus[pos+len+i]);
    for (i=3;i;i--) *a++ = '.';
    *a++ = '\0';


    /*
     * Positions in contig
     */
    if (sense == BACKWARDS) {
	consensus_start = r-OSP_RESULTS[oligo].end_position;
	consensus_end   = consensus_start + len - 1;
    } else {
	consensus_start = l+OSP_RESULTS[oligo].start_position;
	consensus_end   = consensus_start + len - 1;
    }

    messagef("Oligo: %s\n",seq);
    messagef("\
Primer # %2d                                      PRIMER-SELF      PRIMER-OTHER\n\
5' end   3' end    length  Score G+C(%%)  Tm      3'  Internal     3'   Internal\n\
%6d   %6d      %4d    %4.1f  %4.1f  %4.1f    %4.1f    %4.1f     %4.1f    %4.1f\n",
	    oligo+1,
	    consensus_start,
	    consensus_end,
	    len,
	    OSP_RESULTS[oligo].score,
	    OSP_RESULTS[oligo].gc * 100.0,
	    OSP_RESULTS[oligo].tm,
	    OSP_RESULTS[oligo].psI_score,
	    OSP_RESULTS[oligo].ps3_score,
	    OSP_RESULTS[oligo].poI_score,
	    OSP_RESULTS[oligo].po3_score);

}






static void nextOligo(EdStruct *xx, int oligo, int sense)
/*
 * We cycle through the oligo list
 * curr-oligo gives the current oligo entry under consideration
 */
{
    /*
     * Hilight position of next oligo in contig editor temporarily
     */
    int *templateList;
    int i = oligo;

    /*
     * Print out information on oligos
     */
#ifdef VERBOSENESS
    if (verbose_debug) messagef("************** %d ************\n",i);
    if (verbose_debug) messagef("stp %d, endp %d, score %f,  gc %f, tm %f, psI %f ps3 %f poI  %f po3 %f\n",
	    OSP_RESULTS[i].start_position,
	    OSP_RESULTS[i].end_position,
	    OSP_RESULTS[i].score,
	    OSP_RESULTS[i].gc,
	    OSP_RESULTS[i].tm,
	    OSP_RESULTS[i].psI_score,
	    OSP_RESULTS[i].ps3_score,
	    OSP_RESULTS[i].poI_score,
	    OSP_RESULTS[i].po3_score);
#endif /*VERBOSENESS*/
    display_oligo_details(xx,sense,i);

    if (sense == BACKWARDS) {
	/*
	 * Convert position returned from oligo selection to
	 * position in contig
	 */
	templateList =
	    find_templates_for_oligo(xx, 
				    r-OSP_RESULTS[i].end_position,
				    OSP_RESULTS[i].end_position-OSP_RESULTS[i].start_position+1,
				    sense);
	create_temporary_tag(xx,
			     r-OSP_RESULTS[i].end_position,
			     OSP_RESULTS[i].end_position-OSP_RESULTS[i].start_position+1);
    } else {
	/*
	 * Convert position returned from oligo selection to
	 * position in contig
	 */
	templateList =
	    find_templates_for_oligo(xx, 
				    l+OSP_RESULTS[i].start_position,
				    OSP_RESULTS[i].end_position-OSP_RESULTS[i].start_position+1,
				    sense);
	create_temporary_tag(xx,
			     l+OSP_RESULTS[i].start_position,
			     OSP_RESULTS[i].end_position-OSP_RESULTS[i].start_position+1);
    }


    set_default_template(xx,templateList);
    create_template_menu(xx, templateList);

    display_template_details();

}


/*****************************************************************************************/
/*
 * External routines
 */



void createOligoWidget(Widget parentWid)
/*
 * Prtend to create it now
 */
{
    oldFogieWid = parentWid;
}





static void create_oligo_wid(Widget parentWid)
/*
 * Create the window for oligo selection
 * This routine should be called just once, in the initialisation phase of xdap
 */
{
    Cardinal nargs;
    Arg args[10];
    Position	x, y; 	 /* top-left hand corner of new widget */
    Dimension	height;  /* height of parent widget */

    /*
     * Determine the position on the screen for this widget
     */
#define fromVertWid (thisxx->edWid)
    nargs = 0;
    XtSetArg(args[nargs], XtNheight, &height); nargs++;
    XtGetValues(fromVertWid, args, nargs);
    XtTranslateCoords(fromVertWid, (Position) 0, (Position) height, &x, &y);

    /*
     * Create popup shell
     */
    nargs = 0;
    XtSetArg(args[nargs], XtNx, x); nargs++;
    XtSetArg(args[nargs], XtNy, y); nargs++;
    oligoWid = XtCreatePopupShell("oligo", transientShellWidgetClass, parentWid, args, nargs);

    /*
     * Create main form
     */
    nargs = 0;
    form = XtCreateManagedWidget("form", formWidgetClass, oligoWid, args, nargs);

    /*
     * Create title for form
     */
    nargs = 0;
    XtSetArg(args[nargs], XtNborderWidth, 0); nargs++;
    XtSetArg(args[nargs], XtNlabel, "Select Oligos and Templates"); nargs++;
    label = XtCreateManagedWidget("label", labelWidgetClass, form, args, nargs);

#define XtOrientHorizontal "horizontal"

    /*
     * Create buttons for oligo sense
     */
    nargs = 0;
    XtSetArg(args[nargs], XtNfromVert, label); nargs++;
    XtSetArg(args[nargs], XtNborderWidth, 0); nargs++;
    XtSetArg(args[nargs], XtNorientation, XtOrientHorizontal); nargs++;
    bbox = XtCreateManagedWidget("bbox", boxWidgetClass, form, args, nargs);
    nargs = 0;
    XtSetArg(args[nargs], XtNborderWidth, 0); nargs++;
    XtSetArg(args[nargs], XtNlabel, "Direction:"); nargs++;
    (void) XtCreateManagedWidget("label", labelWidgetClass, bbox, args, nargs);
    nargs = 0;
    strand = XtCreateManagedWidget("strand", commandWidgetClass,bbox,args,nargs);
    XtAddCallback(strand, XtNcallback, strandCallback, (XtPointer) NULL);
    set_strand_state(strand,strand_state);


    /*
     * Create Change Parameter Buttons
     */
    nargs = 0;
    XtSetArg(args[nargs], XtNfromVert, bbox); nargs++;
    XtSetArg(args[nargs], XtNborderWidth, 0); nargs++;
    XtSetArg(args[nargs], XtNorientation, XtOrientHorizontal); nargs++;
    cbox = XtCreateManagedWidget("cbox", boxWidgetClass, form, args, nargs);
    nargs = 0;
    XtSetArg(args[nargs], XtNlabel, "Parameters"); nargs++;
    change = XtCreateManagedWidget("change", commandWidgetClass, cbox, args, nargs);
    XtAddCallback(change, XtNcallback, changeMineCallback, (XtPointer) NULL);
    nargs = 0;
    XtSetArg(args[nargs], XtNlabel, "Oligo Selection Parameters"); nargs++;
    change = XtCreateManagedWidget("change", commandWidgetClass, cbox, args, nargs);
    XtAddCallback(change, XtNcallback, changeParamsCallback, (XtPointer) NULL);
    nargs = 0;
    XtSetArg(args[nargs], XtNlabel, "Oligo Selection Weights"); nargs++;
    change = XtCreateManagedWidget("change", commandWidgetClass, cbox, args, nargs);
    XtAddCallback(change, XtNcallback, changeWeightsCallback, (XtPointer) NULL);
    nargs = 0;
    XtSetArg(args[nargs], XtNlabel, "Display Selection Information"); nargs++;
    change = XtCreateManagedWidget("information", commandWidgetClass, cbox, args, nargs);
    XtAddCallback(change, XtNcallback, informationCallback, (XtPointer) NULL);

    /*
     * Create action button
     */
    nargs = 0;
    XtSetArg(args[nargs], XtNfromVert, cbox); nargs++;
    XtSetArg(args[nargs], XtNborderWidth, 0); nargs++;
    XtSetArg(args[nargs], XtNorientation, XtOrientHorizontal); nargs++;
    XtSetArg(args[nargs], XtNresizable, True); nargs++;
    dbox = XtCreateManagedWidget("dbox", boxWidgetClass, form, args, nargs);
    nargs = 0;
    XtSetArg(args[nargs], XtNlabel, "Find Oligos"); nargs++;
    find = XtCreateManagedWidget("find", commandWidgetClass, dbox, args, nargs);
    XtAddCallback(find, XtNcallback, findCallback, (XtPointer) NULL);
    nargs = 0;
    XtSetArg(args[nargs], XtNlabel, "Select Next"); nargs++;
    next = XtCreateManagedWidget("next", commandWidgetClass, dbox, args, nargs);
    XtAddCallback(next, XtNcallback, nextCallback, (XtPointer) NULL);
    nargs = 0;
    XtSetArg(args[nargs], XtNlabel, "Choose Template for this Oligo"); nargs++;
    XtSetArg(args[nargs], XtNmenuName, "templateMenu"); nargs++;
    template = XtCreateManagedWidget("template", menuButtonWidgetClass, dbox, args, nargs);


    /*
     * Exit action
     */
    nargs = 0;
    XtSetArg(args[nargs], XtNfromVert, dbox); nargs++;
    XtSetArg(args[nargs], XtNborderWidth, 0); nargs++;
    XtSetArg(args[nargs], XtNorientation, XtOrientHorizontal); nargs++;
    XtSetArg(args[nargs], XtNresizable, True); nargs++;
    ebox = XtCreateManagedWidget("ebox", boxWidgetClass, form, args, nargs);
    nargs = 0;
    XtSetArg(args[nargs], XtNlabel, "Create Tag for this Oligo"); nargs++;
    ok = XtCreateManagedWidget("ok", commandWidgetClass, ebox, args, nargs);
    XtAddCallback(ok, XtNcallback, okCallback, (XtPointer) NULL);
    nargs = 0;
    XtSetArg(args[nargs], XtNlabel, "Quit"); nargs++;
    quit = XtCreateManagedWidget("quit", commandWidgetClass, ebox, args, nargs);
    XtAddCallback(quit, XtNcallback, quitCallback, (XtPointer) NULL);
}





int invokeOligo(EdStruct *xx)
/*
 * Pop up the oligo selection window
 */
{
    if (up) return 0;
    thisxx = xx;

    if (oligoWid == NULL) {
	initialise();
	osp_initialise();
	create_oligo_wid(oldFogieWid);
    }

    XtUnmanageChild(ok);
    XtUnmanageChild(next);
    XtUnmanageChild(template);

    XtPopup(oligoWid,   XtGrabNone);

    up = 1;

    /* find oligos */
    XtCallCallbacks(find, XtNcallback, (XtPointer) NULL);

    return 0;

}







int destroyOligo()
/*
 * Shut this baby down
 */
{
    if (up)
	XtCallCallbacks(quit, XtNcallback, (XtPointer) NULL);

    return 0;
}





static void destroy_oligo_popup()
/*
 * Popdown the oligo popup window,
 * free a few variables
 * then relax
 */
{
    EdStruct *xx = thisxx;

    XtPopdown(oligoWid);

    destroy_temporary_tag(xx);
    free (consensus);
    consensus = NULL;

    redisplaySequences (xx,
			xx->namesWid,
			xx->sequencesWid,
			xx->displayPos,
			xx->displayWidth);
}


