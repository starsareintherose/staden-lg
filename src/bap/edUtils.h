#include <X11/Intrinsic.h>
#include "tagUtils.h"
#include "fort.h"

#ifndef _edUtils_h
#define _edUtils_h

/*
** constant definitions
*/
#define MAX_DISPLAY_WIDTH 200
#define DEFAULT_DISPLAY_WIDTH 80
#define DB_NAMELEN 16  /* size of records in AR file */
#define DB_GELNOLEN 5
#define NAMELEN (DB_NAMELEN + DB_GELNOLEN + 1)
#define BASES 6

/*
** Useful distances
** (treat as symbolic rather than actual distances)
*/
#define D_screen     80
#define D_halfScreen 40
#define D_character   1

/*
** Flags for the internal database
*/
/* for whole database */
#define DB_ACCESS            (1L<<0)
#define DB_DATA_TYPE         (1L<<1)
#define DB_STORAGE           (1L<<2)
#define DB_ACCESS_READONLY   (0L)
#define DB_ACCESS_UPDATE     (DB_ACCESS)
#define DB_DATA_TYPE_DNA     (0L)
#define DB_DATA_TYPE_PROTEIN (DB_DATA_TYPE)
#define DB_STORAGE_DISK      (0L)
#define DB_STORAGE_INTERNAL  (DB_STORAGE)
#define DB_DELAYED_READ      (0L)
/* for each sequence */
#define DB_FLAG_NONE             (0L)
#define DB_FLAG_IN_MEMORY        (1L<<0)
#define DB_FLAG_SEQ_MODIFIED     (1L<<1)
#define DB_FLAG_REL_MODIFIED     (1L<<2)
#define DB_FLAG_TAG_MODIFIED     (1L<<3)
#define DB_FLAG_SELECTED         (1L<<4)
#define DB_FLAG_TAG_IN_MEMORY    (1L<<5)
#define DB_FLAG_SEQ_IN_MEMORY    (1L<<0)
#define DB_FLAG_NAME_IN_MEMORY   (1L<<6)
/*
** type definitions
*/
typedef struct {
        int relPos;
        int length;
        int number;
        int complemented;
        char *name;
        char *sequence;
	long flags;
	tagStruct *tagList;
        } DBStruct, *DBptr;


/* REMEMBER TO: update structure initialisation in edUtils.c */
typedef struct {
        int oldMax;
	long DB_flags;
        int DB_gelCount;
        int DB_contigNum;
        int *DBlist;
        int *DBorder;
        DBStruct *DB;
        int displayPos ;
        int displayWidth;
        int displayHeight;
        int cursorPos;
        int cursorSeq;
        int rulerDisplayed;
        int consensusDisplayed;
        int fontWidth;
        int fontHeight;
	Widget edWid;
        Widget namesWid;
	Widget sequencesWid;
	Widget scrollButtonsWid;
	Widget sliderWid;
        char displayedConsensus[ MAX_DISPLAY_WIDTH ];
	int select_made;
	int select_seq;
	int select_start_pos;
	int select_end_pos;
	tagStruct *select_tag;
	int reveal_cutoffs;
	int showDifferences;
        } EdStruct, *EdStructPtr;
/* REMEMBER TO: update structure initialisation in edUtils.c */


/*
** Useful macros concerning internal database
*/
#define DBgetRelPos(X,A)   ((X)->DB[(A)].relPos)
#define DBgetLength(X,A)   ((X)->DB[(A)].length)
#define DBgetNumber(X,A)   ((X)->DB[(A)].number)
#define DBgetComp(X,A)     ((X)->DB[(A)].complemented)
#define DBgetFlags(X,A)    ((X)->DB[(A)].flags)
extern char *DBgetSeq();
extern tagStruct *DBgetTags();
extern char *DBgetName();

/****************/
#define DB_RelPos(X,A)   ((X)->DB[(A)].relPos)
#define DB_Length(X,A)   ((X)->DB[(A)].length)
#define DB_Number(X,A)   ((X)->DB[(A)].number)
#define DB_Comp(X,A)     ((X)->DB[(A)].complemented)
#define DB_Name(X,A)     ((X)->DB[(A)].name)
#define DB_Seq(X,A)      ((X)->DB[(A)].sequence)
#define DB_Flags(X,A)    ((X)->DB[(A)].flags)
#define DB_Tags(X,A)     ((X)->DB[(A)].tagList)
/****************/
#define COMPLEMENTED -1
#define BOTH_STRANDS 0
#define UNCOMPLEMENTED 1
#define DBgetGelName(xx,i) ( &( DBgetName(xx,i) )[DB_GELNOLEN+1] )

#define DBsetRelPos(X,A,B) (X)->DB[(A)].relPos = (B)
#define DBsetLength(X,A,B) (X)->DB[(A)].length = (B)
#define DBsetNumber(X,A,B) (X)->DB[(A)].number = (B)
#define DBsetComp(X,A,B)   (X)->DB[(A)].complemented = (B)
#define DBsetName(X,A,B)   (X)->DB[(A)].name = (B)
#define DBsetSeq(X,A,B)    (X)->DB[(A)].sequence = (B)
#define DBsetFlags(X,A,B)  (X)->DB[(A)].flags = (B)
#define DBsetTags(X,A,B)   (X)->DB[(A)].tagList = (B)

/*
** Useful macros
*/
#define normalisePos(X,S,P,L) \
    ( (DBgetComp((X),(S))==UNCOMPLEMENTED) ? (P) : (DBgetLength((X),(S)) - (P) - (L) + 2) )


/*
** External definitions
*/
extern int initialiseDB(
	EdStruct *xx,
        int_f *idevr,    /* unit number for relationships */
	int_f *idevw,	/* unit number for working versions of sequences */
	int_f *idevn,	/* unit number for sequence names */
	int_f *relpg,	/* relative positions of gels in sequences */
	int_f *lngthg,	/* lengths of sequences */
	int_f *lnbr,	/* left neighbours */
	int_f *rnbr,	/* right neighbours */
	int_f *maxgel,	/* maximum length of gel */
	int_f *idbsiz,	/* size of database */
	int_f *llino	/* left-most gel in contig */
	);

extern void saveDB(
	EdStruct *xx,
        int_f *idevr,    /* unit number for relationships */
        int_f *idevw,    /* unit number for working versions of sequences */
        int_f *idevn,    /* unit number for sequence names */
        int_f *relpg,    /* relative positions of gels in sequences */
        int_f *lngthg,   /* lengths of sequences */
        int_f *lnbr,     /* left neighbours */
        int_f *rnbr,     /* right neighbours */
        int_f *maxgel    /* maximum length of gel */
    );

extern void joinDB(
        int_f *idevr,    /* unit number for relationships */
        int_f *idevw,    /* unit number for working versions of sequences */
        int_f *idevn,    /* unit number for sequence names */
        int_f *relpg,    /* relative positions of gels in sequences */
        int_f *lngthg,   /* lengths of sequences */
        int_f *lnbr,     /* left neighbours */
        int_f *rnbr,     /* right neighbours */
        int_f *maxgel,   /* maximum length of gel */
        int_f *llinol,
        int_f *lnconl,
        int_f *llinor,
        int_f *lnconr,
        int_f *ngels,
        int_f *nconts,
        int_f *idbsiz
    );


extern void freeDB(EdStruct *xx);
extern void countDisagreements(int *overlapLength, int *wingeCount);

extern int  createEdDisplay(EdStruct *xx,Widget namesWid, Widget sequencesWid,int seq,int pos);

extern void incDisplayPos(EdStruct *xx, int distance);
extern void decDisplayPos(EdStruct *xx, int distance);
extern void setDisplayPos(EdStruct *xx, int pos);
extern void setDisplayPosPercent(EdStruct *xx, float percent);

extern int posToIndex(EdStruct *xx, int pos);

extern void undoLastCommand();
extern void saveDatabase(EdStruct *xx);

extern void caretRight(Widget, XEvent *, String *, Cardinal *);
extern void caretLeft(Widget, XEvent *, String *, Cardinal *);
extern void caretDown(Widget, XEvent *, String *, Cardinal *);
extern void caretUp(Widget, XEvent *, String *, Cardinal *);
extern void deleteKey(Widget, XEvent *, String *, Cardinal *);
extern void keyPress(Widget, XEvent *, String *, Cardinal *);
extern void buttonDown(Widget, XEvent *, String *, Cardinal *);
extern void invokeTrace(Widget, XEvent *, String *, Cardinal *);
extern void selectRead(Widget, XEvent *, String *, Cardinal *);

extern EdStructPtr intToEdStruct(int i);
extern EdStructPtr widgetToEdStruct(Widget w);
extern EdStructPtr getFreeEdStruct();

extern void DBgetSequence(EdStruct *xx, int seq, int pos, int width, char *str);
/*
** get part of a sequence from its `pos' base for `width' bases
** Bases number from 0?
*/
extern int *sequencesInRegion(EdStruct *xx,int pos, int width);
/*
** Return a pointer to list of sequences in region of contig
*/
extern int *sequencesOnScreen(EdStruct *xx,int pos, int width);
/*
** Return a pointer to list of sequences on screen
*/
extern int positionInContig(EdStruct *xx, int seq, int pos);
/*
** returns relative position in a sequence as an 
** absolute position in the contig
*/
extern void DBcalcConsensus (EdStruct *xx,int pos, int width, char *str, int strand);
/*
** calculate the consensus for position `pos' in contig,
** for `width' characters. Take into accound readings on `strand' only:
**
*/
extern void redisplaySequences (EdStruct *xx, Widget namesWid, Widget sequencesWid, int pos, int width);
/*
** Redisplay the whole sequence display
*/
extern void redisplayWithCursor(EdStruct *xx);
/*
** Redisplay screen, ensuring cursor display
*/
void calculateConsensusLength(EdStruct *xx);
/*
** Calculate dynamic consensus length
*/

#endif /* _edUtils_h */
