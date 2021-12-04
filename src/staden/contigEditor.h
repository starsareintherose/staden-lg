#ifndef _contigEditor_h
#define _contigEditor_h
#include <X11/Intrinsic.h>
#include "fort.h"
extern Widget disagreeWid;
extern float  pcCut;           /* cut off used for consensus */

typedef struct _def_colours {
    Pixel lightGrey;
} DefColours;
extern DefColours defColours;

#define EDITMODE 1
#define JOINMODE 2
typedef struct {
        int_f *idevr;    /* unit number for relationships */
        int_f *idevw;    /* unit number for working versions of sequences */
        int_f *idevn;    /* unit number for sequence names */
	int_f *idevt;    /* unit number for tag information */
	int_f *idevc;    /* unit number for comment file */
        int_f *relpg;    /* relative positions of gels in sequences */
        int_f *lngthg;   /* lengths of sequences */
        int_f *lnbr;     /* left neighbours */
        int_f *rnbr;     /* right neighbours */
        int_f *maxgel;   /* maximum length of gel */
        int_f *idbsiz;   /* size of database */
        int_f *lincon;   /* current contig to edit */
        int_f *llino;    /* left-most gel in contig */
        int_f *lnconl;   /* left contig for join */
        int_f *llinol;   /* left-most gel in left contig for join */
        int_f *lnconr;   /* right contig fro join */
        int_f *llinor;   /* left-most gel in right contig for join */
        int_f *perced;   /* cutoff for consensus calculation */
        int_f *ngels;    /* number of gels in database */
        int_f *nconts;   /* number of contigs in database */
	int_f *idm;      /* database type */
    } SaveStruct;
extern SaveStruct saveState;

extern int editorMode;

extern int editModeIsInsert();
extern int editModeIsSuperman();
extern int inJoinMode();
extern int editorLocked();
extern int editorLockedPos(int force);
#endif  /* _contigEditor_h */
