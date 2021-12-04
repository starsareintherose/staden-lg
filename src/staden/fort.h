#ifndef _FORT_H
#define _FORT_H

/*
 * When passing INTEGERs from Fortran to C we need to know the type they'll be
 * at the C end. We could define int_f as 'int *' (or 'long *') but this may
 * cause obscureness and unreadability of C.
 *
 * Also we define the extra argument given to C by fortran for the length of
 * an array passed. This may not necessarily be the same as int_f (either
 * now or the future). So we define it as a separate type (int_fl).
 */

typedef int int_f;
typedef int int_fl;

/*
 * Prototypes for Fortran functions. This helps to establish when we pass the
 * wrong arguments to a function.
 *
 * Terminology:
 * name_a : name is an array
 * name_s : name is a string
 * name_l : name is the length of items in array name_a or length of string
 *          name_s
 * name   : name is simple type
 */

/* dbsyscommon.f */
void  readw_ (int_f *idevw,   int_f *n,       char *gel_a,    int_f *maxgel,
	      int_fl gel_l); /* gel_l == *maxgel */
void  writew_(int_f *idevw,   int_f *n,       char *gel_a,    int_f *maxgel,
              int_fl gel_l);

void  readn_ (int_f *idevn,   int_f *n,       char *name_s,   int_fl name_l);
             /* name_l == DB_NAMELEN */
void  writen_(int_f *idevn,   int_f *n,       char *name_s,   int_fl name_l);

void  readr_ (int_f *idev_r,  int_f *n,       int_f *relpg,   int_f *lngthg,
 	      int_f *lnbr,    int_f *rnbr);
void  writer_(int_f *idev_r,  int_f *n,       int_f *relpg,   int_f *lngthg,
 	      int_f *lnbr,    int_f *rnbr);

void  pcon1_ (char  *chr,     int_f *chrsum);

int_f indexs_(char  *c,       int_f *s);

void  munotp_(char  *ret_s,   int_fl ret_l,   int_f *ip);

void  gtconc_(char  *ret_s,   int_fl rel_l,   int_f *counts_a,int_f *idm,
	      float *cut/*,   int_fl counts_l */);

/* xsapConEdit.f */
void  dojoin_(int_f *relpg_a, int_f *lngthg_a,int_f *lnbr_a,  int_f *rnbr_a,
	      int_f *ngels,   int_f *nconts,  int_f *lnconl,  int_f *lnconr,
	      int_f *idbsiz,  int_f *idevr,   int_f *idevw,   int_f *relx/*,
	      int_fl relpg_l, int_fl lngthg_l,int_fl lnbr_l,  int_fl rnbr_l*/);

/* subs89.f */
void  sqcom_ (char  *seq_a,   int_f *idim,    int_fl seq_l);

/* dbsysnew.f */
void  readtg_(int_f *idevt,   int_f *i,       int_f *lpos,    int_f *llen,
	      int_f *lcom,    int_f *ltype,   int_f *next);

void  writtg_(int_f *idevt,   int_f *i,       int_f *lpos,    int_f *llen,
	      int_f *lcom,    int_f *ltype,   int_f *next);

void  readcc_(int_f *idevc,   int_f *i,       int_f *icnt,    int_f *next,
	      char  *note_s,  int_fl note_l);

void  writcc_(int_f *idevc,   int_f *i,       int_f *icnt,    int_f *next,
	      char  *note_s,  int_fl note_l);

void  sqrev_ (char  *seqnce_a,int_f *idim,    int_fl seqnce_l);

/* seeme.f */
void  flusho_();

/* fmain.f */
void  fmain_ ();

/* xspec.f / nxspec.f */
int_f xversn_();

#endif
