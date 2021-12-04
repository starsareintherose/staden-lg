#ifndef _fortran_h
#define _fortran_h

#include "fort.h"

/* This defines the interfaces between fortran and c */

/*
** COMMON /DEVILS/ IDEVT,IDEVC,IDBSIZ,RELPG
*/

/*
** Map onto fortran common block holding RELPG array and IDEVRD
*/
/*
** Snatch Fortran Common Block
** Don't EVER EVER let Rodger see this!!
*/
extern struct {
	 int_f idevt;
	 int_f idevc;
	 int_f idbsiz;
	 int_f relpg[1000];
	} devils_;

#endif /* _fortran_h */
