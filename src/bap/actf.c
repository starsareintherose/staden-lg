#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>
#include "mcspec.h"
#include "fort.h"

/* static error messages for use in actf_() */
static char *actferrlist[] = {
    "Error! - stack underflow",
    "Error! - unknown JOB to ACTF()",
    "Error creating busy file",
    "Error deleting busy file",
    "Sorry, database busy"
    };

/* global variables - exported in actf.h */
int actfptr = 0;	/* stack pointer - points to first free location */

/* types */
typedef struct {
    char *filename;
} actfstack_t;

/* local variables */
static actfstack_t actfstack[10];

/*
 * Display an error either via ERROM() if we've got a KBOUT or to stderr
 * otherwise.
 */
static void actferr(int errnum, int_f *KBOUT) {
    if (KBOUT)
	errom_(KBOUT, actferrlist[errnum-1], strlen(actferrlist[errnum-1]));
    else 
	fprintf(stderr, "%s\n", actferrlist[errnum-1]);
    return;
}

/*
 * in FORTRAN as INTEGER FUNCTION ACTF(JOB,FILNAM,FILNAMLEN,COPYNUM,KBOUT)
 *
 * JOB = 1 : lock FILNAM (push filename and copynumber onto stack)
 *
 * JOB = 2 : unlock last locked file (pull off stack)
 *           (in this case only trust COPYNUM and KBOUT to contain legal
 *            and relevant data.)
 *
 * Locking mechanism done by creating a 'busy' file of format 'FILE_busy'.
 */
int_f actf_(int_f *JOB_p,
	   char *FILNAM_p,
	   int_f *FILNAMLEN_p,
	   char *COPYNUM_p,
	   int_f *KBOUT,
	   int_fl  FILNAM_l,
	   int_fl  COPYNUM_l)
{
    char fname[256], *cptr;
    struct stat statbuf;
    int fd, i;

    if (*JOB_p == 1) {
	/* char buf[10]; */

	/* copy correct amount of FILNAM_p */
	for (i=0; i<*FILNAMLEN_p; i++) {
	    fname[i] = FILNAM_p[i];
	}
	strcpy(&fname[i], "_BUSYx");
	fname[i+5] = *COPYNUM_p;

	/* add data to our stack */
/*	actfstack[actfptr].filename = strdup(fname); */
	i = strlen(fname);
	cptr = actfstack[actfptr].filename = (char *)malloc(i+1);
	for (; i>=0; i--)
	    cptr[i] = fname[i];
	/* do the locking */
	if (stat(fname, &statbuf) != -1) {
	    /* lock already exists */
	    actferr(5, KBOUT);
	    return 5;
	}
	if ((fd = open(fname, O_CREAT, 0600)) == -1) {
	    actferr(3, KBOUT);
	    return 3;
	}
/*	fprintf(buf, "%d", getuid()); - wrong... ADDRESS of getuid()
	write(fd, buf, sizeof(buf));*/
	close(fd);
	/* inc stack pointer */
	actfptr++;
	return 0;
    } else if (*JOB_p == 2) {
	/* dec stack pointer */
	actfptr--;
	if (actfptr < 0) {
	    actferr(1, KBOUT);
	    return 1;
	}
	/* do the unlocking */
	if (unlink(actfstack[actfptr].filename) == -1) {
	    actferr(4, KBOUT);
	    return 4;
	}
	return 0;
    } else {
	/* unknown job */
	actferr(2, KBOUT);
	return 2;
    }
}

