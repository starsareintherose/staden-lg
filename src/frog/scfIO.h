/*
 * File: scfIO.h
 *
 * Author: Simon Dear
 *         MRC Laboratory of Molecular Biology
 *         Hills Road
 *         Cambridge CB2 2QH
 *         United Kingdom
 *
 * Description: header file for SCF file IO
 *
 * Created: 8 October 1992 (from seqIOSCF.c)
 * Updated:
 *
 */

#ifndef _SCFIO_H_
#define _SCFIO_H_

#include "mach-io.h"

/* ---- Constants ---- */
#define SCF_MAGIC (((((uint_4)'.'<<8)+(uint_4)'s'<<8)+(uint_4)'c'<<8)+(uint_4)'f')

/*
** Type definition for the Header structure
*/
typedef struct {
	uint_4 magic_number;       /* Always ((('.'<<8)+'s'<<8)+'c'<<8)+'f'; */
	uint_4 samples;            /* Number of elements in Samples matrix */
	uint_4 samples_offset;     /* Byte offset from start of file */
	uint_4 bases;              /* Number of bases in Bases matrix */
	uint_4 bases_left_clip;    /* Number of bases in left clip (vector)*/
	uint_4 bases_right_clip;   /* Number of bases in right clip (unreliable) */
	uint_4 bases_offset;       /* Byte offset from start of file */
	uint_4 comments_size;      /* Number of bytes in Comment section */
	uint_4 comments_offset;    /* Byte offset from start of file */
	uint_4 spare[23];          /* Unused */
} Header;

/*
** Type definition for the Sample data
*/
typedef struct {
	uint_1 sample_A;           /* Sample for A trace */
	uint_1 sample_C;           /* Sample for C trace */
	uint_1 sample_G;           /* Sample for G trace */
	uint_1 sample_T;           /* Sample for T trace */
} Samples;

/*
** Type definition for the sequence data
*/
typedef struct {
	uint_4 peak_index;        /* Index into Samples matrix for base position */
	uint_1 prob_A;            /* Probability of it being an A */
	uint_1 prob_C;            /* Probability of it being an C */
	uint_1 prob_G;            /* Probability of it being an G */
	uint_1 prob_T;            /* Probability of it being an T */
	uint_1 base;		  /* Base called */
	uint_1 spare[3];          /* Spare */
} Bases;


/*
** Type definition for the comments
*/
typedef char Comments;            /* Zero terminated list of \n separated entries */




extern int read_scf_header(FILE *fp, Header *h);
extern int read_scf_sample(FILE *fp, Samples *s);
extern int read_scf_base(FILE *fp, Bases *b);
extern int write_scf_header(FILE *fp, Header *h);
extern int write_scf_sample(FILE *fp, Samples *s);
extern int write_scf_base(FILE *fp, Bases *b);
extern int write_scf_comment(FILE *fp, Comments *c, size_t l);

#endif /*_SCFIO_H_*/
