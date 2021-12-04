/*
 * File: scf.h
 * Version:
 *
 * Author: Simon Dear
 *         MRC Laboratory of Molecular Biology
 *	   Hills Road
 *	   Cambridge CB2 2QH
 *	   United Kingdom
 *
 * Description: file structure definitions for SCF file
 *
 * Created: 19 November 1992
 * Updated:
 *
 */

#ifndef _SCF_H_
#define _SCF_H_

#include "mach-io.h"

#define SCF_MAGIC (((((uint_4)'.'<<8)+(uint_4)'s'<<8)+(uint_4)'c'<<8)+(uint_4)'f')

#define VERSION 2.00

/*
 * Type definition for the Header structure
 */
typedef struct {
    uint_4 magic_number;       /* SCF_MAGIC */
    uint_4 samples;            /* Number of elements in Samples matrix */
    uint_4 samples_offset;     /* Byte offset from start of file */
    uint_4 bases;              /* Number of bases in Bases matrix */
    uint_4 bases_left_clip;    /* Number of bases in left clip (vector)*/
    uint_4 bases_right_clip;   /* Number of bases in right clip (unreliable) */
    uint_4 bases_offset;       /* Byte offset from start of file */
    uint_4 comments_size;      /* Number of bytes in Comment section */
    uint_4 comments_offset;    /* Byte offset from start of file */
    char version[4];	     /* "version.revision" */
    uint_4 sample_size;	     /* precision of samples (in bytes) */
    uint_4 code_set;	     /* uncertainty codes used */
    uint_4 spare[20];          /* Unused */
} Header;

#define CSET_DEFAULT 0  /* {A,C,G,T,-} */
#define CSET_STADEN  1
#define CSET_NC_IUB  2
#define CSET_ALF     3  /* extended NC_IUB */
#define CSET_ABI     4  /* {A,C,G,T,N} */

/*
 * Type definition for the Sample data
 */
typedef unsigned char byte;
typedef struct {
    byte sample_A;           /* Sample for A trace */
    byte sample_C;           /* Sample for C trace */
    byte sample_G;           /* Sample for G trace */
    byte sample_T;           /* Sample for T trace */
} Samples1;
typedef struct {
    unsigned short sample_A;           /* Sample for A trace */
    unsigned short sample_C;           /* Sample for C trace */
    unsigned short sample_G;           /* Sample for G trace */
    unsigned short sample_T;           /* Sample for T trace */
} Samples2;

/*
 * Type definition for the sequence data
 */
typedef struct {
    uint_4 peak_index;        /* Index into Samples matrix for base position */
    byte prob_A;            /* Probability of it being an A */
    byte prob_C;            /* Probability of it being an C */
    byte prob_G;            /* Probability of it being an G */
    byte prob_T;            /* Probability of it being an T */
    char base;		    /* Base called */
    byte spare[3];          /* Spare */
} Bases;


/*
 * Type definition for the comments
 */
typedef char Comments;            /* Zero terminated list of \n separated entries */


#endif /*_SCF_H_*/
