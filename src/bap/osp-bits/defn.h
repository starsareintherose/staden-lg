#ifndef _defn_h
#define _defn_h

/*
  Program Name: defn
  File name: def.h
  Purpose: assigns macros and stdio and global declaration of
          our_alloc
  Last Update: Fri Mar 23 1991
  Copyright 1991: LaDeana Hillier and Philip Green
  Change Log:
*/

#if vms
#include stdio
#include ctype
#else
#include <stdio.h>
#include <ctype.h>
#endif


#define MAX_WORDS 10
#define MAX_I  10
#define MAX_WORD_SIZE 100
#define MAX_NAME_SIZE 100
#define MAX_PARAM_SIZE 10000
#define MAX_NUM_OLIGOS 300
#define MAX_SEQ_LEN 50000
#define MAX_NUM_OTHER_SEQS 25
#define OTHER_SEQ_MAX_SIZE 100000
#define OTHER_SEQ_FILE_MAX_SIZE 100000
#define MAX_GRAPH_RESULTS_WIDTH 1000
#define MAX_GRAPH_RESULTS_HEIGHT 1000
#define MAX_INFO_LEN 3000   /* score information, which is the
			      information about how many primers
			      were accepted and rejected and for
			      what reasons*/

#define OriginX 20
#define OriginY 25
#define oligoFont1 "6x13"
#define Primer_Intvl 10 /* pixel interval between primers */

char *our_alloc();



#endif /* _defn_h */
