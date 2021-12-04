/*
 * expFileIO.c
 *
 * Routines for reading and writing to experiment files.
 *
 * 1. Opening experiment files
 * 2. Reading information from an experiment file
 * 3. Appending to experiment files
 * 4. Closing an opened experiment file
 *
 */

#include <stdio.h>
#include "expFileIO.h"
#include "eflt.h"


/* The only feature id that matters */
#define SEQUENCE 22





static int exp_get_feature_index(char *e)
{
    int i;
    
    for (i = 0; i < MAXIMUM_EFLTS; i++)
	if(strcmp(feature_ids[i],e)==0) return i;
    
    return -1;
}


static Exp_info *exp_create_info()
/*
 * Allocate space for new experiment file information
 */
{
    Exp_info *new;
    int i;
    
    new = (Exp_info *) malloc(sizeof(Exp_info));
    if (new != NULL) {
	for(i=0; i< MAXIMUM_EFLTS ; i++) new->entry[i] = NULL;
    }
    new->fp = NULL;
    
    return new;
}


void exp_destroy_info(Exp_info *e)
/*
 * Destroy experiment file information
 */
{
    int i;
    if (e != NULL_Exp_info) {
	for (i = 0; i < MAXIMUM_EFLTS; i++)
	    if (e->entry[i] != NULL) free(e->entry[i]);
	if (e->fp != NULL) fclose(e->fp);
	free(e);
    }
}






static char *exp_read_sequence(FILE *fp)
/*
 * Read from file a sequence, discarding all white space til a // is encountered
 */
{
    char *seq;
    int seql;
    char line[EXP_FILE_LINE_LENGTH+1];
    char *l;
    
    seql = 0;
    seq = (char *)malloc(seql+1);
    seq[0] = '\0';
    
    l = fgets(line,EXP_FILE_LINE_LENGTH,fp);
    while (l!= NULL && strncmp(l,"//",2)) {
	char *a, *b;
	for(a=b=line;*a;a++)
	    if (! isspace(*a)) *b++=*a;
	*b = '\0';
	seql = seql + b-line;
	seq = (char *)realloc(seq,seql+1);
	strcat(seq,line);
	l = fgets(line,EXP_FILE_LINE_LENGTH,fp);
    }
    
    return seq;
}






Exp_info *exp_read_info(char *file)
/*
 * Read in an experiment file and return handle
 */
{
    Exp_info *e;
    char line[EXP_FILE_LINE_LENGTH+1];
    
    e = exp_create_info();
    
    /*
     * open for read
     */
    if ((e->fp = fopen(file,"r"))==NULL) {
	exp_destroy_info(e);
	return NULL_Exp_info;
    }
    
    
    if (e != NULL_Exp_info) {
	while (fgets(line,EXP_FILE_LINE_LENGTH,e->fp) != NULL) {
	    char *c;
	    int entry;
	    /*
	     * zero terminate first argument
	     * set c to point to second argument
	     */
	    for (c=line;*c && !isspace(*c); c++) ;
	    if (*c) {
		*c++ = '\0';
	        for (;*c && isspace(*c); c++) ;
	    }

	    
	    entry = exp_get_feature_index(line);
	    if (entry >= 0) {
		/* junk previous entry for same line type is there was one*/
		if (e->entry[entry]) free(e->entry[entry]);
		
		if (entry == SEQUENCE) {
		    e->entry[entry] = exp_read_sequence(e->fp);
		} else {
		    int l;
		    l = strlen(c);
		    if (!l) l = 1; /* handle zero length lines elegantly */
		    e->entry[entry] = (char *)malloc(l);
		    strncpy(e->entry[entry],c,l-1);
		    e->entry[entry][l-1] = '\0';
		}
	    }
	}
    }
    
    fclose(e->fp);
    /*
     * reopen for appending
     */
    e->fp = fopen(file,"a");
    
    return e;
    
}


static int exp_check_eid_read(Exp_info *e,int id)
/*
 * Check these are a valid combination and that
 * an entry exists for read
 */
{
    return (e == NULL || id < 0 || id >= MAXIMUM_EFLTS || e->entry[id] == NULL || feature_ids[id][0]=='\0');
}

static int exp_check_eid_write(Exp_info *e,int id)
/*
 * Check these are a valid combination and that
 * an entry exists for write
 */
{
    return (e == NULL || id < 0 || id >= MAXIMUM_EFLTS || e->fp == NULL || feature_ids[id][0]=='\0');
}






int exp_get_int(Exp_info *e, int id, int *val)
/*
 * Get the integer for entry id
 * returns:
 *    0 - success
 *    1 - no entry
 */
{
    if ( exp_check_eid_read(e,id) ) return 1;
    *val = atoi(e->entry[id]);
    return 0;
}


int exp_get_rng(Exp_info *e, int id, int *from, int *to)
/*
 * Get the integer pair for entry id
 * returns:
 *    0 - success
 *    1 - no entry
 */
{
    if ( exp_check_eid_read(e,id) ) return 1;
    sscanf(e->entry[id],"%d..%d",from,to);
    return 0;
}



int exp_get_str(Exp_info *e, int id, char *s, int_fl s_l)
/*
 * Get the string for entry id
 * returns:
 *    0 - success
 *    1 - no entry
 */
{
    if ( exp_check_eid_read(e,id) ) return 1;
    strncpy(s,e->entry[id],s_l);
    
    return 0;
}


static int exp_append_str(Exp_info *e, int id, char *s)
/*
 * Append the string to experiment file for entry id
 * returns:
 *    0 - success
 *    1 - no update
 */
{
    char *copy;
    if ( (copy = (char *)malloc(strlen(s)+1))==NULL ) return 1;
    if (fprintf(e->fp,"%s   %s\n",feature_ids[id],s)<0) return 1;
    /* copied and written successfully... now switch */
    strcpy(copy,s);
    free(e->entry[id]);
    e->entry[id] = copy;
    return 0;
}


int exp_put_int(Exp_info *e, int id, int *val)
/*
 * Append the integer for entry id to the experiment file
 * returns:
 *    0 - success
 *    1 - no update
 */
{
    char buf[EXP_FILE_LINE_LENGTH];
    if ( exp_check_eid_write(e,id) ) return 1;
    sprintf(buf,"%d",*val);
    return exp_append_str(e,id,buf);
}


int exp_put_rng(Exp_info *e, int id, int *from, int *to)
/*
 * Append the integer pair for entry id to the experiment file
 * returns:
 *    0 - success
 *    1 - no update
 */
{
    char buf[EXP_FILE_LINE_LENGTH];
    if ( exp_check_eid_write(e,id) ) return 1;
    sprintf(buf,"%d..%d",*from,*to);
    return exp_append_str(e,id,buf);
}



int exp_put_str(Exp_info *e, int id, char *s, int_fl s_l)
/*
 * Append the string for entry id to the experiment file
 * returns:
 *    0 - success
 *    1 - no update
 */
{
    if ( exp_check_eid_write(e,id) ) return 1;
    /* don't allow multi-line entries to be written */
    if ( id == SEQUENCE ) return 1;
    return exp_append_str(e,id,s);
}


/*
 * FORTRAN INTERFACE
 */

static void c2fstr(char *c, int max_c, char *f, int max_f)
{
#define min(A,B) ( (A) < (B) ? (A) : (B) )
    int i;
    i = strlen(c);
    i = min(i,max_f);
    i = min(i,max_c);
    strncpy(f,c,i);
    for( ; i<max_f; i++) f[i]=' ';
    
}

static int fstrlen(char *f, int max_f)
{
    for (; max_f > 0 && (isspace(f[max_f-1]) || f[max_f-1]=='\0'); max_f--);
    return max_f;
}

static void f2cstr(char *f, int max_f, char *c, int max_c)
{
    int i;
    
    i = min(fstrlen(f,max_f),max_c);
    strncpy(c,f,i);
    c[i]='\0';
}





/*************************************************************
 * FORTRAN INTERFACE
 *************************************************************/



static int init_done = 0;
static int NHandles = 0;
static Exp_info **Handles = NULL;

static int initialise()
{
    int i;
    
    if (init_done) return 0;
    init_done++;
    
    NHandles = getdtablesize();
    if (!NHandles) return 1;
    
    if ( (Handles = (Exp_info **)malloc(sizeof(Exp_info *) * NHandles)) == NULL) {
	NHandles = 0;
	return 1;
    }
    
    for (i=0; i<NHandles; i++) Handles[i] = NULL;
    
    return 0;
}


static int get_free_handle()
/*
 * find a free entry in the Exp array
 * returns -1 if there is none
 */
{
    int i;
    
    (void) initialise();
    
    if (!NHandles) return -1; /* no slots! */
    for (i=0; i<NHandles && Handles[i]!=NULL; i++) ;
    return (i==NHandles)?-1:i;
}


static int check_handle(int_f *handle)
{
    return (handle == NULL ||
	    (int) (*handle) <= 0 ||
	    (int) (*handle) > NHandles);
}



int_f expopn_(char *fn, int_fl fn_l)
/*
 * FORTRAN interface to exp_open_file()
 */
{
    char cfn[1025];
    int handle;
    
    if ( (handle = get_free_handle()) >= 0 ) {
	f2cstr(fn,fn_l,cfn,1024);
	Handles[handle] = exp_read_info(cfn);
    }
    
    return (int_f) (handle+1);
}



int_f expkil_(int_f *handle)
/*
 * FORTRAN interface to exp_destroy_info
 */
{
    Exp_info *e;
    if ( check_handle(handle) ) return 0;
    e = (Exp_info *) Handles[(int)(*handle)-1];
    
    exp_destroy_info(e);
    
    Handles[(int)(*handle)-1] = NULL;
    *handle = 0;
    
    return 0;
}

int_f expri_(int_f *handle, int_f *id, int_f *val)
/*
 * FORTRAN interface to exp_get_int
 */
{
    Exp_info *e;
    if ( check_handle(handle) ) return 1;
    e = (Exp_info *) Handles[(int)(*handle)-1];
    
    return exp_get_int(e, (int)*id, (int *)val);
}


int_f exprr_(int_f *handle, int_f *id, int_f *from, int_f *to)
/*
 * FORTRAN interface to exp_get_rng
 */
{
    Exp_info *e;
    if ( check_handle(handle) ) return 1;
    e = (Exp_info *) Handles[(int)(*handle)-1];
    
    return exp_get_rng(e,(int)*id,(int *)from,(int *)to);
    
}

int_f exprsa_(int_f *handle, int_f *id, char *s, int_f *max_len, int_fl s_l)
/*
 * FORTRAN interface to exp_get_str workalike
 * NOTE: for use with FORTRAN CHARACTER arrays instead CHARACTER strings
 */
{
    Exp_info *e;
    if ( check_handle(handle) ) return 1;
    e = (Exp_info *) Handles[(int)(*handle)-1];
    
    if ( exp_check_eid_read(e,*id) ) return 1;
    c2fstr(e->entry[*id],(int)*max_len,s,(int)*max_len);
    return 0;
}


int_f exprs_(int_f *handle, int_f *id, char *s, int_fl s_l)
/*
 * FORTRAN interface to exp_get_str workalike
 * NOTE: for use with FORTRAN CHARACTER strings instead CHARACTER arrays
 */
{
    Exp_info *e;
    if ( check_handle(handle) ) return 1;
    e = (Exp_info *) Handles[(int)(*handle)-1];
    
    if ( exp_check_eid_read(e,*id) ) return 1;
    c2fstr(e->entry[*id],s_l,s,s_l);
    return 0;
}


int_f expwi_(int_f *handle, int_f *id, int_f *val)
/*
 * FORTRAN interface to exp_put_int
 */
{
    Exp_info *e;
    if ( check_handle(handle) ) return 1;
    e = (Exp_info *) Handles[(int)(*handle)-1];
    
    return exp_put_int(e, (int)*id, (int *)val);
}


int_f expwr_(int_f *handle, int_f *id, int_f *from, int_f *to)
/*
 * FORTRAN interface to exp_put_rng
 */
{
    Exp_info *e;
    if ( check_handle(handle) ) return 1;
    e = (Exp_info *) Handles[(int)(*handle)-1];
    
    return exp_put_rng(e, (int)*id, (int *)from, (int *)to);
}


int_f expwsa_(int_f *handle, int_f *id, char *s, int_f *max_len, int_fl s_l)
/*
 * FORTRAN interface to exp_put_str workalike
 * NOTE: for use with FORTRAN CHARACTER arrays instead CHARACTER strings
 */
{
    Exp_info *e;
    char buf[EXP_FILE_LINE_LENGTH];
    if ( check_handle(handle) ) return 1;
    e = (Exp_info *) Handles[(int)(*handle)-1];
    
    
    if ( exp_check_eid_write(e,*id) ) return 1;
    /* don't allow multi-line entries to be written */
    if (*id == SEQUENCE ) return 1;
    f2cstr(s,(int)*max_len,buf,sizeof(buf));
    return exp_append_str(e,*id,buf);
    
}

int_f expws_(int_f *handle, int_f *id, char *s, int_fl s_l)
/*
 * FORTRAN interface to exp_put_str workalike
 * NOTE: for use with FORTRAN CHARACTER strings instead CHARACTER arrays
 */
{
    char buf[EXP_FILE_LINE_LENGTH];
    Exp_info *e;
    if ( check_handle(handle) ) return 1;
    e = (Exp_info *) Handles[(int)(*handle)-1];
    
    
    if ( exp_check_eid_write(e,*id) ) return 1;
    /* don't allow multi-line entries to be written */
    if (*id == SEQUENCE ) return 1;
    f2cstr(s,s_l,buf,sizeof(buf));
    return exp_append_str(e,*id,buf);
}


