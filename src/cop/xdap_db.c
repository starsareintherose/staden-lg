#include <stdio.h>
#include "xdap_db.h"


int maxgel;

typedef char IOString[200];


FILE *ar_fp;
FILE *rl_fp;
FILE *sq_fp;
FILE *tg_fp;
FILE *cc_fp;

static IOString ar_file;
static IOString rl_file;
static IOString sq_file;
static IOString tg_file;
static IOString cc_file;

int max_gels;
int num_gels;
int num_contigs;
int max_gel_length;
int data_class;

static ar_file_rec ar_header;
static rl_file_rec rl_header;


read_tg(FILE *f, int rec, tg_file_rec *t)
{

    if ( fseek(f,tg_byte_index(rec),0) ) {
	fprintf(stderr,"Seek failure on tag file, record %d\n",rec);
	exit (1);
    }

    if ( fread(t, sizeof(tg_file_rec), 1, f) != 1) {
	fprintf(stderr, "Read failure on tag file\n");
	exit (1);
    }

}



read_ar(FILE *f, int rec, ar_file_rec *t)
{

    if ( fseek(f,ar_byte_index(rec),0) ) {
	fprintf(stderr,"Seek failure on archive file, record %d\n",rec);
	exit (1);
    }

    if ( fread(t, sizeof(ar_file_rec), 1, f) != 1) {
	fprintf(stderr, "Read failure on archive file\n");
	exit (1);
    }

}




read_rl(FILE *f, int rec, rl_file_rec *t)
{

    if ( fseek(f,rl_byte_index(rec),0) ) {
	fprintf(stderr,"Seek failure on relationships file, record %d\n",rec);
	exit (1);
    }

    if ( fread(t, sizeof(rl_file_rec), 1, f) != 1) {
	fprintf(stderr, "Read failure on relationships file\n");
	exit (1);
    }

}


read_cc(FILE *f, int rec, cc_file_rec *t)
{

    if ( fseek(f,cc_byte_index(rec),0) ) {
	fprintf(stderr,"Seek failure on comment file, record %d\n",rec);
	exit (1);
    }

    if ( fread(t, sizeof(cc_file_rec), 1, f) != 1) {
	fprintf(stderr, "Read failure on comment file, record %d\n",rec);
	exit (1);
    }

}


read_sq(FILE *f, int rec, sq_file_rec t)
{
#define sq_byte_index(R) ( ((R)-1) * max_gel_length )
    if ( fseek(f,sq_byte_index(rec),0) ) {
	fprintf(stderr,"Seek failure on sequence file, record %d\n",rec);
	exit (1);
    }

    if ( fread(t, max_gel_length, 1, f) != 1) {
	fprintf(stderr, "Read failure on sequence file\n");
	exit (1);
    }

}



char *read_comment(FILE *f, uint_4 cp)
{
    int dummy;
    cc_file_rec c;
    int count;
    int i;
    uint_4 nc;
    char *com,*comptr;

    if (!cp) return NULL;
    /* determine how long string is */
    count = 1;
    nc=cp;
    read_cc(f, nc, &c);
    while (c.lines.next != 0) {
	nc = c.lines.next;
	count++;
        read_cc(f, nc, &c);
    }

    com = comptr = (char *)malloc(count * COMMENT_SIZE+1);
    nc=cp;
    read_cc(f, nc, &c);
    strncpy(com,c.lines.comment,COMMENT_SIZE); com+=COMMENT_SIZE;
    while (c.lines.next != 0) {
	nc = c.lines.next;
	count++;
        read_cc(f, nc, &c);
        strncpy(com,c.lines.comment,COMMENT_SIZE); com+=COMMENT_SIZE;
    }

    *com = '\0';

    return comptr;
    

}



static void set_file_names(char *name, char *version)
{
    strcpy(ar_file,name); strcat(ar_file,".AR"); strcat(ar_file,version);
    strcpy(rl_file,name); strcat(rl_file,".RL"); strcat(rl_file,version);
    strcpy(sq_file,name); strcat(sq_file,".SQ"); strcat(sq_file,version);
    strcpy(tg_file,name); strcat(tg_file,".TG"); strcat(tg_file,version);
    strcpy(cc_file,name); strcat(cc_file,".CC"); strcat(cc_file,version);
}

void open_for_read(char *name, char *version)
/*
**
*/
{
    /*
    ** Create file names
    */
    set_file_names(name,version);

    /*
    ** Open files
    */
    if ( ( ar_fp = fopen(ar_file,"r") ) == NULL )
	crash("No archive file %s\n",ar_file);
    if ( ( rl_fp = fopen(rl_file,"r") ) == NULL )
	crash("No relationships file %s\n",rl_file);
    if ( ( sq_fp = fopen(sq_file,"r") ) == NULL )
	crash("No sequence file %s\n",sq_file);
    if ( ( tg_fp = fopen(tg_file,"r") ) == NULL )
	crash("No tag file %s\n",tg_file);
    if ( ( cc_fp = fopen(cc_file,"r") ) == NULL )
	crash("No tag-comment file %s\n",cc_file);
    
    read_ar(ar_fp,ar_header_rec(),&ar_header);
#define rl_header_rec() (ar_header.header.idbsiz)
    read_rl(rl_fp,rl_header_rec(),&rl_header);

    max_gels = ar_header.header.idbsiz;
    max_gel_length = ar_header.header.maxgel;
    data_class = ar_header.header.idm;
    num_gels = rl_header.header.num_gels;
    num_contigs = rl_header.header.num_contigs;

}

void close_files()
/*
** Close all relevant files
*/
{

    fclose(ar_fp);
    fclose(rl_fp);
    fclose(sq_fp);
    fclose(tg_fp);
    fclose(cc_fp);

}
