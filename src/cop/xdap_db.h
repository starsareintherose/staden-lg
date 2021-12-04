#ifndef _xdap_db_h
#define _xdap_db_h

#include "mach-io.h"

/*
** Definition of xdap database files
*/



/*
** Archive file (*.AR?)
*/
typedef union {

    struct _ar_header {
	uint_4 idbsiz;
	uint_4 maxgel;
	uint_4 idm;
    } header;

    struct _ar_lines {
	char name[12];
    } lines;

} ar_file_rec;

#define ar_header_rec()  ( 1000 )
#define ar_byte_index(R) ( ((R)-1) *sizeof(ar_file_rec) )







/*
** Relationships file (*.RL?)
*/
typedef union {

    struct _rl_header { 
	uint_4 num_gels;
	uint_4 num_contigs;
	uint_4 spare1;
	uint_4 spare2;
    } header;

    struct _rl_lines {
	uint_4 rel_pos;
	uint_4 length;
	uint_4 left_nbr;
	uint_4 right_nbr;
    } lines;

    struct _rl_clines {
	uint_4 length;
	uint_4 spare3;
	uint_4 left_end;
	uint_4 right_end;
    } clines;

} rl_file_rec;

#define rl_byte_index(R) ( ((R)-1) * sizeof(rl_file_rec) )






/*
** Sequence file (*.SQ?)
*/
typedef char *sq_file_rec;



/*
** Tag files (*.TG?)
*/
typedef union {
    int i;
    char c[4];
} TagType;



typedef union {

    struct _tg_header {
	uint_4 count;
	uint_4 spare1;
	uint_4 spare2;
	TagType spare3;
	uint_4 free_list;
    } header;

    struct _tg_lines {
	uint_4 position;
	uint_4 length;
	uint_4 comment;
	TagType type;
	uint_4 next;
    } lines;

} tg_file_rec;

#define tg_byte_index(R) ( ((R)-1) *sizeof(tg_file_rec) )


/*
** Comment files (*.CC?)
*/
#define COMMENT_SIZE 40
typedef union {

    struct _cc_header {
	uint_4 free_list;
	uint_4 count;
	char spare[COMMENT_SIZE - sizeof(uint_4)];
    } header;

    struct _cc_lines {
	uint_4 next;
	char comment[COMMENT_SIZE];
    } lines;

} cc_file_rec;

#define cc_byte_index(R) ( ((R)-1) *sizeof(cc_file_rec) )



/*
** Useful variables
*/
extern int max_gels;
extern int num_gels;
extern int num_contigs;
extern int max_gel_length;
extern int data_class;

extern FILE *ar_fp;
extern FILE *rl_fp;
extern FILE *sq_fp;
extern FILE *tg_fp;
extern FILE *cc_fp;





#endif /* _xdap_db_h */

