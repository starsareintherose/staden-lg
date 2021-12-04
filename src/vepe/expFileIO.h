#ifndef _read_exp_h
#define _read_exp_h

#include "fort.h"

/*
** Definitions
*/
#define MAXIMUM_EFLT_LENGTH     4
#define MAXIMUM_EFLTS          32
#define EFLT_FILE_LINE_LENGTH 128
#define EXP_FILE_LINE_LENGTH  128

typedef struct {
    char *entry[MAXIMUM_EFLTS];
    FILE *fp;
} Exp_info;

#define NULL_Exp_info ( (Exp_info *) NULL )





 /*************************************************************************************/


extern void exp_destroy_info(Exp_info *e);
/*
** Destroy experiment file information
*/










extern Exp_info *exp_read_info(char *file);
/*
** Read in an experiment file and return handle
*/




extern int exp_get_int(Exp_info *e, int id, int *val);
/*
** Get the integer for entry id
** returns:
**    0 - success
**    1 - no entry
*/


extern int exp_get_rng(Exp_info *e, int id, int *from, int *to);
/*
** Get the integer pair for entry id
** returns:
**    0 - success
**    1 - no entry
*/


extern int exp_get_str(Exp_info *e, int id, char *s, int_fl s_l);
/*
** Get the string for entry id
** returns:
**    0 - success
**    1 - no entry
*/


extern int exp_put_int(Exp_info *e, int id, int *val);
/*
** Append the integer for entry id to the experiment file
** returns:
**    0 - success
**    1 - no update
*/


extern int exp_put_rng(Exp_info *e, int id, int *from, int *to);
/*
** Append the integer pair for entry id to the experiment file
** returns:
**    0 - success
**    1 - no update
*/



extern int exp_put_str(Exp_info *e, int id, char *s, int_fl s_l);
/*
** Append the string for entry id to the experiment file
** returns:
**    0 - success
**    1 - no update
*/


/*
** FORTRAN INTERFACE
*/



extern int_f expopn_(char *fn, int_fl fn_l);
/*
** FORTRAN interface to exp_open_file()
*/

extern int_f expkil_(int_f *le);
/*
** FORTRAN interface to exp_destroy_info
*/

extern int_f expri_(int_f *le, int_f *id, int_f *val);
/*
** FORTRAN interface to exp_get_int
*/


extern int_f exprr_(int_f *le, int_f *id, int_f *from, int_f *to);
/*
** FORTRAN interface to exp_get_rng
*/


extern int_f exprsa_(int_f *le, int_f *id, char *s, int_f *max_len, int_fl s_l);
/*
** FORTRAN interface to exp_get_str workalike
** NOTE: for use with FORTRAN CHARACTER arrays instead CHARACTER strings
*/

extern int_f exprs_(int_f *le, int_f *id, char *s, int_fl s_l);
/*
** FORTRAN interface to exp_get_str workalike
** NOTE: for use with FORTRAN CHARACTER strings instead CHARACTER arrays
*/

extern int_f expwi_(int_f *le, int_f *id, int_f *val);
/*
** FORTRAN interface to exp_put_int
*/


extern int_f expwr_(int_f *le, int_f *id, int_f *from, int_f *to);
/*
** FORTRAN interface to exp_put_rng
*/



extern int_f expwsa_(int_f *le, int_f *id, char *s, int_f *max_len, int_fl s_l);
/*
** FORTRAN interface to exp_put_str workalike
** NOTE: for use with FORTRAN CHARACTER arrays instead CHARACTER strings
*/



extern int_f expws_(int_f *le, int_f *id, char *s, int_fl s_l);
/*
** FORTRAN interface to exp_put_str workalike
** NOTE: for use with FORTRAN CHARACTER strings instead CHARACTER arrays
*/


#endif /* _read_exp_h */
