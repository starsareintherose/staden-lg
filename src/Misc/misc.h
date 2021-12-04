#ifndef _misc_h
#define _misc_h

#include <stdio.h>
#include <stdarg.h>  /* varargs needed for v*printf() prototypes */


extern int is_directory(char * fn);
extern int is_file(char * fn);
extern int file_exists(char * fn);
extern int file_size(char * fn);
extern int fstrlen(char *f, int max_f);
extern void f2cstr(char *f, int max_f, char *c, int max_c);
extern void c2fstr(char *c, int max_c, char *f, int max_f);
extern char *mystrtok(char *s, char *ct);
extern char *myfind(char *file, char* searchpath, int (*found) (char *) );
extern void crash (char* format,...);
extern void str_tolower (char *s);
extern void str_toupper (char *s);
extern char *fn_tail (char *s);
extern void fn_tolower (char *s);
extern void fn_toupper (char *s);
extern void shell_call(char *command, char *output, int len);
extern char *date_str();
#ifdef NOSTRDUP
extern char *strdup(char *s);
#endif
#ifdef NOSTRSTR
extern char *strstr(char *cs, char *ct);
#endif
#ifdef NOMEMMOVE
extern void *memmove(void *to, const void *from, size_t len);
#endif

#define findfile(F,S) myfind((F),(S),file_exists)
/*is_file fails for symbolic links*/
/*#define findfile(F,S) myfind((F),(S),is_file)*/

#define min(A,B) ( ( (A) < (B) ) ? (A) : (B) )
#define max(A,B) ( ( (A) > (B) ) ? (A) : (B) )
#define sgn(A) ( (A) ? ( ( (A) < 0 ) ? -1 : 1 ) : 0 )


#endif /*_misc_h*/
