#ifndef _mach_io_h
#define _mach_io_h
/*
** Machine independant io
** For reading and writing to big-endian and little-endian files
**
** Routines available:
**     be_write_int_1()
**     be_write_int_2()
**     be_write_int_4()
**     be_read_int_1()
**     be_read_int_2()
**     be_read_int_4()
**     le_write_int_1()
**     le_write_int_2()
**     le_write_int_4()
**     le_read_int_1()
**     le_read_int_2()
**     le_read_int_4()
**
** All routine return:
**    0 - an error has occurred during io operation
**    1 - value successfully read or written
*/

#include <stdio.h>

typedef char		int_1;
typedef short		int_2;
typedef int		int_4;
typedef unsigned char	uint_1;
typedef unsigned short	uint_2;
typedef unsigned int	uint_4;

/**********************************************************************/
/* IO for big-endian files                                            */
/**********************************************************************/

extern int be_write_int_1(FILE *fp, uint_1 *i1);
/*
** Write a big-endian int_1
*/

extern
int be_write_int_2(FILE *fp, uint_2 *i2);
/*
** Write a big-endian int_2
*/

extern
int be_write_int_4(FILE *fp, uint_4 *i4);
/*
** Write a big-endian int_4
*/

extern
int be_read_int_1(FILE *fp, uint_1 *i1);
/*
** Read a big-endian int_1
*/

extern
int be_read_int_2(FILE *fp, uint_2 *i2);
/*
** Read a big-endian int_2
*/

extern
int be_read_int_4(FILE *fp, uint_4 *i4);
/*
** Read a big-endian int_4
*/

/**********************************************************************/
/* IO for little-endian files                                         */
/**********************************************************************/

extern
int le_write_int_1(FILE *fp, uint_1 *i1);
/*
** Write a little-endian int_1
*/

extern
int le_write_int_2(FILE *fp, uint_2 *i2);
/*
** Write a little-endian int_2
*/

extern
int le_write_int_4(FILE *fp, uint_4 *i4);
/*
** Write a little-endian int_4
*/

extern
int le_read_int_1(FILE *fp, uint_1 *i1);
/*
** Read a little-endian int_1
*/

extern
int le_read_int_2(FILE *fp, uint_2 *i2);
/*
** Read a little-endian int_2
*/

extern
int le_read_int_4(FILE *fp, uint_4 *i4);
/*
** Read a little-endian int_4
*/


#endif /* _mach_io_h */
