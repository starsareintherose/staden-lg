/*
** Machine independant io:
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
**    1 - value suggessfully read or written
*/

#include <stdio.h>
#include "mach-io.h"




/**********************************************************************/
/* IO for big-endian files                                            */
/**********************************************************************/

int be_write_int_1(FILE *fp, uint_1 *i1)
/*
** Write a big-endian int_1
*/
{
    if (fwrite(i1, sizeof(uint_1), 1, fp) != 1) return (0);
    return (1);
}





int be_write_int_2(FILE *fp, uint_2 *i2)
/*
** Write a big-endian int_2
*/
{
    uint_1 buf[sizeof(int_2)];

    buf[0] = (uint_1) (*i2>>8)&255;
    buf[1] = (uint_1) *i2&255;
    if (fwrite(buf, sizeof(buf), 1, fp) != 1) return (0);
    return (1);
}





int be_write_int_4(FILE *fp, uint_4 *i4)
/*
** Write a big-endian int_4
*/
{
    uint_1 buf[sizeof(*i4)];


    buf[0] = (uint_1) (*i4>>24)&255;
    buf[1] = (uint_1) (*i4>>16)&255;
    buf[2] = (uint_1) (*i4>>8)&255;
    buf[3] = (uint_1) *i4&255;
    if (fwrite(buf, sizeof(buf), 1, fp) != 1) return (0);
    return (1);
}





int be_read_int_1(FILE *fp, uint_1 *i1)
/*
** Read a big-endian int_1
*/
{
    if (fread(i1, sizeof(uint_1), 1, fp) != 1) return (0);
    return (1);
}





int be_read_int_2(FILE *fp, uint_2 *i2)
/*
** Read a big-endian int_2
*/
{
    uint_1 buf[sizeof(int_2)];

    if (fread(buf, sizeof(buf), 1, fp) != 1) return (0);
    *i2 =
        (((uint_2)buf[1]) +
         ((uint_2)buf[0]<<8));
    return (1);
}





int be_read_int_4(FILE *fp, uint_4 *i4)
/*
** Read a big-endian int_4
*/
{
    uint_1 buf[sizeof(int_4)];

    if (fread(buf, sizeof(buf), 1, fp) != 1) return (0);
    *i4 =
        (((uint_4)buf[3]) +
         ((uint_4)buf[2]<<8) +
         ((uint_4)buf[1]<<16) +
         ((uint_4)buf[0]<<24));
    return (1);
}










/**********************************************************************/
/* IO for little-endian files                                         */
/**********************************************************************/

int le_write_int_1(FILE *fp, uint_1 *i1)
/*
** Write a little-endian int_1
*/
{
    if (fwrite(i1, sizeof(uint_1), 1, fp) != 1) return (0);
    return (1);
}





int le_write_int_2(FILE *fp, uint_2 *i2)
/*
** Write a little-endian int_2
*/
{
    uint_1 buf[sizeof(int_2)];

    buf[1] = (uint_1) (*i2>>8)&255;
    buf[0] = (uint_1) *i2&255;
    if (fwrite(buf, sizeof(buf), 1, fp) != 1) return (0);
    return (1);
}





int le_write_int_4(FILE *fp, uint_4 *i4)
/*
** Write a little-endian int_4
*/
{
    uint_1 buf[sizeof(int_4)];

    buf[3] = (uint_1) (*i4>>24)&255;
    buf[2] = (uint_1) (*i4>>16)&255;
    buf[1] = (uint_1) (*i4>>8)&255;
    buf[0] = (uint_1) *i4&255;
    if (fwrite(buf, sizeof(buf), 1, fp) != 1) return (0);
    return (1);
}





int le_read_int_1(FILE *fp, uint_1 *i1)
/*
** Read a little-endian int_1
*/
{
    if (fread(i1, sizeof(uint_1), 1, fp) != 1) return (0);
    return (1);
}





int le_read_int_2(FILE *fp, uint_2 *i2)
/*
** Read a little-endian int_2
*/
{
    uint_1 buf[sizeof(int_2)];

    if (fread(buf, sizeof(buf), 1, fp) != 1) return (0);
    *i2 =
        (((uint_2)buf[0]) +
         ((uint_2)buf[1]<<8));
    return (1);
}





int le_read_int_4(FILE *fp, uint_4 *i4)
/*
** Read a little-endian int_4
*/
{
    uint_1 buf[sizeof(int_4)];

    if (fread(buf, sizeof(buf), 1, fp) != 1) return (0);
    *i4 =
        (((uint_4)buf[0]) +
         ((uint_4)buf[1]<<8) +
         ((uint_4)buf[2]<<16) +
         ((uint_4)buf[3]<<24));
    return (1);
}
