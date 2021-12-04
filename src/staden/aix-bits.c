#include <stdio.h>
 
/*
 * KH 2/06/92
 * simulate a CALL FLUSH(devsepc) from SUN FORTRAN
 * for STADEN programs
 */
 
void * flush (int *fd)
{
  FILE *a = fdopen(*fd,"a");
  if (ferror(a) ) perror("problems with flushing: fdopen(int, char *)");
  fflush(a);
  if (ferror(a) ) perror("problems with flushing: fflush(FILE *)");
}
