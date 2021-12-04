/*       cdromheader     */

#include <stdio.h>
#include "mach-io.h"

/*   adds a cdrom style header to file ofp */
int cdromheader( FILE *ofp, uint_2 recordSize )
{

  int i;
  uint_4 fSize = 0;
  uint_4 nRecords = 0;
  char dbName[20] = "PIR45678901234567890";
  char dbRel[10] = "Release890";
  char dt[4] = "1234";
  char empty[256];

/* header contains: 

        ul fsize      
        ul nRecords      (needs to be updated after rest of file is written)
        us recordSize
      [20] dbName
      [10] dbRel
      [4]  dt
     [256] empty

   all are constant except recordSize which hence must be passed

*/

  for (i=0;i<256;i++) empty[i] = ' ';
  if(le_write_int_4(ofp, &fSize) == 0) return 4;
  if(le_write_int_4(ofp, &nRecords) == 0) return 5;
  if(le_write_int_2(ofp, &recordSize) == 0) return 6;

  fwrite(dbName, sizeof(dbName), 1, ofp);
  fwrite(dbRel, sizeof(dbRel), 1, ofp);
  fwrite(dt, sizeof(dt), 1, ofp);
  fwrite(empty, sizeof(empty), 1, ofp);

  return 0;
}

