#include "misc.h"
#include <stdio.h>

/******************************************************************************/
/*
** Time and date calculations
*/
#include <time.h>
char *date_str()
{
    time_t clock;
    clock = time(NULL);
    return ctime(&clock);
}
