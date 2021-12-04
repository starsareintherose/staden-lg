#include "misc.h"
#include <stdio.h>

void shell_call(char *command, char *output, int len)
{
    FILE *pipe;
    char *a;

    output[0]='\0';
    pipe = popen(command,"r");
    fgets(output,len,pipe);
    pclose(pipe);

    /* clobber last new line */
    for (a=output;*a && *a != '\n'; a++);
    *a = '\0';
}
