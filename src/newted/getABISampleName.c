/*
    Title: 	 getABISampleName

    Purpose:	 Extract the Sample name from and ABI trace file
    Last update: Fri Oct 25 1991

    Change log:

	25-Oct-1991 SD Machine independant I/O
*/


/*

*/




/* ---- Includes ---- */


#include <stdio.h>     /* IMPORT: stderr, fprintf */
#include <stdlib.h>
#include <sys/types.h>
#include "mach-io.h"



/* ---- Constants ---- */


/*
    The ABI trace file structure is based around an `index'
    which is located towards the end of the file. It's location
    is given by a longword at a fixed place.
*/
#define IndexPO ((off_t)26)

/*
    The index consists of a number of entries.
    Each of these is a label of 4 characters followed by 6 long words.
*/
#define LW1 4
#define LW2 8
#define LW3 12
#define LW4 16
#define LW5 20
#define LW6 24
#define IndexEntryLength 28

/*
    Here is the label we will be looking for (SMPL), packed
    into a long word.
*/
#define SampleNameEntryLabel  ((int_4) ((((('S'<<8)+'M')<<8)+'P')<<8)+'L')



/* ---- Internal functions ---- */


static int_4 readABIInt4(FILE *fp)
{
    unsigned char buf[sizeof(int_4)];

    if (fread(buf, sizeof(int_4), 1, fp) != 1) {
	fprintf(stderr,"Fread fail\n");
	exit(1);
    }
    return (int_4) 
	   (((int_4)buf[3]) +
	    ((int_4)buf[2]<<8) +
	    ((int_4)buf[1]<<16) +
	    ((int_4)buf[0]<<24));

}






/* ---- Exported functions ---- */


void main(unsigned int argc, char **argv)
{   FILE *fp = NULL;
    int_4 indexO, sampleNameO;
    int sampleNameLen;
    int_4 val;
    int entryNum;

    if (argc != 2)
    {   fprintf(stderr, "%s: usage\n", argv[0]);
	exit(1);
    }


    /* Open the gel file: read, binary */
    if ((fp = fopen(argv[1], "rb")) == NULL)
    {   fprintf(stderr, "%s: Unable to open file %s\n", argv[0], argv[1]);
	exit(1);
    }


    /* Get the index offset */
    if (fseek(fp, IndexPO, 0) != 0)
    {   fprintf(stderr,"%s: Seek fail\n", argv[0]);
	exit(1);
    }
    indexO = readABIInt4(fp);


    /* Find the sample name entry by searching for the correct label */
    entryNum = 0;
    (void) fseek(fp, indexO+(entryNum*IndexEntryLength), 0);
    while ((val = readABIInt4(fp)) != SampleNameEntryLabel)
    {   entryNum++;
	(void) fseek(fp, indexO+(entryNum*IndexEntryLength), 0);
    }

    /* Get the offset of the actual sample name */
    (void) fseek(fp, indexO+(entryNum*IndexEntryLength)+LW4, 0);
    if (readABIInt4(fp) <= 4) {
	sampleNameO = indexO+(entryNum*IndexEntryLength)+LW5;
    } else {
	/* Get the offset of the actual sample name */
	(void) fseek(fp, indexO+(entryNum*IndexEntryLength)+LW5, 0);
	sampleNameO = readABIInt4(fp);
    }

    /* If this is 0, there is The Sample With No Name */
    if (sampleNameO == (int_4)0)
    {   fclose(fp);
	exit(0);
    }


    /*
        Write the sample name out on the standard output
	The name is in a BCPL like format -
	the first byte holds the string length
    */
    if (fseek(fp, sampleNameO, 0) != 0)
    {   fprintf(stderr,"%s: Seek fail\n", argv[0]);
	exit(1);
    }
    sampleNameLen = (int) fgetc(fp);
    while (sampleNameLen > 0)
    {   fputc(fgetc(fp), stdout);
	sampleNameLen--;
    }


    fclose(fp);
    exit(0);
}



