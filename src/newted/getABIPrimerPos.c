/*
    Title: 	 getABIPrimerPos

    Purpose:	 Extract the Primer Position from and ABI trace file
    Last update: Fri 17 Jul 1992

    Change log:

*/


/*

*/




/* ---- Includes ---- */


#include <stdio.h>     /* IMPORT: stderr, fprintf */




/* ---- Constants ---- */


/*
    The ABI trace file structure is based around an `index'
    which is located towards the end of the file. It's location
    is given by a longword at a fixed place.
*/
#define IndexPO ((long)26)

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
#define PrimerPosEntryLabel  ((long) ((((('P'<<8)+'P')<<8)+'O')<<8)+'S')




/* ---- Types ---- */

typedef long int4;
typedef short int2;




/* ---- Internal functions ---- */


static int4 readABIInt4_no_address(FILE *fp)
{
    unsigned char buf[sizeof(int4)];

    if (fread(buf, sizeof(long), 1, fp) != 1) {
	fprintf(stderr,"Fread fail\n");
	exit(1);
    }
    return (int4) 
	   (((unsigned long)buf[3]) +
	    ((unsigned long)buf[2]<<8) +
	    ((unsigned long)buf[1]<<16) +
	    ((unsigned long)buf[0]<<24));

}

static int readABIInt2(FILE *fp, int2 *i2)
{
    unsigned char buf[sizeof(int2)];

    if (fread(buf, sizeof(buf), 1, fp) != 1) return (0);
    *i2 = (int2)
        (((unsigned short)buf[1]) +
         ((unsigned short)buf[0]<<8));
    return (1);
}


static int readABIInt4(FILE *fp, int4 *i4)
{
    unsigned char buf[sizeof(int4)];

    if (fread(buf, sizeof(buf), 1, fp) != 1) return (0);
    *i4 = (int4)
        (((unsigned long)buf[3]) +
         ((unsigned long)buf[2]<<8) +
         ((unsigned long)buf[1]<<16) +
         ((unsigned long)buf[0]<<24));
    return (1);
}



static int getIndexEntryW(FILE *fp, long indexO, long label, int lw, int2 *val)
/*
    From the ABI results file connected to `fp' whose index starts
    at byte offset `indexO', return in `val' the `lw'th  word (int2)
    from the entry labelled `label'.
    The result indicates success.
*/
{   int entryNum=-1;
    int i;
    int4 entryLabel, entryLw1;
    int4 jval;

    do
    {   entryNum++;
        if (fseek(fp, indexO+(entryNum*IndexEntryLength), 0) != 0) return(-1);
        if (!readABIInt4(fp, &entryLabel)) return(0);
    } while (!(entryLabel == label));


    for(i=2; i<lw; i++)
        if (!readABIInt4(fp, &jval)) return(-1);
    if (!readABIInt2(fp, val)) return(-1);

    return(1);
}




/* ---- Exported functions ---- */


void main(unsigned int argc, char **argv)
{   FILE *fp = NULL;
    int4 indexO, sampleNameO;
    int sampleNameLen;
    int4 val;
    int entryNum;
    int2 primerPos;

    if (argc != 2)
    {   fprintf(stderr, "%s: usage: getPrimerPos ABI_filename\n", argv[0]);
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
    indexO = readABIInt4_no_address(fp);


    /* Get Primer Position...where the ABI file found the start of */
    if (!getIndexEntryW(fp,indexO,PrimerPosEntryLabel,6,&primerPos))
    {   fclose(fp);
	printf("Could not find primer position...\n");
	exit(1);
    }
    printf("%s: %d\n",argv[1],primerPos);


    /*
        Write the sample name out on the standard output
	The name is in a BCPL like format -
	the first byte holds the string length
    */
/*    if (fseek(fp, sampleNameO, 0) != 0)
    {   fprintf(stderr,"%s: Seek fail\n", argv[0]);
	exit(1);
    }
    sampleNameLen = (int) fgetc(fp);
    while (sampleNameLen > 0)
    {   fputc(fgetc(fp), stdout);
	sampleNameLen--;
    }
*/

    fclose(fp);
    exit(0);
}



