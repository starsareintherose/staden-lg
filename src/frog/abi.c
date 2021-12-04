#include <stdio.h>
#include "mach-io.h"

typedef struct {
	char id[12];
	int_2 s1;
	int_2 s2;
	int_2 s3;
	int_2 s4;
	int_2 recs;
	int_4 l1;
	int_4 offset;
	int_4 l2;
} Header;

typedef struct {
	char id[4];
	int_4 index;
	int_2 type;
	int_2 size;
	int_4 N;
	int_4 length;
	int_4 ptr1;
	int_4 ptr2;
} Rec;

/*
** Meaningful ids: (Explanations c/o jes)
**
** GMBF       Gel type
** DATA.1-4   Raw data block
** DATA.5-8   Central (rather featureless) data block
** DATA.9-12  Processed data block
** FWO_       Base order (see seqIOABI.c)
** PBAS.1-2   Base sequence
** PLOC.1-2   Base positions
** S/N%       Signal strengths (array of 4 shorts floats)
** SMPL       Sample name
** SPAC       Base spacing (float)
**
** It appears that if the value for an id occupies less than or equal four
** bytes, it is shoe-horned into the ptr1 field. Otherwise, ptr1 holds
** the byte offset in the file where the data can be found.
** 
**
**
**
*/

int read_header(FILE *fp, Header *h)
{
#define FREAD(R) \
    if ( fread(&(h->R),sizeof(h->R),1,fp) == 0) \
	return 1;

    fseek(fp,0,0);
    FREAD(id);
    FREAD(s1);
    FREAD(s2);
    FREAD(s3);
    FREAD(s4);
    FREAD(recs);
    FREAD(l1);
    FREAD(offset);
    FREAD(l2);
    return 0;
}

int abi_dump(char *fn)
{
    FILE *fp;
    Rec *recs;
    Header header;
    int i;
    int reads;

    if ( (fp=fopen(fn,"rb")) == NULL ) return 1;

    if (read_header(fp,&header)) { fclose(fp); return 2;}

    recs = (Rec *) malloc(header.recs * sizeof(Header));

    fseek(fp,header.offset,0);
    reads = fread(recs,sizeof(Rec),header.recs,fp);

    if (reads!=header.recs) {
	fclose(fp);
	free(recs);
	return 3;
    }
    
    printf("file:%s\n",fn);
    printf(" ID   indx type size   recs length pointer1 pointer2 Text...\n");
    printf(" ---- ---- ---- ---- ------ ------ -------- -------- -------\n");
    for (i=0; i<header.recs;i++) {
	printf(" %4.4s %4d %04x %4d %6d %6d %08x %08x",
	recs[i].id,
	recs[i].index,
	recs[i].type,
	recs[i].size,
	recs[i].N,
	recs[i].length,
	recs[i].ptr1,
	recs[i].ptr2
	);
	switch(recs[i].type) {
	case 0x12: /* string */
	{
	    char *s;
	    char buf[32];
	    int len;
	    len = (recs[i].length > sizeof(buf)) ? sizeof(buf) : recs[i].length;
	    if(recs[i].length<=4) {
		/*
		** stored in ptr1
		*/
		s = (char *) &recs[i].ptr1;
	    } else {
		/*
		** stored in file at offset ptr1
		** NOTE - byte at offset is string length
		*/
		fseek(fp,recs[i].ptr1,0);
		if (len = fread(buf,1,len,fp)) len--;
		s = buf+1;
	    }
	    printf(" %-32.*s\n",len,s,(len>32)?"...\n":"\n");
	    break;
	}
	case 0x0c: /* character array? */
	case 0x0d: /* bit array? */
	case 0x04: /* integer array */
	case 0x02: /* sequence ?*/
	case 0x0a: /* date */ 
	case 0x0b: /* time */ 
	case 0x07: /* float */ 
	default:
	    printf("\n");
	    break;

	}
    }

    printf("\n");

    free(recs);
    fclose(fp);
    return 0;
}



main(int argc, char **argv)
{
    while (--argc) {
	int err;
	argv++;
	if (err=abi_dump(*argv)) printf("Couldn't read %s (%d)\n",*argv,err);
    }
}
