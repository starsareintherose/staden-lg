/*
 * File: frog.c
 *
 * Author: Simon Dear
 *         MRC Laboratory of Molecular Biology
 *         Hills Road
 *         Cambridge CB2 2QH
 *         United Kingdom
 *
 * Description: utility to alter base lane ordering of an ABI file
 *
 * Created: 8 October 1992
 * Updated:
 *
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <sys/types.h>
#include "mach-io.h"

typedef struct {
	char id[12];
	uint_2 s1;
	uint_2 s2;
	uint_2 s3;
	uint_2 s4;
	uint_2 recs;
	uint_4 l1;
	uint_4 offset;
	uint_4 l2;
} Header;

typedef struct {
	char id[4];
	uint_4 index;
	uint_2 type;
	uint_2 size;
	uint_4 N;
	uint_4 length;
	uint_4 ptr1;
	uint_4 ptr2;
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
    if (fread(&h->id[0],12,1,fp) == 0) return 1;
    if (be_read_int_2(fp,&h->s1)==0) return 1;
    if (be_read_int_2(fp,&h->s2)==0) return 1;
    if (be_read_int_2(fp,&h->s3)==0) return 1;
    if (be_read_int_2(fp,&h->s4)==0) return 1;
    if (be_read_int_2(fp,&h->recs)==0) return 1;
    if (be_read_int_4(fp,&h->l1)==0) return 1;
    if (be_read_int_4(fp,&h->offset)==0) return 1;
    if (be_read_int_4(fp,&h->l2)==0) return 1;

    return 0;
}

int read_record(FILE *fp, Rec *r)
{
    if (fread(&r->id[0],4,1,fp) == 0) return 1;
    if (be_read_int_4(fp,&r->index)==0) return 1;
    if (be_read_int_2(fp,&r->type)==0) return 1;
    if (be_read_int_2(fp,&r->size)==0) return 1;
    if (be_read_int_4(fp,&r->N)==0) return 1;
    if (be_read_int_4(fp,&r->length)==0) return 1;
    if (be_read_int_4(fp,&r->ptr1)==0) return 1;
    if (be_read_int_4(fp,&r->ptr2)==0) return 1;

    return 0;
}

int write_record(FILE *fp, Rec *r)
{
    if (fwrite(&r->id[0],4,1,fp) == 0) return 1;
    if (be_write_int_4(fp,&r->index)==0) return 1;
    if (be_write_int_2(fp,&r->type)==0) return 1;
    if (be_write_int_2(fp,&r->size)==0) return 1;
    if (be_write_int_4(fp,&r->N)==0) return 1;
    if (be_write_int_4(fp,&r->length)==0) return 1;
    if (be_write_int_4(fp,&r->ptr1)==0) return 1;
    if (be_write_int_4(fp,&r->ptr2)==0) return 1;

    return 0;
}


int fix_abi(char *fn,char *old, char *new)
{
    FILE *fp;
    Header header;
    Rec rec;
    int i;
    off_t pl,pn;

    if ( (fp=fopen(fn,"r+b")) == NULL ) {
        fprintf(stderr,"frog: %s: error opening file\n",fn);
	return 1;
    }

    fseek(fp,0,0);
    if (read_header(fp,&header)) {
        fprintf(stderr,"frog: %s: error reading header\n",fn);
        fclose(fp);
        return 2;
    }

    pl = 0;
    pn = header.offset;
    for(i=0;i<header.recs;i++) {
	pl = pn;
	fseek(fp,pn,0);
        if(read_record(fp,&rec)) {
            fprintf(stderr,"frog: %s: error reading record\n",fn);
            fclose(fp);
            return 2;
         }
	pn = ftell(fp);

        if(strncmp(rec.id,"FWO_",4)==0) {
	    char map[4];
	    char tmap[4];

#define ind(base) ((base)=='C'?0:(base)=='A'?1:(base)=='G'?2:3)

/*
	    printf("Changing base to trace mapping (FWO_)\n");
*/

	    /*
	     * map of lanes to old bases
	     */
	    tmap[ind('C')] = (rec.ptr1>>24)&255;
	    tmap[ind('A')] = (rec.ptr1>>16)&255;
	    tmap[ind('G')] = (rec.ptr1>> 8)&255;
	    tmap[ind('T')] = rec.ptr1&255;
	    /*
	     * map of old bases to new bases
	     */
	    map[ind(old[0])] = new[0];
	    map[ind(old[1])] = new[1];
	    map[ind(old[2])] = new[2];
	    map[ind(old[3])] = new[3];
	    /*
	     * map lanes to new bases
	     */
	    rec.ptr1 = (((map[ind(tmap[ind('C')])]<<8)+
			  map[ind(tmap[ind('A')])]<<8)+
			  map[ind(tmap[ind('G')])]<<8)+
			  map[ind(tmap[ind('T')])];

	    fseek(fp,pl,0);
	    if (write_record(fp,&rec)) {
                fprintf(stderr,"frog: %s: error writing record\n",fn);
                fclose(fp);
                return 2;
	    }

        } else if(strncmp(rec.id,"PBAS",4)==0) {
	    char *seq;
	    char map[4];
	    int k;
/*
	    printf("Changing bases (PBAS)\n");
*/
	    seq = (char *) malloc(rec.length);
	    /* read sequence */
	    fseek(fp,rec.ptr1,0);
	    if (fread(seq,rec.length,1,fp)!=1) {
                fprintf(stderr,"frog: %s: error reading sequence\n",fn);
                fclose(fp);
                return 2;
	    }
	    /* swap bases */
	    map[ind(old[0])] = new[0];
	    map[ind(old[1])] = new[1];
	    map[ind(old[2])] = new[2];
	    map[ind(old[3])] = new[3];
	    for (k=0;k<rec.length;k++) if (strchr("ACGT",seq[k])) seq[k] = map[ind(seq[k])];

	    /* write back */
	    fseek(fp,rec.ptr1,0);
	    if (fwrite(seq,rec.length,1,fp)!=1) {
                fprintf(stderr,"frog: %s: error writing sequence\n",fn);
                fclose(fp);
                return 2;
	    }
        }

    }

    fclose(fp);
    return 0;
}


void usage()
{
    fprintf(stderr,"Usage: frog old new [ABI_files...]\n\n");
    fprintf(stderr,"examples\n");
    fprintf(stderr,"To swap all Gs with Ts and Ts with Gs\n    frog CAGT CATG trace.abi\n");
    fprintf(stderr,"To swap all As with Cs, Cs with Gs and Gs with As\n    frog ACGT CGAT trace.abi\n");
}

int check(char *bases)
{
    int i;
    int map[4];
    if (strlen(bases)!=4) {
	fprintf(stderr,"frog: should be four bases %s\n",bases);
	return 1;
    }

    for(i=0;i<4;i++) map[i]=0;
    for(i=0;i<4;i++) {
	if (islower(bases[i])) bases[i]=toupper(bases[i]);
        if(strchr("ACGT",bases[i])==NULL) {
	    fprintf(stderr,"frog: invalid base '%c' in bases \"%s\"\n",bases[i],bases);
	    return 1;
	}
	map[ind(bases[i])] = 1;
    }
    for(i=0;i<4;i++) {
	if (map[i]==0) {
	    fprintf(stderr,"frog: repeated bases in \"%s\"\n",bases);
	    return 1;
        }
    }

    return 0;
}

int main(int argc, char **argv)
{
    int i;

    if (argc<3) {usage(); exit(1);}
    if (check(argv[1])) exit(1);
    if (check(argv[2])) exit(1);
	
    for(i=3;i<argc;i++) {
	int err;
	if (err=fix_abi(argv[i],argv[1],argv[2])) printf("Couldn't read %s (%d)\n",argv[i],err);
    }
    return 0;
}
