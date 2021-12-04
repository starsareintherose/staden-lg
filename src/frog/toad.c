/*
 * File: toad.c
 *
 * Author: Simon Dear
 *         MRC Laboratory of Molecular Biology
 *         Hills Road
 *         Cambridge CB2 2QH
 *         United Kingdom
 *
 * Description: utility to alter base lane ordering of an SCF file
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
#include "scfIO.h"
#include "mach-io.h"

#define ind(base) ((base)=='C'?0:(base)=='A'?1:(base)=='G'?2:3)

int fix_scf(char *fn,char *old, char *new)
{
    FILE *fp;
    Header header;
    Samples *samples;
    Bases *bases;
    int i;
    char map[4];

    if ( (fp=fopen(fn,"r+b")) == NULL ) {
        fprintf(stderr,"toad: %s: error opening file\n",fn);
	return 1;
    }

    fseek(fp,0,0);
    if (read_scf_header(fp,&header)==0) {
        fprintf(stderr,"toad: %s: error reading header\n",fn);
        fclose(fp);
        return 2;
    }

    /* swap samples */
    samples = (Samples *)malloc(sizeof(Samples)*header.samples);
    if (samples==NULL) {
        fprintf(stderr,"toad: %s: out of memory\n",fn);
        fclose(fp);
        return 2;
    }
    fseek(fp,header.samples_offset,0);
    for(i=0;i<header.samples;i++) {
	if (read_scf_sample(fp,&samples[i])==0) {
            fprintf(stderr,"toad: %s: error reading sample %d\n",fn,i);
            fclose(fp);
            return 2;
        }
    }
    for(i=0;i<header.samples;i++) {
	uint_1 smap[4];
	uint_1 osmap[4];
	osmap[ind('A')] = samples[i].sample_A;
	osmap[ind('C')] = samples[i].sample_C;
	osmap[ind('G')] = samples[i].sample_G;
	osmap[ind('T')] = samples[i].sample_T;
	smap[ind(new[0])] = osmap[ind(old[0])];
	smap[ind(new[1])] = osmap[ind(old[1])];
	smap[ind(new[2])] = osmap[ind(old[2])];
	smap[ind(new[3])] = osmap[ind(old[3])];
	samples[i].sample_A = smap[ind('A')];
	samples[i].sample_C = smap[ind('C')];
	samples[i].sample_G = smap[ind('G')];
	samples[i].sample_T = smap[ind('T')];
    }
    fseek(fp,header.samples_offset,0);
    for(i=0;i<header.samples;i++) {
	if (write_scf_sample(fp,&samples[i])==0) {
            fprintf(stderr,"toad: %s: error writing sample %d\n",fn,i);
            fclose(fp);
            return 2;
        }
    }
    free(samples);


    /* swap bases */
    bases = (Bases *)malloc(sizeof(Bases)*header.bases);

    if (bases==NULL) {
        fprintf(stderr,"toad: %s: out of memory\n",fn);
        fclose(fp);
        return 2;
    }
    fseek(fp,header.bases_offset,0);
    for(i=0;i<header.bases;i++) {
	if (read_scf_base(fp,&bases[i])==0) {
            fprintf(stderr,"toad: %s: error reading bases %d\n",fn,i);
            fclose(fp);
            return 2;
        }
    }
    map[ind(old[0])] = new[0];
    map[ind(old[1])] = new[1];
    map[ind(old[2])] = new[2];
    map[ind(old[3])] = new[3];
    for(i=0;i<header.bases;i++) {
	uint_1 smap[4];
	uint_1 osmap[4];
	osmap[ind('A')] = bases[i].prob_A;
	osmap[ind('C')] = bases[i].prob_C;
	osmap[ind('G')] = bases[i].prob_G;
	osmap[ind('T')] = bases[i].prob_T;
	smap[ind(new[0])] = osmap[ind(old[0])];
	smap[ind(new[1])] = osmap[ind(old[1])];
	smap[ind(new[2])] = osmap[ind(old[2])];
	smap[ind(new[3])] = osmap[ind(old[3])];
	bases[i].prob_A = smap[ind('A')];
	bases[i].prob_C = smap[ind('C')];
	bases[i].prob_G = smap[ind('G')];
	bases[i].prob_T = smap[ind('T')];
	if (strchr("ACGT",bases[i].base)) bases[i].base = map[ind(bases[i].base)];
    }
    fseek(fp,header.bases_offset,0);
    for(i=0;i<header.bases;i++) {
	if (write_scf_base(fp,&bases[i])==0) {
            fprintf(stderr,"toad: %s: error writing base %d\n",fn,i);
            fclose(fp);
            return 2;
        }
    }
    free(bases);







    fclose(fp);
    return 0;
}


void usage()
{
    fprintf(stderr,"Usage: toad old new [SCF_files...]\n\n");
    fprintf(stderr,"examples\n");
    fprintf(stderr,"To swap all Gs with Ts and Ts with Gs\n    toad CAGT CATG trace.scf\n");
    fprintf(stderr,"To swap all As with Cs, Cs with Gs and Gs with As\n    toad ACGT CGAT trace.scf\n");
}

int check(char *bases)
{
    int i;
    int map[4];
    if (strlen(bases)!=4) {
	fprintf(stderr,"toad: should be four bases %s\n",bases);
	return 1;
    }

    for(i=0;i<4;i++) map[i]=0;
    for(i=0;i<4;i++) {
	if (islower(bases[i])) bases[i]=toupper(bases[i]);
        if(strchr("ACGT",bases[i])==NULL) {
	    fprintf(stderr,"toad: invalid base '%c' in bases \"%s\"\n",bases[i],bases);
	    return 1;
	}
	map[ind(bases[i])] = 1;
    }
    for(i=0;i<4;i++) {
	if (map[i]==0) {
	    fprintf(stderr,"toad: repeated bases in \"%s\"\n",bases);
	    return 1;
        }
    }

    return 0;
}

int main(int argc, char **argv)
{
    int i;

/*
    fprintf(stderr,"Eeek! Not finished yet.\n");
    return 1;
*/

    if (argc<3) {usage(); exit(1);}
    if (check(argv[1])) exit(1);
    if (check(argv[2])) exit(1);
	
    for(i=3;i<argc;i++) {
	int err;
	if (err=fix_scf(argv[i],argv[1],argv[2])) printf("Couldn't read %s (%d)\n",argv[i],err);
    }
    return 0;
}
