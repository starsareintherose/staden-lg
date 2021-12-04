/*
 * File: scfIO.c
 *
 * Author: Simon Dear
 *         MRC Laboratory of Molecular Biology
 *         Hills Road
 *         Cambridge CB2 2QH
 *         United Kingdom
 *
 * Description: IO routines for reading and writing SCF files
 *
 * Created: 8 October 1992 (from seqIOSCF.c)
 * Updated:
 *
 */

#include <stdio.h>
#include <sys/types.h>
#include "mach-io.h"
#include "scfIO.h"

#define False 0
#define True  1

int read_scf_header(FILE *fp, Header *h)
{
    if (be_read_int_4(fp,&h->magic_number)==False) return (False);
    if (be_read_int_4(fp,&h->samples)==False) return (False);
    if (be_read_int_4(fp,&h->samples_offset)==False) return (False);
    if (be_read_int_4(fp,&h->bases)==False) return (False);
    if (be_read_int_4(fp,&h->bases_left_clip)==False) return (False);
    if (be_read_int_4(fp,&h->bases_right_clip)==False) return (False);
    if (be_read_int_4(fp,&h->bases_offset)==False) return (False);
    if (be_read_int_4(fp,&h->comments_size)==False) return (False);
    if (be_read_int_4(fp,&h->comments_offset)==False) return (False);

    return (True);
}


int read_scf_sample(FILE *fp, Samples *s)
{
    if (be_read_int_1(fp,&s->sample_A)==False) return (False);
    if (be_read_int_1(fp,&s->sample_C)==False) return (False);
    if (be_read_int_1(fp,&s->sample_G)==False) return (False);
    if (be_read_int_1(fp,&s->sample_T)==False) return (False);

    return (True);
}



int read_scf_base(FILE *fp, Bases *b)
{
    if (be_read_int_4(fp,&b->peak_index)==False) return (False);
    if (be_read_int_1(fp,&b->prob_A)==False) return (False);
    if (be_read_int_1(fp,&b->prob_C)==False) return (False);
    if (be_read_int_1(fp,&b->prob_G)==False) return (False);
    if (be_read_int_1(fp,&b->prob_T)==False) return (False);
    if (be_read_int_1(fp,(uint_1 *)&b->base)==False) return (False);
    if (be_read_int_1(fp,&b->spare[0])==False) return (False);
    if (be_read_int_1(fp,&b->spare[1])==False) return (False);
    if (be_read_int_1(fp,&b->spare[2])==False) return (False);

    return (True);
}








int write_scf_header(FILE *fp, Header *h)
{
    int i;

    if (be_write_int_4(fp,&h->magic_number)==False) return (False);
    if (be_write_int_4(fp,&h->samples)==False) return (False);
    if (be_write_int_4(fp,&h->samples_offset)==False) return (False);
    if (be_write_int_4(fp,&h->bases)==False) return (False);
    if (be_write_int_4(fp,&h->bases_left_clip)==False) return (False);
    if (be_write_int_4(fp,&h->bases_right_clip)==False) return (False);
    if (be_write_int_4(fp,&h->bases_offset)==False) return (False);
    if (be_write_int_4(fp,&h->comments_size)==False) return (False);
    if (be_write_int_4(fp,&h->comments_offset)==False) return (False);
    for (i=0;i<23;i++)
	if (be_write_int_4(fp,&h->spare[i])==False) return (False);

    return (True);
}


int write_scf_sample(FILE *fp, Samples *s)
{
    if (be_write_int_1(fp,&s->sample_A)==False) return (False);
    if (be_write_int_1(fp,&s->sample_C)==False) return (False);
    if (be_write_int_1(fp,&s->sample_G)==False) return (False);
    if (be_write_int_1(fp,&s->sample_T)==False) return (False);

    return (True);
}





int write_scf_base(FILE *fp, Bases *b)
{
    if (be_write_int_4(fp,&b->peak_index)==False) return (False);
    if (be_write_int_1(fp,&b->prob_A)==False) return (False);
    if (be_write_int_1(fp,&b->prob_C)==False) return (False);
    if (be_write_int_1(fp,&b->prob_G)==False) return (False);
    if (be_write_int_1(fp,&b->prob_T)==False) return (False);
    if (be_write_int_1(fp,(uint_1 *)&b->base)==False) return (False);
    if (be_write_int_1(fp,&b->spare[0])==False) return (False);
    if (be_write_int_1(fp,&b->spare[1])==False) return (False);
    if (be_write_int_1(fp,&b->spare[2])==False) return (False);

    return (True);
}



int write_scf_comment(FILE *fp, Comments *c, size_t l)
{
    if (fwrite(c, l, 1, fp) !=1) return (False);
    return (True);

}
