/* #define DEBUG_OLIGO_SEL */

#define SUBVERSION

#include <stdio.h>
#include <stdlib.h>
#include "oligocom.h"
#include "defn.h"
#include "tagUtils.h"
#include "edUtils.h"
#include "fort.h"

#define DSTR stdout

#define MAXCOMLEN (100)

/* Static global variables */
static int_f last_gel;	/* remembers where we last were for		*/
    			/* for efficiency (defaults leftmost)		*/
static char primfilename[100];

/* --- hooks --- */

int insert_size() {
    return 1000;
}

int avg_read_len() {
    return 400;
}

/*
 * Do the actual oligo selection.
 * Returns the new offset to search from. (This takes into account the
 * average gel reading length so that we do not finish we lots of suggested
 * oligos next to each other.)
 */
int find_oligos(int_f offset,   int_f *relpg_p,  int_f *lngthg_p,
		int_f *rnbr_p,	char *cons_p,    char  *comment,
		char *oligo,    char *consensus, int_f *idevn_p,
		char  sense,    FILE *outfile,   int_f olilen,
		int_f olibak,   int_f *temnum,   char *datnam,
		int_f olinum,	int_f lincon)
{
    int i, cur_gel, oligostart, choice, oligolen, oligoend;
    int bestscore = 0, bestgel = 0, template = 0, score, ok, olis;
    char gelname[DB_NAMELEN + 1];

    /*
     * Find oligo based on this position.
     * We choose a region starting OLISTART back from here, extending
     * for OLILEN bases.
     */
    olis = offset - (olilen + olibak);
    if (olis < 1)
	olis = 1;
    memcpy(consensus, &cons_p[olis], olilen);
    consensus[olilen+1] = '\0';
    
    for (i=0;i<MAX_NUM_OLIGOS; i++) OSP_RESULTS[i].score = 0.0;
    ok = osp_analyse(OSP_RESULTS,consensus,&prm,screens,score_info);

    if (ok == 0) {
	return -1;
    }

#ifdef DEBUG_OLIGO_SEL
    fprintf(DSTR, "osp_analyse() = %d\n", ok);
#endif

#ifdef DEBUG_OLIGO_SEL
    i = 0;
    while (OSP_RESULTS[i].score != 0) {
	fprintf(DSTR, "Oligo found at %d, score = %f, '%.*s'\n",
	       OSP_RESULTS[i].start_position + offset - (olilen+olibak),
	       OSP_RESULTS[i].score,
	       OSP_RESULTS[i].end_position -
	       OSP_RESULTS[i].start_position + 1,
	       &consensus[OSP_RESULTS[i].start_position]);
	i++;
    }
#endif

    /*
     * Choose the first in the list (for the time being). This should be
     * the oligo with the best score.
     */

    /* choice = 0; */
    
    oligostart = 0;
    /* Choose the first 'olinum' oligos from the list */
    for (choice = 0; choice < olinum; choice++) {

	/*
	 * In case we don't have as many oligos to chose from as the user
	 * requested
	 */
	if (OSP_RESULTS[choice].score == 0) 
	    return oligostart + avg_read_len();

	memset(oligo, 0, olilen);
	memcpy(oligo, &consensus[OSP_RESULTS[choice].start_position],
	       OSP_RESULTS[choice].end_position - OSP_RESULTS[choice].
	       start_position + 1);

#ifdef DEBUG_OLIGO_SEL
    fprintf(DSTR, "Choosing oligo '%s'\n", oligo);
#endif

	/* find sequences that could yield a valid primer */
	cur_gel = last_gel;
	oligostart = OSP_RESULTS[choice].start_position + offset -
	    (olilen+olibak);
	oligolen = OSP_RESULTS[choice].end_position
	    - OSP_RESULTS[choice].start_position;

#ifdef DEBUG_OLIGO_SEL
	fprintf(DSTR, "oligo starts %d, ends %d\n",
		oligostart, oligostart + oligolen);
#endif

	do {
	    /* only look for positive directing sequences */
	    if (lngthg_p[cur_gel] >= 0) {
		/* stop when we've gone past our oligo */
		if (relpg_p[cur_gel] >= oligostart)
		    break;
		/*
		 * If the sequence ends past end of oligo then we have a
		 * candidate template.
		 */
		if (relpg_p[cur_gel] + insert_size() - avg_read_len() 
		    >= oligostart + oligolen) {
		    /* remember possible template */
		    if (relpg_p[cur_gel] + lngthg_p[cur_gel] >
			oligostart + oligolen)
			template = cur_gel;
		    /*
		     * Choose the gel that is furthest right (nearest to the
		     * oligo), but that also has a large amount of data to
		     * the right of the oligo. So if the right most gel is
		     * a short reading (maybe due to bad template) then we
		     * choose the next most right gel instead.
		     */

#ifdef DEBUG_OLIGO_SEL
		    fprintf(DSTR, "Covered by gel no. %d (ends %d)\n",
			    cur_gel, relpg_p[cur_gel] + lngthg_p[cur_gel]);
#endif

		    /* Base 'score' on nearness to oligo, and length */
		    score = avg_read_len() - (oligostart - relpg_p[cur_gel])
			+ 0.5 * lngthg_p[cur_gel];
		    if (score > bestscore)
			bestgel = cur_gel, bestscore = score;
		}
	    }
	    cur_gel = rnbr_p[cur_gel];
	} while (cur_gel != 0);
	
	if (bestgel == 0)
	    return -1;
#ifdef DEBUG_OLIGO_SEL
	fprintf(DSTR, "Choosing gel no. %d for template\n", bestgel);
#endif

	/*
	 * Create tag - far more convoluted than it needs to be. We cannot tag
	 * the consensus so we have to tag a sequence instead. Preferably we
	 * use the template, failing that a sequence in the same direction, or
	 * failing that any sequence that at least covers the region.
	 */
	oligoend = oligostart + oligolen;
	if (bestgel && relpg_p[bestgel] + lngthg_p[bestgel] >= oligoend)
	    template = bestgel;
	else if (template == 0) {
	    /* no template in this direction */
	    cur_gel = last_gel;
	    while (cur_gel != 0) {
		if ((relpg_p[cur_gel] + 
		     (lngthg_p[cur_gel]>0?lngthg_p[cur_gel]:-lngthg_p[cur_gel]
		      )) > oligoend) {
		    template = cur_gel;
		    break;
		}
		cur_gel = rnbr_p[cur_gel];
	    }
	}
	if (template) {
	    /*
	     * In the comment we use 'bestgel' as the template even if we
	     * are tagging a different sequence.
	     */
	    readn_(idevn_p, &bestgel, gelname, (int_fl)(DB_NAMELEN));
	    Fstr2Cstr(gelname, DB_NAMELEN, gelname, DB_NAMELEN);

#ifdef DEBUG_OLIGO_SEL
	    fprintf(DSTR, "Using gel no. %d for template\n", template);
#endif
	    sprintf(comment, "Template=%s\nName=%s.%d\nSequence=%s\n",
		    gelname, datnam, *temnum, oligo);

	    insert_NEW_tag(template, oligostart - relpg_p[template] + 1,
			   oligolen + 1, "OLIG", comment); 

#ifdef DEBUG_OLIGO_SEL
	    puts(comment);
#endif
	    ok = fprintf(outfile, "%s.%d %s %s (@ %d ) %c\n",
			 datnam, *temnum, gelname, oligo, oligostart, sense);
	    if (ok < 0 || ok == EOF) {
		puts("*** Failed to write to output file");
		return -1;
	    }
	    fprintf(stdout, "At %d - template %s, primer %s, number %d\n",
		    (sense == '+') ? offset : relpg_p[lincon] - offset +1,
		    gelname, oligo,	(*temnum)++);
	    fflush(stdout);
	    updout_();
	} else {
	    fprintf(stdout, "At %d - no suitable oligos found\n",
		    (sense == '+') ? offset : relpg_p[lincon] - offset +1);
	    return -1;
	}
	updout_();
    }
    return oligostart + avg_read_len();
}

/*
 * Find oligos for a contig region. Output a list of suggested experiments
 * to extend into problem areas (such as single stranded sections or the
 * ends of the contig
 */
void olisel_(
     int_f *relpg_p,	/* int array - relative position (gel reading) */
			/*        or - length of contig */
     int_f *lngthg_p,	/* int array - length of gel */
			/*        or - empty for contig */
     int_f *lnbr_p,	/* int array - left nodes */
     int_f *rnbr_p,	/* int array - right nodes */
     char  *qual_p,	/* char array - quality of contig */
     char  *cons_p,	/* char array - consensus of contig */
     int_f *llino_p,	/* int - left gel in contig */
     int_f *lincon_p,	/* int - record no. of contig */
     int_f *lreg_p,	/* int - left start in qual */
     int_f *rreg_p,	/* int - right start in qual */
     int_f *idevn_p,	/* int - stream of name file */
     char  *sense,	/* char array - actually single char */
     int_f *olilen_p,	/* int - length of oligo selection region */
     int_f *olibak_p, 	/* int - offset bak of start of oli sel region */
     int_f *lstrt_p,	/* int - start (not offset) of oli sel region */
     int_f *temnum_p,	/* int - start template number */
     char  *datnam_p,	/* string - name of database */
     int_f *olinum_p,	/* int - number of oligos per template */
     int_f *datnaml,	/* int length - of database name */
     int_fl datnam_l	/* length - of elements in datnam (==1)*/
     )
{
    register int_f i, j, l;
    int_f rreg = *rreg_p;	/* faster copy of *rreg_p */
    int_f lreg = *lreg_p;
    char *consensus, comment[MAXCOMLEN], *oligo;
    static FILE *outfile;
    char fname[256];

    strncpy(fname, datnam_p, *datnaml);
    fname[*datnaml] = '\0';
    oligo = (char *)malloc(*olilen_p+1);
    consensus = (char *)malloc(*olilen_p+1);
    last_gel = *llino_p;

    osp_initialise();

    if (*sense == '+' && (outfile = fopen(primfilename, "w")) == NULL) {
	fprintf(stdout, "Failed to open 'oligo_sel.out'\n");
	return;
    }

    relpg_p--;
    lngthg_p--;
    lnbr_p--;
    rnbr_p--;
    qual_p--;
    cons_p--;

    /* scan through quality buffer */
    /* Do not look at the very left hand end. */
    if (lreg < (*olilen_p + *olibak_p + 1))
	lreg = (*olilen_p + *olibak_p + 1);
    for (i = lreg; i<=rreg; i++) {

	/* strong negative strand, but no positive strand */
	if (qual_p[i] == '2' || qual_p[i] == '8') {

	    /* find length of single stranded section */
	    j = i;
	    while(qual_p[j] == '2' || qual_p[j] == '8')
		j++;

#ifdef DEBUG_OLIGO_SEL
	    fprintf(DSTR, "Single strand at %d - %d, len %d\n", i, j, j-i);
#endif
/*
	    fprintf(stdout, "At %d - ", (*sense == '+') ? i :
		    relpg_p[*lincon_p] - i + 1);
*/
	    l = find_oligos(i,         relpg_p,  lngthg_p,  rnbr_p,
			    cons_p,    comment,  oligo,     consensus,
			    idevn_p,   *sense,   outfile,   *olilen_p,
			    *olibak_p, temnum_p, fname,     *olinum_p,
			    *lincon_p);
	    if (l != -1)
		i = (int_f)l;
/*
	    else
		puts("No suitable oligos found");
*/

	    if (i < j)
		i = j;
	}
    }
    
    free(oligo);
    free(consensus);
    osp_cleanup();
    if (*sense == '-' && outfile) {
	fclose(outfile);
    }
}

/*
 * Initialises the olisel_() function by asking the required questions to
 * the user. The only question here is the filename - the others are currently
 * written in FORTRAN.
 */
void olinit_(int_f *status_p,   int_f *olilen_p, int_f *olibak_p,
	     int_f *dialogue_p, int_f *maxgel_p, int_f *temnum_p,
	     int_f *numoli_p) {
    if (gtstr("Name of file for primers", "primers", primfilename,
	      sizeof(primfilename)) == -1) {
	*status_p = -1;
	return;
    }

    *temnum_p = getint(1, 9999, 1, "Start oligo number", status_p);
    if (*status_p < 0) {
	*status_p = -1; return;
    }

    if (*dialogue_p) {
	*olibak_p = getint(1, *maxgel_p, 20, 
			   "Start of oligo choice region", status_p);
	if (*status_p < 0) {
	    *status_p = -1; return;
	}

	*olilen_p = getint(*olibak_p, *maxgel_p, *olibak_p+40, 
			   "End of oligo choice region", status_p)-*olibak_p;
	if (*status_p < 0) {
	    *status_p = -1; return;
	}
	
	*numoli_p = getint(1, 99, 2, "Number of oligos per region", status_p);
	if (*status_p < 0) {
	    *status_p = -1; return;
	}
	
	*status_p = 0;
	return;
    } else {
	*olibak_p = 20;
	*olilen_p = 60;
	*numoli_p = 2;
    }
}
