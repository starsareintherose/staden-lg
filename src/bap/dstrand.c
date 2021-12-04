/*#define DEBUG_DSTRAND 1*/

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "dstrand.h"
#include "guk.h"
#include "edUtils.h"
#include "fort.h"
#include "misc.h"

/* Static global variables */
static int_f last_gel;	/* remembers where we last were for		*/
    			/* for efficiency (defaults leftmost)		*/

static int countdb;	/* how many bases have been double stranded	*/
static int consins;	/* how many insertions into consensus		*/

/*
 * SunOS doesn't have a memmove() function, so we use bcopy() instead.
 */
#ifdef NOMEMMOVE
#    define memmove(a,b,c) bcopy(b,a,c)
#endif

#ifdef notdef
/*----------------------------------------------------------------------*/

#define MAXINS (3)	/* maximum number of neighbouring inserts	*/
#define INSFREQ (15)	/* lowest len/inserts frequency allowed		*/

/*
 * Evaluates how good an alignment is.
 * The integer returned is how much of the alignment to trust (the length
 * of this trusted part).
 */
static int evalal(char *seq1, char *seq2, int *albuf, int_f seqlen) {
    int continserts, freqinserts, scan;
    
    /* display the alignment as it stands */
#if DEBUG_DSTRAND
    dispmm_(seq1, &seqlen, seq2, &seqlen, albuf, 0, 0);
#endif

    /* end if more than MAXINS inserts or len/inserts < INSFREQ */
    continserts = 0;
    freqinserts = -1;
    for (scan = 0; scan < seqlen; scan++) {
	if (albuf[scan] != 0) {
	    continserts++;
	    freqinserts++;
	    if (continserts >= MAXINS) {
		scan-=continserts;
		break;
	    }
	    if (freqinserts && scan/freqinserts <= INSFREQ) {
		scan--;
		break;
	    }
	} else
	    continserts = 0;
    }
#if DEBUG_DSTRAND
    fprintf(DSTR, "scan:%d, conti:%d, freqi:%d\n",
	    scan, continserts, freqinserts);
#endif

    return scan;
}
#endif

#ifdef notdef
/*----------------------------------------------------------------------*/

/*
 * Evaluates how good an alignment is.
 * The integer returned is how much of the alignment to trust (the length
 * of this trusted part).
 */
static int evalal(char *seq1, char *seq2, int *albuf, int_f seqlen) {
    int op = 0, scan = 0, score = 20;
    float mismatch = 0, cutgaps = 0, congaps = 0;
    int *alptr = albuf;
    char *s1ptr = seq1, *s2ptr = seq2;
    
    /* display the alignment as it stands */
#if DEBUG_DSTRAND
    dispmm_(seq1, &seqlen, seq2, &seqlen, albuf, 0, 0);
#endif

    /*
     * Need to add max_score - cut the score off when it gets too high so
     * we cannot accumalate good data and hence resistance to bad data.
     */
    while (scan++ < seqlen) {
	mismatch*=.95;
	cutgaps*=.95;
	congaps*=.95;
	if (op == 0 && *alptr == 0) {
	    if (*s1ptr == *s2ptr) {
#if DEBUG_DSTRAND == 2
		fprintf(DSTR, "'%c' = '%c'", *s1ptr, *s2ptr);
#endif
		score++;
	    } else {
#if DEBUG_DSTRAND == 2
		fprintf(DSTR, "'%c' ! '%c'", *s1ptr, *s2ptr);
#endif
		score-=11;
		mismatch++;
	    }
	    alptr++;
	    s1ptr++;
	    s2ptr++;
	} else {
	    if (op == 0) {
		op = *alptr++;
	    }

	    if (op > 0) {
#if DEBUG_DSTRAND == 2
		fprintf(DSTR, "' ' - '%c'", *s2ptr);
#endif
		op--;
		s2ptr++;
		cutgaps++;
		mismatch++;
		score-=(cutgaps*2);
	    } else /* op < 0 */ {
#if DEBUG_DSTRAND == 2
		fprintf(DSTR, "'%c' - ' '", *s1ptr);
#endif
		op++;
		s1ptr++;
		congaps++;
		mismatch++;
		score-=(congaps*2);
	    }
	}
#if DEBUG_DSTRAND == 2
	fprintf(DSTR," score=%4d, mismatch=%2.2f, gaps(cut)=%2.2f, gaps(con)=%2.2f, off=%d\n",
	score, mismatch, cutgaps, congaps, scan); 
#endif
	
	if (mismatch > 3.5 || cutgaps > 3 || congaps > 1)
	    break;
    }
    return scan -1;
}
#endif

/*----------------------------------------------------------------------*/

/*
 * Evaluates how good an alignment is.
 * The integer returned is how much of the alignment to trust (the length
 * of this trusted part).
 */
static int evalal(char *seq1, char *seq2, int *albuf, int_f seqlen,
		  int_f maxmis, int_f missc, int_f matchsc, int_f padsc) {
    int score = 0, op = 0, scan = 0, miscount = 0, bestscan = 0, bestscore = 0;
    int *alptr = albuf;
    char *s1ptr = seq1, *s2ptr = seq2;

    /* display the alignment as it stands */
#if DEBUG_DSTRAND
    dispmm_(seq1, &seqlen, seq2, &seqlen, albuf, 0, 0);
#endif

    while (scan < seqlen && miscount <= maxmis) {
	if (score >= bestscore) {
	    bestscore = score;
	    bestscan = scan;
	}
	if (op == 0 && *alptr == 0) {
	    if (*s1ptr == *s2ptr) {
		/* correct match */
		/*
		 * Note *FAILURE* here to do reasonable things if both the
		 * aligned sequences happen to contain a '-'. It will simply
		 * assume that both the sequences agree with each other and
		 * give them a positive score. However for speeds sake it's
		 * one less thing to check and the end result is that very
		 * ocassionally we'll double strand something we should have.
		 * This will show up in the quality checks so it'll still
		 * get dealt with.
		 */
#if DEBUG_DSTRAND
		fprintf(DSTR, "'%c' = '%c'", *s1ptr, *s2ptr);
#endif
		score += matchsc;
	    } else {
		/* incorrect match */
		/*
		 * Should we count less for failing to match our new sequence
		 * against the existing one (consensus in this case) then the
		 * existing one happens to be a '-' (ie unknown). In this case
		 * if the consensus is unknown should we penalise the sequence
		 * regardless? Currently we do....
		 */
#if DEBUG_DSTRAND
		fprintf(DSTR, "'%c' ! '%c'", *s1ptr, *s2ptr);
#endif
		score += missc;
		miscount++;
	    }
	    alptr++;
	    s1ptr++;
	    s2ptr++;
	} else {
	    miscount++;
	    if (op == 0) {
		op = *alptr++;
	    }
	    if (op > 0) {
#if DEBUG_DSTRAND
		fprintf(DSTR, "' ' - '%c'", *s2ptr);
#endif
		/* pad in sequence */
		score += padsc;
		op--;
		s2ptr++;
	    } else {
#if DEBUG_DSTRAND
		fprintf(DSTR, "'%c' - ' '", *s1ptr);
#endif
		/* pad in consensus */
		score += padsc;
		op++;
		s1ptr++;
	    }
	}
#if DEBUG_DSTRAND
	fprintf(DSTR, " score=%4d, off=%3d, miscount=%d\n",
		score, scan, miscount);
#endif
	scan++;
    }

    return bestscan;
}

/*----------------------------------------------------------------------*/

/*
 * Generates two buffers from an alignment. Each buffer represents a sequence
 * of edits to perform on the contig in an easily parsable fashion.
 */
static void dstrform(char *seq1, int *albuf, int_f seqlen, char *new1,
		     char *new2) {
    int_f scan = 0;
    int op = 0;
    char *ptr1 = new1, *ptr2 = new2, *seq1ptr = seq1;
    int *alptr = albuf;

    while (scan++ < seqlen) {
	*ptr1 = '.';
	*ptr2 = '.';
	if (op == 0 && *alptr == 0) {
	    alptr++;
	    *ptr1 = *seq1ptr++;
	} else {
	    if (op == 0)
		op = *alptr++;
	    if (op > 0) {
		*ptr1 = '*';
		op--;
	    } else {
		*ptr2 = '*';
		*ptr1 = *seq1ptr++;
		op++;
	    }
	}
	ptr1++;
	ptr2++;
    }
    *ptr1 = '\0';
    *ptr2 = '\0';
}

/*----------------------------------------------------------------------*/

/*
 * patch() will attempt to 'patch' up a single stranded section into a double
 * stranded section. The integer returned is the actual length we managed to
 * double strand.
 */
static int patch(
     int_f *relpg_p,	/* int array - relative position (gel reading)	*/
			/*        or - length of contig 		*/
     int_f *lngthg_p,	/* int array - length of gel 			*/
			/*        or - empty for contig 		*/
     int_f *lnbr_p,	/* int array - left nodes			*/
     int_f *rnbr_p,	/* int array - right nodes			*/
     int_f  maxgel,	/* maximum gel length				*/
     int_f  off,	/* current relative offset in contig		*/
     int_f  plen,	/* length of hole to patch			*/
     char  *cons_p,	/* the consensus				*/
     int_f *ngels_p,	/* int - passed back to fortran			*/
     int_f *kbout_p,	/* int - passed back to fortran (output stream)	*/
     int_f *idbsiz_p,	/* int - passed back to fortran			*/
     char  *gel_p,	/* char array - passed back to fortran (tmp buf)*/
     int_f *idevr_p,	/* int - passed back to fortran (relations db)	*/
     int_f *idevw_p,	/* int - passed back to fortran (working db)	*/
     int_f *idevn_p,	/* int - passed back to fortran (name db)	*/
     int_f *lincon_p,	/* int - passed back to fortran			*/
     int_f *nconts_p,	/* int - passed back to fortran			*/
     int_f *cons_e,	/* int - extension of cons_p 			*/
     int_fl gel_l,	/* int - length of gel_p 			*/
     int_f  rreg,	/* length of consensus buffer (?!?)		*/
     int_f  maxmis,	/* int - maximum number of mismatches		*/
     int_f  missc,	/* int - score for mismatch			*/
     int_f  matchsc,	/* int - score of match				*/
     int_f  padsc,	/* int - score for pad				*/
     int_f  sense	/* int - direction of double stranding		*/
     )
{
    /* Local variables */
    int sc;		/* a score for how good the alignment is 	*/
    int_f cur_gel;	/* record of which gel we're looking at 	*/
    int_f bestgeln = 0;	/* the best gel num found so far		*/
    int bestlen = 0;	/* longest 'patch' coverage found so far	*/
    int bestuse = 0;	/* longest needed gel extension found so far	*/
    int *albuf = (int *)malloc(maxgel * 2 * sizeof(int));
			/* buffer for aligned sequences			*/
    int_f fail;		/* does getext() succeed?			*/
    char *cutbuf = (char *)malloc(maxgel);
			/* buffer for cutoff data			*/
    int_f cutlen;	/* length of cutoff data			*/
    int_f tgelend, gelend;
			/* end of current gel (t = including cutoff)	*/
    int tmp, len, len2, one = 1;
    int_f tmp_f, posn;
    char *newb1, *newb2;
    char gelname[DB_NAMELEN + 1];

    /*
     * We're going to try to 'patch' this single strand section.
     * So we need to find which gels (in the positive direction)
     * exist at the start of the 'hole'.
     */
    
    cur_gel = last_gel;
    do {
	/* positive only */
	if (lngthg_p[cur_gel] >= 0) {
	    /* are we two far past? */
	    if (relpg_p[cur_gel] >= off)
		break;
	    /*
	     * If the end of the gel covers this gap (ie NOT the
	     * cutoff data) then we've obviously found a bit like so:
	     * (unlikely though it is).
	     * -------AGAAT------->
	     * -------GAGCC------->
	     * <-------------------
	     */
	    gelend = relpg_p[cur_gel] + lngthg_p[cur_gel];
	    /*
	     * if (gelend > off)
	     *     return plen;
	     */

	    if (relpg_p[cur_gel] + maxgel > off) {
		/* get cutoff data so we can compute total length */
		fail = 0;
		cutlen = maxgel;
		getext_(&cur_gel, cutbuf, &cutlen, &fail,
			(int_fl)sizeof(cutbuf));
		tgelend = gelend + cutlen;
	    } else 
		fail = 1;

	    if (!fail && tgelend >= off) {

#if DEBUG_DSTRAND
		fprintf(DSTR, "covered by gel %d\n", cur_gel);
#endif
		/*
		 * now do a quick(?) alignment check to see if better than
		 * any we've found before.
		 *
		 * we need to only align with as little as possible. That is
		 * do not align further than 10 bases past the end of the
		 * 'hole'. (10 is some arbitrary amount to account for the
		 * insertion of padding characters).
		 */
		if (tgelend >= (tmp_f = off + plen + ALEXTRA)) {
		    cutlen = tmp_f - gelend + 1;
		    tgelend = gelend + cutlen;
		}
		if (tgelend > (tmp_f = off + plen)) {
		    cutlen -= tgelend - tmp_f -1;
		    tgelend = gelend + cutlen;		    
		}

		/*
		 * align maximum possible and 'evaluate' alignment (walking
		 * left to right along albuf[]).
		 */
		sc = mmalign(cutbuf, cutlen, &cons_p[gelend], cutlen,
			     albuf);
		len = evalal(cutbuf, &cons_p[gelend], albuf, cutlen,
			     maxmis, missc, matchsc, padsc);
#if DEBUG_DSTRAND
		fprintf(DSTR, "aligned %d bases @ %d = %d, eval = %d\n",
			cutlen, gelend, sc, len);
#endif
		len2 = len + (int)gelend - (int)off;
#if DEBUG_DSTRAND
		fprintf(DSTR, "overlap of %d (/%d)\n", len2, bestlen);
#endif
		/*
		 * Pick best coverage length.
		 * If equal pick gel with shortest extension.
		 */
		if (len2 > bestlen ||
		    (len2 == bestlen && len < bestuse)) {
		    bestlen = len2;
		    bestuse = len;
		    bestgeln = cur_gel;
		    /* bestgels = ... */
#if DEBUG_DSTRAND
		    fprintf(DSTR, "New best gel! %d(%d)\n", bestgeln, bestlen);
#endif
		}
	    }
	    last_gel = cur_gel;
	}
	/* jump to next element in list */
	cur_gel = rnbr_p[cur_gel];
    } while (cur_gel != 0);

    /*
     * When we've got here, bestlen is the best overlap (upto max 10 more
     * than the hole length) and bestuse is the amount of data needed to
     * be extended for the gel (bestgeln).
     */

    /*
     * find best gel to use - only bother if the extra data would save an
     * experiment. This is when we either totally double strand the section
     * or if we extend by approx the average gel reading length.
     */
    /*
     * Currently cheating - use if over half the hold or longer than 20 bases.
     */
/*    if (bestgeln && (bestlen >= 20 || plen/bestlen < 2)) {*/
#if DEBUG_DSTRAND
    fprintf(DSTR, "bestgeln=%d, bestlen=%d, plen=%d\n",
	    bestgeln, bestlen, plen);
#endif
    if (bestgeln && (bestlen >= 20 || bestlen >= plen)) {
#if DEBUG_DSTRAND
	fprintf(DSTR, "Shall use gel no. %d (len %d x=%d)\n",
		bestgeln, bestlen, bestuse);
#endif

	if (bestgeln != cur_gel) {
	    /* get cutoff data so we can compute total length */
	    cutlen = maxgel;
	    /* do we need to get 'cutlen' amount? why not 'bestuse'? */
	    getext_(&bestgeln, cutbuf, &cutlen, &fail, (int_fl)sizeof(cutbuf));
	}

	/* align an extra couple chars just to allow for a tidy ending */
	gelend = relpg_p[bestgeln] + lngthg_p[bestgeln];
	mmalign(cutbuf, bestuse+2, &cons_p[gelend], bestuse+2, albuf);

	/* temporary */
#if DEBUG_DSTRAND
	dispmm_(cutbuf, &bestuse, &cons_p[gelend], &bestuse, albuf, 0, 0);
#endif

	/* format data in a fashion that is easy to use */
	newb1 = (char *)malloc(bestuse * 2 +1);
	newb2 = (char *)malloc(bestuse * 2 +1);
	dstrform(cutbuf, albuf, bestuse, newb1, newb2);
#if DEBUG_DSTRAND
	fprintf(DSTR, "'%s'\n'%s'\n\n", newb1, newb2);
#endif

/*
	countdb += bestlen>plen?plen:bestlen;
	printf("Double stranded %d bases at offset %d\n",
	       bestlen>plen?plen:bestlen, off);
*/
	countdb += bestuse;
	readn_(idevn_p, &bestgeln, gelname, (int_fl)(DB_NAMELEN));
	Fstr2Cstr(gelname, sizeof(gelname), gelname, (int_fl)sizeof(gelname));

	{
	    int_f offset = relpg_p[bestgeln] + lngthg_p[bestgeln];

	    if (sense == 1) /* negative direction */
		offset = relpg_p[*lincon_p] - offset + 1;

#if DEBUG_DSTRAND
	    printf("Double stranded %s by %d base%s at offset %d (was %d)\n",
		   gelname, bestuse, bestuse==1 ? "" : "s",
		   offset, offset - consins);
#else
	    printf("Double stranded %s by %d base%s at offset %d\n",
		   gelname, bestuse, bestuse==1 ? "" : "s",
		   offset);
#endif
	    updout_();
	}
	bestlen = bestuse; /* Yuk! bestlen changes meaning here :-( */
	for (tmp = 0; tmp<bestuse; tmp++) {
	    if (newb2[tmp] == '*') {
		/* pad in consensus */
#if DEBUG_DSTRAND
		fprintf(DSTR, "Inserting pad (c) at offset %d\n", gelend+tmp);
#endif
		posn = gelend+tmp;
		padcon_(&relpg_p[1], &lngthg_p[1], &lnbr_p[1], &rnbr_p[1],
			ngels_p, nconts_p, gel_p, lincon_p, &posn, &one,
			idbsiz_p, idevr_p, idevw_p, &maxgel, kbout_p, gel_l);
		memmove(&cons_p[posn+1], &cons_p[posn], rreg-posn-1);

		(*cons_e)++;
	    }
	    /*
	     * At the same time we're computing how much extra to leave on
	     * the cutoff data. (This occurs when we pad out the sequence
	     * and hence have a longer sequence than before.
	     */
	    if (newb1[tmp] == '*')
		bestlen--;
	}

	/* shrink the cutoff data - should ideally check the return. */
	(void)modext((int)bestgeln, bestlen);

	/*
	 * Add our new end of sequence onto the existing one.
	 * This requires reading the sequence ('w'orking version), 
	 * adding onto the end, and writing it back. Similarly for
	 * 'r'elationships.
	 */
	readw_(idevw_p, &bestgeln, gel_p, &maxgel, maxgel);
	strncpy(gel_p + lngthg_p[bestgeln], newb1, (size_t)bestuse);
	writew_(idevw_p, &bestgeln, gel_p, &maxgel, maxgel);
	lngthg_p[bestgeln] += (int_f)bestuse;
	writer_(idevr_p, &bestgeln, &relpg_p[bestgeln], &lngthg_p[bestgeln],
		&lnbr_p[bestgeln], &rnbr_p[bestgeln]);

	/* create the necessary tags for any pads in our extension. */
	for (tmp = 0; tmp<bestuse; tmp++) {
	    if (newb1[tmp] == '*') {
#if DEBUG_DSTRAND
		fprintf(DSTR, "Inserting pad (g) at offset %d\n", gelend+tmp);
#endif
		posn = gelend+tmp;
		padtag_(&bestgeln, &posn, &one, &lngthg_p[bestgeln]);
		countdb++;
	    }
	}

	free(newb1);
	free(newb2);
    } else
#if DEBUG_DSTRAND
	fprintf(DSTR, "No suitable gel.\n");
#endif

    /* tidy up memory */
    free(albuf);
    free(cutbuf);

    /*
     * We could either return the length of the original single stranded
     * section, or we could return the length of the amount we managed
     * to double strand. (if any). In this case must make sure we do not
     * return 0 for a failed patch and hence get into infinite loops.
     * Taking the easy solution currently...
     */
    return plen;
}

/*----------------------------------------------------------------------*/

#ifdef notdef
/*
 * Calculate the average length of 'used' data in the gel readings.
 */
int avggellen(int_f *lngthg_p, int_f *ngels_p) {
    int_f i, len = 0;

    for (i=0; i<*ngels_p; i++)
	len += lngthg_p[i];
    
    return (int)(len / *ngels_p);
}
#endif

/*----------------------------------------------------------------------*/

/*
 * dstrand_x() - attempts to double strand the single stranded segments of
 * sequence within a contig. This is performed by analysing the 'cutoff' data
 * and comparing it with the opposite strand. If a close match is found then
 * we extend as far as possible.
 */
void dstrnd_(
     int_f *relpg_p,	/* int array - relative position (gel reading) */
			/*        or - length of contig */
     int_f *lngthg_p,	/* int array - length of gel */
			/*        or - empty for contig */
     int_f *lnbr_p,	/* int array - left nodes */
     int_f *rnbr_p,	/* int array - right nodes */
     int_f *ngels_p,	/* int - number of gels */
     int_f *nconts_p,	/* int - number of contigs */
     char  *qual_p,	/* char array - quality of contig */
     char  *cons_p,	/* char array - consensus of contig */
     int_f *arr_l,	/* int - length of arrays */
     int_f *lreg_p,	/* int - left start in qual */
     int_f *rreg_p,	/* int - right start in qual */
     int_f *llino_p,	/* int - left gel in contig */
     int_f *lincon_p,	/* int - record no. of contig */
     int_f *maxgel_p,	/* int - max length of a gel */
     int_f *kbout_p,	/* int - passed back to fortran (output stream) */
     int_f *idbsiz_p,	/* int - passed back to fortran */
     char  *gel_p,	/* char array - passed back to fortran (tmp buf) */
     int_f *idevr_p,	/* int - passed back to fortran (relations db) */
     int_f *idevw_p,	/* int - passed back to fortran (working db) */
     int_f *idevn_p,	/* int - passed back to fortran (name db) */
     int_f *sense_p,	/* int - direction of contig (0=pos, 1=neg) */
	     		/* following four used in alignment evaluation */
     int_f *maxmis_p,	/* int - maximum number of mismatches */
     int_f *missc_p,	/* int - score for mismatch */
     int_f *matchsc_p,	/* int - score of match */
     int_f *padsc_p,	/* int - score for pad */
     int_fl qual_l,	/* int - length of qual_p */
     int_fl cons_l,	/* int - length of cons_p */
     int_fl gel_l	/* int - length of gel_p */
     )
{
    register int_f i, j;		/* loop variables */
    int_f rreg = *rreg_p;	/* faster copy of *rreg_p */
    int cons_e;
    static int countdbt, consinst;

    if (*sense_p == 0)
	countdbt =  consinst = 0;
    countdb = consins = 0;

    /*
     * Reset array pointers so our indexes correspond to FORTRANs ones
     */
    relpg_p--;
    lngthg_p--;
    lnbr_p--;
    rnbr_p--;
    qual_p--;
    cons_p--;

#if DEBUG_DSTRAND
    fprintf(DSTR,"\nllreg = %d, rreg = %d\n", *lreg_p, rreg);
#endif
    /* initialise remembered last gel to left most one of this contig */
    last_gel = *llino_p; 

    /* scan through quality buffer */
    for (i = *lreg_p; i<=rreg; i++) {

	/* strong negative strand, but no positive strand */
	if (qual_p[i] == '2' || qual_p[i] == '8') {

	    /* find length of single stranded section */
	    j = i;
	    while(qual_p[j] == '2' || qual_p[j] == '8')
		j++;

	    if (j > rreg)
		j = rreg+1;

#ifdef ndef
	    /*
	     * We have a minimum length of single strand to patch.
	     * This saves us having to do too many checks on strands
	     * with padding characters in etc.
	     */
	    if ((j-i) < MINHOLELEN) {
		i = j-1;
		continue;
	    }
#endif
#if DEBUG_DSTRAND
	    fprintf(DSTR, "Single strand at %d - %d, (was %d - %d) len %d\n",
		    i, j, i-consins, j-consins, j-i);
#endif

	    /*
	     * Perform the 'operation' on the contig.
	     * And leap forward to next potential problem.
	     */
	    cons_e = 0;
	    /*
	     * We pass over 'j-i+1' as the length due to an (as yet) unfound
	     * 'feature'. For some reason it appears that the actual length
	     * double stranded is not always the same as the amount we asked
	     * for. So just to make sure we don't leave any 1 base gaps we
	     * cheat a bit. Note: this bug appears to be indeterminate in that
	     * runnin double strand twice on the same data with the same args
	     * doesn't always do the same thing! (jkb 23/12/92)
	     */
	    j = patch(relpg_p, lngthg_p, lnbr_p, rnbr_p, *maxgel_p, i, j-i+1,
		      cons_p, ngels_p, kbout_p, idbsiz_p, gel_p, idevr_p,
		      idevw_p, idevn_p, lincon_p, nconts_p, &cons_e, gel_l,
		      rreg, *maxmis_p, *missc_p, *matchsc_p, *padsc_p,
		      *sense_p);
	    /* move back quality buffer to ensure alignment with consensus */
#if DEBUG_DSTRAND
	    fprintf(DSTR, "Inserted %d pads into consensus\n", cons_e);
#endif
	    
	    /*
	     * Take into account number of additional consensus entries.
	     * This involves shifting our consensus buffer along to the
	     * right by a bit, and changing our right margin (rreg) for the
	     * region.
	     */
	    /* qual_p -= cons_e;*/
	    memmove(&qual_p[i+cons_e], &qual_p[i], rreg-i);
	    rreg += cons_e;
	    consins += cons_e;

	    /* Skip over the hole we just patched (& take account of cons_e) */
	    i += j + cons_e;
	}
    }

    countdbt += countdb;
    consinst += consins;
    printf("%s strand : double stranded %d base%s with %d insert%s into consensus\n",
	   *sense_p?"Negative":"Positive",
	   countdb, countdb==1 ? "" : "s",
	   consins, consins==1 ? "" : "s");
    if (*sense_p)
	printf("Total : double stranded %d bases with %d inserts\n",
	       countdbt, consinst);
    updout_();
}

void dblint_(int_f *status_p,  int_f *maxmis_p, int_f *missc_p,
	     int_f *matchsc_p, int_f *padsc_p,  int_f *dialogue_p) {

    if (*dialogue_p) {
	*maxmis_p = getint(0, 99, 5, "Maximum number of mismatches", status_p);
	if (*status_p < 0) {
	    *status_p = -1; return;
	}
	
	*missc_p = getint(-100, 0, -8, "Score for mismatch", status_p);
	if (*status_p < 0) {
	    *status_p = -1; return;
	}
	
	*matchsc_p = getint(0, 100, 1, "Score for correct match", status_p);
	if (*status_p < 0) {
	    *status_p = -1; return;
	}
	
	*padsc_p = getint(-100, 0, -5, "Score for insertion", status_p);
	if (*status_p < 0)
	    *status_p = -1;

	*status_p = 0;
	return;
    } else {
	*maxmis_p = 6;
	*missc_p = -8;
	*matchsc_p = 1;
	*padsc_p = -5;
	*status_p = 0;
	return;
    }
}
