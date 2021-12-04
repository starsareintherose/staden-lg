/*
 * Check xdap database for errors
 */

#include <stdio.h>
#include <ctype.h>    /* IMPORT : tolower */
#include "misc.h"
#include "seq.h"

typedef char StringBuffer[200];










/*
 * Database io
 */
#ifdef BAP_VERSION
/*
 * BAP version
 */
#include "bapIO.h"
BapIO io;
#define FILE_NAME_LENGTH BAP_FILE_NAME_LENGTH
#define open_for_read bap_open_for_read
#define close_files bap_close_files
#define read_rl bap_read_rl
#define read_sq bap_read_sq
#define read_ar bap_read_ar
#define read_tg bap_read_tg
#define read_comment bap_read_comment
#define rl_file_rec bap_rl_file_rec
#define ar_file_rec bap_ar_file_rec
#define tg_file_rec bap_tg_file_rec
#else
/*
 * DAP version
 */
#include "dapIO.h"
DapIO io;
#define FILE_NAME_LENGTH DAP_FILE_NAME_LENGTH
#define open_for_read dap_open_for_read
#define close_files dap_close_files
#define read_rl dap_read_rl
#define read_sq dap_read_sq
#define read_ar dap_read_ar
#define read_tg dap_read_tg
#define read_comment dap_read_comment
#define rl_file_rec dap_rl_file_rec
#define ar_file_rec dap_ar_file_rec
#define tg_file_rec dap_tg_file_rec
#endif
/*
 * Prototypes
 */
extern void *malloc(size_t size);
extern void *calloc(size_t nobj, size_t size);
extern char *getenv(char *s);

/*
 * Global variables - to this file at least
 */
static int consensusCutoff;
static int alignmentCutoff = 0; /* scores range from -2000 to +2000 */
static int bridge=2;

/* was -200 */
static StringBuffer rawData;
static StringBuffer contigName;
static FILE *log_fp;


#include "upam.gbl"
#include "uascii.gbl"
#include "llin.h"

initpam2()
/*
 * Initialise alignment routine
 */
{
    int i, j, k;
    
    pam = npam;
    nsq = naa;
    
    k=0;
    for (i=0; i<nsq; i++)
	for (j=0; j<=i; j++)
	    pam2[j][i] = pam2[i][j] = -pam[k++];
}


static void seqout(char *cseq)
/*
 * Print out a string in lines 50 characters long
 */
{
    char *s;
    int i;
    
    for (i = 0,s = cseq; *s; s++) {
	putchar(*s);
	i++;
	if (i==50) {
	    putchar('\n');
	    i = 0;
	}
    }
    if (i) putchar('\n');
}




static void print_seq(char *seq, int from, int to)
/*
 * Print a portion of sequence
 */
{
    char *s;
    int i;
    
    for (i = 0, s = seq+from-1; *s && s < seq+to; s++) {
	putchar(*s);
	i++;
	if (! (i%50) ) {
	    putchar('\n');
	}
    }
    if (i%50) putchar('\n');
}



/*
 * Consensus calculation routines
 */

static int base_scores[256];
static int base_indexes[256];
static char base_complement[256];

typedef int Scores[7];

static void inits()
/*
 * Initialise tables
 * Based on Rodger Staden's INITS
 */
{
    static char bases[]  = "CTAG1234DVBHKLMNRY5678ctag*,-";
    static char cbases[] = "GATC4321HBVDNMLKYR6578gatc*,-";
    static int ind[]  = {1,2,3,4,1,2,3,4,1,2,3,4,1,2,3,4,6,6,6,6,6,6,1,2,3,4,5,5,6};
    static int scr[]  = {
	100,100,100,100,
	75,75,75,75,
	100,100,100,100,
	100,100,100,100,
	10,10,10,10,10,10,
	100,100,100,100,100,100,10};
    
    int i;
    for (i=0;i<256;i++) {
	base_scores[i] = base_indexes[i] = 0;
	base_complement[i] = '-';
    }
    
    for (i=0;i<sizeof(bases);i++) {
	base_scores[bases[i]] = scr[i];
	base_indexes[bases[i]] = ind[i];
	base_complement[bases[i]] = cbases[i];
    }
    
}






static void seq_complement(char *seq, int len)
/*
 * Complement a sequence (but don't reverse)
 */
{
    int i;
    
    for (i=0; i < len; i++) {
	seq[i] = base_complement[seq[i]];
    }
}



static void seq_reverse(char *seq, int len)
/*
 * Reverse a sequence
 */
{
    char temp;
    int i;
    
    for (i=0; i < len/2; i++) {
	temp = seq[i];
	seq[i] = seq[len-i-1];
	seq[len-i-1] = temp;
    }
    
}


static void complement_seq(char *seq, int len)
/*
 * Complement a sequence
 */
{
    seq_reverse(seq,len);
    seq_complement(seq,len);
}


static void complement_zseq(char *seq)
/*
 * Complement a zero-terminated sequence
 */
{
    complement_seq(seq, strlen(seq));
}





static int indexs(unsigned char c, int *score)
/*
 * Return inde and score of character c
 * Based on Rodger Staden's INDEXS
 */
{
    *score = base_scores[c];
    return base_indexes[c];
}




static char charsu(int i)
/*
 * Return necleotide with index i
 * Based on Rodger Staden's CHARSU
 */
{
    static char c[] = "CTAG*-";
    
    return c[i-1];
}




static char gtconc(Scores scores, int idm, int cut)
/*
 * Returns the consensus of matrix scores
 * 	cut is the percentage cutoff
 *	idm should always be 7
 * Based on Rodger Staden's GTCONC
 */
{
    char c;
    int i;
    
    if (!scores[idm-1]) return '-';
    
    for (i=0;i<idm;i++)
	if ( scores[i] * 100 >= cut * scores[idm-1] ) return charsu(i);
    
    return '-';
    
}





static void consensus_region(int contig, int start, int end, char *cseq, int cutoff)
/*
 * Determine the consensus of a region in a contig
 * Adequate space must be allocated in cseq.
 * It used a memory intensive algorithm! (The score maxtrix is 7xlength of region!)
 */
{
    Scores *scores;
    rl_file_rec cline;
    rl_file_rec gline;
    char *gseq;
    int gel;
    int region_len;
    int i;
    
    region_len = end - start + 1;
    scores = (Scores *)calloc(1,region_len * sizeof(Scores));
    
    read_rl(&io,contig,&cline);
    gel = cline.clines.left_end;
    
    gseq = malloc(io.max_gel_length+1);
    
    /*
     * find left-most gel in region
     */
    read_rl(&io,gel,&gline);
    while (gel && gline.lines.rel_pos + abs(gline.lines.length) <= start) {
	if (gel = gline.lines.right_nbr) read_rl(&io,gel,&gline);
    }
    
    while (gel && gline.lines.rel_pos <= end) {
	int gstart, gend, goffset;
	char *s;
	
	read_sq(&io,gel,gseq);
	gseq[abs(gline.lines.length)] = '\0';
	
	gstart = max(start, gline.lines.rel_pos) -
	    gline.lines.rel_pos + 1;
	gend = min(end, gline.lines.rel_pos + abs(gline.lines.length) - 1) -
	    gline.lines.rel_pos + 1;
	goffset = gline.lines.rel_pos - start - 1;
	
	for (s = &gseq[gstart-1];gstart<=gend;gstart++, s++) {
	    int j,score;
	    
	    j = indexs(*s,&score);
	    
	    scores[gstart+goffset][j] += score;
	    scores[gstart+goffset][6] += score;
	}
	
	if (gel = gline.lines.right_nbr) read_rl(&io,gel,&gline);
    }
    
    for (i=0;i<region_len;i++){
	cseq[i] = gtconc(scores[i],7,cutoff);
    }
    cseq[region_len] = '\0';
    
    free(gseq);
    free(scores);
    
}





static void consensus_contig(int contig, char *cseq, int cutoff)
/*
 * Determine the consensus sequence of a contig
 * It allocates just enough space and returns a pointer to the string.
 * Remember to free it when you've finished with it!
 */
{
    rl_file_rec cline;
    
    read_rl(&io, contig, &cline);
    
    consensus_region(contig, 1, cline.clines.length, cseq, cutoff);
}



static int isUncertain(char c)
/*
 * Return true if c not in "acgtACGT".
 */
{
    switch (c) {
    case 'a': case 'c': case 'g': case 't':
    case 'A': case 'C': case 'G': case 'T':
	return 0;
    default:
	return 1;
    }
}




static void select_good_raw_bits(Seq trace,
				 int rawLCut,
				 int rawLength,
				 char *rawSeq,
				 int *map)
/*
 * Select only the good quality pieces in the raw sequence.
 * This routine will use the quality measures developed by
 * LaDeana, once they have been tested.`
 * In the mean time - FUDGE IT!
 */
{
    int rcut;
    int score;
    int i;
    
    
    /*
     * Determine a rough estimate of how much to use.
     */
    
    /* MAXREADLEN bases max */
#define MAXREADLEN rawLength
    rcut = MAXREADLEN; 
    
    
    /* MAXPERCENT of rawLength max */
#define MAXPERCENT 100
    
    rcut = min(rcut, rawLength*MAXPERCENT/100);
    
    /* Up to where NN uncertainties in MM window */
#define NN 0
#define MM 5
    if (NN>0) {
	for(i=1-MM,score=0;i<rawLength+MM-1;i++) {
	    if (i>=0) score -= isUncertain(rawSeq[i]);
	    if (i+MM-1<rawLength) score += isUncertain(rawSeq[i+MM-1]);
	    if (score >= NN) break;
	}
	rcut = min(rcut,max(i,0));
    }
    
    /*
     * Remove 3' crap from map
     */
    for (i=rcut;i<rawLength;i++) map[i] = ~map[i];
    
    /*
     * Remove XX either side of "-" from map
     */
#define XX 1
    for (i=0;i<rcut;i++) {
	if (rawSeq[i] == '-') {
	    int k;
	    for (k=(i<XX)?0:i-XX; k<=i+XX; k++)
		if (map[k]>0) map[k] = ~map[k];
	}
    }
}



static int pos_in_contig(int pos,
			 int rel_pos,
			 int length,
			 int complemented)
{
    if (complemented)
	return rel_pos + length - pos;
    else
	return rel_pos + pos - 1;
}


static int bases_equal(char a, char b)
/*
 * Return true if bases a and b are equal.
 * This ignores the case of the bases
 */
{
    /*
     * Beware of non ANSI implementations of tolower()
     * that only work when argument isupper().
     */
    if (isupper(a)) a = tolower(a);
    if (isupper(b)) b = tolower(b);
    
    return (a == b);
}




static void region_covered(int start,
			   int end,
			   int *coverage)
/*
 * Mark as covered, the region from start to end in the map
 */
{
    int i;
    
    for (i=start; i<=end+(1-bridge); i++) {
	/*
	 * Setting coverage at position x means bases between x and x+(1-bridge) (inclusive) are covered
	 */
	coverage[i-1]++;
    }
}




static void compare_seq_update_coverage(int gel,
					char *seq,
					int length,
					char *rawSeq,
					int rawLength,
					int rel_pos,
					int complemented,
					int *coverage,
					int *map
					)
/*
 * Report the differences, insertions and deletions
 */
{
    int i;
    int mstart,mend;
    int gstart,gend;
    int cstart,cend;
    
    i = 0;
    while (i < rawLength) {
	/*
	 * Skip over regions:
	 *    (a) not mapped because the quality of the trace was deemed too poor (*map < 0)
	 * or (b) where deletions are indicated (*map == 0)
	 * or (c) where bases disagree ( *rawSeq != seq[*map] )
	 */
	while (i < rawLength &&
	       (map[i] <= 0 || 
		! bases_equal(rawSeq[i],seq[map[i]-1]) ) )
	    i++;
	
	/*
	 * We have hit start of good coverage
	 * Determine regions where there are no insertions or changes,
	 * then mark them as covered
	 */
	if (i < rawLength) {
	    int mstart = i;
	    i++;
	    while (i < rawLength && map[i] > 0) {
		if (map[i] != map[i-1]+1) {
		    /* insertion detected */
		    break;
		}
		
		if ( ! bases_equal(rawSeq[i],seq[map[i]-1]) ) {
		    /* change detected */
		    break;
		}
		i++;
	    }
	    
	    mend = i-1;
	    
	    gstart = map[mstart];
	    gend = map[mend];
	    
	    if (complemented) {
		cstart = pos_in_contig(gend,rel_pos,length,complemented);
		cend   = pos_in_contig(gstart,rel_pos,length,complemented);
	    } else {
		cstart = pos_in_contig(gstart,rel_pos,length,complemented);
		cend   = pos_in_contig(gend,rel_pos,length,complemented);
	    }
	    
	    region_covered(cstart,cend,coverage);
	    
	    i++;
	    
	}
	
    }
    
    
    
    
    
}



static int make_alignment_map(int gel,
			      char *seq,
			      int length,
			      char *rawSeq,
			      int rawLength,
			      int *map
			      )
/*
 * Produce an alignment map which maps base positions in the raw sequence
 * to positions in the gel reading
 */
{
    int totalLength;
    int *res; /* used by alignment program */
    int score;
    
    totalLength = length+rawLength;
    res = (int *) malloc(totalLength * sizeof(int));
    
    score = DIFF(rawSeq-1,seq-1,
		 rawLength,length,
		 pam2, -gdelval, -ggapval, res);
    
    if (score > alignmentCutoff) {
	fprintf(log_fp,";Alignment for gel %d failed to meet cutoff score (%d/%d)\n",
		gel,score,alignmentCutoff);
	return 0;
    }
    
    /*
     * Use alignment results to produce an array
     * mapping trace to consensus positions
     */
    
    /*************************************************************************/
    {
	int mindex;
	int mvalue;
	int i, j, op;
	int *S, M, N;
	
	S = res;
	M = rawLength;
	N = length;
	i = j = op = 0;
	mindex = 0;
	mvalue = 1;
	while (i < M || j < N) {
	    if (op == 0 && *S == 0) {
		map[mindex++] = mvalue;
		mvalue++;
		op = *S++;
		i++; j++;
	    } else {
		if (op == 0)
		    op = *S++;
		if (op > 0) {
		    mvalue++;
		    op--;
		    j++;
		} else {
		    map[mindex++] = 0;
		    op++;
		    i++;
		}
	    }
	}
    }
    /*************************************************************************/
    
    /***********
      {
	(void) DISPLAY(rawSeq-1, seq-1,
		       rawLength, length,
		       res);
	printf("\n\n");
      }
      ************/
    
    
    free(res);
    
    return 1;
}







static void compare_sequences(/* This gel reading info */
			      int gel,
			      char *seq,
			      int length,
			      int rel_pos,
			      int complemented,
			      /* This trace info */
			      Seq trace,
			      int rawLCut,
			      int rawLength,
			      char *rawSeq,
			      /* This cosmid info*/
			      int *coverage
			      )
/*
 * Perform an alignment between the consensus sequence and the raw data.
 * Check only high quality bits and update coverage
 */
{
    int *map;
    
    map = (int *)malloc(rawLength*sizeof(int));
    
    if (make_alignment_map(gel,seq,length,rawSeq,rawLength,map)) {
	
        select_good_raw_bits(trace,rawLCut,rawLength,rawSeq,map);
	
	compare_seq_update_coverage(gel,seq,length,rawSeq,rawLength,rel_pos,
				    complemented,coverage,map);
	
    }
    
    free(map);
    
}








static void process_gel_reading(int gel,
				int length,
				int complemented,
				int rel_pos,
				char *cseq,
				int contig,
				int *coverage
				)
/*
 * Perform the quality check on a specific gel reading
 */
{
    ar_file_rec ar_rec;
    tg_file_rec tg_rec;
    char *rd;
    int rd_length;
    int rd_cut;
    int rd_ulen;
    char rd_type[5];
    char rd_file[19];
    
    /*
     * Read Name
     */
    read_ar(&io,gel,&ar_rec);
    
    fprintf (log_fp,";%d %s\n",complemented?-gel:gel,ar_rec.lines.name);
    
    /*
     * Read Raw data file name
     */
    read_tg(&io,gel,&tg_rec);
    if (tg_rec.lines.comment) {
	char *fullFileName;
	rd = (char *) read_comment(&io, tg_rec.lines.comment);
	sscanf(rd,"%6d%6d%6d%*s",&rd_length, &rd_cut, &rd_ulen);
	f2cstr(&rd[22],18,rd_file,18);
	f2cstr(&rd[18],4,rd_type,4);
	if ( (fullFileName = findfile(rd_file,rawData)) == NULL) {
	    /* Trace file specified but not found */
	    fprintf(log_fp,";Trace file %s specified but not found for gel reading %d %s\n",
		    rd_file,gel,ar_rec.lines.name);
	} else {
	    Seq trace;
	    trace = (Seq) getSeq(fullFileName,rd_type);
	    if (trace == NULLSeq) {
		/* Trace file specified but not found */
		fprintf(log_fp,";Error reading Trace file %s for gel reading %d %s\n",
			fullFileName,gel,ar_rec.lines.name);
	    } else {
		char *seq;
		char *rawSeq;
		
		rawSeq = (char *)getSequence(trace);
		seq = (char *)malloc(length+1);
		strncpy(seq, &cseq[rel_pos-1],length);
		seq[length] = '\0';
		
		if (complemented) {
		    /*
		     * Complement consensus sequence
		     */
		    complement_zseq(seq);
		}
		
		compare_sequences(gel,seq,length,rel_pos,complemented,
				  trace,rd_cut,rd_ulen,&rawSeq[rd_cut],
				  coverage);
		
		freeSeq(trace);
		free(rawSeq);
		free(seq);
	    }
	}
	
	
	free(rd);
    } else
	/* Skip */
	fprintf(log_fp,";No raw data tag for gel reading %d %s\n",gel,ar_rec.lines.name);
}






static void print_coverage_gaps(int *coverage, int length)
/*
 * Print out a list of places where there is not coverage
 */
{
    int starti,endi;
    int i;
    
    printf("Problem areas:\n");
    fprintf(log_fp,"Problem areas:\n");
    
    for (i=0;i<length-(bridge-1);) {
	if (coverage[i])
	    i++;
	else {
	    starti = i+1;
	    for(;!coverage[i] && i<length; i++);
	    endi = i + bridge - 2;
	    
	    if (starti == endi) {
		printf("  %d\n",starti);
		fprintf(log_fp,"  %d\n",starti);
	    } else {
		printf("  %d-%d\n",starti,endi);
		fprintf(log_fp,"  %d-%d\n",starti,endi);
	    }
	}
    }
    
    
}






static void process_contig(int contig)
/*
 * Perform the error check on a specific contig
 */
{
    char *cseq;
    int *coverage;
    rl_file_rec cline;
    ar_file_rec ar_rec;
    
    /*
     * Read Contig Details
     */
    read_rl(&io, contig, &cline);
    read_ar(&io,cline.clines.left_end,&ar_rec);
    
    /*
     * Write to log
     */
    printf("Checking contig %d: %s\n",cline.clines.left_end,ar_rec.lines.name);
    fprintf(log_fp,"Checking contig %d: %s\n",cline.clines.left_end,ar_rec.lines.name);
    
    /*
     * Determine consensus sequence
     */
    cseq = malloc(cline.clines.length + 1);
    consensus_region(contig, 1, cline.clines.length, cseq, consensusCutoff);
    
    /*
     * Allocate coverage arrays
     */
    coverage = (int *)calloc(1,cline.clines.length * sizeof(int));
    
    {
	int gel;
	rl_file_rec gline;
	
	/*
	 * Print out a list of names and raw data files
	 */
	if (gel = cline.clines.left_end) read_rl(&io,gel,&gline);
	while (gel) {
	    process_gel_reading(gel,
				abs(gline.lines.length),
				gline.lines.length < 0,
				gline.lines.rel_pos,
				cseq,
				contig,
				coverage);
	    if (gel = gline.lines.right_nbr) read_rl(&io,gel,&gline);
	}
	
    }
    
    print_coverage_gaps(coverage,cline.clines.length);
    printf("\n");
    fprintf(log_fp,"\n");
    
    free(coverage);
    free(cseq);
    
}




static void process()
/*
 * Perform the error check on the whole database
 */
{
    int i;
    
    for (i=0; i<io.num_contigs; i++)
	process_contig(io.max_gels-i-1);
}


static void open_log_file(char *projectName, char *versionNumber)
{
    StringBuffer fn;
    
    sprintf(fn,"%s.%s.LOG",projectName,versionNumber);
    
    if (file_exists(fn)) {
	StringBuffer fnold;
	sprintf(fnold,"%s~",fn);
	fprintf(stderr,"Previous log file renamed %s\n\n",fnold);
	rename(fn,fnold);
    }
    
    if ( (log_fp = fopen(fn,"w")) == NULL )
	crash("Cannot open log file %s\n",fn);
    
    fprintf(log_fp,";Log started: %s\n",date_str());
    
}

static void close_log_file()
{
    fprintf(log_fp,";Log stopped: %s\n",date_str());
    fclose(log_fp);
}




static int read_from_name(char *contigName)
/*
 *
 */
{
    int i,j;
    ar_file_rec ar_rec;
    char buf[FILE_NAME_LENGTH+1];
    
    for (i=1; i<io.num_gels;i++) {
	/* read next name */
	read_ar(&io,i,&ar_rec);
	/* copy name to first space */
	for(j=0;j<FILE_NAME_LENGTH && ar_rec.lines.name[j]!=' ';j++)
	    buf[j] = ar_rec.lines.name[j];
	buf[j] = '\0';
	/* if a match, return i */
	if (strcmp(buf,contigName)==0) return i;
	
    }
    return 0;
}


static int valid_read_number(int readNumber)
/*
 *
 */
{
    return (readNumber < 1) || (readNumber >io.num_gels);
}


int find_contig(int readNumber)
/*
 *
 */
{
    int i;
    rl_file_rec cline;
    int contigNum;
    
    for (i=0; i<io.num_contigs; i++) {
	contigNum = io.max_gels-i-1;
	read_rl(&io,contigNum,&cline);
	if (cline.clines.left_end == readNumber) return contigNum;
    }
    return 0;
}


int left_most_read(int readNumber)
/*
 *
 */
{
    int lmr = readNumber;
    rl_file_rec line;
    
    read_rl(&io,lmr,&line);
    while (line.lines.left_nbr) {
	lmr = line.lines.left_nbr;
	read_rl(&io,lmr,&line);
    }
    return lmr;
}




static int contig_from_read(int readNumber)
/*
 */
{
    if (valid_read_number(readNumber)) return 0;
    return find_contig(left_most_read(readNumber));
}




static int check_contig(char *contigName)
/*
 * contigName is
 * (a) number of a reading
 * (b) a reading name prefixed a "/"
 */
{
    int readNumber;
    if (*contigName=='/')
	readNumber = read_from_name(contigName+1);
    else
	readNumber = atoi(contigName);
    
    return contig_from_read(readNumber);
}




static void usage()
/*
 * Print out usage
 */
{
    printf("Usage: cop [options]\n  where options are:\n  -p project\n  -v version\n  -c consensus_cutoff_percentage\n  -r raw_data_search_path\n  -h\n");
    
}


static void help()
/*
 * Print out help and advice
 */
{
    printf("Usage: cop [options]\n  where options are:\n");
    printf("  -p project\n     Database project name. Prompted for if not supplied. Case is not\n     important.\n");
    printf("  -v version\n     Database version. Prompted for if not supplied.\n");
    printf("  -c consensus_cutoff_percentage\n     Default is 100(%%)\n");
    printf("  -r raw_data_search_path\n     Where to look if trace files are not found in the present working\n     directory. Default is that specified by environment variable\n     RAWDATA.\n");
    printf("  -C contig_name\n     Only check a named contig\n");
    printf("  -h\n     Print out this help\n");
    printf("\nExample: cop -p f59b2 -v 0 -c 66 -r ~mmm/F59B2\n     Run cop on project F59B2 version 0 with consensus cutoff\n     percentage 66%, looking for trace files in ~mmm/F59B2.\n");
}



main(int argc, char**argv)
/*
 * COP - error checking program
 *
 * Usage: cop [options]
 *   where options are:
 *   -p project
 *   -v version
 *   -c consensus_cutoff_percentage
 *   -r raw_data_search_path
 *   -C contig
 *   -h
 *
 * Example:
 * 	cop -p f59b2 -v r -c 100
 *
 */
{
    StringBuffer projectName;
    StringBuffer versionNumber;
    
    int c;
    extern char *optarg;
    extern int optint;
    int contigNum;
    
    int p_opt=0, v_opt=0, c_opt=0, r_opt=0, h_opt=0, C_opt=0;
    int err_opt=0;
    
    while ( (c = getopt(argc,argv,"hp:v:c:r:C:")) != -1 )
	switch (c) {
	case 'h':
	    h_opt++;
	    break;
	case 'p':
	    strcpy(projectName, optarg);
	    p_opt++;
	    break;
	case 'v':
	    strcpy(versionNumber, optarg);
	    v_opt++;
	    break;
	case 'c':
	    consensusCutoff = atoi(optarg);
	    c_opt++;
	    break;
	case 'r':
	    strcpy(rawData, optarg);
	    r_opt++;
	    break;
	case 'C':
	    strcpy(contigName, optarg);
	    C_opt++;
	    break;
	case '?':
	    err_opt++;
	    break;
	}
    
    if (err_opt) {
	usage();
	exit(1);
    }
    
#ifdef BAP_VERSION
    printf("COP v1.2: Check Out Project\nChecks xbap database for errors\n\n");
#else
    printf("COP v1.2: Check Out Project\nChecks xdap database for errors\n\n");
#endif
    if (h_opt) {
	help();
	exit(0);
    }
    
    if (! p_opt) {
	printf("Project name ? ");
	gets(projectName);
    }
    
    if (! v_opt) {
	printf("Version ? ");
	gets(versionNumber);
    }
    
    if (! c_opt) {
	consensusCutoff = 100;
	printf("Consensus cutoff = %d%%\n",consensusCutoff);
    }
    
    if (! r_opt) {
	char *r;
	r = getenv("RAWDATA");
	if (r != NULL)
	    strcpy(rawData,r);
	else
	    strcpy(rawData,".");
	printf("Trace directory = %s\n",rawData);
    }
    
    printf("\n");
    
    fn_toupper(versionNumber);
    fn_toupper(projectName);
    
    /*
     * Open files
     */
    open_for_read(&io,projectName,versionNumber);
    open_log_file(projectName,versionNumber);
    
    if (io.data_class != 5)
	crash ("Database must be for DNA only\n");
    
    /* initialisations */
    inits();    /* for consensus calculations */
    initpam2(); /* for alignments */
    
    if (!C_opt) {
	printf("Check which contig? [all] ");
	gets(contigName);
	C_opt++;
    }
    
    if (strlen(contigName)==0 || strcmp(contigName,"all")==0) C_opt=0;
    
    if (C_opt) {
	contigNum = check_contig(contigName);
	if (contigNum==0)
	    crash("cop: Invalid contig number/name '%s'\n",contigName);
	process_contig(contigNum);
    } else
	process();
    
    close_files(&io);
    close_log_file();
}


