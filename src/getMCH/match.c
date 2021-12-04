int string_match(seq1, n1, seq2, n2, nmiss, indices)

/*
  This function may be called once from initialDisplayedSeq if
  the user has input a string they wish to search for in the
  input sequence -- this subroutine returns *indices, the first
  position at which the query sequence (app_resources.astring)
  matched the input file sequence (currSeq). */

/* a modified version of match.c 
  finds alignments between a search sequence, seq1, and a target sequence, 
  seq2, with no gaps (except possibly at ends) 
   and at most nmiss mismatches (relative to seq1),
   n1 is the length of seq1 where seq1 is assumed to start at indices 0,
   n2 is the length of seq2 where seq2 is assumed to start at indices 0 */
      char *seq1, *seq2;
      int n1, n2, nmiss;
      int *indices;
{
    int i, j, d, istart, iend, i_miss, n_match;
    int mtable[100][5];  /* 100 would be, now the total number
			    of matches the program may find between
			    a given input string and the sequence */


/* d = j - i is the "offset" between the two sequences */
    if (n1 - n2 > nmiss) return (0);
    n_match = 0;
    for (d = -nmiss; d <= n2 + nmiss - n1; d++){
	if (d < 0)  istart = i_miss = -d;
	else istart = i_miss = 0;
	if (d > n2 - n1) {
	    iend = n2 - d;
	    i_miss += n1 + d - n2;
	}
	else iend = n1;
	for (i = istart, j = d + i; i < iend; i++, j++)
	    if (seq1[i] != seq2[j] && ++i_miss > nmiss) goto nextd; 
        mtable[n_match][0] = d + istart; /* indices (assuming they start at
					    0) of starting nucleotide in the
					    searched sequence */
        mtable[n_match][1] = istart; /* nucleotide position in the query
					sequence where match starts (assuming
					query index starts with 0) */
        mtable[n_match][2] = iend - istart; /* number of nucleotides in the
					       match */
	if (mtable[n_match][2]==n1) {
	  indices[n_match] = mtable[n_match][0];
	}
	  
        mtable[n_match][3] = i_miss; /* number of mismatches */
	n_match++; /* number of matches */
	if (n_match >= 100) return(n_match);

    nextd:;
    }

    return (n_match);
/*    return(0); /* return 0 if it got to this point; that means
		 it must have not found an exact match, so 
		 you want the baseNum to be 0 in that case*/
}
