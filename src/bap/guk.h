#include "fort.h"

extern int mmalign(char *seq1, int length1, char *seq2, int length2,
		   int *res);

extern int_f mm_(char *seq1, int_f *length1, char *seq2, int_f *length2,
		 int_f *res, int_fl seq_l, int_fl seq2_l);

extern int_f dispmm_(char *seq1, int_f *length1, char *seq2, int_f *length2,
		     int_f *res, int_fl seq1_l, int_fl seq2_l);
