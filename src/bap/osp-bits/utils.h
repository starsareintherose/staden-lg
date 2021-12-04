#ifndef _utils_h
#define _utils_h

/* 
  Program Name: utils
  File name: utils.h
  Purpose:  utilities, parsing lines, reading in files, sorting
  Last Update: Fri Mar 23 1991
  Copyright 1991: LaDeana Hillier and Philip Green
  Change Log:
*/

extern int space_file_to_vec();

extern void text_to_output();

/*text_to_output(vec,stp,endp,dvice,outfile)
        input: char **vec,*outfile; int stp, endp, dvice;
        this program output a specified portion of a genbank file
(from vec[stp] to vec[endp]) to the screen (default), a file (dvice
=1), or lpr (dvice =2).  Outfile is the input filename if you wish to
output the information to a specified file. stp and endp are integers,
not pointers. */


extern int parse_chars();

/*int parse_chars(vec,intgrs,stp,endp,alphas)
        input: char *vec; int stp,endp;
        output: int *intgrs; char **alphas;
        parses vec[stp] to vec[endp] into strings !isspace held in the
two dimensional character array alphas with intgrs holding the purely
integer fields.  Returns the total number of fields parsed.
*/



extern int parse_intgrs();

/*int parse_intgrs(vec,intgrs,stp,endp)
        input: char *vec; int stp,endp;
        output: int *intgrs;
        parses vec[stp] to vec[endp] into strings of integers --
throws away char strings.  Saves the integers in the array intgrs.
Returns the number of integer fields found. */

extern int file_to_vec();

/* file_to_vec(f_name, vec, vec_st,max_n)
	input: char *f_name; int max_n;
	output: char *vec, int *vec_st;
	reads the input file, f_name, to a vec array of chars of
size vec_st with a maximum size of max_n

returns any error messages in str.  returns a 0 if there was a problem, a 1 
if there was not*/



extern int ted_nnl_file_to_vec();

/* int ted_nnl_file_to_vec(f_name, vec, max_n)
	input: char *f_name; int max_n;
	output: char *vec;
	looks at the first char for a semicolon, if it is,
	it knows the file has been input from ted, so it
	gets the information about the sequence from the 
	first line and gets the sequence from the rest 
	
	reads the input file, f_name, to a vec array of chars
 with a maximum size of max_n; does not read in any of
the end of line characters , returns the size of vec array.*/



#endif /* _utils_h */



