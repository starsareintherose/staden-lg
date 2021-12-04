/* 
  Program Name: utils
  File name: utils.c
  Purpose:  utilities, parsing lines, reading in files, sorting
  Last Update: Apr 9, 1991
  Copyright 1991: LaDeana Hillier and Philip Green

  Change Log:
*/


/* ---- Includes ---- */
#include <stdlib.h>
#include "our_allo.h"
#include "defn.h" /* macros */
#include "Xmess.h" /*IMPORT: message */

/* ---- Exports ---- */
void text_to_output(vec,stp,endp,dvice,outfile)
char *vec;
int stp,endp;
int dvice;
char *outfile;
/*text_to_output(vec,stp,endp,dvice,outfile)
	input: char **vec,*outfile; int stp, endp, dvice;
	this program output a specified portion of a genbank file
(from vec[stp] to vec[endp]) to the screen (default), a file (dvice
=1), or lpr (dvice =2).  Outfile is the input filename if you wish to
output the information to a specified file. stp and endp are integers,
not pointers. */


/*char *vec,*outfile;
int stp,endp,dvice; */

/* this program puts a specified portion of a genbank file and outputs */
 /* it to the screen (default), a file (dvice = 1), or the lpr */
 /* (dvice = 2) */

{
  int i;
  FILE *fopen(),*fp,*where;
 
 
  if (dvice == 1) {
    if ((fp = fopen(outfile,"a"))==NULL) {
      printf ("\nERROR: can't open file %s\n",outfile);
      return;
    }
    else where = fp;
  }
  else if (dvice == 2) {
    if ((fp = fopen("junkfile.","a"))==NULL) {
      printf ("\nERROR: can't open file junkfile. to output to the lpr\n");
      return;
    }
    else where = fp;
  }
  else {
    /*  default : */
    where = stdout;
  }
  
  for (i = stp; i < endp; i++)
    putc(vec[i],where);
  
  if ((dvice == 1) || (dvice == 2)) fclose(fp);
/*  if (dvice == 2) { */
/*    fclose(fp); */
/*    system("lpr junkfile."); */
/*    system("rm junkfile.");   */
/*  } */
}



int parse_chars(vec,intgrs,stp,endp,alphas)
char *vec;
char *alphas[];
int *intgrs;
int stp,endp;

/*int parse_chars(vec,intgrs,stp,endp,alphas)
	input: char *vec; int stp,endp;
	output: int *intgrs; char **alphas;
	parses vec[stp] to vec[endp] into strings !isspace held in the
two dimensional character array alphas with intgrs holding the purely
integer fields.  Returns the total number of fields parsed.
*/


{ char *atemp;
  int i,j,k,icount,is_intgr,acount;

  /* read the input field char by char until you come to a blank -- */
  /* indicating a termination of that field.  Then test to see if that */
  /* field is all digits.  If it is enter it into the integer array */
  /* that will be passed back to the calling program, else continue */
  /* reading the vec for the next string and blank. */
  
  atemp = (char *)our_alloc(MAX_WORD_SIZE * sizeof(char));
  icount = 0;
  acount = 0;

  /* intialize alphas and intgrs arrays so old data doesn't stay around */
  for (i = 0; i < MAX_WORDS; i++) alphas[i][0] = '\0';
  for (i = 0; i < MAX_I;     i++) intgrs[i] = 0;

  for (i=stp; i<endp;) {
    k=0;
    while((isspace(vec[i])) && (i<endp)) i++;
    if (i==endp) return(0);
    is_intgr = 1;
    while((!isspace(vec[i])) && (i<endp)) 
      if (!isdigit(atemp[k++]=vec[i++])) is_intgr = 0;
    atemp[k] = '\0';

    /* store it in the intrs array only if it is all intgrs */
    if (is_intgr) {
      sscanf(atemp,"%d",intgrs+icount);
      icount++;
      if (icount > MAX_I) 
	printf("ERROR: Number of integers parsed was greater than MAX_I allowable\n");
    }
      if (k>MAX_WORD_SIZE) {
	printf("ERROR: Word parsed was bigger than MAX_WORD_SIZE allowable\n");
	exit(1);
      }

    /* store it in the alphas array no matter what */
      for (j=0; atemp[j]; j++) alphas[acount][j] = atemp[j];
      alphas[acount][j] = '\0';
      acount++;
      if (acount>MAX_WORDS) {
	printf("ERROR: Number of words parsed greater than MAX_WORDS\n");
	exit(1);
      }
  }

  our_free(atemp);
  return(acount);
}





int parse_intgrs (vec,intgrs,stp,endp)
char *vec;
int *intgrs;
int stp,endp;

/*int parse_intgrs(vec,intgrs,stp,endp)
	input: char *vec; int stp,endp;
	output: int *intgrs;
	parses vec[stp] to vec[endp] into strings of integers --
throws away char strings.  Saves the integers in the array intgrs.
Returns the number of integer fields found. */

/*char *vec;
int *intgrs;
int stp,endp;*/

{ char *temp;
  int i,k,icount,is_intgr;
  int temp_size = 256;
/* read the input field char by char until you come to a blank -- */
 /* indicating a termination of that field.  Then test to see if that */
 /* field is all digits.  If it is enter it into the integer array */
 /* that will be passed back to the calling program, else continue */
 /* reading the vec for the next string and blank. */

temp = (char *)our_alloc(temp_size * sizeof(char));
icount = 0;
for (i=stp; i<endp;) {
  k=0;
  while((isspace(vec[i])) && (i<endp)) i++;
  is_intgr = 1;
  while((!isspace(vec[i])) && (i<endp)) 
    if (!isdigit(temp[k++]=vec[i++])) is_intgr = 0;
  if (k>temp_size) printf("ERROR: Integer parsed greater than maximum size integer allowed:\n  See  parse_intgrs subroutine\n");


  if (is_intgr) {
    temp[k] = '\0';
    sscanf(temp,"%d",intgrs+icount);
    icount++;
  }
}
our_free(temp);
return(icount-1);
}










int file_to_vec(f_name,vec,vec_st,max_n,str)
char *f_name;
char *vec; 
int *vec_st;
int max_n; 
char *str;

/* file_to_vec(f_name, vec, vec_st,max_n)
	input: char *f_name; int max_n;
	output: char *vec, int *vec_st;
	reads the input file, f_name, to a vec array of chars of
size vec_st with a maximum size of max_n 

returns an error message in str, if there is one. Returns a 0 if there
was some problem when reading in the file */

{
    FILE *fopen(), *fp;
    int i;

    if (NULL == (fp = fopen(f_name, "r"))) {
	sprintf(str,"\nERROR: file %s does not exist.  See file_to_vec.c\n",f_name);
	return(0);
    }
    for (i = 0; i < max_n; i++) 
      if (EOF == (vec[i] = getc(fp))) {
	vec[++i] = '\0';
	*vec_st = i;
	return(1);
      }
    sprintf (str,"vector size exceeded when reading file %s\n",f_name);
    return(0);
}	
	

int space_file_to_vec(f_name,vec,vec_st,max_n,str)
char *f_name;
char *vec;
int *vec_st;
int max_n;
char *str;
/* space_file_to_vec(f_name, vec, vec_st,max_n)
	input: char *f_name; int max_n;
	output: char *vec, int *vec_st;
	reads the input file, f_name, to a vec array of chars of
size vec_st with a maximum size of max_n , puts in a space after all
new lines

returns an error message in str, if there is one. Returns a 0 if there
was some problem when reading in the file */

{
    FILE *fopen(), *fp;
    int i;

    if (NULL == (fp = fopen(f_name, "r"))) {
	sprintf(str,"\nERROR: file %s does not exist.  See file_to_vec.c\n",f_name);
	return(0);
    }
    for (i = 0; i < max_n; i++) {
      vec[i]=getc(fp);
      if (vec[i]==EOF) {
	vec[++i] = '\0';
	*vec_st = i;
	return(1);
      }
      if (vec[i]=='\n') vec[++i]=' ';
    }
    sprintf (str,"vector size exceeded when reading file %s\n",f_name);
    return(0);
}	
	
#if defined(XVERSION) || defined(TEXTVERSION)
/* variables relating to the opening of ted windows */
extern int tedNedBases;
extern int tedleftCutoff;
extern int tedSeqLen; /* NedBases-rightCutoff-leftCutoff */
extern char tedType[4]; /*ABI, PLN or ALF */
extern char tedFileName[200]; /* usually just the sequence filename less the .seq
 */
#endif

#ifdef SUBVERSION
/* variables relating to the opening of ted windows */
int tedNedBases;
int tedleftCutoff;
int tedSeqLen; /* NedBases-rightCutoff-leftCutoff */
char tedType[4]; /*ABI, PLN or ALF */
char tedFileName[200]; /* usually just the sequence filename less the .seq
 */
#endif


int ted_nnl_file_to_vec(f_name,vec,max_n)
char *f_name;
char *vec;
int max_n;
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

{
    FILE *fopen(), *fp;
    int i,j;
    char line[500];
    int lookBlank,lookBlank_i,newLine;
    int MAXLINE=500;
    int comment=0; /* keeps track of whether there was 
		    a comment line or not */
    int more_comments=0;


    lookBlank=0;
    newLine=1;
    


    if (NULL == (fp = fopen(f_name, "r"))) {
      return(0);
    }

    /* go read the first line of the sequence to see if it is
       a ted file or a comment line */
    fgets(line, MAXLINE, fp); /* fgets only reads a line at a time*/

    if (line[0]==';')  {
    sscanf(line,";%6d%6d%6d%s%s\n",&tedNedBases,&tedleftCutoff,&tedSeqLen,tedType,tedFileName);
    comment=1;
  }
    else { /* I check to make sure that tedType[0] is either A or P for ABI or PLN
	      before I will actually try to run a ted window.  So if I blank out
	      the value for tedType when the file wasn't ted file,  then the
	      program will not be tempted to open a ted window on a non-ted
	      file */
      tedType[0]='\0';

    /* also check to see if the first line is a comment, by checking
     for greater than signs */
      comment=0;
      for (i=0; line[i]; i++) {

	if (line[i]=='>') comment=1;
	/* then this must be a comment line, just
	   get the rest of the line and throw it away*/


      }

    }

    
    if (comment==1 || tedType[0]!='\0') more_comments=1;
    /* if there were any comments go past any more lines that were comments */
      /* fgets only reads a line at a time, returns a NULL when eof*/
    while (fgets(line, MAXLINE, fp)!=NULL && more_comments==1)  {
      for (i=0; line[i]; i++) {
	if (line[i]=='>' || line[i]==';') { comment++; break;}
	/* then this must be a comment line, just
	   get the rest of the line and throw it away*/
	more_comments=0;
      }
    }
    

    /* close the file because well start over reading it again after
       we check the first line */
    fclose(fp);


    if (NULL == (fp = fopen(f_name, "r"))) {
      return(0);
    }

    /* if it was a comment or ted file find the end of the first line*/
    if (comment>0) {
      /* then find the end of the first line, the comment */
      for (j=0; j<comment; j++) 
	if (fgets(line, MAXLINE, fp)==NULL) break;
    }





/* all the lookBlank and new line stuff is looking for any
   groups of numbers starting a line if it finds after a 
   newline a set of contiguous numbers then a blank before it finds an
   alpha it assumes those were just nucleotide position
   numbers*/

    for (i = 0; i < max_n; i++) { 
      vec[i] = getc(fp);
      /* make sure you leave the check for vec[i] being EOF 
	 as the first check*/
      if (vec[i] == EOF) {
	vec[i] = '\0';
	return(i);
      }

      /*check if the sequence is numbered on its left end side*/
      if (isdigit(vec[i]) && newLine==1) {
	/* keep looking to see if you find a blank before any more alphas,
	 which would indicate that there are numbers in the sequence */
	lookBlank=1;
	lookBlank_i = i;
	newLine=0;
      }

      /* make sure that all '-' are 'N' and that
	all characters are uppercase */
      if (vec[i]=='-') vec[i]='N';
      else if (isalpha(vec[i])) {
	newLine=0;
	lookBlank=0;
	if (!isupper(vec[i])) vec[i]=toupper(vec[i]);
      }
      /*throw away any new lines*/
      if (vec[i] == '\n') {
	if (i>-1) i--;
	lookBlank=0;
        newLine=1;
      }
      /*throw away any blanks*/
      if (vec[i] == ' ') {
	if (i>-1) i--;
	if (lookBlank==1) { /* go back and throw away all those
			       digits between the \n and the blank,
			       because they must be numbering of
			       the sequence*/
	   i=lookBlank_i-1;
	 }
      } /* if vec[i] is a blank*/

      if (!isalpha(vec[i]) && !isdigit(vec[i]) && vec[i]!=' ' && i>-1) {
	printf("WARNING: Your sequence contains a non-alphabetic character, %c.\nThis character was removed from the sequence.\n",vec[i]);
	if (i>-1) i--;
      }

     }/* for i=0, i< max_n*/
  
    printf ("file_to_vec vector size exceeded when reading file %s\n",f_name);
    return(0);
   }
	



  

	

















