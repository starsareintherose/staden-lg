#ifndef _paramIO_h
#define _paramIO_h

/* 
  Program Name: paramIO
  File name: paramIO.c
  Purpose: input and output of constraint information, reads constraints
  from files or from the keyboard, prints constraints out to file or
  screen, etc.
  Last Update: Fri Mar 23 1991 

  Copyright 1991: LaDeana Hillier and Philip Green
  Change Log:
*/

/*  Description: This function gets new parameters when the
           user changes constraints using the changeParamsPopup */
   extern void GetParams();

/* saves the values that Get Params has gotten from the constraints
information to a file */
   extern void SaveParams();

/* reads in a constraints file */
   extern int ReadDef();

/* checks that all the parameters are within a reasonable range */
   extern int check_params();

/* reads the constraint file name off the of changeParamspopup*/
   extern void inputParams();

/* prints all constraints to an input string*/
   extern void print_params();

/*this fcn is used to interpret strings in .def file or from keyboard*/
/*     this function returns a 0 if it is the last parameter
        and a 1 if there must be more; it thinks its the last
         parameter if the var_name is '*' */
   extern int read_params();

/*write the parameters out to a file*/
extern void write_params_file();

/* popup window allowing you to change parameters */
extern void osp_change_parameters();

#endif /* _paramIO_h */


