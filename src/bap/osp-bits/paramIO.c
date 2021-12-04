/* 
  Program Name: paramIO
  File name: paramIO.c
  Purpose: input and output of constraint information, reads constraints
  from files or from the keyboard, prints constraints out to file or
  screen, etc.
  Last Update: Apr 15 1991
  Copyright 1991: LaDeana Hillier and Philip Green

  Change Log:
*/


/* ---- Includes ---- */
#include <stdlib.h>
#include <string.h>
#include "our_allo.h"
#include "paramIO.h"
#include "defn.h" /* macros and stdio */
#include "struct.h"/* global structure defns */
#include "Xmess.h" /* IMPORT: message, popupMessage */
#include "utils.h" /* IMPORT: parse_chars, text_to_output, space_file_to_vec */

 extern Prm prm;
 extern int program_version;
 extern int program_option;
#if defined(XVERSION) || defined(TEXTVERSION)
 extern char *other_seqfn;
#endif

 extern int otherRight;
 extern int otherDouble_stranded;
 extern int double_stranded;


#ifdef SUBVERSION
char other_seqfn[1];
#endif


/* ---- Exports ---- */
void print_params(astr)
/* print constraints to astr */
 char *astr;
 { int j;

   j = strlen(astr);
 sprintf(astr+j," PROD_LEN_MIN %d",prm.prod_len_low);
   j = strlen(astr);
 sprintf(astr+j,"\n PROD_LEN_MAX %d ",prm.prod_len_high);
   j = strlen(astr);
 sprintf(astr+j,"\n PROD_GC_MIN %3.0f",(prm.prod_gc_low*100.0));
   j = strlen(astr);
 sprintf(astr+j,"\n PROD_GC_MAX %3.0f ",(prm.prod_gc_high*100.0));
   j = strlen(astr);
 sprintf(astr+j,"\n PROD_TM_MIN %4.1f ",prm.prod_tm_low);
   j = strlen(astr);
 sprintf(astr+j,"\n PROD_TM_MAX %4.1f",prm.prod_tm_high);
   j = strlen(astr);
 sprintf(astr+j,"\n PRIM_GC_MIN %3.0f",(prm.prim_gc_low*100.0));
   j = strlen(astr);
 sprintf(astr+j,"\n PRIM_GC_MAX %3.0f",(prm.prim_gc_high*100.0));
   j = strlen(astr);
  sprintf(astr+j,"\n PRIM_LEN_MIN %d",prm.min_prim_len);
   j = strlen(astr);
 sprintf(astr+j,"\n PRIM_LEN_MAX %d",prm.max_prim_len);
   j = strlen(astr);
 sprintf(astr+j,"\n PRIM_TM_MIN %4.1f",prm.prim_tm_low);
   j = strlen(astr);
 sprintf(astr+j,"\n PRIM_TM_MAX %4.1f",prm.prim_tm_high);
   j = strlen(astr);
 sprintf(astr+j,"\n PRIM_TM_DIFF %4.1f",prm.delta_tm_cut);
   j = strlen(astr);
 sprintf(astr+j,"\n PRIM_NUCS %s",prm.end_nucs);
   j = strlen(astr);
 sprintf(astr+j,"\n AT_SCORE %4.1f",prm.AT_score);
   j = strlen(astr);
 sprintf(astr+j,"\n CG_SCORE %4.1f",prm.CG_score);
   j = strlen(astr);
 sprintf(astr+j,"\n WT_AMBIG %5s",prm.wt_ambig);
   j = strlen(astr);
 sprintf(astr+j,"\n PRIM_SELF_I_ANN %4.1f",prm.selfI_hmlg_cut);
   j = strlen(astr);
 sprintf(astr+j,"\n PRIM_SELF_3_ANN %4.1f ",prm.self3_hmlg_cut);
   j = strlen(astr);
 sprintf(astr+j,"\n PRIM_PRIM_I_ANN %4.1f",prm.ppI_hmlg_cut);
   j = strlen(astr);
 sprintf(astr+j,"\n PRIM_PRIM_3_ANN %4.1f ",prm.pp3_hmlg_cut);
   j = strlen(astr);
 sprintf(astr+j,"\n PRIM_PROD_I_ANN %4.1f",prm.primprodI_hmlg_cut);
   j = strlen(astr);
 sprintf(astr+j,"\n PRIM_PROD_3_ANN %4.1f ",prm.primprod3_hmlg_cut);
   j = strlen(astr);
 sprintf(astr+j,"\n PRIM_OTHER_I_ANN %4.1f",prm.primotherI_hmlg_cut);
   j = strlen(astr);
 sprintf(astr+j,"\n PRIM_OTHER_3_ANN %4.1f ",prm.primother3_hmlg_cut);
   j = strlen(astr);
 sprintf(astr+j," \n WT_PROD_LEN %5.2f",prm.wt_prod_len);
   j = strlen(astr);
 sprintf(astr+j,"\n WT_PROD_GC %5.2f",prm.wt_prod_gc);
   j = strlen(astr);
 sprintf(astr+j,"\n WT_PROD_TM %5.2f",prm.wt_prod_tm);
   j = strlen(astr);
 sprintf(astr+j,"\n WT_PRIM_1_GC %5.2f",prm.wt_prim_s_gc);
   j = strlen(astr);
 sprintf(astr+j,"\n WT_PRIM_2_GC %5.2f",prm.wt_prim_a_gc);
   j = strlen(astr);
 sprintf(astr+j,"\n WT_PRIM_1_LEN %5.2f",prm.wt_prim_s_len);
   j = strlen(astr);
 sprintf(astr+j,"\n WT_PRIM_2_LEN %5.2f",prm.wt_prim_a_len);
   j = strlen(astr);
 sprintf(astr+j,"\n WT_PRIM_1_TM %5.2f",prm.wt_prim_s_tm);
   j = strlen(astr);
   sprintf(astr+j,"\n WT_PRIM_2_TM %5.2f",prm.wt_prim_a_tm);
   j = strlen(astr);
 sprintf(astr+j,"\n WT_PRIM_TM_DIFF %5.2f",prm.wt_delta_tm_cut);
   j = strlen(astr);
 sprintf(astr+j,"\n WT_PRIM_SELF_I_ANN %5.2f",prm.wt_selfI_hmlg_cut);
   j = strlen(astr);
 sprintf(astr+j,"\n WT_PRIM_SELF_3_ANN %5.2f",prm.wt_self3_hmlg_cut);
   j = strlen(astr);
 sprintf(astr+j,"\n WT_PRIM_PRIM_I_ANN %5.2f",prm.wt_ppI_hmlg_cut);
   j = strlen(astr);
 sprintf(astr+j,"\n WT_PRIM_PRIM_3_ANN %5.2f",prm.wt_pp3_hmlg_cut);
   j = strlen(astr);
 sprintf(astr+j,"\n WT_PRIM_PROD_I_ANN %5.2f",prm.wt_primprodI_hmlg_cut);
   j = strlen(astr);
 sprintf(astr+j,"\n WT_PRIM_PROD_3_ANN %5.2f",prm.wt_primprod3_hmlg_cut);
   j = strlen(astr);
 sprintf(astr+j,"\n WT_PRIM_OTHER_I_ANN %5.2f",prm.wt_primotherI_hmlg_cut);
   j = strlen(astr);
 sprintf(astr+j,"\n WT_PRIM_OTHER_3_ANN %5.2f",prm.wt_primother3_hmlg_cut);
   j = strlen(astr);
 sprintf(astr+j,"\n NUM_STRANDS %d",double_stranded+1);
   j = strlen(astr);
 sprintf(astr+j,"\n OTHER_SEQ_NAME %s",other_seqfn);
   j = strlen(astr);
 sprintf(astr+j,"\n OTHER_SEQ_NUM_STRAND %d",otherDouble_stranded+1);
 j = strlen(astr);
 if (otherRight==1) 
   sprintf(astr+j,"\n OTHER_SEQ_STRAND top");
 else
   sprintf(astr+j,"\n OTHER_SEQ_STRAND bottom");
   j = strlen(astr);
  sprintf(astr+j,"\n *\n");
   j = strlen(astr);
 }



int ReadDef(def_fn, Xup)
char *def_fn;
int Xup; /* 1 if X is up and running, 0 if this is
	    the text version or if this is the X version
	    and the default filename was a command line paramter;
	    then I can not use the popupmessage or message to
	    an x window because the x application has not
	    started up yet*/
/* read in the default file , returns
 a 0 if there was a problem when reading in the parameter file*/
{ 
       int *intgrs;
       char **alphas;
       char *str;
       int i;
       FILE *gp;
       int n,j;
       int line_count,vec_st,max_n;
       int readdef_okay=1;
       char *mess_str;
       int max_param_str_len=5000;
       
       
       str = (char *)our_alloc(max_param_str_len * sizeof(char ));
       mess_str = (char *)our_alloc(1000 * sizeof(char ));
       intgrs = (int *)our_alloc(MAX_I * sizeof(int));
       alphas = (char **)our_alloc(MAX_WORDS * sizeof(char *));
       for (i=0; i < MAX_WORDS; i++) alphas[i] = (char *)our_alloc(MAX_WORD_SIZE*sizeof(char));
       
       max_n = 3000;
       n=0;
       
  /* open the default parameters file to see if any old contents are there*/
  if ((gp=fopen(def_fn,"r"))==NULL) {
    if (!Xup)
      printf("No file found with name %s\n original default parameters assumed\n",def_fn);
    else {
      sprintf(str,"No file found with name %s\n original default parameters assumed\n",def_fn);
    message(str);
    }
    readdef_okay=0;
  }
  else {
    if (getc(gp)==EOF) {
      if (!Xup)
	printf("File %s is empty, original default parameters assumed.\n",def_fn);
      else {
	sprintf(str,"File %s is empty, original default parameters assumed.\n",def_fn);
	message(str);
      }
      readdef_okay=0;
    }
    else { /*read in old default parameters */
      readdef_okay=space_file_to_vec(def_fn, str, &vec_st, max_n,mess_str);
      if (!readdef_okay) {
	message(mess_str);
	goto freeVar;
      }
      if (strlen(str)>max_param_str_len) {
	message("Parameter file larger than max size allowable");
	popUpErrorMessage();
	readdef_okay=0;
	goto freeVar;
      }

      line_count=1;
      for (i=0; str[i]; i++) {
	line_count++;
        /*get the first line and 
	  put spaces in place of any equal signs*/
	for (j=i; str[j] != '\n'; j++) if (str[j]=='=') str[j]=' ';
	parse_chars(str,intgrs,i,j,alphas);
	if (read_params(alphas[0],alphas[1],intgrs[0])==0) break;
	j++;
	i=j;
      } /* for i = 0... */
    } /* else*/
  } /* else*/
	fclose(gp);

     freeVar:;
       our_free(str);
       our_free(mess_str);
       our_free(intgrs);
      for (i = 0; i < MAX_WORDS; i++) our_free(alphas[i]);
       our_free(alphas);
       return(readdef_okay);
     }



int read_params(var_name,curr_string,curr_int)
char *var_name;
char *curr_string;
int curr_int;
/*this fcn is used to interpret strings in .def file or from keyboard*/
/*     var_name is the name of the variable, 
       curr_string is the string value of the variable;
       curr_int is the first integer on the line, usually the
	value of the variable,
	
	this function returns a 0 if it is the last parameter
	and a 1 if there must be more; it thinks its the last
	 parameter if the var_name is '*' */
{  int i;

  if (!strcmp(var_name,"*")) return(0);
  if (!strcmp(var_name,"PRIM_NUCS")) {
    if (strlen(curr_string) > MAX_NAME_SIZE)
      popupMessage("Length of PRIM_NUCS is larger than size, MAX_NAME_SIZE, allowable\nChoice of PRIM_NUCS ignored.");
    else
      strcpy(prm.end_nucs,curr_string);
    return(1);
  }

  if (!strcmp(var_name,"PROD_LEN_MIN")) prm.prod_len_low = curr_int;
  else if (!strcmp(var_name,"PROD_LEN_MAX")) prm.prod_len_high = curr_int;
  else if (!strcmp(var_name,"PROD_GC_MIN")) {
    sscanf(curr_string,"%f",&prm.prod_gc_low);
    prm.prod_gc_low/=100.0;
  }
  else if (!strcmp(var_name,"PROD_GC_MAX")) {
    sscanf(curr_string,"%f",&prm.prod_gc_high);
    prm.prod_gc_high/=100.0;
  }
  else if (!strcmp(var_name,"PROD_TM_MIN")) 
    sscanf(curr_string,"%f",&prm.prod_tm_low);
  else if (!strcmp(var_name,"PROD_TM_MAX")) 
    sscanf(curr_string,"%f",&prm.prod_tm_high);
  else if (!strcmp(var_name,"PRIM_LEN_MIN")) prm.min_prim_len = curr_int;
  else if (!strcmp(var_name,"PRIM_LEN_MAX")) prm.max_prim_len = curr_int;
  else if (!strcmp(var_name,"PRIM_GC_MIN")) {
    sscanf(curr_string,"%f",&prm.prim_gc_low);
    prm.prim_gc_low/=100.0;
  }
  else if (!strcmp(var_name,"PRIM_GC_MAX")) {
    sscanf(curr_string,"%f",&prm.prim_gc_high);
    prm.prim_gc_high/=100.0;
  }
  else if (!strcmp(var_name,"PRIM_TM_MIN"))
     sscanf(curr_string,"%f",&prm.prim_tm_low);
  else if (!strcmp(var_name,"PRIM_TM_MAX")) 
     sscanf(curr_string,"%f",&prm.prim_tm_high);
  else if (!strcmp(var_name,"PRIM_SELF_I_ANN")) 
     sscanf(curr_string,"%f",&prm.selfI_hmlg_cut);
  else if (!strcmp(var_name,"PRIM_SELF_3_ANN")) 
     sscanf(curr_string,"%f",&prm.self3_hmlg_cut);
  else if (!strcmp(var_name,"PRIM_PRIM_I_ANN"))
     sscanf(curr_string,"%f",&prm.ppI_hmlg_cut);
  else if (!strcmp(var_name,"PRIM_PRIM_3_ANN"))
     sscanf(curr_string,"%f",&prm.pp3_hmlg_cut);
  else if (!strcmp(var_name,"PRIM_PROD_I_ANN"))
    sscanf(curr_string,"%f",&prm.primprodI_hmlg_cut);
  else if (!strcmp(var_name,"PRIM_PROD_3_ANN")) 
    sscanf(curr_string,"%f",&prm.primprod3_hmlg_cut);
  else if (!strcmp(var_name,"PRIM_OTHER_I_ANN"))
    sscanf(curr_string,"%f",&prm.primotherI_hmlg_cut);
  else if (!strcmp(var_name,"PRIM_OTHER_3_ANN"))
    sscanf(curr_string,"%f",&prm.primother3_hmlg_cut);
  else if (!strcmp(var_name,"PRIM_TM_DIFF"))
     sscanf(curr_string,"%f",&prm.delta_tm_cut);
  else if (!strcmp(var_name,"WT_PROD_LEN")) 
    sscanf(curr_string,"%f",&prm.wt_prod_len);
  else if (!strcmp(var_name,"WT_PROD_GC"))
    sscanf(curr_string,"%f",&prm.wt_prod_gc);
  else if (!strcmp(var_name,"WT_PROD_TM"))
    sscanf(curr_string,"%f",&prm.wt_prod_tm);
  else if (!strcmp(var_name,"WT_PRIM_1_LEN"))
    sscanf(curr_string,"%f",&prm.wt_prim_s_len);
  else if (!strcmp(var_name,"WT_PRIM_2_LEN"))
    sscanf(curr_string,"%f",&prm.wt_prim_a_len);
  else if (!strcmp(var_name,"WT_PRIM_1_GC"))
    sscanf(curr_string,"%f",&prm.wt_prim_s_gc);
  else if (!strcmp(var_name,"WT_PRIM_2_GC"))
    sscanf(curr_string,"%f",&prm.wt_prim_a_gc);
  else if (!strcmp(var_name,"WT_PRIM_1_TM"))
    sscanf(curr_string,"%f",&prm.wt_prim_s_tm);
  else if (!strcmp(var_name,"WT_PRIM_2_TM"))
    sscanf(curr_string,"%f",&prm.wt_prim_a_tm);
  else if (!strcmp(var_name,"WT_PRIM_TM_DIFF"))
    sscanf(curr_string,"%f",&prm.wt_delta_tm_cut);
  else if (!strcmp(var_name,"WT_PRIM_SELF_I_ANN"))
    sscanf(curr_string,"%f",&prm.wt_selfI_hmlg_cut);
  else if (!strcmp(var_name,"WT_PRIM_SELF_3_ANN")) 
    sscanf(curr_string,"%f",&prm.wt_self3_hmlg_cut);
  else if (!strcmp(var_name,"WT_PRIM_PRIM_I_ANN"))
    sscanf(curr_string,"%f",&prm.wt_ppI_hmlg_cut);
  else if (!strcmp(var_name,"WT_PRIM_PRIM_3_ANN"))
    sscanf(curr_string,"%f",&prm.wt_pp3_hmlg_cut);
  else if (!strcmp(var_name,"WT_PRIM_PROD_I_ANN")) 
    sscanf(curr_string,"%f",&prm.wt_primprodI_hmlg_cut);
  else if (!strcmp(var_name,"WT_PRIM_PROD_3_ANN"))
    sscanf(curr_string,"%f",&prm.wt_primprod3_hmlg_cut);
  else if (!strcmp(var_name,"WT_PRIM_OTHER_I_ANN"))
    sscanf(curr_string,"%f",&prm.wt_primotherI_hmlg_cut);
  else if (!strcmp(var_name,"WT_PRIM_OTHER_3_ANN")) 
    sscanf(curr_string,"%f",&prm.wt_primother3_hmlg_cut);
  else if (!strcmp(var_name,"AT_SCORE")) 
    sscanf(curr_string,"%f",&prm.AT_score);
  else if (!strcmp(var_name,"CG_SCORE")) 
    sscanf(curr_string,"%f",&prm.CG_score);
  else if (!strcmp(var_name,"WT_AMBIG")) {
    if (strlen(curr_string)>6) 
      popupMessage("WT_AMBIG should be full or avg\nValue for WT_AMBIG ignored\n");
    else
	strcpy(prm.wt_ambig,curr_string);
  }
  else if (!strcmp(var_name,"NUM_STRANDS")) 
    double_stranded=curr_int-1;
  else if (!strcmp(var_name,"OTHER_SEQ_NAME")) {
    if (strlen(curr_string)>MAX_NAME_SIZE) 
      popupMessage("OTHER_SEQ_NAME longer than MAX_NAME_SIZE\nPlease use a shorter name.\n");
    else
      sscanf(curr_string,"%s",other_seqfn);
  }
  else if (!strcmp(var_name,"OTHER_SEQ_STRAND")) {
    for (i=0; curr_string[i]; i++) {
      if (curr_string[i]==' ') i--;
      else if (isupper(curr_string[i])) curr_string[i]=tolower(curr_string[i]);
    }
    if (!strcmp(curr_string,"bottom"))
      otherRight=0;
    else otherRight=1;
  }
  else if (!strcmp(var_name,"OTHER_SEQ_NUM_STRAND"))
    otherDouble_stranded=curr_int-1;
  else  printf("\n\n ERROR: parameter name %s unknown\n\n", var_name);

  return(1);
}

int check_params()
     /* check to see for each program option that the parameters
	are what you expect, i.e. set to 0 things that shouldn't
	matter for program_version==1, nemo.
	*/
{
  char *str;
  int temp_int;
  float temp_float;
  
  str = (char *)our_alloc(1000 * sizeof(char));
  
  /* PRODUCT CONSTRAINTS */
  /* product length constraints, and distance from end constraints */
  if (prm.prod_len_low > prm.prod_len_high  && prm.prod_len_high != 0)  {
    sprintf(str,"Product minimum length must be less\n  than product maximum length\n");
    sprintf(str+strlen(str),"The minimum length now\n  equals the maximum and vice versa\n");
    popupMessage(str);
    temp_int = prm.prod_len_low;
    prm.prod_len_low=prm.prod_len_high;
    prm.prod_len_high = temp_int;
  }
  if (program_option != 3) {
    /* product gc content*/
    if (prm.prod_gc_low > prm.prod_gc_high && prm.prod_gc_high != 0 )  {
      sprintf(str,"Product minimum gc content must be less\n  than product maximum gc content\n");
      sprintf(str+strlen(str),"The minimum gc content now\n  equals the maximum and vice versa\n");
      popupMessage(str);
      temp_float = prm.prod_gc_low;
      prm.prod_gc_low=prm.prod_gc_high;
      prm.prod_gc_high = temp_float;
    }
    /* product upper and lower bound on melting temperature, degrees C */
    if (prm.prod_tm_low > prm.prod_tm_high && prm.prod_tm_high != 0 )  {
      sprintf(str,"Product minimum Tm must be less than\n  product maximum Tm\n");
      sprintf(str+strlen(str),"The minimum Tm now equals\n  the maximum and vice versa\n");
      popupMessage(str);
      temp_float = prm.prod_tm_low;
      prm.prod_tm_low=prm.prod_tm_high;
      prm.prod_tm_high = temp_float;
    }
  }
  
  /* PRIMER CONSTRAINTS */
  /* minimum and maximum primer length */
  if (prm.min_prim_len < 2) {
    if (prm.min_prim_len==0) sprintf(str,"CONSTRAINT ERROR:\nYou must choose a minimum primer length.\n0 is not acceptable\n");
    else {
      sprintf(str,"CONSTRAINT ERROR:\nYour minimum primer length should be 2 or greater\nIt has been reset to 2.\n");
      sprintf(str+strlen(str),"Please reexamine your constraint settings\nand run the program again.\n");
    }
    popupMessage(str);
    our_free(str);
    return(0);
  }
  if (prm.min_prim_len > 1000) {
    sprintf(str,"CONSTRAINT ERROR:\nYour minimum primer length should be\n  less than 1000\n");
    sprintf(str+strlen(str),"min_prim_len has been reset\n");
    sprintf(str+strlen(str),"Please reexamine your constraint settings\n  and run the program again.\n");
    prm.min_prim_len=1000;
    popupMessage(str);
    our_free(str);
    return(0);
  }
  if (prm.max_prim_len > 1000 || prm.max_prim_len == 0) {
    if (prm.max_prim_len==0)
      sprintf(str,"CONSTRAINT ERROR:\nYou must choose a maximum primer length;\n 0 is not acceptable\n");
    else
      sprintf(str,"CONSTRAINT ERROR:\nYour maximum primer length should be less than 1000\n");
    sprintf(str+strlen(str),"max_prim_len has been reset to min_prim_len+4\n");
    sprintf(str+strlen(str),"Please reexamine your constraint settings\n  and run the program again.\n");
    prm.max_prim_len=prm.min_prim_len+4;
    popupMessage(str);
    our_free(str);
    return(0);
  }
  if (prm.min_prim_len > prm.max_prim_len) {
    sprintf(str,"CONSTRAINT ERROR:\nYour minimum primer length was set to greater than\n  your maximum primer length\n");
    sprintf(str+strlen(str),"Your maximum primer length has\n  been reset to min_prim_len + 4\n");
    sprintf(str+strlen(str),"Please reexamine your constraint\n  settings and run the program again.\n");
    popupMessage(str);
    prm.max_prim_len=prm.min_prim_len + 4;
    our_free(str);
    return(0);
  }

  if (prm.min_prim_len==prm.max_prim_len) 
    prm.max_prim_len+=1;



/* end nucleotides*/
  if (prm.end_nucs[0]=='0' || (!strcmp(prm.end_nucs,""))) {
    sprintf(str,"You must choose a primer-ending nucleotide.\n  S has been selected for you.\n");
    popupMessage(str);
    sprintf(prm.end_nucs,"S");
  }

  
  /* primer gc content*/
  if (prm.prim_gc_low > prm.prim_gc_high && prm.prim_gc_high != 0 )  {
    sprintf(str,"Primer minimum gc content must be less\n  than primer maximum gc content\n");
    sprintf(str+strlen(str),"The minimum gc content now\n  equals the maximum and vice versa\n");
    popupMessage(str);
    temp_float = prm.prim_gc_low;
    prm.prim_gc_low=prm.prim_gc_high;
    prm.prim_gc_high = temp_float;
  }
  
  /* primer upper and lower bound on melting temperature, degrees C */
  if (prm.prim_tm_low > prm.prim_tm_high && prm.prim_tm_high != 0 )  {
    sprintf(str,"Primer minimum Tm must be less\n  than primer maximum Tm\n");
    sprintf(str+strlen(str),"The minimum Tm now\n  equals the maximum and vice versa\n");
    popupMessage(str);
    temp_float = prm.prim_tm_low;
    prm.prim_tm_low=prm.prim_tm_high;
    prm.prim_tm_high = temp_float;
  }
  
  
  /* ANNEALING CONSTRAINTS, I will not put constraints on those */
  /*homology cutoff scores primer-self*/
  /*prm.self3_hmlg_cut;*/
  /*prm.selfI_hmlg_cut;*/
  /*prm.pp3_hmlg_cut,prm.ppI_hmlg_cut;*/ /*
    homology cutoff scores primer-primer (not used in nemo)*/
  /*prm.primprodI_hmlg_cut, prm.primprod3_hmlg_cut;*/  /*cutoff scores for primer
    product annealing scores*/
  /*prm.primotherI_hmlg_cut,prm.primother3_hmlg_cut;*/ /*homology cutoff scores for 
    the primer versus the
    other sequence file */
  
  /* the other sequence file contains vector, cosmid, repeated sequences, etc */
  /*prm.delta_tm_cut;*/ /* cutoff for difference in tm between primer1 and tm of primer2*/
  
  if (program_version==1) { /*nemo*/
    prm.wt_prod_gc=0;
    prm.wt_prod_tm=0;
    prm.wt_prim_a_len=0;
    prm.wt_prim_a_gc=0;
    prm.wt_prim_a_tm=0;
    prm.wt_pp3_hmlg_cut=0;
    prm.wt_ppI_hmlg_cut=0;
    prm.wt_primprodI_hmlg_cut=0;
    prm.wt_primprod3_hmlg_cut=0;
    prm.delta_tm_cut=0;
  }
  
  
  /* scoring weights, used for ranking of primers chosen */
  /*prm.wt_prod_len;
    prm.wt_prod_gc;
    prm.wt_prod_tm;
    prm.wt_prim_s_len;
    prm.wt_prim_a_len;
    prm.wt_prim_s_gc;
    prm.wt_prim_a_gc;
    prm.wt_prim_s_tm;
    prm.wt_prim_a_tm;
    prm.wt_self3_hmlg_cut,prm.wt_selfI_hmlg_cut;
    prm.wt_pp3_hmlg_cut,prm.wt_ppI_hmlg_cut;
    prm.wt_primprodI_hmlg_cut,prm.wt_primprod3_hmlg_cut;
    prm.wt_primotherI_hmlg_cut,prm.wt_primother3_hmlg_cut;
    prm.wt_delta_tm_cut;*/
  /*prm.AT_score;*/ /* used in the calculation of annealing score, each time
    an A-T align, the score would be prm.AT_score */
  
  /*prm.CG_score;*/ 
  /* used in the calculation of annealing score, each time
     an C-G align, the score would be prm.CG_score */
  
  
  if (strlen(prm.wt_ambig)>5) sprintf(prm.wt_ambig,"%5s",prm.wt_ambig);
  
  our_free(str);
  return(1);
}


void write_params_file(fn)
char *fn;
{ FILE *gp;
  int max_param_str_len=5000;
  char *pstr; /* holds parameters string*/
  char *astr; /* holds any error message*/


  pstr = (char *)our_alloc(max_param_str_len * sizeof(char));
  astr = (char *)our_alloc(200 *sizeof(char));


    if ((gp=fopen(fn,"r"))!=NULL) { /* if it exists */
      /* wipe out the file */
      fclose(gp);
      if ((gp=fopen(fn,"w"))==NULL) {
        sprintf(astr,"ERROR: file %s cannot be written to.",fn);
	message(astr);
        fclose(gp);  
        our_free(astr);
        our_free(pstr);
        return;
      }
    }

    fclose(gp);
    pstr[0] = '\0';
    print_params(pstr);
    if (strlen(pstr)>max_param_str_len) {
        message("Parameter file larger than max size allowable");
        popUpErrorMessage();
      }
    text_to_output(pstr,0,strlen(pstr),1,fn);

  our_free(pstr);
  our_free(astr);
  return;
}

  

















