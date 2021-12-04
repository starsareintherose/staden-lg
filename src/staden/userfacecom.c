#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include "userface.h"
#include "helpnmenu.h"
#include "FtoC.h" /* IMPORT: Fstr2Cstr and Cstr2Fstr */

/*
 * Prompts the user with either one yes/no question or another.
 * Args:
 *   choice: which question to ask (0 = prompt1, 1 = prompt2)
 *   prompt[12]: the questions.
 * Returns;
 *    0 = yes
 *    1 = no
 *   -1 = cancel
 */
int yesono(int choice, char *prompt1, char *prompt2) {
    int a;

    switch(choice) {
    case 0:
	return yesno(prompt1);
    case 1:
	a = yesno(prompt2);
	if (a>=0)
	    a ^= 1;
	return a;
    default:
	fputs("yesno() called with bad 'choice'\n", stderr);
	return -1;
    }
}

/*--------------------------------------------------------------------------*\
|*									    *|
|* FORTRAN interface routines. These are overlays on top of the C routines. *|
|* There is no actual 'real' code here - that is in either userface.c (for  *|
|* the text version) or dialogues.c (for the X windows version)		    *|
|*									    *|
\*--------------------------------------------------------------------------*/

void getint_x(int_f *MININ_p,
	      int_f *MAXIN_p,
	      int_f *DEF_p,
	      char  *PROMPT_p,
	      int_f *VALUE_p,  /* out: result */
	      int_f *KBIN_p,
	      int_f *KBOUT_p,
	      int_f *IHELPS_p,
	      int_f *IHELPE_p,
	      char  *HELPF_p,
	      int_f *IDEVH_p,
	      int_f *IOK_p,    /* out: status */
	      int_fl PROMPT_l,
	      int_fl HELPF_l)
/*
 * Prompt for an integer in the range *MININ_p <= i <= *MAXIN_p
 * IOK=0 => OK
 * IOK=1 => Cancel
 */
{
    char prompt[256];
    int status;

    Fstr2Cstr(PROMPT_p, PROMPT_l, prompt, (int_fl)sizeof(prompt));

    *VALUE_p = getint((int)*MININ_p, (int)*MAXIN_p, (int)*DEF_p,
		      prompt, &status);
    *IOK_p = (int_f)((status < 0)? 1:0);

    return;
}

void getrl_x(float *MININ_p,
	     float *MAXIN_p,
	     float *DEF_p,
	     char  *PROMPT_p,
	     float *VALUE_p,  /* out: result */
	     int_f *KBIN_p,
	     int_f *KBOUT_p,
	     int_f *IHELPS_p,
	     int_f *IHELPE_p,
	     char  *HELPF_p,
	     int_f *IDEVH_p,
	     int_f *IOK_p,
	     int_fl PROMPT_l,
	     int_fl HELPF_l)
/*
 * Prompt for a real in the range *MININ_p <= i <= *MAXIN_p
 * The prompt is printed using format F8.2
 * IOK=0 => OK
 * IOK=1 => Cancel
 */
{
    char prompt[256];
    int status;

    Fstr2Cstr(PROMPT_p, PROMPT_l, prompt, (int_fl)sizeof(prompt));

    *VALUE_p = getfloat(*MININ_p, *MAXIN_p, *DEF_p, prompt, &status, 2);
    *IOK_p = (int_f)((status < 0)? 1:0);

    return;
}

void getrls_x(float *MININ_p,
	      float *MAXIN_p,
	      float *DEF_p,
	      char  *PROMPT_p,
	      float *VALUE_p,  /* out: result */
	      int_f *KBIN_p,
	      int_f *KBOUT_p,
	      int_f *IHELPS_p,
	      int_f *IHELPE_p,
	      char  *HELPF_p,
	      int_f *IDEVH_p,
	      int_f *IOK_p,
	      int_fl PROMPT_l,
	      int_fl HELPF_l)
/*
 * Prompt for a real in the range *MININ_p <= i <= *MAXIN_p
 * The prompt is printed using format F8.2
 * IOK=0 => OK
 * IOK=1 => Cancel
 */
{
    char prompt[256];
    int status;

    Fstr2Cstr(PROMPT_p, PROMPT_l, prompt, (int_fl)sizeof(prompt));

    *VALUE_p = getfloat(*MININ_p, *MAXIN_p, *DEF_p, prompt, &status, 10);
    *IOK_p = (int_f)((status < 0)? 1:0);

    return;
}

void yesno_x(int_f *ANSWER_p,
	     char  *PROMPT_p,
	     int_f *IHELPS_p,
	     int_f *IHELPE_p,
	     char  *HELPF_p,
	     int_f *IDEVH_p,
	     int_f *KBIN_p,
	     int_f *KBOUT_p,
	     int_fl PROMPT_l,
	     int_fl HELPF_l)
/*
 * Prompt the user with *PROMPT_l and get a YES or NO response.
 * ANSWER=-1 => Cancel
 * ANSWER=0  => YES
 * ANSWER=1  => NO
 */
{
    char prompt[256];

    Fstr2Cstr(PROMPT_p, PROMPT_l, prompt, (int_fl)sizeof(prompt));
    *ANSWER_p = (int_f)yesno(prompt);
}

void yesono_x(int_f *CHOICE_p,
	      char  *P1_p,
	      char  *P2_p,
	      int_f *IHELPS_p,
	      int_f *IHELPE_p,
	      char  *HELPF_p,
	      int_f *IDEVH_p,
	      int_f *KBIN_p,
	      int_f *KBOUT_p,
	      int_fl P1_l,
	      int_fl P2_l,
	      int_fl HELPF_l)
/*
 * If CHOICE is 0, present a YESNO dialogue with prompt P1.
 * If CHOICE is 1, present a YESNO dialogue with prompt P2.
 */
{
    char prompt1[256], prompt2[256];

    Fstr2Cstr(P1_p, P1_l, prompt1, (int_fl)sizeof(prompt1));
    Fstr2Cstr(P2_p, P2_l, prompt2, (int_fl)sizeof(prompt2));

    *CHOICE_p = (int_f)yesono(*CHOICE_p, prompt1, prompt2);
}

void gtstr_x(char  *P_p,      /* prompt */
	     char  *STRING_p, /* default value (if *LENGTH_p > 0) */
	     char  *NEW_p,    /* out: dialogue result */
	     int_f *LENGTH_p, /* in:  significant length of *STRING_p
			 	out: significant length of *NEW_p */
	     int_f *KBOUT_p,
	     int_f *KBIN_p,
	     int_f *INFLAG_p, /* out: status */
	     int_fl P_l,
	     int_fl STRING_l,
	     int_fl NEW_l)
/*
 * Prompt with P for a string, default value STRING.
 * Return new string in NEW.
 * INFLAG=0 => OK
 * INFLAG=1 => Help
 * INFLAG=2 => Cancel, LENGTH=1
 * INFLAG=3 => LENGTH=0 and NEW full of blanks (ie default)
 */
{
    char question[256], def[256];
    char *defptr;
    int ret;

    Fstr2Cstr(P_p, P_l, question, (int_fl)sizeof(question));
    if (*LENGTH_p > 0) {
	Fstr2Cstr(STRING_p, STRING_l, def, (int_fl)sizeof(def));
	defptr = def;
    } else
	defptr = 0;

    ret = gtstr(question, defptr, NEW_p, (size_t)((NEW_l==1)?40:NEW_l));
    switch(ret) {
    case -1:
	*INFLAG_p = 2;
	*LENGTH_p = 1;
	return;
    case 0:
	*INFLAG_p = 0;
	*LENGTH_p = (int_f)strlen(NEW_p);
	Cstr2Fstr(NEW_p, NEW_p, NEW_l);
	return;
    case 1:
	*INFLAG_p = 3;
	*LENGTH_p = 0;
    }
}

void getstr_x(char  *P_p,      /* prompt */
	      char  *STRING_p, /* default value (if *LENGTH_p > 0) */
	      char  *NEW_p,    /* out: dialogue result */
	      int_f *MAXSTR_p, /* array length of *STRING_p and *NEW_p */
	      int_f *LENGTH_p, /* in:  significant length of *STRING_p
				 out: significant length of *NEW_p */
	      int_f *KBOUT_p,
	      int_f *KBIN_p,
	      int_f *INFLAG_p, /* out: status */
	      int_fl P_l)
/*
 * Present a dialogue requesting a string result with the given prompt
 * and default.
 *
 * In the Fortran, P is a character string, so it passes both *P_p
 * and P_l. STRING and NEW are character arrays, so we are merely
 * given *STRING_p and *NEW_p.
 */
{
    gtstr_x(P_p,
	    STRING_p,
	    NEW_p,   
	    LENGTH_p,
	    KBOUT_p,
	    KBIN_p,
	    INFLAG_p,
	    P_l,
	    *MAXSTR_p,
	    *MAXSTR_p);
}

void radion_x(char  *TITLE_p,
	      char  *PROMPT_p, /* Actually: *PROMPT_p[*NB_p] */
	      int_f *NB_p,     /* Number of buttons (1..NB) */
	      int_f *CHOICE_p, /* in:  default button
			         out: selected button */
	      int_f *IHELPS_p,
	      int_f *IHELPE_p,
	      char  *HELPF_p,
	      int_f *IDEVH_p,
	      int_f *KBIN_p,
	      int_f *KBOUT_p,
	      int_fl TITLE_l,
	      int_fl PROMPT_l,
	      int_fl HELPF_l)
/*
 * Present a ``radio button'' dialogue allowing the user to select
 * one of a number of prompted items.
 * In the text version this is simply a list of options and a prompt.
 * CHOICE=(1..NB) => That option selected
 * CHOICE=-1      => Cancel
 */
{
    char **opt, title[256];
    int i, numopts = (int)*NB_p;

    /* initialise options array */
    opt = (char **)malloc(numopts * sizeof(char *));
    opt[0] = (char *)malloc((unsigned int)((PROMPT_l+1) * numopts));
    for (i=0; i<numopts; i++) {
	opt[i] = opt[0] + (PROMPT_l+1)*i;
	Fstr2Cstr(PROMPT_p+(i*PROMPT_l), PROMPT_l, opt[i], PROMPT_l+1);
    }

    Fstr2Cstr(TITLE_p, TITLE_l, title, (int_fl)sizeof(title));

    *CHOICE_p = (int_f)radion(title, opt, *NB_p, *CHOICE_p);
    
    free((void *)opt[0]);
    free((void *)opt);
    return;
}

void check4_x(char  *P1_p, char  *P2_p, char  *P3_p, char  *P4_p,
	      int_f *C1_p, int_f *C2_p, int_f *C3_p, int_f *C4_p,
	      int_f *IHELPS_p,
	      int_f *IHELPE_p,
	      char  *HELPF_p,
	      int_f *IDEVH_p,
	      int_f *KBIN_p,
	      int_f *KBOUT_p,
	      int_f *IOK_p,
	      int_fl P1_l, int_fl P2_l, int_fl P3_l, int_fl P4_l,
	      int_fl HELPF_l)
/*
 * Present a ``checkbox'' dialogue allowing the user to alter the
 * Boolean state of four items.
 * C1 - C4 give the initial states of the items: 0 = unset, 1 = set.
 * The final states of the items are also returned in these.
 * IOK=0 => OK
 * IOK=1 => Cancel
 */
{
    int ret;
    char *prompts[4];
    int *bools[4];

    prompts[0] = P1_p; bools[0] = (int *)C1_p;
    prompts[1] = P2_p; bools[1] = (int *)C2_p;
    prompts[2] = P3_p; bools[2] = (int *)C3_p;
    prompts[3] = P4_p; bools[3] = (int *)C4_p;

    ret = checkn(4, prompts, bools);

    if (ret == 0)
	*IOK_p = 0;
    else
	*IOK_p = 1;
}

void getopt_x(int_f *KBIN_p,
	      int_f *KOPT_p,
	      int_f *IOPT_p) /* out: option number selected */
/*
 * Select a function to execute
 * KOPT=0 => OK
 * KOPT=1 => dialogue wanted
 * KOPT=3 => menu option requested
 */
{
    *IOPT_p = (int_f)getcopt((int *)KOPT_p);
    switch(*KOPT_p) {
    case -3:
    case -2:
	*KOPT_p = 0;
	*IOPT_p = 2;
	break;
    case -1:
	*KOPT_p = 0;
	*IOPT_p = 1;
	break;
    }
}

void showfi_x(int_f *KBOUT_p,
	      char  *STRING_p,
	      int_fl STRING_l)
/*
 * Display `STRING', the name of the current file(s).
 */
{
    char func[256];

    Fstr2Cstr(STRING_p, STRING_l, func, (int_fl)sizeof(func));
    showfi(func);
}


void showfu_x(int_f *KBOUT_p,
	      char  *STRING_p,
	      int_fl  STRING_l)
/*
 * Display `STRING', the name of the current function.
 */
{
    char func[256];

    Fstr2Cstr(STRING_p, STRING_l, func, (int_fl)sizeof(func));
    showfu(func);
}

void errom_x(int_f *KBOUT_p,
	     char  *STRING_p,
	     int_fl  STRING_l)
/*
 * Display the error message `STRING'.
 */
{
    char err[256];

    Fstr2Cstr(STRING_p, STRING_l, err, (int_fl)sizeof(err));
    errom(err);
}

void busy_x(int_f *KBOUT_p)
/*
 * Inform the user that there will be a long pause before the
 * next dialogue function is called.
 */
{
    busy();
}

/*
 * Returns:
 *    0 for ok
 *   -1 for cancel
 */
void bpause_x(int_f *KBIN_p,
	      int_f *KBOUT_p,
	      int_f *IOK_p) {

    *IOK_p = (int_f)bpause();
}

