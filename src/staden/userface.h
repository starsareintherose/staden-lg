#include "fort.h"

#define yesno_x yesno_
#define yesono_x yesono_
#define gtstr_x gtstr_
#define getstr_x getstr_
#define getint_x getint_
#define getrl_x getrl_
#define getrls_x getrls_
#define radion_x radion_
#define check4_x check4_
#define getopt_x getopt_
#define showfi_x showfi_
#define showfu_x showfu_
#define errom_x errom_
#define busy_x busy_
#define bpause_x bpause_
#define menu_x menu_

int yesono(int choice, char *prompt1, char *prompt2);

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
	      int_fl HELPF_l);

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
	     int_fl HELPF_l);

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
	     int_fl HELPF_l);

void yesno_x(int_f *ANSWER_p,
	     char  *PROMPT_p,
	     int_f *IHELPS_p,
	     int_f *IHELPE_p,
	     char  *HELPF_p,
	     int_f *IDEVH_p,
	     int_f *KBIN_p,
	     int_f *KBOUT_p,
	     int_fl PROMPT_l,
	     int_fl HELPF_l);

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
	      int_fl HELPF_l);

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
	     int_fl NEW_l);

void getstr_x(char  *P_p,      /* prompt */
	      char  *STRING_p, /* default value (if *LENGTH_p > 0) */
	      char  *NEW_p,    /* out: dialogue result */
	      int_f *MAXSTR_p, /* array length of *STRING_p and *NEW_p */
	      int_f *LENGTH_p, /* in:  significant length of *STRING_p
				 out: significant length of *NEW_p */
	      int_f *KBOUT_p,
	      int_f *KBIN_p,
	      int_f *INFLAG_p, /* out: status */
	      int_fl P_l);

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
	      int_fl HELPF_l);

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
	      int_fl HELPF_l);

void getopt_x(int_f *KBIN_p,
	      int_f *KOPT_p,
	      int_f *IOPT_p); /* out: option number selected */

void showfi_x(int_f *KBOUT_p,
	      char  *STRING_p,
	      int_fl STRING_l);

void showfu_x(int_f *KBOUT_p,
	      char  *STRING_p,
	      int_fl STRING_l);

void errom_x(int_f *KBOUT_p,
	     char  *STRING_p,
	     int_fl STRING_l);

void busy_x(int_f *KBOUT_p);

int gtstr(char *prompt, char *defval, char *out, size_t outlen);

int yesno(char *prompt);

int getint(int minval, int maxval, int defval, char *prompt, int *status);

float getfloat(float minval, float maxval, float defval, char *prompt,
	       int *status, int precision);

int checkn(int num, char **prompts, int **bools);

int radion(char *title, char **options, int numopts, int def);

int getcopt(int *status);

void showfu(char *fname);

void showfi(char *fname);

void errom(char *errmsg);

void busy();

void menu_x(int_f *OPT_p,
	    int_f *KOPT_p,
	    int_f *MOPT_p,
	    int_f *MAXOPT_p,
	    int_f *MINMEN_p,
	    int_f *KBIN_p,
	    int_f *KBOUT_p,
	    int_f *IHELPS_p,
	    int_f *IHELPE_p,
	    char  *HELPF_p,
	    int_f *IDEVH_p,
	    int_fl HELPF_l);

int bpause();

void showfunc();
