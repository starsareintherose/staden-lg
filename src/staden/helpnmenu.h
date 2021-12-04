#define help_x		help_
#define help2_x		help2_
#define sethlp_x	sethlp_
#define inthlp_x	inthlp_

#define MAXOPTS 	100 /* Should be big enough for a fair while */
#define MAXMENUS 	10  /* Leaves MAXOPTS-MAXMENUS for actual options */
#define MAXDUP		3   /* maximum number of menus for a single option */

/* structures and types */

typedef struct menuarr {
    char *name;
    char optname[4];
    int number;
} menuarr;

typedef struct helpindex_t {
    int menus;		/* how many menus it appears in */
    int menunum[MAXDUP];/* which menus it appears in */
    int offset;		/* byte offset into help file */
    int lines;		/* number of lines of help for this option */
    char *name;		/* Name of option */
} helpindex_t;

extern helpindex_t *helpindex;
extern int optTransTab[];
extern char *helptopics[];
extern int maxopts;

/* function prototypes */
char *gethelp();
char *gethelp2();
int inithelp(char *progname, int numopts);
int create_menu(int menunum, menuarr *menu, int menusize);
int query_opt();
void set_opt(int op);
