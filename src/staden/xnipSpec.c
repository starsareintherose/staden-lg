/*
    Title:       xnipSpec

    File: 	 xnipSpec.c
    Purpose:	 Definitions specific to `xnip'
    Last update: Tue May 19 1990
*/


/*
    This module contains the setup for the menus for the `xnip' program
    and also information needed for `xnip' help.
*/




/* ---- Includes ---- */

#include "progSpec.h"

#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/Xaw/Box.h>
#include <X11/Xaw/MenuButton.h>
#include <X11/Xaw/SimpleMenu.h>
#include <X11/Xaw/Sme.h>
#include <X11/Xaw/SmeBSB.h>




/* ---- Types ---- */


typedef struct
{   String	name;   /* IMPORT: Name of function */
    int 	number; /* IMPORT: Number of function */
} MenuData, *MenuDataList;




/* ---- Static variables ---- */

    
static MenuData general_menu_data[] =
{   {"Read new sequence",                   3},
    {"Redefine active region",              4},
    {"List the sequence",                   5},
    {"List a text file",                    6},
    {"Direct text output to disc",          7},
    {"Write active sequence to disc",       8},
    {"Edit the sequence",                   9},
    {"Restriction enzyme search",           17},
    {"Compare a sequence",                  18},
    {"Reverse and complement the sequence", 29},
    {"Interconvert t and u",                66},
};
  
  
static MenuData screen_menu_data[] =
{   {"Clear graphics",   10},
    {"Clear text",       11},
    {"Draw a ruler",     12},
    {"Use cross hair",   13},
    {"Reposition plots", 14},
    {"Label diagram",    15},
    {"Display a map",    16},
};


static  MenuData statistics_menu_data[] =
{   {"Count base composition",                             21},
    {"Count dinucleotide frequencies",                     22},
    {"Count codons and amino acids",                       23},
    {"Plot base composition",                              24},
    {"Plot local deviations in base composition",          25},
    {"Plot local deviations in dinucleotide composition",  26},
    {"Plot local deviations in trinucleotide composition", 27},
    {"Plot negentropy",                                    59},
    {"Plot observed-expected word frequencies",            64},
};


static  MenuData structure_menu_data[] =
{   {"Search for hairpin loops",                 30},
    {"Search for long range inverted repeats",   31},
    {"Search for repeats",                       32},
    {"Examine repeats",                          38},
    {"Search for z dna(total ry,yr)",            33},
    {"Search for z dna(runs of ry or yr",        34},
    {"Search for z dna(best phased value)",      35},
    {"Find local similarity or complementarity", 36},
};


static  MenuData translation_menu_data[] =
{   {"Set genetic code",                             37},
    {"Translate and list in up to six phases",       39},
    {"Translate and write protein sequence to disc", 40},
    {"Calculate codon constraint",                   28},
    {"Count codons and amino acids",                 23},
    {"Write codon table to disk",                    41},
    {"Search for open reading frames",               54},
};


static  MenuData gene_menu_data[] =
{   {"Codon usage",                                     42},
    {"Positional base preferences",                     43},
    {"Uneven positional base frequencies",              44},
    {"Codon improbability (on base composition)",       45},
    {"Codon improbability (on amino acid composition)", 46},
    {"Shepherd RNY preference",                         47},
    {"Fickett testcode",                                48},
    {"tRNA gene search",                                49},
};


static  MenuData specific_signals_menu_data[] =
{   {"Plot e. coli promters (general)",                 55},
    {"Plot e. coli promters (complementary)",           56},
    {"Plot e. coli promters (-35 and -10)",             57},
    {"Plot e. coli ribosome binding sites",             58},
    {"Plot eukaryotic ribosome binding sites",          61},
    {"Plot splice junctions",                           62},
    {"Plot polya sites",                                65},
};


static  MenuData general_signals_menu_data[] =
{   {"Compare a short sequence",                        18},
    {"Compare a sequence using a score matrix",         19},
      {"Search using a weight matrix",                  20},
      {"Search using a weight matrix (complementary)",  63},
      {"Search using a dinucleotide weight matrix",     60},
   {"Plot start codons",                               50},
    {"Plot stop codons",                                51},
    {"Plot complementary stop codons",                  52},
    {"Plot stop codons on both strands",                53},
    {"Search for patterns of motifs",                   67},
};




/* --- Callback functions ---- */

static XtCallbackProc externalCallbackProc;
static XtPointer      externalClient_data;

static void MenuSelectCallback(Widget w, XtPointer i, XtPointer junk)
/*
    Pass the menu item callback back to `externalCallbackProc'
    which the user supplied to `CreateProgMenus'.
*/
{   externalCallbackProc(w, externalClient_data, i);
}




/* ---- Private functions ---- */


static void CreateMenu(Widget parentWid, String menuButtonName,
		       MenuDataList md, Cardinal num_md)
{   Widget buttonWid, menuWid;
    int i;

    /*
        Create the (empty) menu button
    */
    buttonWid = XtCreateManagedWidget(menuButtonName, menuButtonWidgetClass,
				      parentWid, NULL, 0);
    menuWid = XtCreatePopupShell("menu", simpleMenuWidgetClass, buttonWid,
				 NULL, 0);

    /*
        Put the individual items in.
	When selected, each entry will generate a callback with
	its associated number.
    */
    for (i = 0; i < (int) num_md ; i++)
    {	Widget entryWid = XtCreateManagedWidget(md[i].name, smeBSBObjectClass,
						menuWid, NULL, 0);
	XtAddCallback(entryWid, XtNcallback, MenuSelectCallback,
		      (XtPointer) md[i].number);
    }

}




/* ---- Exported functions ---- */



void CreateProgMenus(Widget parentWid,
		     XtCallbackProc cbp, XtPointer client_data)
/*
    Install the menus for this program into `parentWid'.
    When pressed, each item (which is the name of a function) will
    call `cbp' providing `client_data' and the number of the function
    as `call_data'.
*/
{   externalCallbackProc = cbp;
    externalClient_data = client_data;

    CreateMenu(parentWid, "General",
	       general_menu_data, XtNumber(general_menu_data));
    CreateMenu(parentWid, "Screen",
	       screen_menu_data, XtNumber(screen_menu_data));
    CreateMenu(parentWid, "Statistics",
	       statistics_menu_data, XtNumber(statistics_menu_data));
    CreateMenu(parentWid, "Structure",
	       structure_menu_data, XtNumber(structure_menu_data));
    CreateMenu(parentWid, "Translation and codons",
	       translation_menu_data, XtNumber(translation_menu_data));
    CreateMenu(parentWid, "Gene search by content",
	       gene_menu_data, XtNumber(gene_menu_data));
    CreateMenu(parentWid, "General signals menu",
	       general_signals_menu_data, XtNumber(general_signals_menu_data));
    CreateMenu(parentWid, "Specific signals menu",
	       specific_signals_menu_data, XtNumber(specific_signals_menu_data));
}




const int botHelpOpt = -10;
const int topHelpOpt = 70;
/*
    The range of option numbers for the help system.
*/


const char helpTextFN[] = "NIPHELP";
const char helpPtrsFN[] = "NIPHPNT";
/*
    File names for the help text and pointer files.
*/


const char *helpTopics[] =
{   "NIP",
    "HELP",
    "Quit",
    "read a new sequence",
    "define active region",
    "list the sequence",
    "list a text file",
    "direct output to disk",
    "write active sequence to disk",
    "edit the sequence",
    "clear graphics screen",
    "clear text screen",
    "draw a ruler",
    "use cross hair",
    "reposition plots",
    "label diagram",
    "display a map",
    "restriction enzyme search",
    "compare a sequence",
    "compare a sequence using a score matrix",
    "search using a weight matrix",
    "count base frequencies",
    "count dinucleotide frequencies",
    "count codons and amino acids",
    "plot base composition",
    "plot deviations in base composition",
    "plot deviations in dinucleotide composition",
    "plot deviations in trinucleotide composition",
    "calculate codon constraint",
    "plot negentropy",
    "search for hairpin loops",
    "search for long range inverted repeats",
    "search for repeats",
    "search for Z DNA (total RY, YR)",
    "search for Z DNA (runs of RY or YR)",
    "search for Z DNA (best phased value)",
    "find local similarity or complementarity",
    "set genetic code",
    "examine repeats",
    "translate and list in 6 phases",
    "translate and write protein to disk",
    "calculate and write codon table to disk",
    "gene search using codon preference",
    "gene search using positional base preferences",
    "gene search using uneven positional base frequencies",
    "gene search using codon improbability (on base composition)",
    "gene search using codon improbability (on amino acid composition)",
    "gene search using RNY preference",
    "gene search using Ficketts method",
    "tRNA gene search",
    "plot start codons",
    "plot stop codons",
    "plot complementary stop codons",
    "plot stop codons on both strands",
    "find longest open reading frames",
    "plot E. coli promoters (general)",
    "plot E. coli promoters (complementary)",
    "plot E. coli promoters (-5 and -10 separate)",
    "plot E. coli ribosome binding sites",
    "reverse and complement the active sequence",
    "universal signal plot (on dinucleotides)",
    "plot eukaryotic ribosome binding sites",
    "plot splice junctions",
    "search the complementary strand using a weight matrix",
    "plot observed-expected word frequencies",
    "plot polyA sites",
    "interconvert T and U",
    "search for patterns of motifs",
    NULL,
};
/*
    Help topics, indexed in C between 0 and topHelpOpt-botHelpOpt
    but referring to topics botHelpOpt to topHelpOpt
*/
