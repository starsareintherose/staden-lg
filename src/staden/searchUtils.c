/*
** Search functions for the contig editor
**
** This file is split into two parts, probably implying that it should be
** divided into two files. The first half consists of all the search
** engines, the second half the user interface code.
**
** Observations:
**   Only one search window can be up at one time.
**
** Changes:
**
**  7-Nov-1991 SD Select the tag when searching on tag.
** 20-Feb-1992 SD Renamed "OK" button to "Quit"
** 29-Apr-1992 SD Changes relevant to general speedup in edUtils.c
** 11-May-1992 SD Search by name now dependant on cursor position
** 15-May-1992 SD now NOSTRSTR and NOSTRDUP
**
*/


#define REDISPLAY(X) setDisplayPos((X),positionInContig((X),(X)->cursorSeq,(X)->cursorPos))

/*
** The first half
*/
#include <stdio.h>
#include <stdlib.h>
#include "edUtils.h"
#include "tagUtils.h"
#include "searchUtils.h"
#include "select.h"
#include "string.h"
#include "ctype.h"
#include "misc.h"

/*
** Prototypes
*/
extern char *re_comp(char *s);
extern int re_exec(char *s);

#define in_interval(A,B,C) ( ((A)<(B))?((A)<=(C) && (C)<=(B)):((B)<=(C) && (C)<=(A)) )

/*
** We don't want to have all the textwidget translations
** Here are the list of the ones we want.
** This is taken from Xaw/TextTr.c source
*/
static char *defaultTranslations = "\
<Key>Right:	forward-character() \n\
<Key>Left:	backward-character() \n\
<Key>Delete:	delete-previous-character() \n\
<Key>BackSpace:	delete-previous-character() \n\
<Key>:		insert-char() \n\
<FocusIn>:	focus-in() \n\
<FocusOut>:	focus-out() \n\
<Btn1Down>:	select-start() \n\
<Btn1Motion>:	extend-adjust() \n\
<Btn1Up>:	extend-end(PRIMARY, CUT_BUFFER0) \n\
<Btn2Down>:	insert-selection(PRIMARY, CUT_BUFFER0) \n\
<Btn3Down>:	extend-start() \n\
<Btn3Motion>:	extend-adjust() \n\
<Btn3Up>:	extend-end(PRIMARY, CUT_BUFFER0) \
";

static void my_translations(Widget w)
/*
** Use my default translations
*/
{
    XtTranslations parsedTranslations;

    parsedTranslations = XtParseTranslationTable(defaultTranslations);

    XtUninstallTranslations(w);
    XtAugmentTranslations(w,parsedTranslations);
}

static int findGelByNumber (EdStruct *xx, char *s)
/*
** Position cursor on left end of gel sequence matched by string s.
** If s starts with a slash '/' assume a gel name is specified.
** Otherwise assume a gel number is specified.
*/
{
    int i;

    int gel;
    gel = atoi(s);
    for (i=1; i <= xx->DB_gelCount; i++) {
	if (DBgetNumber(xx,i) == gel) {
	    xx->cursorSeq = i;
	    xx->cursorPos = 1;
	    REDISPLAY(xx);
	    return 1;
	}
    }

    return 0;
}

static int findNextByName(EdStruct *xx, char *s)
/*
** Search forwards from the cursor position until the sequence specified
** is found. The cursor is positioned at the left end of the sequence,
** if found.
**
*/
{
    int spos = positionInContig(xx,xx->cursorSeq,xx->cursorPos)+1;
    int i;
    int n;

    if (!*s) return 0;
    n = strlen(s);

    if ((i = posToIndex(xx,spos))==0) return 0;

    for (; i <= xx->DB_gelCount; i++) {
	/* search through tag list for sequence */

	if (strncmp(DBgetGelName(xx,xx->DBorder[i]),s,n)==0) {
	    xx->cursorSeq = xx->DBorder[i];
	    xx->cursorPos = 1;
	    REDISPLAY(xx);
	    return 1;
	}
    }


    return 0;
}


static int findPrevByName(EdStruct *xx, char *s)
/*
** Search forwards from the cursor position until the sequence specified
** is found. The cursor is positioned at the left end of the sequence,
** if found.
**
*/
{
    int spos = positionInContig(xx,xx->cursorSeq,xx->cursorPos)-1;
    int i;
    int n;

    if (!*s) return 0;
    n = strlen(s);

    if ((i = posToIndex(xx,spos))==0) return 0;

    for (i--; i >= 0 ; i--) {
	/* search through tag list for sequence */

	if (strncmp(DBgetGelName(xx,xx->DBorder[i]),s,n)==0) {
	    xx->cursorSeq = xx->DBorder[i];
	    xx->cursorPos = 1;
	    REDISPLAY(xx);
	    return 1;
	}
    }


    return 0;
}

static int findNextGelByName (EdStruct *xx, char *s)
/*
** Position cursor on left end of gel sequence matched by string s.
** If s starts with a slash '/' assume a gel name is specified.
** Otherwise assume a gel number is specified.
*/
{
    if (*s) {
	if (*s == '/') {
	    s++;
	    return findNextByName(xx,s);
	} else
	    return findGelByNumber(xx,s);
    }
    return 0;
}


static int findPrevGelByName (EdStruct *xx, char *s)
/*
** Position cursor on left end of gel sequence matched by string s.
** If s starts with a slash '/' assume a gel name is specified.
** Otherwise assume a gel number is specified.
*/
{
    if (*s) {
	if (*s == '/') {
	    s++;
	    return findPrevByName(xx,s);
	} else
	    return findGelByNumber(xx,s);
    }
    return 0;
}





static int findNextTagByType (EdStruct *xx, char *type)
/*
** Search forwards from the cursor position until a tag of a specified
** type is encountered. The cursor is positioned at the left end of the
** tag, if found.
*/
{
    int spos = positionInContig(xx,xx->cursorSeq,xx->cursorPos)+1;
    int epos = DBgetLength(xx,0);
    int fpos,fseq,i;
    int fseqpos;
    int *seqList;
    tagStruct *found_tag;

    seqList = sequencesInRegion(xx,spos, epos);
    fseq = 0;
    fseqpos = 0;
    fpos = epos + 1;

    for (i=0; seqList[i] && DBgetRelPos(xx,seqList[i]) < fpos ; i++) {
	/* search through tag list for sequence */
	int seq = seqList[i];
	tagStruct *t;

	t = (tagStruct *) DBgetTags(xx,seq);
	while (t != NULL) {
	    int normpos;
	    int tagpos;
	    normpos = normalisePos(xx,seq,t->tagrec.position,t->tagrec.length);
	    tagpos=positionInContig(xx,seq,normpos);
	    if (!(t->flags & TAG_DELETED) &&
		in_interval(spos,fpos,tagpos) &&
		strncmp(t->tagrec.type.c,type,4)==0) {
		fseq = seq;
		fseqpos = normpos;
		fpos = tagpos;
		found_tag = t;
		if (DBgetComp(xx,seq) == COMPLEMENTED) 
		    /* Keep looking */
		    t = t->next;
		else
		    /* Stop now */
		    t = NULL;
	    } else
		t = t->next;
	}
  
    }


    if (fseq) {
	xx->cursorSeq = fseq;
	xx->cursorPos = fseqpos;
	_select_tag(xx,fseq,found_tag);
        REDISPLAY(xx);
    }

    return fseq;
}




static int findPrevTagByType (EdStruct *xx, char *type)
/*
** Search backwards from the current cursor position until a tag of the
** specified type is encountered. The cursor is positioned at the left
** end of the tag, if found.
*/
{
    int spos = positionInContig(xx,xx->cursorSeq,xx->cursorPos)-1;
    int epos = 1;
    int fpos,fseq,i;
    int fseqpos;
    int *seqList;
    tagStruct *found_tag;

    seqList = sequencesInRegion(xx,epos, spos);
    fseq = 0;
    fseqpos = 0;
    fpos = epos - 1;

    for (i=0; seqList[i]; i++) ;
    for (i--; i>=0 && DBgetRelPos(xx,seqList[i])+DBgetLength(xx,seqList[i]) > fpos ; i--) {
	/* search through tag list for sequence */
	int seq = seqList[i];
	tagStruct *t;

	t = (tagStruct *) DBgetTags(xx,seq);
	while (t != NULL) {
	    int normpos;
	    int tagpos;
	    normpos = normalisePos(xx,seq,t->tagrec.position,t->tagrec.length);
	    tagpos = positionInContig(xx,seq,normpos);
	    if (!(t->flags & TAG_DELETED) &&
		in_interval(spos,fpos,tagpos) &&
		strncmp(t->tagrec.type.c,type,4)==0) {
		fseq = seq;
		fseqpos = normpos;
		fpos = tagpos;
		found_tag = t;
		if (DBgetComp(xx,seq) != COMPLEMENTED) 
		    /* Keep looking */
		    t = t->next;
		else
		    /* Stop now */
		    t = NULL;
	    } else
		t = t->next;
	}
  
    }


    if (fseq) {
	xx->cursorSeq = fseq;
	xx->cursorPos = fseqpos;
	_select_tag(xx,fseq,found_tag);
        REDISPLAY(xx);
    }

    return fseq;
}





static int findNextSequence(EdStruct *xx, char *s)
/*
** Search forwards from the cursor position until the sequence specified
** is found. The cursor is positioned at the left end of the sequence,
** if found.
**
** Observations:
**   The search is done on a reading by reading basis, rather than from
**   the consensus. 
**   The search is not case sensitive.
*/
{
    int spos = positionInContig(xx,xx->cursorSeq,xx->cursorPos)+1;
    int epos = DBgetLength(xx,0);
    int fpos,fseq,i;
    int fseqpos;
    int *seqList;
    char *reading;
    int maxlen;

    seqList = sequencesInRegion(xx,spos, epos);
    fseq = 0;
    fseqpos = 0;
    fpos = epos + 1;


    for (maxlen = 0, i=0; seqList[i] ; i++)
	maxlen = max(maxlen, DBgetLength(xx,seqList[i]));

    reading = malloc(maxlen+1);

    for (i=0; seqList[i] && DBgetRelPos(xx,seqList[i]) < fpos ; i++) {
	/* search through tag list for sequence */
	int seq = seqList[i];
	char *str;
	char *ind;

	str = DBgetSeq(xx,seq);
	strncpy(reading,str,DBgetLength(xx,seq));
	reading[DBgetLength(xx,seq)] = '\0';

	if (in_interval(1,DBgetLength(xx,seq),spos - DBgetRelPos(xx,seq)+1)) {
	    int offset = spos - DBgetRelPos(xx,seq);
	    ind = strstr(reading + offset,s);
	} else
	    ind = strstr(reading,s);

	if (ind != NULL) {
	    int pos;
	    pos = positionInContig(xx,seq,(int) (ind - reading) + 1);
	    if (in_interval(spos,fpos,pos)) {
		fseqpos = (int) (ind - reading) + 1;
		fpos = pos;
		fseq = seq;
	    }
	   
	}

    }

    if (fseq) {
	xx->cursorSeq = fseq;
	xx->cursorPos = fseqpos;
        REDISPLAY(xx);
    }

    free(reading);
    return fseq;
}




static void reverse_str(char *s, int len)
/*
** Verse the character sequence of a string
*/
{
    int i;
    char temp;

    for (i=0; i < len/2; i++) {
	temp = s[i];
	s[i] = s[len-i-1];
	s[len-i-1] = temp;
    }

}




static int findPrevSequence(EdStruct *xx, char *inseq)
/*
** Search backwards from the cursor position until the sequence specified
** is found. The cursor is positioned at the left end of the sequence,
** if found.
**
** Observations:
**   The search is done on a reading by reading basis, rather than from
**   the consensus.
**   The search is not case sensitive.
*/
{
    int spos = positionInContig(xx,xx->cursorSeq,xx->cursorPos)+strlen(inseq)-2;
    int epos = 1;
    int fpos,fseq,i;
    int fseqpos;
    int *seqList;
    char *reading;
    char *s;
    int maxlen;

    s = strdup(inseq);
    reverse_str(s,strlen(s));

    seqList = sequencesInRegion(xx,epos, spos);
    fseq = 0;
    fseqpos = 0;
    fpos = epos - 1;


    for (maxlen = 0, i=0; seqList[i] ; i++)
	maxlen = max(maxlen, DBgetLength(xx,seqList[i]));

    reading = malloc(maxlen+1);

    for (i=0; seqList[i]; i++) ;
    for (i--; i>=0 && DBgetRelPos(xx,seqList[i])+DBgetLength(xx,seqList[i]) > fpos ; i--) {
	/* search through tag list for sequence */
	int seq = seqList[i];
	char *str;
	char *ind;

	str = DBgetSeq(xx,seq);
	strncpy(reading,str,DBgetLength(xx,seq));
	reading[DBgetLength(xx,seq)] = '\0';
	reverse_str(reading,DBgetLength(xx,seq));

	if (in_interval(1,DBgetLength(xx,seq),spos - DBgetRelPos(xx,seq)+1)) {
	    int offset = DBgetLength(xx,seq) - (spos - DBgetRelPos(xx,seq)+1);
	    ind = strstr(reading + offset,s);
	} else
	    ind = strstr(reading,s);

	if (ind != NULL) {
	    int pos;
	    pos = positionInContig(xx,
				   seq,
				   DBgetLength(xx,seq) - (int)(ind - reading) - strlen(s) + 1);
	    if (in_interval(spos,fpos,pos)) {
		fseqpos = pos - DBgetRelPos(xx,seq) + 1;
		fpos = pos;
		fseq = seq;
	    }
	   
	}

    }

    if (fseq) {
	xx->cursorSeq = fseq;
	xx->cursorPos = fseqpos;
        REDISPLAY(xx);
    }

    free(reading);
    return fseq;
}





static int findNextAnno(EdStruct *xx, char *anno)
/*
** Search forwards from the cursor position until a tag containing the
** specified annotation is found. The cursor is positioned at the left
** end of the tag, if found.
**
** Observations:
**   A regular expression search is found, giving unpredictable results
**   to people unfamiliar with such searches
*/
{
    int spos = positionInContig(xx,xx->cursorSeq,xx->cursorPos)+1;
    int epos = DBgetLength(xx,0);
    int fpos,fseq,i;
    int fseqpos;
    int *seqList;
    char *error;
    char *find_all = "$";
    tagStruct *found_tag;

    if (! *anno) anno = find_all;

    error = re_comp(anno);

    if (error != NULL) {
	fprintf(stderr,"findNextAnno: %s\n",error);
	return 0;
    }


    seqList = sequencesInRegion(xx,spos, epos);
    fseq = 0;
    fseqpos = 0;
    fpos = epos + 1;

    for (i=0; seqList[i] && DBgetRelPos(xx,seqList[i]) < fpos ; i++) {
	/* search through tag list for sequence */
	int seq = seqList[i];
	tagStruct *t;

	t = (tagStruct *) DBgetTags(xx,seq);
	while (t != NULL) {
	    int normpos;
	    int tagpos;
	    int match;
	    /* Code isn't very efficient here! */
	    normpos = normalisePos(xx,seq,t->tagrec.position,t->tagrec.length);
	    tagpos=positionInContig(xx,seq,normpos);
	    /*
	    ** Get annotation
	    */
	    force_comment(t);
	    match = ( re_exec(t->newcomment) == 1);
	    if (!(t->flags & TAG_DELETED) &&
		in_interval(spos,fpos,tagpos) &&
		t->tagrec.type.c[0] != '*' &&  /* avoid special tags */
		t->tagrec.position != 0 &&
		match) {
		fseq = seq;
		fseqpos = normpos;
		fpos = tagpos;
		found_tag = t;
		if (DBgetComp(xx,seq) == COMPLEMENTED) 
		    /* Keep looking */
		    t = t->next;
		else
		    /* Stop now */
		    t = NULL;
	    } else
		t = t->next;
	}
  
    }


    if (fseq) {
	xx->cursorSeq = fseq;
	xx->cursorPos = fseqpos;
	_select_tag(xx,fseq,found_tag);
        REDISPLAY(xx);
    }

    return fseq;
}

static int findPrevAnno(EdStruct *xx, char *anno)
/*
** Search backwards from the cursor position until a tag containing the
** specified annotation is found. The cursor is positioned at the left
** end of the tag, if found.
**
** Observations:
**   A regular expression search is found, giving unpredictable results
**   to people unfamiliar with such searches
*/
{
    int spos = positionInContig(xx,xx->cursorSeq,xx->cursorPos)-1;
    int epos = 1;
    int fpos,fseq,i;
    int fseqpos;
    int *seqList;
    char *error;
    char *find_all = "$";
    tagStruct *found_tag;

    if (! *anno) anno = find_all;

    error = re_comp(anno);

    if (error != NULL) {
	fprintf(stderr,"findPrevAnno: %s\n",error);
	return 0;
    }


    seqList = sequencesInRegion(xx,epos, spos);
    fseq = 0;
    fseqpos = 0;
    fpos = epos - 1;

    for (i=0; seqList[i]; i++) ;
    for (i--; i>=0 && DBgetRelPos(xx,seqList[i])+DBgetLength(xx,seqList[i]) > fpos ; i--) {
	/* search through tag list for sequence */
	int seq = seqList[i];
	tagStruct *t;

	t = (tagStruct *) DBgetTags(xx,seq);
	while (t != NULL) {
	    int normpos;
	    int tagpos;
	    int match;
	    /* Code isn't very efficient here! */
	    normpos = normalisePos(xx,seq,t->tagrec.position,t->tagrec.length);
	    tagpos=positionInContig(xx,seq,normpos);
	    /*
	    ** Get annotation
	    */
	    force_comment(t);
	    match = ( re_exec(t->newcomment) == 1);
	    if (!(t->flags & TAG_DELETED) &&
		in_interval(spos,fpos,tagpos) &&
		t->tagrec.type.c[0] != '*' &&  /* avoid special tags */
		t->tagrec.position != 0 &&
		match) {
		fseq = seq;
		fseqpos = normpos;
		fpos = tagpos;
		found_tag = t;
		if (DBgetComp(xx,seq) != COMPLEMENTED) 
		    /* Keep looking */
		    t = t->next;
		else
		    /* Stop now */
		    t = NULL;
	    } else
		t = t->next;
	}
  
    }


    if (fseq) {
	xx->cursorSeq = fseq;
	xx->cursorPos = fseqpos;
	_select_tag(xx,fseq,found_tag);
        REDISPLAY(xx);
    }

    return fseq;
}



static int findPosition(EdStruct *xx, char *text_pos)
/*
** Position the cursor at the position specified.
** There are three modes:
**   1. By position in contig.       eg 30717
**   2. By position in a gel.        eg @100
**   3. By a relative offset.        eg +1000 eg -1000
**
** Observations:
**   The cursor is positioned in the same gel if possible.
**   If it possible to specify negative or large numbers when 
**   specifying position in gel.
*/
{

    int pos;
    int cseq = xx->cursorSeq;
    int cpos = xx->cursorPos;

    for(; *text_pos && isspace(*text_pos) ; text_pos++) ;

    switch (*text_pos) {
    case '\0':
	return 0;
    case '+':
    case '-':
	pos = positionInContig(xx,cseq,cpos) + atoi(text_pos);
	break;
    case '@':
	pos = DBgetRelPos(xx,cseq) + atoi(++text_pos) - 1;
	break;
    default:
	pos = atoi(text_pos);
	break;
    }

    if (pos > 0 && pos <= DBgetLength(xx,0)) {
	if ( in_interval(DBgetRelPos(xx,cseq),DBgetRelPos(xx,cseq)+DBgetLength(xx,cseq)-1,pos ) ) {
	    xx->cursorPos = pos - DBgetRelPos(xx,cseq) + 1;
	} else {
	    xx->cursorSeq = 0;
	    xx->cursorPos = pos;
	}
        REDISPLAY(xx);
	return 1;
    }
    return 0;
    
}



int findNextProblem (EdStruct *xx)
/*
** Search forward from the cursor position until a the consensus is not
** A, C, G or T. The cursor is positioned on the problem base, if found.
*/
{
#define SEARCH_CHUNKS MAX_DISPLAY_WIDTH
    int spos = positionInContig(xx,xx->cursorSeq,xx->cursorPos)+1;
    int epos = DBgetLength(xx,0);
    char buffer[SEARCH_CHUNKS+1];
    int i,width;

    for (i=spos; i<= epos; i+=SEARCH_CHUNKS) {
	width = min(epos-i+1,SEARCH_CHUNKS);
	DBcalcConsensus (xx,i,width, buffer,BOTH_STRANDS);
	{
	    int check = strcspn(buffer,"-*");
	    if (check != width) { /* we have problem! */
		xx->cursorSeq = 0;
		xx->cursorPos = i+check;
		REDISPLAY(xx);
		return 1;
	    }
	}
    }
    return 0;
}



static int findPrevProblem (EdStruct *xx)
/*
** Search forward from the cursor position until a the consensus is not
** A, C, G or T. The cursor is positioned on the problem base, if found.
*/
{
#define SEARCH_CHUNKS MAX_DISPLAY_WIDTH
    int spos = positionInContig(xx,xx->cursorSeq,xx->cursorPos)-1;
    int epos = 1;
    char buffer[SEARCH_CHUNKS+1];
    int i,width;
    int j;

    for (i=spos; i>= epos; i-=SEARCH_CHUNKS) {
	width = min(i-epos+1,SEARCH_CHUNKS);
	DBcalcConsensus (xx,i-width+1,width, buffer, BOTH_STRANDS);
	for (j=width-1;j>=0;j--) {
	    char *check = strchr("-*",buffer[j]);
	    if (check != NULL) { /* we have problem! */
		xx->cursorSeq = 0;
		xx->cursorPos = i-width+1+j;
		REDISPLAY(xx);
		return 1;
	    }
	}
    }
    return 0;
}


static int qual_prob(char a, char b)
/*
** Returns true if character 'a' is out of sorts with character 'b'
*/
{
    switch (a) {
    case ' ':
	return 0;
    case '*':
    case '-':
	return 1;
    default:
	switch (b) {
	case ' ':
	    return 0;
	case '*':
	case '-':
	    return 1;
	default:
	    return (a != b);
	}
    }
}




static int findNextQualProb (EdStruct *xx)
/*
** Search forwards from the cursor position until a problem relating to
** quality is found. The cursor is positioned on the problematic base, if found.
**
** Observations:
**   Large stretches of sequence on one strand only could cause frustration
*/
{
#define SEARCH_CHUNKS MAX_DISPLAY_WIDTH
    int spos = positionInContig(xx,xx->cursorSeq,xx->cursorPos)+1;
    int epos = DBgetLength(xx,0);
    char buffer1[SEARCH_CHUNKS+1];
    char buffer2[SEARCH_CHUNKS+1];
    int i,width;
    int j;

    for (i=spos; i<= epos; i+=SEARCH_CHUNKS) {
	width = min(epos-i+1,SEARCH_CHUNKS);
	DBcalcConsensus (xx,i,width,buffer1,COMPLEMENTED);
	DBcalcConsensus (xx,i,width,buffer2,UNCOMPLEMENTED);
	for (j=0;j<width;j++)
	{
	    if (qual_prob(buffer1[j],buffer2[j])) { /* we have problem! */
		xx->cursorSeq = 0;
		xx->cursorPos = i+j;
		REDISPLAY(xx);
		return 1;
	    }
	}
    }
    return 0;
}



static int findPrevQualProb (EdStruct *xx)
/*
** Search backwards from the cursor position until a problem relating to
** quality is found. The cursor is positioned on the problematic base, if found.
**
** Observations:
**   Large stretches of sequence on one strand only could cause frustration
*/
{
#define SEARCH_CHUNKS MAX_DISPLAY_WIDTH
    int spos = positionInContig(xx,xx->cursorSeq,xx->cursorPos)-1;
    int epos = 1;
    char buffer1[SEARCH_CHUNKS+1];
    char buffer2[SEARCH_CHUNKS+1];
    int i,width;
    int j;

    for (i=spos; i>= epos; i-=SEARCH_CHUNKS) {
	width = min(i-epos+1,SEARCH_CHUNKS);
	DBcalcConsensus (xx,i-width+1,width,buffer1,COMPLEMENTED);
	DBcalcConsensus (xx,i-width+1,width,buffer2,UNCOMPLEMENTED);
	for (j=width-1;j>=0;j--) {
	    if (qual_prob(buffer1[j],buffer2[j])) { /* we have problem! */
		xx->cursorSeq = 0;
		xx->cursorPos = i-width+1+j;
		REDISPLAY(xx);
		return 1;
	    }
	}
    }
    return 0;
}





/*
** The second half
*/

/*
** Widgets and things
*/
#include <X11/StringDefs.h>
#include <X11/Intrinsic.h>
#include <X11/Shell.h>
#include <X11/Xaw/Form.h>
#include <X11/Xaw/Label.h>
#include <X11/Xaw/Box.h>
#include <X11/Xaw/Toggle.h>
#include <X11/Xaw/Text.h>
#include <X11/Xaw/AsciiText.h>
#include <X11/Xaw/MenuButton.h>
#include <X11/Xaw/Command.h>
#include <ctype.h>

#include "edMenu.h"
#include "main.h"

#define FORWARDS 1
#define BACKWARDS 2

static EdStruct *XX;
static char tag_type[4];
static int up = 0;
static Widget oldFogieWid;

static Widget searchWid = NULL;
static Widget form;
static Widget label;
static Widget dir_box,    backwards, forwards;
static Widget opt_box,    by_pos, by_name, by_tag_type, by_tag_val, by_seq, by_prob, by_qual;
static Widget val_box,    text, type;
static Widget act_box,    search,quit;




static void searchCallback(Widget w, XtPointer client_data, XtPointer call_data)
/*
** Interrogate buttons to determine search direct and mode, and call
** the appropriate routine.
*/
{
    char *text_str;
    Arg args[10];
    Cardinal nargs;
    int found;

    int option, isforwards;

    nargs = 0;
    XtSetArg(args[nargs], XtNstring, &text_str); nargs++;
    XtGetValues(text,args,nargs);

    option = (int) XawToggleGetCurrent(by_pos);
    isforwards = ( FORWARDS == (int) XawToggleGetCurrent(forwards) );

    if (isforwards) {

	switch(option){
	case OptByGelName:
	    found = findNextGelByName(XX,text_str);
	    break;
	case OptByAnnotation:
	    found = findNextAnno(XX,text_str);
	    break;
	case OptBySequence:
	    found = findNextSequence(XX,text_str);
	    break;
	case OptByTagType:
	    found = findNextTagByType (XX, tag_type);
	    break;
	case OptByPosition:
	    found = findPosition(XX,text_str);
	    break;
	case OptByProblem:
	    found = findNextProblem(XX);
	    break;
	case OptByQuality:
	    found = findNextQualProb(XX);
	    break;
	}

    } else {

	switch(option){
	case OptByGelName:
	    found = findPrevGelByName(XX,text_str);
	    break;
	case OptByAnnotation:
	    found = findPrevAnno(XX,text_str);
	    break;
	case OptBySequence:
	    found = findPrevSequence(XX,text_str);
	    break;
	case OptByTagType:
	    found = findPrevTagByType (XX, tag_type);
	    break;
	case OptByPosition:
	    found = findPosition(XX,text_str);
	    break;
	case OptByProblem:
	    found = findPrevProblem(XX);
	    break;
	case OptByQuality:
	    found = findPrevQualProb(XX);
	    break;
	}

    }

    if (!found) XBell(GetDisplay(),100);

}




static void quitCallback(Widget w, XtPointer client_data, XtPointer call_data)
/*
** Leave the search window gracefully
*/
{
    up = 0;
    XtPopdown(searchWid);
}


#include "tagdb.h"
static void tagMenuCallback(Widget w, XtPointer client_data, XtPointer call_data)
/*
** The changing of the tag type - don't miss it!
*/
{
    tag_db_struct *t = (tag_db_struct *) client_data;
    setButtonName(type,t->type);
    strncpy(tag_type,t->id,4);

}




static void create_search_wid(Widget parentWid)
/*
** Create the widgets for the search window.
*/
{
    /*
    **      Label
    **      Search: [Backwards] [Forwards]
    **      Operation:
    **                 [Position] [Gel Name]
    **                 [Annotation Type] [Annotation Content]
    **                 [Sequence] [Problems]
    **	    Value: [         ] [Type]
    **	    [Search] [Quit]
    */



    Cardinal nargs;
    Arg args[10];
    Position	x, y; 	 /* top-left hand corner of new widget */
    Dimension	height;  /* height of parent widget */

    /*
    ** Determine the position on the screen for this widget
    */
#define fromVertWid (XX->edWid)
    nargs = 0;
    XtSetArg(args[nargs], XtNheight, &height); nargs++;
    XtGetValues(fromVertWid, args, nargs);
    XtTranslateCoords(fromVertWid, (Position) 0, (Position) height, &x, &y);

    /*
    ** Create popup shell
    */
    nargs = 0;
    XtSetArg(args[nargs], XtNx, x); nargs++;
    XtSetArg(args[nargs], XtNy, y); nargs++;
    searchWid = XtCreatePopupShell("search", transientShellWidgetClass, parentWid, args, nargs);

    /*
    ** Create main form
    */
    nargs = 0;
    form = XtCreateManagedWidget("form", formWidgetClass, searchWid, args, nargs);

    /*
    ** Create title for form
    */
    nargs = 0;
    XtSetArg(args[nargs], XtNborderWidth, 0); nargs++;
    XtSetArg(args[nargs], XtNlabel, "Search Window"); nargs++;
    label = XtCreateManagedWidget("label", labelWidgetClass, form, args, nargs);

    /*
    ** Create buttons for search direction
    */
    nargs = 0;
    XtSetArg(args[nargs], XtNfromVert, label); nargs++;
    XtSetArg(args[nargs], XtNborderWidth, 0); nargs++;
#define XtOrientHorizontal "horizontal"
    XtSetArg(args[nargs], XtNorientation, XtOrientHorizontal); nargs++;
    dir_box = XtCreateManagedWidget("dir_box", boxWidgetClass, form, args, nargs);
    nargs = 0;
    XtSetArg(args[nargs], XtNborderWidth, 0); nargs++;
    XtSetArg(args[nargs], XtNlabel, "Direction:"); nargs++;
    (void) XtCreateManagedWidget("label", labelWidgetClass, dir_box, args, nargs);
    nargs = 0;
    XtSetArg(args[nargs],XtNradioData,BACKWARDS); nargs++;
    backwards = XtCreateManagedWidget("backwards", toggleWidgetClass, dir_box, args, nargs);
    nargs = 0;
    XtSetArg(args[nargs],XtNradioGroup, backwards); nargs++;
    XtSetArg(args[nargs],XtNstate, True); nargs++;
    XtSetArg(args[nargs],XtNradioData,FORWARDS); nargs++;
    forwards = XtCreateManagedWidget("forwards", toggleWidgetClass, dir_box, args, nargs);

    /*
    ** Create buttons for operation selection
    */
    nargs = 0;
    XtSetArg(args[nargs], XtNfromVert, dir_box); nargs++;
    XtSetArg(args[nargs], XtNborderWidth, 0); nargs++;
#define XtOrientHorizontal "horizontal"
    XtSetArg(args[nargs], XtNorientation, XtOrientHorizontal); nargs++;
    opt_box = XtCreateManagedWidget("opt_box", boxWidgetClass, form, args, nargs);
    nargs = 0;
    XtSetArg(args[nargs], XtNborderWidth, 0); nargs++;
    XtSetArg(args[nargs], XtNlabel, "Search by:"); nargs++;
    (void) XtCreateManagedWidget("label", labelWidgetClass, opt_box, args, nargs);
    nargs = 0;
    XtSetArg(args[nargs],XtNstate, True); nargs++;
    XtSetArg(args[nargs],XtNlabel,"Position"); nargs++;
    XtSetArg(args[nargs],XtNradioData,OptByPosition); nargs++;
    by_pos = XtCreateManagedWidget("by_pos", toggleWidgetClass, opt_box, args, nargs);
    nargs = 0;
    XtSetArg(args[nargs],XtNradioGroup, by_pos); nargs++;
    XtSetArg(args[nargs],XtNlabel,"Reading Name"); nargs++;
    XtSetArg(args[nargs],XtNradioData,OptByGelName); nargs++;
    by_name = XtCreateManagedWidget("by_name", toggleWidgetClass, opt_box, args, nargs);
    nargs = 0;
    XtSetArg(args[nargs],XtNradioGroup, by_pos); nargs++;
    XtSetArg(args[nargs],XtNlabel,"Tag Type"); nargs++;
    XtSetArg(args[nargs],XtNradioData,OptByTagType); nargs++;
    by_tag_type = XtCreateManagedWidget("by_tag_type", toggleWidgetClass, opt_box, args, nargs);
    nargs = 0;
    XtSetArg(args[nargs],XtNradioGroup, by_pos); nargs++;
    XtSetArg(args[nargs],XtNlabel,"Annotation"); nargs++;
    XtSetArg(args[nargs],XtNradioData,OptByAnnotation); nargs++;
    by_tag_val = XtCreateManagedWidget("by_tag_val", toggleWidgetClass, opt_box, args, nargs);
    nargs = 0;
    XtSetArg(args[nargs],XtNradioGroup, by_pos); nargs++;
    XtSetArg(args[nargs],XtNlabel,"Sequence"); nargs++;
    XtSetArg(args[nargs],XtNradioData,OptBySequence); nargs++;
    by_seq = XtCreateManagedWidget("by_seq", toggleWidgetClass, opt_box, args, nargs);
    nargs = 0;
    XtSetArg(args[nargs],XtNradioGroup, by_pos); nargs++;
    XtSetArg(args[nargs],XtNlabel,"Problem"); nargs++;
    XtSetArg(args[nargs],XtNradioData,OptByProblem); nargs++;
    by_prob = XtCreateManagedWidget("by_prob", toggleWidgetClass, opt_box, args, nargs);
    nargs = 0;
    XtSetArg(args[nargs],XtNradioGroup, by_pos); nargs++;
    XtSetArg(args[nargs],XtNlabel,"Quality"); nargs++;
    XtSetArg(args[nargs],XtNradioData,OptByQuality); nargs++;
    by_qual = XtCreateManagedWidget("by_qual", toggleWidgetClass, opt_box, args, nargs);
    

    /*
    ** Create selection criteria box
    */
    nargs = 0;
    XtSetArg(args[nargs], XtNfromVert, opt_box); nargs++;
    XtSetArg(args[nargs], XtNborderWidth, 0); nargs++;
    XtSetArg(args[nargs], XtNorientation, XtOrientHorizontal); nargs++;
    val_box = XtCreateManagedWidget("val_box", boxWidgetClass, form, args, nargs);
    nargs = 0;
    XtSetArg(args[nargs], XtNborderWidth, 0); nargs++;
    XtSetArg(args[nargs], XtNlabel, "    Value:"); nargs++;
    (void) XtCreateManagedWidget("label", labelWidgetClass, val_box, args, nargs);
    nargs = 0;
    XtSetArg(args[nargs], XtNresizable, True);              nargs++;
    XtSetArg(args[nargs], XtNresize, XawtextResizeBoth);    nargs++;
    XtSetArg(args[nargs], XtNeditType, XawtextEdit);        nargs++;
    text = XtCreateManagedWidget("text",asciiTextWidgetClass, val_box, args, nargs);
    my_translations(text);
    nargs = 0;
    XtSetArg(args[nargs], XtNmenuName, "tagMenu"); nargs++;
    type = XtCreateManagedWidget("type",menuButtonWidgetClass, val_box, args, nargs);
    createTagTypeMenu(type,tagMenuCallback);
    /* Set default type */
    setButtonName(type, tag_db[0].type);
    strncpy(tag_type,tag_db[0].id,4);

    /*
    ** Create Exit Action Buttons
    */
    nargs = 0;
    XtSetArg(args[nargs], XtNfromVert, val_box); nargs++;
    XtSetArg(args[nargs], XtNborderWidth, 0); nargs++;
    XtSetArg(args[nargs], XtNorientation, XtOrientHorizontal); nargs++;
    act_box = XtCreateManagedWidget("act_box", boxWidgetClass, form, args, nargs);
    nargs = 0;
    XtSetArg(args[nargs], XtNlabel, "Search"); nargs++;
    search = XtCreateManagedWidget("search", commandWidgetClass, act_box, args, nargs);
    XtAddCallback(search, XtNcallback, searchCallback, (XtPointer) NULL);
    nargs = 0;
    XtSetArg(args[nargs], XtNlabel, "Quit"); nargs++;
    quit = XtCreateManagedWidget("quit", commandWidgetClass, act_box, args, nargs);
    XtAddCallback(quit, XtNcallback, quitCallback, (XtPointer) NULL);

}



int invokeSearchGeneric(EdStruct *xx)
/*
** Pop up the search window, if it isn't already up
*/
{
    if (up) return 0;
    XX = xx;

    if (searchWid == NULL) {
	create_search_wid(oldFogieWid);
    }

    XtPopup(searchWid,   XtGrabNone);

    up = 1;

    return 0;
}



int destroySearchWindow()
/*
** Shut this baby down
** Called by the routine that takes the contig editor down.
*/
{
    if (up)
	XtCallCallbacks(quit, XtNcallback, (XtPointer) NULL);

    return 0;
}

void createSearchWidget(Widget parentWid)
/*
** Prtend to create it now
*/
{
    oldFogieWid = parentWid;
}
