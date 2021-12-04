#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include "tagdb.h"
    
/* TOKENS */
#define NULLTOKEN 0
#define END 1
#define ID 2
#define SEPARATOR 3
#define NL 4
#define EQ 5
    
static char *infile; /* file being parsed */

static char word[128];
static int lineno;

/* ---------------- private routines ----------------- */
static void spring_clean_text (s,t)
     /* parse out normal escape characters bar hex and octal */
     char *s;
     char *t;
{
    while (*s != '\0') {
	if (*s == '\\') 
	    switch (*++s) {
	    case 'a':  *t++ = '\a'; s++; break;
	    case 'b':  *t++ = '\b'; s++; break;
	    case 'f':  *t++ = '\f'; s++; break;
	    case 'n':  *t++ = '\n'; s++; break;
	    case 'r':  *t++ = '\r'; s++; break;
	    case 't':  *t++ = '\t'; s++; break;
	    case 'v':  *t++ = '\v'; s++; break;
	    case '\n':
		while (isspace(*++s));
		break;
		/*
		  case 'x' : 
		  if (isxdigit(s[1]) && isxdigit(s[2])) {
		  #define ASCII_TO_HEX(A) (isdigit(A)?(A-'0'):(tolower(A)-'a'+10))
		  *t++ = ASCII_TO_HEX(s[1])*16 + ASCII_TO_HEX(s[2]);
		  s++;s++;s++;
		  }
		  break;
		  */
	    case '\\':
	    case '\?':
	    case '\'':
	    case '\"':
	    case '\0': break;
	    } else *t++ = *s++;
    }
    *t = '\0';
    
}


static int next_word(FILE *fp)
/*
 ** lexical analyser
 ** Get's the next word from the input stream
 */
{
    int a;
    int token;
    char *s;
    
    token = NULLTOKEN;
    s = word;
    
    while (token == NULLTOKEN) {
	switch (a = getc(fp)) {
	case EOF:
	    token = END;
	    break;
	case '\n':
	    lineno++;
	    token = NL;
	    break;
	case '=':
	    token = EQ;
	    break;
	case ':':
	    token = SEPARATOR;
	    break;
	case '#':
	    /* comment: skip to end of line */
	    for(a=getc(fp); a!=EOF && a!='\n'; a=getc(fp));
	    if (a=='\n') lineno++;
	    if (a==EOF) token = END;
	    break;
	case '\\':
	    /* back quoted newlines are skipped */
	    /* back quoted "anything else" is "anything else" */
	    a = getc(fp);
	    if (a != EOF && a != '\n') ungetc(a,fp);
	    break;
	case '"':
	    /* quoted string */
	    for(a=getc(fp);a!=EOF && a!='"';a=getc(fp)) {
		if (a=='\n') lineno++;
		*s++ = a;
	    }
	    token = ID;
	    break;
	default:
	    if (isalnum(a)) {
		*s++ = a;
		for(a=getc(fp);a!=EOF && isalnum(a);a=getc(fp)) *s++ = a;
		if (a!=EOF) ungetc(a,fp);
		token = ID;
	    }
	    break;
	}
    }
    
    *s = '\0';
    spring_clean_text(word,word);
    return token;
    
}


static void parse_error(char *s)
{
    fprintf( stderr, "%s on line %d of %s\n", s,lineno,infile);
}

static int snatch(char *s)
{
    if (strcmp(s,"id")==0) return TAG_SEARCH_ID;
    if (strcmp(s,"fg")==0) return TAG_FG_COLOUR;
    if (strcmp(s,"bg")==0) return TAG_BG_COLOUR;
    if (strcmp(s,"dt")==0) return TAG_DEFAULT_TEXT;
    return TAG_UNKNOWN;
}

static void initTagDBFields(char *type)
{
    tag_db[tag_db_count].type = (char *)malloc(strlen(type)+1);
    strcpy(tag_db[tag_db_count].type,type);
    tag_db[tag_db_count].search_id = NULL;
    tag_db[tag_db_count].fg_colour = NULL;
    tag_db[tag_db_count].bg_colour = NULL;
    tag_db[tag_db_count].default_text = NULL;
}

static void tidyUpTagDBFields()
{
    int len;
    
    if (tag_db[tag_db_count].search_id == NULL) {
	tag_db[tag_db_count].search_id = tag_db[tag_db_count].type;
    }
    len =  strlen(tag_db[tag_db_count].search_id);
    if (len < 4)
	strncpy(tag_db[tag_db_count].id,"    ",4);
    else
	len = 4;
    strncpy(tag_db[tag_db_count].id,tag_db[tag_db_count].search_id,len);
    
    if (tag_db_count < (MAX_TAG_TYPES-1))
	tag_db_count++;
}





static int parse_file(FILE *fp)
     /*
      ** Parse file and load info into tag_db[]
      */
{
    int at_end_of_file;
    int at_end_of_entry;

    /* for safe keeping */
    int field;
    char *WORD;
    
    at_end_of_file = 0;
    
    while (!at_end_of_file) {
	switch (next_word(fp)) {
	case END:
	    at_end_of_file = 1;
	case NL:
	    break;
	case ID: 
	    /* the big time */
	    initTagDBFields(word);
	    switch(next_word(fp)) {
	    case END:
		at_end_of_file = 1;
	    case NL:
		break;
	    case SEPARATOR:
		/* arglist */
		at_end_of_entry = 0;
		while (!at_end_of_entry) {
		    switch(next_word(fp)) {
		    case SEPARATOR:
			break;
		    case ID:
			field = snatch(word);
			switch(next_word(fp)) {
			case EQ:
			    switch(next_word(fp)) {
			    case ID:
				WORD = (char *)malloc(strlen(word)+1);
				strcpy(WORD,word);
				switch (field) {
				case TAG_SEARCH_ID :
				    tag_db[tag_db_count].search_id = WORD;
				    break;
				case TAG_FG_COLOUR :
				    tag_db[tag_db_count].fg_colour = WORD;
				    break;
				case TAG_BG_COLOUR :
				    tag_db[tag_db_count].bg_colour = WORD;
				    break;
				case TAG_DEFAULT_TEXT :
				    tag_db[tag_db_count].default_text = WORD;
				    break;
				default:
				    break;
				}
				switch(next_word(fp)) {
				case END:
				    at_end_of_file = 1;
				case NL:
				    at_end_of_entry = 1;
				case SEPARATOR:
				    break;
				default:
				    parse_error("Syntax error");
				    return ( 1 );
				}
				break;
			    default:
				parse_error("Syntax error");
				return ( 1 );
			    }
			    break;
			case END:
			    at_end_of_file = 1;
			case NL:
			    at_end_of_entry = 1;
			case SEPARATOR:
			    break;
			default:
			    parse_error("Syntax error");
			    return ( 1 );
			}
			break;
		    case END:
			at_end_of_file = 1;
		    case NL:
			at_end_of_entry = 1;
			break;
		    default:
			parse_error("Syntax error");
			return ( 1 );
		    }
		}
		break;
	    default:
		parse_error("Syntax error");
		return 1;
	    }
	    tidyUpTagDBFields();
	    break;
	default:
	    parse_error("Syntax error");
	    return 1;
	}
    }
    
    return 0;
}





/* ------------------- exported routines ------------------ */

tag_db_struct tag_db[MAX_TAG_TYPES];
int tag_db_count;

void parse(char *file)
{
    int parse_failed;
    FILE *fp;
    
    char *default_type = "Comment";
    char *default_search_id = "DEFC";
    char *default_fg_colour = NULL;
    char *default_bg_colour = "yellow";
    char *default_default_text = "Enter your comment here";
    
    infile = file;
    tag_db_count = 0;
    
    if(fp = fopen(file,"r")) {
	lineno = 1;
	parse_failed = parse_file(fp);
	if (parse_failed) {
	    fprintf (stderr,"Error encountered while parsing tag database\nGiving up\n");
	    tag_db_count = 0;
	}
	fclose(fp);
    } else
	fprintf (stderr,"Error parsing tag database\nFile `%s' does not exist\nGiving up\n",infile);
    
    if (! tag_db_count) {
	tag_db[tag_db_count].type = default_type;
	tag_db[tag_db_count].search_id = default_search_id;
	tag_db[tag_db_count].fg_colour = default_fg_colour;
	tag_db[tag_db_count].bg_colour = default_bg_colour;
	tag_db[tag_db_count].default_text = default_default_text;
	tag_db_count++;
    }
    
}
