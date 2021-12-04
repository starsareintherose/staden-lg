#include <stdio.h>

#include "subclone.h"

static CloneInfo NULLCloneInfo = {
    "",  /* mtd */
    "",  /* cosmid */
    1000, /* range_from */
    1000, /* range_to */
    "",  /* method */
    "",  /* vector */
};


/*
** Imports
*/
extern double atof(char *s);




static void get_entry(char *fn, char *mtd, char *entry)
{
    FILE *f;
    char in_line[256];

    entry[0]='\0';
    if ( (f = fopen(fn,"r")) == NULL) return;

    while (fgets(in_line,(int)sizeof(in_line),f) != NULL) {
	int l_mtd = strlen(mtd);
	if (strncmp(in_line,mtd,l_mtd) == 0 && isspace(in_line[l_mtd])) {
	    strcpy(entry,in_line);
	    fclose(f);
	    return;
	}
    }

    fclose(f);
}
	






#define MAX_CACHE_ENTRIES 5

static struct {
    char key[200];
    int time;
    CloneInfo info;
} cache[MAX_CACHE_ENTRIES];

static int cache_entries = 0;
static int cache_time = 0;




static int check_in_cache(char *fn, char *mtd, CloneInfo *info)
{
    int i;
    char key[200];

    if (! cache_entries) return 0;

    strcpy(key,mtd);
    strcat(key,fn);

    for (i=0; i < cache_entries; i++) {
	if (strcmp(key,cache[i].key)==0) {
	    cache[i].time = cache_time++;
	    *info = cache[i].info;
	    return 1;
	}
    }

    return 0;
}







static void store_in_cache(char *fn, char *mtd, CloneInfo *info)
{
    int slot;
    char key[200];

    strcpy(key,mtd);
    strcat(key,fn);

    if (cache_entries < MAX_CACHE_ENTRIES)
	slot = cache_entries++;
    else {
	int i;
	int slot_time = cache_time;

	for (i = 0; i < cache_entries; i++) {
	    if (slot_time > cache[i].time) {
		slot_time = cache[i].time;
		slot = i;
	    }
	}
    }

    strcpy(cache[slot].key, key);
    cache[slot].info = *info;
    cache[slot].time = cache_time++;
}








int read_subclone_info(char *fn, char *mtd, CloneInfo *info)
/*
** Use grep to get info from subclone file
*/
{
    char temp[100];
    char entry[256];
#ifdef OLD_CLUNKY_FORMAT
    float float_range_from, float_range_to;
#endif /*OLD_CLUNKY_FORMAT*/

    /*
    ** Initialise all fields
    */
    *info = NULLCloneInfo;

    /*
    ** Check arguments are sensible
    */
    if (!strlen(fn) || !strlen(mtd) || info==NULL)
	return 1;

    /*
    ** Check in cache
    */
    if ( check_in_cache(fn,mtd,info) ) return 0;

    /*
    ** Get subclone entry (as a string)
    */
    get_entry(fn,mtd,entry);

    /* check that a sensible result was reached */
    if (!entry[0])
	return 1;

    /*
    ** Parse line
    */
    sscanf(entry, "%s %s %s %s %s",
	   info->mtd,
	   info->cosmid,
	   temp,
	   info->method,
	   info->vector);

#ifdef OLD_CLUNKY_FORMAT
    /*
    ** Size information is (currently) expressed as a range of two floating
    ** point numbers representing size in kilobases.
    **
    ** Parse range information:
    **     <range> ::= <float> | <null-float> - <null-float>
    **     <null-float> ::= <float> | 
    */
    if (strchr(temp,'-') == NULL) {
	float_range_from = float_range_to = (float) atof(temp) ;
    } else {
	sscanf(temp, "%f-%f", &float_range_from, &float_range_to);
    }

    info->range_from = (int) (float_range_from * 1000.0);
    info->range_to = (int) (float_range_to * 1000.0);
    
#else /* OLD_CLUNKY_FORMAT */

    /*
    ** Size information is (currently) expressed as a range of two integer
    ** numbers representing size in bases.
    **
    ** Parse range information:
    **     <range> ::= <integer> | <null-integer> .. <null-integer>
    **     <null-integer> ::= <integer> | 
    */
    if (strchr(temp,'.') == NULL) {
	info->range_from = info->range_to = atoi(temp) ;
    } else {
	sscanf(temp, "%d..%d", &info->range_from, &info->range_to);
    }

#endif /* OLD_CLUNKY_FORMAT */

    store_in_cache(fn,mtd,info);

    return 0;
    	
}

