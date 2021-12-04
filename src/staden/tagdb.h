#ifndef _tagdb_h
#define _tagdb_h

#include <X11/Intrinsic.h>

#define MAX_TAG_TYPES 256

#define TAG_UNKNOWN      0
#define TAG_SEARCH_ID    1
#define TAG_FG_COLOUR    2
#define TAG_BG_COLOUR    3
#define TAG_DEFAULT_TEXT 4

typedef struct {
    /*
    ** values taken from TAGDB file
    */
    char *type;
    char *search_id;
    char *fg_colour;
    char *bg_colour;
    char *default_text;
    /*
    ** values derived from above
    */
    Pixel fg_pixel;
    Pixel bg_pixel;
    char id[4];
    } tag_db_struct;

extern tag_db_struct tag_db[MAX_TAG_TYPES];
extern int tag_db_count;
extern void parse(char *file);

#endif /* _tagdb_h */
