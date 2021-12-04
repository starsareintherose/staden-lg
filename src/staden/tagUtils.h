#ifndef _tagUtils_h
#define _tagUtils_h

#include "fortran.h"
#include "fort.h"
/* #include "Sheet.h" */
/*
** The following describe two database files:
**     The tag list file
**     The comment list file
**
** the tag list file consists of at least IDBSIZ records.
**
**     1       : header tag for sequence 1
**     2       : header tag for sequence 2
**     ...
**     IDBSIZ-1: header tag for sequence IDBSIZ-1:
**     IDBSIZ  : descriptor record defining MAX_TAG
**     IDBSIZ+1: supplimentary tags
**     ...
**     MAX_TAG : supplimentary tags
**
** the comment list file consists of at least 1 record.
**
**     1       : descriptor record defining MAX_COM
**     2       : supplimentary comment
**     ...
**     MAX_COM : supplimentary comment
**     
*/

#define COMMENT_LENGTH 40
/* COMMENT_LENGTH is hard coded in dbsysnew.f */
/* grep for COMMENT_LENGTH                    */
#define FILE_NAME_LENGTH 18
/* FILE_NAME_LENGTH is hard coded in dbsysnew.f */
/* grep for FILE_NAME_LENGTH                    */

typedef int_f tag_id;
typedef int_f comment_id;

/*
** element in tag list
*/
typedef union {
    char c[4];
    int_f i;
    } tag_types;

typedef struct _tagRecord{
    int_f position;        /* position in sequence */
    int_f length;          /* length of tag */
    tag_types type;
    comment_id comment;  /* index to comment */
    tag_id next;         /* link to next in structure */
} tagRecord;

/*
** comment chain
*/
typedef struct _commentStruct{
    char comment[COMMENT_LENGTH];
    comment_id next;
} commentStruct;

typedef struct _tagstruct{
    /*
    ** Data from the original file
    */
    tagRecord tagrec;
    /*
    ** Data for database management
    */
    tag_id original_tag_id;
    char *newcomment;
    int newcommentlen;
    long flags;
    int time;
    struct _tagstruct *next;
} tagStruct,*tagptr;

#define TAG_UNCHANGED         (0)
#define TAG_POSITION_CHANGED  (1L<<1)
#define TAG_LENGTH_CHANGED    (1L<<2)
#define TAG_TYPE_CHANGED      (1L<<3)
#define TAG_COMMENT_CHANGED   (1L<<4)
#define TAG_INSERTED          (1L<<5)
#define TAG_DELETED           (1L<<6)
#define TAG_COMMENT_IN_MEMORY (1L<<7)

#define TAG_MALLOC(s) (char *)malloc(s)
#define TAG_FREE(c)   free(c)

#define TAG_TYPE_INSERT "*INS"
#define TAG_TYPE_DELETE "*DEL"
#define TAG_TYPE_LCUT   "*LC*"
#define TAG_TYPE_RCUT   "*RC*"
#define TAG_TYPE_RAWDATA "*RAW"

#include "edUtils.h"

/* define external routines */
extern void force_comment(tagStruct *t);

/* comment interface */
extern tagStruct *readTagList(int seq);
extern void writeTagList(EdStruct *xx, int seq);
extern void destroyTagList(tagStruct *s);

extern void createTag(EdStruct *xx);
extern void editTag(EdStruct *xx);
extern void deleteTag(EdStruct *xx);
/* extern void getTagSplodge(EdStruct *xx, int seq, int pos, int width, XawSheetInk *ink); */

extern void insertTag(EdStruct *xx, int seq, tagStruct *t);
extern tagStruct *newTag();
/* this gives me lots of problems...why?
extern char normaliseBase(EdStruct *xx,int seq,char deletedBase);
*/

extern void tagInsertBase(EdStruct *xx,int seq,int pos,char *base);
extern void tagDeleteBase(EdStruct *xx,int seq,int pos,char *base);
extern void tagReplaceBase(EdStruct *xx,int seq,int pos,char *dbse,char *ibse);
extern tagStruct *findTag(EdStruct *xx,int seq,int pos);
extern int origpos(EdStruct *xx, int seq, int pos);
extern void getLeftCutOff(EdStruct *xx,int seq, int width, char *str);
extern void getRightCutOff(EdStruct *xx,int seq, int width, char *str);
extern char *get_comment(comment_id cp);
extern int lenLCut(EdStruct *xx, int seq);
extern int lenRCut(EdStruct *xx, int seq);
extern void getLCut(EdStruct *xx,int seq, int pos, int width, char *str);
extern void getRCut(EdStruct *xx,int seq, int pos, int width, char *str);
extern tagStruct *findTagByType(EdStruct *xx, int seq, char *type);
extern char normaliseBase(EdStruct *xx,int seq,char deletedBase);
extern void dump_tags(EdStruct *xx, int seq);
extern void setButtonName(Widget w, char *c);
extern void createTagTypeMenu(Widget parent, void (*call_back)() );
extern void delete_comment (comment_id cp);
extern comment_id put_comment(char *c);
extern int read_tag(tag_id n, tagRecord *t);
extern int invokeTagEditor(char *type_id, char *newType, char *tagComment, char **newComment);
extern int write_tag(tag_id n, tagRecord t);
extern tag_id get_free_tag();
extern void delete_tag_rec(tag_id t);

#endif /* _tagUtils_h */
