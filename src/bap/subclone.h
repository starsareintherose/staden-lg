#ifndef _subclone_h
#define _subclone_h

typedef char Mtd[10];
typedef char Cosmid[10];
typedef char FragMethod[10];
typedef char Vector[10];

typedef struct _clone_info {
    Mtd mtd;
    Cosmid cosmid;
    int range_from; /* in bases */
    int range_to;   /* in bases */
    FragMethod method;
    Vector vector;
} CloneInfo;



extern int read_subclone_info(char *fn, char *mtd, CloneInfo *info);

#endif /* _subclone_h */
