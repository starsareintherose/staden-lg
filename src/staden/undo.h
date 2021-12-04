#ifndef _undo_h
#define _undo_h

#include "edUtils.h"

enum undoEvents {
	undoDelete=0,
	undoInsert,
	undoReplace,
	undoExtend,
	undoUnextend,
	undoNullCommand,
	undoMark
     };

typedef struct bnode *UndoBucketPtr;

typedef struct bnode{
        int event;
        int sequence;
        int position;
        int character;
        struct bnode *next;
        EdStructPtr xx;
	int time;
        } UndoBucket;

extern UndoBucketPtr bucketStack;

extern void cleanUpStack(UndoBucketPtr *stack);
extern void recordEdit (UndoBucketPtr *stack, EdStruct *xx, int undoEvent, int seq, int pos, char ch, int time);
extern void retrieveEdit (UndoBucketPtr *stack, EdStruct **xx, int *undoEvent, int *seq, int *pos, char *ch, int *time);
extern void cleanUpAllStacks();
extern int editsMade(EdStruct *xx);
extern int get_uniq_id();
#endif /* _undo_h */
