#include <stdio.h>
#include <stdlib.h>
#include "undo.h"
#include "edUtils.h"

UndoBucketPtr bucketStack;

/*
** undo handling code
*/

static UndoBucketPtr bucketFreeList = NULL;
static UndoBucketPtr newBucket()
/*
** Create a new undo bucket for the undo stack
*/
{
    UndoBucketPtr p;

    if (bucketFreeList == NULL) {
	p = (UndoBucketPtr) malloc (sizeof(UndoBucket));
    } else {
	p = bucketFreeList;
	bucketFreeList = bucketFreeList->next;
    }
    return p;
}

static void freeBucket(UndoBucketPtr p)
/*
** Free an undo bucket and store it in a free list
*/
{
    p->next = bucketFreeList;
    bucketFreeList = p;
}

static UndoBucketPtr popBucket(UndoBucketPtr *stack)
/*
** Get the undo bucket that's on top of and undo stack
*/
{
    UndoBucketPtr p;

    if (*stack == NULL) {
        p = NULL;
    } else {
        p = *stack;
        *stack = (*stack)->next;
    }
    return p;
}


static void pushBucket(UndoBucketPtr *stack,UndoBucketPtr p)
/*
** Push a bucket onto an undo stack
*/
{
    p->next = *stack;
    *stack = p;
}
 
void cleanUpStack(UndoBucketPtr *stack)
/*
** free all stack buckets
*/
{
    UndoBucketPtr p;
    while ( (p = popBucket(stack)) != NULL )
	freeBucket(p);
}

void recordEdit (UndoBucketPtr *stack, EdStruct *xx, int undoEvent, int seq, int pos, char ch, int time)
/*
** record edits in an undo bucket and stick it on a stack
*/
{
    UndoBucketPtr p;

#define record_multi_edits
#ifndef record_multi_edits
    static int state = 0;

    if (state) {
	if (undoEvent == undoMark) state = 0;
    } else {
	cleanUpStack(stack);
	if (undoEvent == undoMark) state = 1;
    }
#endif

    p = newBucket();
    if (p==NULL) {
        fprintf (stderr,"error: Undo stack is full... purging\n");
	cleanUpStack(stack);
        p = newBucket();
    }
    if (p!=NULL) {
        p->event     = undoEvent;
        p->sequence  = seq;
        p->position  = pos;
        p->character = ch;
	p->xx        = xx;
	p->time      = time;
        pushBucket (stack,p);
    }
}

void retrieveEdit (UndoBucketPtr *stack, EdStruct **xx, int *undoEvent, int *seq, int *pos, char *ch, int *time)
/*
** Get the last edit made from the undo stack and return its details
*/
{
    UndoBucketPtr p;
    p = popBucket(stack);
    if (p==NULL) {
	*undoEvent = undoNullCommand;
    } else  {
        *undoEvent = p->event;
        *seq       = p->sequence;
        *pos       = p->position;
        *ch        = p->character;
	*xx        = p->xx;
	*time      = p->time;
        freeBucket(p);
    }
}

void cleanUpAllStacks()
/*
** free all stacks
*/
{
    cleanUpStack(&bucketStack);
}

int editsMade(EdStruct *xx)
/*
**    0 - no edits made
**    1 - edits made
*/
{
    int i;
    for (i=1; i<=xx->DB_gelCount; i++)
	if ( DBgetFlags(xx,i)&(DB_FLAG_SEQ_MODIFIED|DB_FLAG_REL_MODIFIED|DB_FLAG_TAG_MODIFIED) )
	    return 1;
    return 0;
}

int get_uniq_id()
{
    static int count = 0;

    return ++count;

}
