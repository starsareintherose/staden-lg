/*
** Change log:
**
**   1/10/91 SD  Added calculateConsensusLength to extend, unextend, undo_unextend
**   29/4/92 SD  Changes related to general speed up in edUtils.c
**   18/5/92 SD  Construct (*nc=*++nc) not liked by dec
*/

#include "edUtils.h"
#include "fortran.h"
#include "tagUtils.h"
#include "contigEditor.h"
#include "undo.h"
#include "select.h"
#include <stdio.h>
#include <stdlib.h>

#define LEFT 1
#define RIGHT 2


tagStruct *force_get_tag(EdStruct *xx, int seq, char *type)
/*
** find a control tag of a particular type
** Create one if one doesn't exist
*/
{
    tagStruct *t;

    t = (tagStruct *)findTagByType(xx,seq,type);

    if (t == NULL) {
	/*
	** create one
	*/
	t = newTag();
	strncpy(t->tagrec.type.c,type,4);
	t->flags = TAG_INSERTED;
	insertTag(xx,seq,t);
    }

    /*
    ** Force comment to be in memory
    */
    force_comment(t);

    return t;

}



int extend(EdStruct *xx, int seq, int dir, int time)
{
    int c = (DBgetComp(xx,seq)==COMPLEMENTED);
    char t_type[4];
    tagStruct *t;
    int use_tag;
    char extend_char;
    int l;

    if (DBgetLength(xx,seq) >= (int)*saveState.maxgel) return 1;

    use_tag = (c && dir==LEFT || !c && dir==RIGHT )?RIGHT: LEFT;
    strncpy(t_type,
	(use_tag==RIGHT )?TAG_TYPE_RCUT:TAG_TYPE_LCUT,4);

    /*
    ** Find appropriate tag,
    ** create one if one doesn't exist
    */
    t = force_get_tag(xx,seq,t_type);

    /*
    ** No cutoff - ignore request
    */
    l = t->newcommentlen;
    if (! l)
	return 1;

    /*
    ** Determine character to extend
    */
    if (use_tag==LEFT) {
	/* Get character off end of cutoff */
	extend_char = t->newcomment[l-1];
	t->newcomment[l-1]='\0';
	t->newcommentlen--;
    } else {
	/* Get character off start of cutoff */
	int i;
	extend_char = t->newcomment[0];
	for (i=0;t->newcomment[i];i++)
	    t->newcomment[i] = t->newcomment[i+1];
	t->newcommentlen--;
    }
    t->flags |= TAG_COMMENT_CHANGED;

    /*
    ** complement if necessary
    */
    if (use_tag != dir) {
	int_f i=1;
	sqcom_(&extend_char,&i,(int_f)1);
    }


    if (dir==LEFT){
	int i,j;	
	char *s;

	/*
	** insert base at start of sequence
	*/
	s=DBgetSeq(xx,seq);
	for (j = DBgetLength(xx,seq)+1; j > 1; j--)
            s[j-1] = s[j-2];
	s[0] = extend_char;
	DBsetLength(xx,seq,DBgetLength(xx,seq)+1);
	DBsetFlags(xx,seq,DBgetFlags(xx,seq)|DB_FLAG_SEQ_MODIFIED|DB_FLAG_REL_MODIFIED);
	selectInsertBase(xx,seq,1);

	/*
	** shift sequence left
	*/
        if (DBgetRelPos(xx,seq)==1) {
	    for (i=1; i <= xx->DB_gelCount; i++) {
		if (seq == xx->DBorder[i]) {
		    for (j=i;j>1;j--)
			xx->DBorder[j] = xx->DBorder[j-1];
		    xx->DBorder[1] = seq;
		} else
		    DBsetRelPos(xx,xx->DBorder[i],DBgetRelPos(xx,xx->DBorder[i])+1);
	    }
	    DBsetFlags(xx,seq,DBgetFlags(xx,seq)|DB_FLAG_REL_MODIFIED);

	} else {
	    DBsetRelPos(xx,seq,DBgetRelPos(xx,seq)-1);
	    DBsetFlags(xx,seq,DBgetFlags(xx,seq)|DB_FLAG_REL_MODIFIED);

	    for (i=1; seq != xx->DBorder[i] && i <= xx->DB_gelCount; i++);
	    for (j=i; DBgetRelPos(xx,xx->DBorder[j-1]) > DBgetRelPos(xx,seq) ;j--)
		xx->DBorder[j] = xx->DBorder[j-1];
	    xx->DBorder[j] = seq;
	}

    } else {
	/*
	** insert base at end of sequence
	*/
	char *s;
	int l=DBgetLength(xx,seq);
	s=DBgetSeq(xx,seq);
	s[l] = extend_char;
	DBsetLength(xx,seq,l+1);
	DBsetFlags(xx,seq,DBgetFlags(xx,seq)|DB_FLAG_SEQ_MODIFIED|DB_FLAG_REL_MODIFIED);
	selectInsertBase(xx,seq,l+1);
    }

    if (use_tag == LEFT) {
	/*
	** shift position of all tags right 1
	*/
	tagStruct *t;
	t = (tagStruct *) DBgetTags(xx,seq);
	while (t != NULL) {
	    if (!(t->flags & TAG_DELETED) &&
	       t->tagrec.position) {
		t->tagrec.position++;
		t->flags |= TAG_POSITION_CHANGED;
		DBsetFlags(xx,seq,DBgetFlags(xx,seq)|DB_FLAG_TAG_MODIFIED);
	    }
	    t = t->next;
	}
    }

    /*
    ** Modify raw data stuff
    */
    {
	tagStruct *t;
	char type[5];
	char name[FILE_NAME_LENGTH+1];
	int llen,lcut,wlen;
	t = (tagStruct *) DBgetTags(xx,seq);
	force_comment(t);
        sscanf(t->newcomment,"%6d%6d%6d%*s",&llen,&lcut,&wlen);
	strncpy(type,&t->newcomment[18],4);
	strncpy(name,&t->newcomment[22],FILE_NAME_LENGTH);
	wlen++;
	if (use_tag==LEFT) lcut--;
        sprintf(t->newcomment,"%6d%6d%6d",llen,lcut,wlen);
	strncat(t->newcomment,type,4);
	strncat(t->newcomment,name,FILE_NAME_LENGTH);
	t->newcommentlen = (int)strlen(t->newcomment);
	t->flags |= TAG_COMMENT_CHANGED;
	DBsetFlags(xx,seq,DBgetFlags(xx,seq)|DB_FLAG_TAG_MODIFIED);

    }

    if (dir == RIGHT) xx->cursorPos++;

    /*
    ** Lengths are a changing
    */
    calculateConsensusLength(xx);

    return 0;

}





int unextend(EdStruct *xx, int seq, int dir, int time)
{

    int c = (DBgetComp(xx,seq)==COMPLEMENTED);
    char t_type[4];
    tagStruct *t;
    int use_tag;
    char extend_char[2];
    char external_buff[100];
    char internal_buff[100];
    char *ec = extend_char;
    char *eb = external_buff;
    char *ib = internal_buff;
    int l;
    int end_char_inserted = 0;

    use_tag = (c && dir==LEFT || !c && dir==RIGHT )?RIGHT: LEFT;
    strncpy(t_type,
	(use_tag==RIGHT )?TAG_TYPE_RCUT:TAG_TYPE_LCUT,4);

    /*
    ** Find appropriate tag,
    ** create one if one doesn't exist
    */
    t = force_get_tag(xx,seq,t_type);

    /*
    ** Process tag information
    ** collating *DE? flags and *INS flags
    ** and adjusting other flags accordingly
    */
    {
	tagStruct *t;
	int ipos;

	t = (tagStruct *) DBgetTags(xx,seq);
	ipos = normalisePos(xx,seq,(dir==LEFT)?1:DBgetLength(xx,seq),1);

	while (t != NULL) {
	    if (!(t->flags & TAG_DELETED) &&
	       t->tagrec.position == ipos &&
	       strncmp(t->tagrec.type.c,TAG_TYPE_INSERT,3)==0) {
		/*
		** Handle unextending past inserts
		*/
		end_char_inserted++;
		t->flags |= TAG_DELETED;
		t->time = time;
		DBsetFlags(xx,seq,DBgetFlags(xx,seq)|DB_FLAG_TAG_MODIFIED);
	    }else if (!(t->flags & TAG_DELETED) &&
	       strncmp(t->tagrec.type.c, TAG_TYPE_DELETE,3)==0) {
		/*
		** Handle unextending past deletes
		*/
		if (use_tag==LEFT && t->tagrec.position == ipos ||
		   use_tag==RIGHT && t->tagrec.position == (ipos+1)) {
		    *eb++ = t->tagrec.type.c[3];
		    t->time = time;
		    t->flags |= TAG_DELETED;
		    DBsetFlags(xx,seq,DBgetFlags(xx,seq)|DB_FLAG_TAG_MODIFIED);
		} else if (use_tag==RIGHT && t->tagrec.position == ipos) {
		    *ib++ = t->tagrec.type.c[3];
		    t->time = time;
		    t->flags |= TAG_DELETED;
		    DBsetFlags(xx,seq,DBgetFlags(xx,seq)|DB_FLAG_TAG_MODIFIED);
		} else if (use_tag == LEFT) {
		    t->tagrec.position--;
		    t->flags |= TAG_POSITION_CHANGED;
		    DBsetFlags(xx,seq,DBgetFlags(xx,seq)|DB_FLAG_TAG_MODIFIED);
		}
	    } else if (!(t->flags & TAG_DELETED) &&
	       t->tagrec.position <= ipos &&
	       t->tagrec.position + t->tagrec.length > ipos){
		/*
		** Handle unextending past other tags
		*/
		t->tagrec.length--;
		t->flags |= TAG_LENGTH_CHANGED;
		DBsetFlags(xx,seq,DBgetFlags(xx,seq)|DB_FLAG_TAG_MODIFIED);
	    } else if (!(t->flags & TAG_DELETED) &&
	        use_tag==LEFT && t->tagrec.position) {
		/*
		** Handle all other positional adjustments
		*/
		t->tagrec.position--;
		t->flags |= TAG_POSITION_CHANGED;
		DBsetFlags(xx,seq,DBgetFlags(xx,seq)|DB_FLAG_TAG_MODIFIED);
	    }
	    t = t->next;
	}
    }

    if (dir==LEFT){
	int i,j;	
	char *s;
	int l=DBgetLength(xx,seq);

	/*
	** get base at start of sequence
	*/
	s=DBgetSeq(xx,seq);
	*ec++ = s[0];
	for (j = 2; j <= l; j++)
            s[j-2] = s[j-1];
	DBsetLength(xx,seq,l-1);
	DBsetFlags(xx,seq,DBgetFlags(xx,seq)|DB_FLAG_SEQ_MODIFIED|DB_FLAG_REL_MODIFIED);
	selectDeleteBase(xx,seq,1);

	/*
	** shift sequence right
	*/
	if (DBgetRelPos(xx,seq)==1)
	    for (i=1; i<xx->DB_gelCount && DBgetRelPos(xx,xx->DBorder[i+1])==1;i++);
	else
	    i=0;
        if (i==1) {
	    for (i=2; i <= xx->DB_gelCount; i++) {
		DBsetRelPos(xx,xx->DBorder[i],DBgetRelPos(xx,xx->DBorder[i])-1);
	    }
	    DBsetFlags(xx,seq,DBgetFlags(xx,seq)|DB_FLAG_REL_MODIFIED);

	} else {
	    DBsetRelPos(xx,seq,DBgetRelPos(xx,seq)+1);
	    DBsetFlags(xx,seq,DBgetFlags(xx,seq)|DB_FLAG_REL_MODIFIED);

	    for (i=1; seq != xx->DBorder[i] && i <= xx->DB_gelCount; i++);
	    for (j=i; j < xx->DB_gelCount &&
               DBgetRelPos(xx,xx->DBorder[j+1]) < DBgetRelPos(xx,seq) ; j++)
		xx->DBorder[j] = xx->DBorder[j+1];
	    xx->DBorder[j] = seq;
	}


    } else {
	/*
	** get base at end of sequence
	*/
	char *s;
	int l=DBgetLength(xx,seq);
	s=DBgetSeq(xx,seq);
	*ec++ = s[l-1];
	DBsetLength(xx,seq,l-1);
	DBsetFlags(xx,seq,DBgetFlags(xx,seq)|DB_FLAG_SEQ_MODIFIED|DB_FLAG_REL_MODIFIED);
	selectDeleteBase(xx,seq,l);
    }

    *eb='\0';
    *ib='\0';
    *ec='\0';
    
    if (end_char_inserted) {
	/*
	** Throw it away
	*/
	*extend_char = '\0';
    } else {
	/*
	** complement if necessary
	*/
	if (use_tag != dir) {
	    int_f i=1;
	    sqcom_(extend_char,&i,(int_fl)1);
	}
    }

    /*
    ** Insert unextend character on end of appropriate tag
    */
    l = (int)(strlen(extend_char) + strlen(external_buff) +
	      strlen(internal_buff));
    if (l) {
	char *newercomment;
	newercomment = (char *) TAG_MALLOC( t->newcommentlen + l + 1);
	if (use_tag==LEFT) {
	    /* add buffers to end of cutoff */
	    strcpy(newercomment,t->newcomment);
	    strcat(newercomment,external_buff);
	    strcat(newercomment,extend_char);
	    strcat(newercomment,internal_buff);
	} else {
	    /* add buffer to start of cutoff */
	    strcpy(newercomment,internal_buff);
	    strcat(newercomment,extend_char);
	    strcat(newercomment,external_buff);
	    strcat(newercomment,t->newcomment);
	}
	TAG_FREE(t->newcomment);
	t->newcomment = newercomment;
	t->newcommentlen = (int)strlen(t->newcomment);
	t->flags |= TAG_COMMENT_CHANGED;
    }

    /*
    ** Modify raw data stuff
    */
    if (l) {
	tagStruct *t;
	char type[5];
	char name[FILE_NAME_LENGTH];
	int llen,lcut,wlen;
	t = (tagStruct *) DBgetTags(xx,seq);
	force_comment(t);
        sscanf(t->newcomment,"%6d%6d%6d%*s",&llen,&lcut,&wlen);
	strncpy(type,&t->newcomment[18],4);
	strncpy(name,&t->newcomment[22],FILE_NAME_LENGTH);
	wlen -= l;
	if (use_tag==LEFT) lcut+=l;
        sprintf(t->newcomment,"%6d%6d%6d",llen,lcut,wlen);
	strncat(t->newcomment,type,4);
	strncat(t->newcomment,name,FILE_NAME_LENGTH);
	t->newcommentlen = (int)strlen(t->newcomment);
	t->flags |= TAG_COMMENT_CHANGED;
	DBsetFlags(xx,seq,DBgetFlags(xx,seq)|DB_FLAG_TAG_MODIFIED);
    }

    if (dir == RIGHT) xx->cursorPos--;

    /*
    ** Lengths are a changing
    */
    calculateConsensusLength(xx);

    return 0;

}

int undo_unextend(EdStruct *xx, int seq, int dir, int time)
{

    int c = (DBgetComp(xx,seq)==COMPLEMENTED);
    char t_type[4];
    tagStruct *t;
    int use_tag;
    char extend_char;
    int l;
    int lextend;
    int end_char_inserted = 0;
    int added_internal = 0;
    int added_external = 0;

    use_tag = (c && dir==LEFT || !c && dir==RIGHT )?RIGHT: LEFT;
    strncpy(t_type,
	(use_tag==RIGHT )?TAG_TYPE_RCUT:TAG_TYPE_LCUT,4);

    /*
    ** Process tag information
    ** collating *DE? flags and *INS flags
    ** and adjusting other flags accordingly
    */
    {
	tagStruct *t;
	int ipos;

	t = (tagStruct *) DBgetTags(xx,seq);
	ipos = normalisePos(xx,seq,(dir==LEFT)?1:DBgetLength(xx,seq),1);
	if (use_tag==RIGHT) ipos++;

	while (t != NULL) {
	    if ((t->time == time) &&
	       strncmp(t->tagrec.type.c,TAG_TYPE_INSERT,3)==0) {
		/*
		** Handle unextending past inserts
		*/
		end_char_inserted++;
		extend_char = normaliseBase(xx,seq,t->tagrec.type.c[3]);
		/*
		extend_char = t->tagrec.type.c[3];
		*/
		t->flags &= ~TAG_DELETED;
		t->time = 0;
	    }else if ((t->time == time) &&
	       strncmp(t->tagrec.type.c, TAG_TYPE_DELETE,3)==0) {
		/*
		** Handle unextending past deletes
		*/
		if (use_tag==LEFT && t->tagrec.position == ipos ||
		   use_tag==RIGHT && t->tagrec.position == (ipos+1)) {
		    added_external++;
		} else {
		    added_internal++;
		}
		t->flags &= ~TAG_DELETED;
		t->time = 0;
	    } else if (!(t->flags & TAG_DELETED) &&
	       use_tag==LEFT && t->tagrec.position) {
		/*
		** Handle all other positional adjustments
		*/
		t->tagrec.position++;
	    }
	    t = t->next;
	}
    }

    /*
    ** Find appropriate tag,
    ** create one if one doesn't exist
    */
    t = force_get_tag(xx,seq,t_type);

    /*
    ** No cutoff - ignore request
    */
    l = t->newcommentlen;
    if (! l)
	return 1;

    /*
    ** Determine character to extend
    */
    lextend = added_internal+added_external+1-end_char_inserted;
    if (use_tag==LEFT) {
	/* Get character off end of cutoff */
        if (!end_char_inserted) {
	    extend_char = t->newcomment[l-1-added_internal];
	    if (use_tag != dir) {
		int_f i=1;
		sqcom_(&extend_char,&i,(int_fl)1);
	    }
	}
	t->newcomment[l-lextend]='\0';
	t->newcommentlen = l-lextend;
    } else {
	/* Get character off start of cutoff */
	char *nc;
        if (!end_char_inserted) {
	    extend_char = t->newcomment[added_internal];
	    if (use_tag != dir) {
		int_f i=1;
		sqcom_(&extend_char,&i,(int_fl)1);
	    }
	}
	for (nc=t->newcomment;*nc=nc[lextend];nc++);
	t->newcommentlen -= lextend;
    }



    if (dir==LEFT){
	int i,j;	
	char *s;

	/*
	** insert base at start of sequence
	*/
	s=DBgetSeq(xx,seq);
	for (j = DBgetLength(xx,seq)+1; j > 1; j--)
            s[j-1] = s[j-2];
	s[0] = extend_char;
	DBsetLength(xx,seq,DBgetLength(xx,seq)+1);
	DBsetFlags(xx,seq,DBgetFlags(xx,seq)|DB_FLAG_SEQ_MODIFIED|DB_FLAG_REL_MODIFIED);
	selectInsertBase(xx,seq,1);

	/*
	** shift sequence left
	*/
        if (DBgetRelPos(xx,seq)==1) {
	    for (i=1; i <= xx->DB_gelCount; i++) {
		if (seq == xx->DBorder[i]) {
		    for (j=i;j>1;j--)
			xx->DBorder[j] = xx->DBorder[j-1];
		    xx->DBorder[1] = seq;
		} else
		    DBsetRelPos(xx,xx->DBorder[i],DBgetRelPos(xx,xx->DBorder[i])+1);
	    }
	    DBsetFlags(xx,seq,DBgetFlags(xx,seq)|DB_FLAG_REL_MODIFIED);

	} else {
	    DBsetRelPos(xx,seq,DBgetRelPos(xx,seq)-1);
	    DBsetFlags(xx,seq,DBgetFlags(xx,seq)|DB_FLAG_REL_MODIFIED);

	    for (i=1; seq != xx->DBorder[i] && i <= xx->DB_gelCount; i++);
	    for (j=i; DBgetRelPos(xx,xx->DBorder[j-1]) > DBgetRelPos(xx,seq) ;j--)
		xx->DBorder[j] = xx->DBorder[j-1];
	    xx->DBorder[j] = seq;
	}

    } else {
	/*
	** insert base at end of sequence
	*/
	char *s;
	int l=DBgetLength(xx,seq);
	s=DBgetSeq(xx,seq);
	s[l] = extend_char;
	DBsetLength(xx,seq,l+1);
	DBsetFlags(xx,seq,DBgetFlags(xx,seq)|DB_FLAG_SEQ_MODIFIED|DB_FLAG_REL_MODIFIED);
	selectInsertBase(xx,seq,l+1);
    }

    /*
    ** Modify raw data stuff
    */
    {
	tagStruct *t;
	char type[5];
	char name[FILE_NAME_LENGTH+1];
	int llen,lcut,wlen;
	t = (tagStruct *) DBgetTags(xx,seq);
	force_comment(t);
        sscanf(t->newcomment,"%6d%6d%6d%*s",&llen,&lcut,&wlen);
	strncpy(type,&t->newcomment[18],4);
	strncpy(name,&t->newcomment[22],FILE_NAME_LENGTH);
	wlen+=lextend;
	if (use_tag==LEFT) lcut-=lextend;
        sprintf(t->newcomment,"%6d%6d%6d",llen,lcut,wlen);
	strncat(t->newcomment,type,4);
	strncat(t->newcomment,name,FILE_NAME_LENGTH);
	t->newcommentlen = (int)strlen(t->newcomment);
	t->flags |= TAG_COMMENT_CHANGED;
	DBsetFlags(xx,seq,DBgetFlags(xx,seq)|DB_FLAG_TAG_MODIFIED);

    }

    if (dir == RIGHT) xx->cursorPos++;

    /*
    ** Lengths are a changing
    */
    calculateConsensusLength(xx);

    return 0;

}


void meta_arrow (Widget widget, XEvent *event, int key)
/*
** Handle cut-off adjust
*/
{
    EdStruct *xx = widgetToEdStruct(XtParent(widget));
    int e_time = get_uniq_id(NULL);
    int seq,pos;
    int seq_length;

    seq = xx->cursorSeq;
    pos = xx->cursorPos;
    seq_length = DBgetLength(xx,seq);

    /*
    ** determine which operation is to take place
    */
    if (key==LEFT) {
	if (seq) {
	    if (pos==1) {
		if (! extend(xx,seq,LEFT,e_time)) {
		    recordEdit (&bucketStack, xx, undoExtend, seq, pos,LEFT,e_time);
		    redisplayWithCursor(xx);
		}
	    } else if (pos>seq_length) {
	    	if (! unextend(xx,seq,RIGHT,e_time)) {
		    recordEdit (&bucketStack, xx, undoUnextend, seq, pos, RIGHT, e_time);
		    redisplayWithCursor(xx);
		}
	    } else
		caretLeft(widget,event,NULL,0);
	} else
	    caretLeft(widget,event,NULL,0);
    } else {
	if (seq) {
	    if (pos==1) {
		if (! unextend(xx,seq,LEFT,e_time)) {
		    redisplayWithCursor(xx);
		    recordEdit (&bucketStack, xx, undoUnextend, seq, pos, LEFT,e_time);
		}
	    } else if (pos>seq_length) {
		if (! extend(xx,seq,RIGHT,e_time)) {
		    redisplayWithCursor(xx);
		    recordEdit (&bucketStack, xx, undoExtend, seq, pos, RIGHT, e_time);
		}
	    } else
		caretRight(widget,event,NULL,0);
	} else
	    caretRight(widget,event,NULL,0);
    }

}






void meta_left (Widget widget, XEvent *event, String *params,
		Cardinal *num_params)
{
    meta_arrow(widget,event,LEFT);
}

void meta_right (Widget widget, XEvent *event, String *params,
		 Cardinal *num_params)
{
    meta_arrow(widget,event,RIGHT);
}

void meta_up (Widget widget, XEvent *event, String *params,
	      Cardinal *num_params)
/*
** Handle cut-off adjust
*/
{
    EdStruct *xx = widgetToEdStruct(XtParent(widget));

    dump_tags(xx,xx->cursorSeq);

}
