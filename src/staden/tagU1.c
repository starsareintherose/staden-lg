#include "contigEditor.h"
#include "edUtils.h"
#include "tagUtils.h"
#include "Sheet.h"
#include "tagdb.h"
#include "fort.h"
#include <stdio.h>
static tagStruct *tagFreeList = NULL;
/* tagUtils.c */
/* define external routines */


/* low level */

static void replace_comment(comment_id cp, char *c)
{
    delete_comment(cp);
    (void) put_comment(c);
    /* a bit dodgy here. Should work ok with this data structure though */
}

void force_comment(tagStruct *t)
/*
** Force comment to be in memory
*/
{
    if (!(t->flags & TAG_COMMENT_IN_MEMORY)) {
	/*
	** Read in from database
	*/
	if (t->tagrec.comment) {
	    t->newcomment = get_comment(t->tagrec.comment);
	    t->newcommentlen = (int)strlen(t->newcomment);
	} else {
	    t->newcomment = (char *) TAG_MALLOC(1);
	    t->newcomment[0] = '\0';
	    t->newcommentlen = 0;
	}
	t->flags |= TAG_COMMENT_IN_MEMORY;
    }
}


/*
** Tag internal memory management routines
*/
tagStruct *newTag()
{
    tagStruct *t;
    if (tagFreeList == NULL) {
        t = (tagStruct *) TAG_MALLOC(sizeof(tagStruct));
    } else {
	t = tagFreeList;
	tagFreeList = t->next;
    }

    /*
    ** Null all the fields
    */
    t->tagrec.position = 0;
    t->tagrec.length = 0;
    t->tagrec.comment = 0;
    t->tagrec.type.i = 0x20202020;
    t->tagrec.next = 0;
    t->original_tag_id = 0;
    t->newcomment = NULL;
    t->newcommentlen = 0;
    t->flags = TAG_UNCHANGED;
    t->next = NULL;
    t->time = 0;

    return t;
}

static void freeTag(tagStruct* t)
{
    TAG_FREE(t->newcomment);
    t->newcommentlen = 0;
    t->next = tagFreeList;
    tagFreeList = t;
}

tagStruct *readTagList(int seq)
{
    tagStruct *s,*t,*u;
    s=t=newTag();
    (void) read_tag(seq, &t->tagrec);
    t->original_tag_id = seq;
    while (t->tagrec.next) {
        u=newTag();
	read_tag(t->tagrec.next,&u->tagrec);
	u->original_tag_id = t->tagrec.next;
	t->next = u;
	t=u;
    }
    t->next = NULL;

    return s;
}

void destroyTagList(tagStruct *s)
{
    tagStruct *t,*u;

    t=s;
    while (t!=NULL) {
        u=t->next;
	freeTag(t);
	t=u;
    }
}

/*
** Tag creation and modification
*/
void insertTag(EdStruct *xx, int seq, tagStruct *t)
/*
** insert tag, sorting by position
*/
{
    tagStruct *u, *v;

    u = (tagStruct *) DBgetTags(xx,seq);
    v = NULL;
    while (u != NULL &&
	(u->tagrec.position <= t->tagrec.position || u->flags & TAG_DELETED) ) {
	v = u;
	u = u->next;
    }
    t->next = u;
    if (v != NULL) {
	v->next = t;
    } else {
	DBsetTags(xx,seq,t);
    }

}

void createTag(EdStruct *xx)
{
    int seq,start,length;
    char *newcomment;
    char newtype[4];
    int aborted;
    tagStruct *t;

    if (! getSelection(xx, &seq, &start, &length, &t)) {
	/* default selection is current cursor position */
	seq = xx->cursorSeq;
	start = xx->cursorPos;
	length = 1;
    }
    /* don't invoke Tag editor for consensus */
    if (seq) {
	aborted = invokeTagEditor("",newtype,"",&newcomment);
	if (! aborted ) {
	    /*
	    ** Create a new tag and insert it with comment into bimbo falix
	    */
	    t = newTag();
	    t->flags = TAG_INSERTED |
		       TAG_LENGTH_CHANGED |
		       TAG_POSITION_CHANGED |
		       TAG_TYPE_CHANGED;
	    t->tagrec.position = normalisePos(xx,seq,start,length);
	    t->tagrec.length = length;
	    strncpy(t->tagrec.type.c,newtype,4);
	    if (newcomment != NULL)
		t->flags |= TAG_COMMENT_CHANGED |
			    TAG_COMMENT_IN_MEMORY;
	    t->newcomment = newcomment;
	    t->newcommentlen = (int)strlen(newcomment);
	    insertTag(xx,seq,t);
	    redisplaySequences (xx,xx->namesWid, xx->sequencesWid, xx->displayPos, xx->displayWidth);
	    xx->select_tag = t;
	    DBsetFlags(xx,seq,DBgetFlags(xx,seq)|DB_FLAG_TAG_MODIFIED);
        }
    }
}

void editTag(EdStruct *xx)
{
    int seq,start,length;
    char *newcomment;
    char *oldcomment;
    tag_types newtype;
    int aborted;
    tagStruct *t;

    if (! getSelection(xx, &seq, &start, &length, &t)) {
	/* default selection is current cursor position */
	seq = xx->cursorSeq;
	start = xx->cursorPos;
        t = findTag(xx,seq,start);
        _select_tag(xx,seq,t);
        (void) getSelection(xx, &seq, &start, &length, &t);
    } else if (t==NULL) {
        t = findTag(xx,seq,start);
        _select_tag(xx,seq,t);
        (void) getSelection(xx, &seq, &start, &length, &t);
    }
    if (t==NULL) return;

    /*
    ** Find current comment
    */
    /*
    if (t->flags & TAG_COMMENT_IN_MEMORY) {
	oldcomment = t->newcomment;
    }
    else {
	.*
	** Read in from database
	*.
	if (t->tagrec.comment) {
	    oldcomment = get_comment(t->tagrec.comment);
	    t->flags |= TAG_COMMENT_IN_MEMORY;
	} else {
	    oldcomment = NULL;
	}
    }
    */
    force_comment(t);
    oldcomment = t->newcomment;

    /*
    ** Edit
    */
    aborted = invokeTagEditor(t->tagrec.type.c,newtype.c,oldcomment,&newcomment);
    if (! aborted ) {
	/*
	** Update components of tag that have changed
	*/
	/*
	** Don't update position!!!
	t->tagrec.position = start;
	t->tagrec.length = length;
	*/
	/*
	** Check for new type
	*/
	if (t->tagrec.type.i != newtype.i) {
	    t->tagrec.type.i = newtype.i;
	    t->flags |= TAG_TYPE_CHANGED;
	}
	/*
	** Check comment is something new
	*/
	if (newcomment != NULL) {
	    t->flags |= TAG_COMMENT_CHANGED | TAG_COMMENT_IN_MEMORY;
	    /*
	    ** throw old comment away
	    */
	    TAG_FREE(oldcomment);
	    t->newcomment = newcomment;
	    t->newcommentlen = (int)strlen(newcomment);
	}

	redisplaySequences (xx,xx->namesWid, xx->sequencesWid, xx->displayPos, xx->displayWidth);
	DBsetFlags(xx,seq,DBgetFlags(xx,seq)|DB_FLAG_TAG_MODIFIED);
    }

}

void _delete_tag(EdStruct *xx, int seq, tagStruct *t)
/*
** Delete tag t from data structure
*/
{
    /*
    ** Physical removal by delinking
    */
    if (t->flags & TAG_INSERTED) {
	tagStruct *u, *v;
	u = (tagStruct *) DBgetTags(xx,seq);
	v = NULL;
	while (u != NULL && u != t) {
	    v = u;
	    u = u->next;
	}
	if (u==NULL) return; /* not found */
	if (v==NULL)
	    DBsetTags(xx,seq,u->next);
	else
	    v->next = u->next;
	freeTag(u);
	u->flags |= TAG_DELETED;
    } else
	t->flags |= TAG_DELETED;

    DBsetFlags(xx,seq,DBgetFlags(xx,seq)|DB_FLAG_TAG_MODIFIED);

}

void deleteTag(EdStruct *xx)
/*
** A rather brutal delete
*/
{
    int seq,start,length;
    tagStruct *t;

    if (! getSelection(xx, &seq, &start, &length, &t)) {
	/* default selection is current cursor position */
	seq = xx->cursorSeq;
	start = xx->cursorPos;
        t = findTag(xx,seq,start);
        _select_tag(xx,seq,t);
        (void) getSelection(xx, &seq, &start, &length, &t);
    } else if (t==NULL) {
        t = findTag(xx,seq,start);
        _select_tag(xx,seq,t);
        (void) getSelection(xx, &seq, &start, &length, &t);
    }
    if (t==NULL) return;
       
    _delete_tag(xx,seq,t);
    redisplaySequences (xx,xx->namesWid, xx->sequencesWid, xx->displayPos, xx->displayWidth);

}

void getTagSplodge(EdStruct *xx, int seq, int pos, int width, XawSheetInk *ink)
/*
** get the hilighting of a sequence from its `pos' base for `width' bases
** Bases number from 0?
*/
{

    int i;
    tagStruct *t;
    int npos,tpos;


    if (xx->reveal_cutoffs) {
	int length = DBgetLength(xx,seq);

        /*blank start*/
        for (i=0; i<width && i<-pos; i++)
	    ink[i].sh=sh_light;

        /*copy sequence*/
        for (; i<width && (pos+i)<length; i++)
	    ink[i].sh=sh_default;

        /*blank end*/
        for (;i<width;i++)
	    ink[i].sh=sh_light;


    } else
	for (i=0;i<width;i++)
	    ink[i].sh=sh_default;

    pos++;
    npos = normalisePos(xx,seq,pos,width);
    t = (tagStruct *) DBgetTags(xx,seq);
    /* skip over raw data */
    if (seq && t != NULL) t = t->next;

    while (t != NULL && 
	(t->tagrec.position < npos+width || t->flags & TAG_DELETED) ) {
	if (!(t->flags & TAG_DELETED) &&
	    t->tagrec.position+t->tagrec.length > npos &&
	    t->tagrec.type.c[0] != '*') {
	    int l,r;
	    int db=idToIndex(t->tagrec.type.c);
	    tpos = normalisePos(xx,seq,t->tagrec.position,t->tagrec.length);
	    if (tpos < pos)
		l=0;
	    else
		l=tpos-pos;
	    if (tpos + t->tagrec.length > pos+width)
		r=width;
	    else
		r=tpos-pos + t->tagrec.length;
	    for (i=l;i<r;i++) {
		if (tag_db[db].fg_colour!=NULL) {
		    ink[i].sh|=sh_fg;
		    ink[i].fg=tag_db[db].fg_pixel;
		}
		if (tag_db[db].bg_colour!=NULL) {
		    ink[i].sh|=sh_bg;
		    ink[i].bg=tag_db[db].bg_pixel;
		}
	    }
        }
	t = t->next;
    }

}

static void tagEditDelete(EdStruct *xx, int seq, int pos, char deletedBase)
{
    tagStruct *t;

    t = (tagStruct *) DBgetTags(xx,seq);
    while (t != NULL &&
    (t->tagrec.position <= pos || t->flags & TAG_DELETED) ) {
	if( !(t->flags & TAG_DELETED) &&
	    t->tagrec.position==pos &&
	    strncmp(t->tagrec.type.c,TAG_TYPE_INSERT,3)==0){
	    _delete_tag(xx, seq, t);
	    return;
	}
	t = t->next;
    }

    /*
    ** We didn't find a tag so create a delete one
    */
    t = newTag();
    t->flags = TAG_INSERTED |
	       TAG_LENGTH_CHANGED |
	       TAG_POSITION_CHANGED |
	       TAG_TYPE_CHANGED;
    t->tagrec.position = pos;
    t->tagrec.length = 0;
    strncpy(t->tagrec.type.c,TAG_TYPE_DELETE,4);

    /*
    ** Stash the deleted base somewhere
    */
    t->tagrec.type.c[3] = deletedBase;

    insertTag(xx,seq,t);

}

static void tagEditInsert(EdStruct *xx, int seq, int pos, char insertedBase)
{
    tagStruct *t;
    /*
    ** Create an new insert tag here
    */
    t = newTag();
    t->flags = TAG_INSERTED |
	       TAG_LENGTH_CHANGED |
	       TAG_POSITION_CHANGED |
	       TAG_TYPE_CHANGED;
    t->tagrec.position = pos;
    t->tagrec.length = 1;
    strncpy(t->tagrec.type.c,TAG_TYPE_INSERT,3);

    /*
    ** Stash the deleted base somewhere
    */
    t->tagrec.type.c[3] = insertedBase;

    insertTag(xx,seq,t);
}


char normaliseBase(EdStruct *xx,int seq,char deletedBase)
{

    if (DBgetComp(xx,seq) == COMPLEMENTED) {
	char base = deletedBase;
	int_f i=1;
	sqcom_(&base,&i,(int_fl)1);
	return base;
    } else
	return deletedBase;
}


void tagInsertBase(EdStruct *xx,int seq,int pos,char *insertedBase)
/*
** A character has been inserted at position `pos' in sequence `seq'
** Adjust tag positions and lengths accordingly
*/
{
    int npos = normalisePos(xx,seq,pos,1);
    char base = normaliseBase(xx,seq,*insertedBase);

    tagStruct *t,*del_tag;
    int atpos = 0;
    t = (tagStruct *) DBgetTags(xx,seq);
    del_tag = NULL;

    while (t != NULL) {
	if ( !(t->flags & TAG_DELETED) &&
	    t->tagrec.position == npos &&
	    strncmp(t->tagrec.type.c,TAG_TYPE_DELETE,3)==0) {
	    if (! atpos && t->tagrec.type.c[3] == base) del_tag = t;
	    atpos++;
	}
	if ( !(t->flags & TAG_DELETED) &&
	    t->tagrec.position >= npos ) {
	    t->tagrec.position++;
	    t->flags |= TAG_POSITION_CHANGED;
	    DBsetFlags(xx,seq,DBgetFlags(xx,seq)|DB_FLAG_TAG_MODIFIED);
	}
	else if ( !(t->flags & TAG_DELETED) &&
	    t->tagrec.position + t->tagrec.length > npos ) {
	    t->tagrec.length++;
	    t->flags |= TAG_LENGTH_CHANGED;
	    DBsetFlags(xx,seq,DBgetFlags(xx,seq)|DB_FLAG_TAG_MODIFIED);
	}
	t = t->next;
    }

    if (del_tag == NULL)
	tagEditInsert(xx, seq, npos,base);
    else
	_delete_tag(xx, seq, del_tag);
}

void tagDeleteBase(EdStruct *xx,int seq,int pos, char *deletedBase)
/*
** A character has been deleted from position `pos' in sequence `seq'
** Adjust tag positions and lengths accordingly
*/
{
    int npos = normalisePos(xx,seq,pos,0);
    char base = normaliseBase(xx,seq,*deletedBase);
    tagStruct *t;

    tagEditDelete(xx, seq, npos, base);

    t = (tagStruct *) DBgetTags(xx,seq);
    while (t != NULL) {
	if ( !(t->flags & TAG_DELETED) &&
	    t->tagrec.position > npos) {
	    t->tagrec.position--;
	    t->flags |= TAG_POSITION_CHANGED;
	    DBsetFlags(xx,seq,DBgetFlags(xx,seq)|DB_FLAG_TAG_MODIFIED);
	}
	else if ( !(t->flags & TAG_DELETED ) &&
	    t->tagrec.position + t->tagrec.length > npos) {
	    t->tagrec.length--;
	    t->flags |= TAG_LENGTH_CHANGED;
	    DBsetFlags(xx,seq,DBgetFlags(xx,seq)|DB_FLAG_TAG_MODIFIED);
	}
	t = t->next;
    }

}

void tagReplaceBase(EdStruct *xx,int seq,int pos, char *deletedBase, char *insertedBase)
/*
** A character has been replaced in position `pos' in sequence `seq'
** Adjest tags accordingly
*/
{
    int tpos = normalisePos(xx,seq,pos,1);
    char dbase = normaliseBase(xx,seq,*deletedBase);
    char ibase = normaliseBase(xx,seq,*insertedBase);
    tagEditDelete(xx, seq, tpos, dbase);
    tagEditInsert(xx, seq, tpos, ibase);
}


tagStruct *findTag(EdStruct *xx,int seq,int pos)
/*
** Find the tag (if any) at position `pos' in sequence `seq'
*/
{
    int npos = normalisePos(xx,seq,pos,1/*character*/);

    tagStruct *t;
    t = (tagStruct *) DBgetTags(xx,seq);
    while (t != NULL) {
	if (!(t->flags & TAG_DELETED) &&
	   t->tagrec.position <= npos &&
	   t->tagrec.position + t->tagrec.length > npos &&
	   t->tagrec.type.c[0] != '*')
	    return t;
	t = t->next;
    }
    return NULL;
}

tagStruct *findAllTags(EdStruct *xx,int seq,int pos)
/*
** Find the tag (if any) at position `pos' in sequence `seq'
*/
{
    static tagStruct *t;
    static int npos;
    if (xx==NULL) {
	if (t != NULL) t = t->next;
    } else {
	npos = normalisePos(xx,seq,pos,1/*character*/);
	t = (tagStruct *) DBgetTags(xx,seq);
    }

    while (t != NULL) {
	if (!(t->flags & TAG_DELETED) &&
	   t->tagrec.position <= npos &&
	   t->tagrec.position + t->tagrec.length > npos)
	    return t;
	t = t->next;
    }
    return NULL;
}

void writeTagList(EdStruct *xx, int seq)
{
    tagRecord anon_tagrec;
    tagStruct *first_tag,*this_tag;
    tag_id last_tagrec_id;
    int flag;

    first_tag = (tagStruct *) DBgetTags(xx,seq);

    /*
    ** *INS - flag as deleted all *INS flags with length 0
    */
    this_tag = first_tag;
    while (this_tag != NULL) {
	if ( !(this_tag->flags & TAG_DELETED) &&
	    this_tag->tagrec.length == 0 &&
	    strncmp(this_tag->tagrec.type.c,TAG_TYPE_INSERT,3)==0)
	    this_tag->flags |= TAG_DELETED;
	this_tag = this_tag->next;
    }

    /*
    ** Assumption: first tag in list is always a header and doesn't change
    */
    last_tagrec_id = DBgetNumber(xx,seq);
    this_tag = first_tag->next;

    /*
    ** Assumption: cutoff information is stored in header's comment
    */
    if (first_tag->flags & TAG_COMMENT_CHANGED) {
	if (first_tag->tagrec.comment)
	    (void) replace_comment(first_tag->tagrec.comment,first_tag->newcomment);
	else {
	    first_tag->tagrec.comment = put_comment(first_tag->newcomment);
	    (void) write_tag(last_tagrec_id,first_tag->tagrec);
	}
    }

    while (this_tag != NULL) {
	flag = this_tag->flags;

	if (flag == TAG_UNCHANGED || flag == TAG_COMMENT_IN_MEMORY) {
	    /*
	    ** get next tag in list
	    */
	    last_tagrec_id = this_tag->original_tag_id;
	    this_tag = this_tag->next;
	}

	else if (flag & TAG_INSERTED && !(flag & TAG_DELETED) ) {
	    tag_id this_tagrec_id;
	    /*
	    ** create a new tag
	    */
	    this_tagrec_id = get_free_tag();
	    /*
	    ** update previous tag record
	    */
	    (void) read_tag(last_tagrec_id,&anon_tagrec);
	    this_tag->tagrec.next = anon_tagrec.next;
	    anon_tagrec.next = this_tagrec_id;
	    (void) write_tag(last_tagrec_id,anon_tagrec);
	    /*
	    ** write new tag record
	    */
	    if (this_tag->newcomment!=NULL && this_tag->newcommentlen!=0)
		this_tag->tagrec.comment = put_comment(this_tag->newcomment);
	    else
		this_tag->tagrec.comment = 0;
	    (void) write_tag(this_tagrec_id,this_tag->tagrec);
	    /*
	    ** get next tag in list
	    */
	    last_tagrec_id = this_tagrec_id;
	    this_tag = this_tag->next;
	}

	else if (!(flag & TAG_INSERTED) && flag & TAG_DELETED) {
	    tag_id this_tagrec_id;
	    this_tagrec_id = this_tag->original_tag_id;
	    /*
	    ** delink record from file
	    */
	    (void) read_tag(last_tagrec_id,&anon_tagrec);
	    anon_tagrec.next = this_tag->tagrec.next;
	    (void) write_tag(last_tagrec_id,anon_tagrec);
	    /*
	    ** destroy deleted tag
	    */
	    (void) delete_tag_rec(this_tagrec_id);
	    /*
	    ** get next tag in list
	    */
	    this_tag = this_tag->next;
	}

	else if (!(flag & TAG_DELETED) )
	{
	    tag_id this_tagrec_id;
	    this_tagrec_id = this_tag->original_tag_id;
	    /*
	    ** Deal with comment first
	    */
	    if (flag & TAG_COMMENT_CHANGED) {
		if (this_tag->tagrec.comment)
		    (void) replace_comment(this_tag->tagrec.comment,this_tag->newcomment);
		else
		    this_tag->tagrec.comment = put_comment(this_tag->newcomment);
	    }
	    (void) write_tag(this_tagrec_id,this_tag->tagrec);
	    /*
	    ** get next tag in list
	    */
	    last_tagrec_id = this_tagrec_id;
	    this_tag = this_tag->next;
	}
	else {
	    /*
	    ** get next tag in list
	    */
	    this_tag = this_tag->next;
	}
    }
}



int origpos(EdStruct *xx, int seq, int pos)
/*
** Determine position in original sequence corresponding to pos
** taking into account all insertions and deletions
*/
{
    int npos = pos;

    tagStruct *t;
    t = (tagStruct *) DBgetTags(xx,seq);
    while (t != NULL &&
	(t->tagrec.position <= pos || t->flags & TAG_DELETED)) {
	if ( !(t->flags & TAG_DELETED ) &&
	    t->tagrec.type.c[0] == '*') {
	    if (strncmp(t->tagrec.type.c, TAG_TYPE_INSERT,3)==0)
		/*insertion*/
		npos--;
	    else if (strncmp(t->tagrec.type.c, TAG_TYPE_DELETE,3)==0)
		/*deletion*/
		npos++;
	}
	t = t->next;
    }

    return npos;
}



tagStruct *findTagByType(EdStruct *xx, int seq, char *type)
/*
** Find the first tag by type
*/
{
    tagStruct *t;
    int looking,found;
    
    /* find tag */
    t = (tagStruct *) DBgetTags(xx,seq);
    looking = 1;
    found = 0;
    while (looking && !found) {
	found = ( strncmp( t->tagrec.type.c, type, 4 ) == 0 );
	if (! found) {
	    t = t->next;
	    looking = (t==NULL)?0:( !t->tagrec.position );
	}
    }
    if (found)
	return t;
    else
	return NULL;
}


void getLeftCutOff(EdStruct *xx,int seq, int width, char *str)
{
    if (xx->reveal_cutoffs && width >0 ) {
	tagStruct *t;
	int c;
	c = (DBgetComp(xx,seq)==COMPLEMENTED);

	/* find tag */
	t = findTagByType(xx,seq,(c?TAG_TYPE_RCUT:TAG_TYPE_LCUT));

	if (t != NULL) {
	    /*
	    ** Find current comment
	    */
	    force_comment(t);

	    if (t->newcomment != NULL) {
		int l = t->newcommentlen;
		for (;l<width;width--)*str++=' ';
		if (c)
		    for (width--;width>=0;width--) {
			int_f i=1;
			*str  = t->newcomment[width];
			sqcom_(str,&i,(int_fl)1);
			str++;
		    }
		else
		    strncpy(str,&t->newcomment[l-width],width);
		return;
	    }
	}
    }

    for(;width>0;width--)*str++=' ';
}

void getLCut(EdStruct *xx,int seq, int pos, int width, char *str)
/*
**
*/
{
    if (xx->reveal_cutoffs && width >0 ) {
	tagStruct *t;
	int c;
	c = (DBgetComp(xx,seq)==COMPLEMENTED);

	/* find tag */
	t = findTagByType(xx,seq,(c?TAG_TYPE_RCUT:TAG_TYPE_LCUT));

	if (t != NULL) {
	    /*
	    ** Find current comment
	    */
	    force_comment(t);

	    if (t->newcomment != NULL) {
		int l = t->newcommentlen;
		for (;l<pos;pos--,width--)*str++=' ';
		if (c)
		    for (pos--,width--;width>=0;width--,pos--) {
			int_f i=1;
			*str  = t->newcomment[pos];
			sqcom_(str,&i,(int_fl)1);
			str++;
		    }
		else
		    strncpy(str,&t->newcomment[l-pos],width);
		return;
	    }
	}
    }

    for(;width>0;width--)*str++=' ';
}


void getRightCutOff(EdStruct *xx,int seq, int width, char *str)
{
    if (xx->reveal_cutoffs && width >0 ) {
	tagStruct *t;
	int c;
	c = (DBgetComp(xx,seq)==COMPLEMENTED);

	/* find tag */
	t = findTagByType(xx,seq,(c?TAG_TYPE_LCUT:TAG_TYPE_RCUT));

	if (t != NULL) {
	    /*
	    ** Find current comment
	    */
	    force_comment(t);

	    if (t->newcomment != NULL) {
		int l = t->newcommentlen;
		for (;l<width;width--)str[width-1]=' ';
		if (c) {
		    char *p;
		    for (p=&t->newcomment[l-1];width>0;width--,p--) {
			int_f i=1;
			*str  = *p;
			sqcom_(str,&i,(int_fl)1);
			str++;
		    }
		} else
		    strncpy(str,t->newcomment,width);
		return;
	    }
	}
    }

    for(;width>0;width--)*str++=' ';
}


void getRCut(EdStruct *xx,int seq, int pos, int width, char *str)
{
    if (xx->reveal_cutoffs && width >0 ) {
	tagStruct *t;
	int c;
	c = (DBgetComp(xx,seq)==COMPLEMENTED);

	/* find tag */
	t = findTagByType(xx,seq,(c?TAG_TYPE_LCUT:TAG_TYPE_RCUT));

	if (t != NULL) {
	    /*
	    ** Find current comment
	    */
	    force_comment(t);

	    if (t->newcomment != NULL) {
		int l = t->newcommentlen;
		for (;l<pos+width;width--)str[width-1]=' ';
		if (c) {
		    char *p;
		    for (p=&t->newcomment[l-pos-1];width>0;width--,p--) {
			int_f i=1;
			*str  = *p;
			sqcom_(str,&i,(int_fl)1);
			str++;
		    }
		} else
		    strncpy(str,&t->newcomment[pos],width);
		return;
	    }
	}
    }

    for(;width>0;width--)*str++=' ';
}


int lenRCut(EdStruct *xx, int seq)
{
    tagStruct *t;
    int c;
    c = (DBgetComp(xx,seq)==COMPLEMENTED);
    
    /* find tag */
    t = findTagByType(xx,seq,(c?TAG_TYPE_LCUT:TAG_TYPE_RCUT));
    
    if (t != NULL) {
	/*
	** Find current comment
        */
	force_comment(t);
	
	if (t->newcomment != NULL)
	    return t->newcommentlen;
	else
	    return 0;
    } else
	return 0;

}


int lenLCut(EdStruct *xx, int seq)
{
    tagStruct *t;
    int c;
    c = (DBgetComp(xx,seq)==COMPLEMENTED);
    
    /* find tag */
    t = findTagByType(xx,seq,(c?TAG_TYPE_RCUT:TAG_TYPE_LCUT));
    
    if (t != NULL) {
	/*
	** Find current comment
        */
	force_comment(t);
	
	if (t->newcomment != NULL)
	    return t->newcommentlen;
	else
	    return 0;
    } else
	return 0;

}

void dump_tags(EdStruct *xx, int seq)
{
    tagStruct *t = (tagStruct *) DBgetTags(xx,seq);

    fprintf(stderr,"Tags for %s\n",DBgetName(xx,seq));
    while (t != NULL) {
	
	fprintf(stderr," %5d %3d   %4s %5d %c%c%c%c%c%c%c\n",
	    t->tagrec.position,
	    t->tagrec.length,
	    t->tagrec.type.c,
	    t->tagrec.comment,
	    (t->flags & TAG_POSITION_CHANGED) ?'P':'-',
	    (t->flags & TAG_LENGTH_CHANGED)   ?'L':'-',
	    (t->flags & TAG_TYPE_CHANGED)     ?'T':'-',
	    (t->flags & TAG_COMMENT_CHANGED)  ?'C':'-',
	    (t->flags & TAG_INSERTED)         ?'I':'-',
	    (t->flags & TAG_DELETED)          ?'D':'-',
	    (t->flags & TAG_COMMENT_IN_MEMORY)?'M':'-'
	    );

	t = t->next;
    }
}
