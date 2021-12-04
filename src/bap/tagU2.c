/*
** tagU2.c
**
** Tag IO routines required by non-X assembly program
**
** Changes:
**
** 8-Jul-92
**      getext_() searchs for an IGN tag, to determine if cutoff should be
**      ignored
** 7-Aug-92
**	now initial tags can be specified in the sequence file
**      format is ";;%4s %6d %6d %s\n",type,position,length,comment
**      The comment is optional
** 27-Aug-92
**      modext() modifies cutoff data
**
*/
#include "contigEditor.h"
#include <stdio.h>
#include <stdlib.h>
#include "tagUtils.h"
#include "fort.h"

#define COMMENT_HEAD_ID (1)
#define TAG_HEAD_ID (devils_.idbsiz)

int read_tag(tag_id n, tagRecord *t)
{
    readtg_(&devils_.idevt,&n,&t->position,&t->length,&t->comment,&t->type.i,&t->next);
    return 0;
}
int write_tag(tag_id n, tagRecord t)
{
    writtg_(&devils_.idevt,&n,&t.position,&t.length,&t.comment,&t.type.i,&t.next);
    return 0;
}
int read_comment(comment_id n, int *cnt, commentStruct *c)
{
    readcc_(&devils_.idevc,&n,cnt,&c->next,&c->comment[0],40);
    return 0;
}
int write_comment(comment_id n, int cnt, commentStruct c)
{
    writcc_(&devils_.idevc,&n,&cnt,&c.next,&c.comment[0],40);
    return 0;
}
/* comment interface */
tag_id get_free_tag()
{
    tagRecord head;
    tagRecord freerec;
    tag_id free_id;
    (void) read_tag(TAG_HEAD_ID,&head);
    if (head.next != 0) {
	/*
	** if a free slot somewhere, use it
	*/
	free_id = head.next;
	(void) read_tag(free_id,&freerec);
	head.next = freerec.next;
	(void) write_tag(TAG_HEAD_ID,head);
    } else {
	/*
	** extend comment list file
	*/
	free_id = ++head.position;
	(void) write_tag(TAG_HEAD_ID,head);
	(void) write_tag(free_id,freerec);
    }

    return free_id;
}


void delete_comment (comment_id cp)
/*
** Put a comment on the free stack
*/
{
    int dummy;
    commentStruct head;
    commentStruct freerec;
    comment_id free_id;
    if (!cp) return;
    free_id = cp;
    (void) read_comment(free_id,&dummy,&freerec);
    while (freerec.next) {
	free_id = freerec.next;
        (void) read_comment(free_id,&dummy,&freerec);
    }
    (void) read_comment(COMMENT_HEAD_ID,&dummy,&head);
    freerec.next = head.next;
    (void) write_comment(free_id,dummy,freerec);
    head.next = cp;
    (void) write_comment(COMMENT_HEAD_ID,dummy,head);
}





void blank_tag_rec(tag_id t)
/*
** Blank out fields in tag record t
*/
{

    tagRecord r;

    (void) read_tag(t, &r);

    r.position = 0;
    r.length = 0;
    r.type.i = 0x20202020;
    r.comment = 0;
    r.next = 0;
    
    (void) write_tag(t, r);

}




void delete_tag_rec(tag_id t)
/*
** remove t from file, discarding comment if necessary
*/
{
    tagRecord head;
    tagRecord freerec;

    (void) read_tag(t,&freerec);

    if (freerec.comment)
	delete_comment(freerec.comment);

    (void) read_tag(TAG_HEAD_ID,&head);
    freerec.next = head.next;
    (void) write_tag(t,freerec);
    head.next = t;
    (void) write_tag(TAG_HEAD_ID,head);
    
}

static comment_id get_free_comment()
{
    commentStruct head;
    commentStruct freerec;
    comment_id free_id;
    int count;
    (void) read_comment(COMMENT_HEAD_ID,&count,&head);
    if (head.next != 0) {
	/*
	** if a free slot somewhere, use it
	*/
	free_id = head.next;
	(void) read_comment(free_id,&count,&freerec);
	head.next = freerec.next;
	(void) write_comment(COMMENT_HEAD_ID,count,head);
    } else {
	/*
	** extend comment list file
	*/
	free_id = ++count;
	(void) write_comment(COMMENT_HEAD_ID,free_id,head);
	(void) write_comment(free_id,0,freerec);
    }

    return free_id;
}

comment_id put_comment(char *c)
{
    commentStruct com;
    comment_id cur,next,this_comment;
    int clen = strlen(c);
    int piece;

    /* write out first block of COMMENT_LENGTH */
    this_comment=cur=get_free_comment();
    if (clen>COMMENT_LENGTH)
	piece = COMMENT_LENGTH;
    else
	piece = clen;

    {int i; for(i=0;i<COMMENT_LENGTH;i++)com.comment[i]=' ';}
    strncpy(com.comment,c,piece);

    c+= piece;
    clen -= piece;
    while (clen > 0) {
	next = get_free_comment();
	com.next = next;
	write_comment(cur,0,com);
	cur = next;
	if (clen<COMMENT_LENGTH)
	    piece = clen;

	{int i; for(i=0;i<COMMENT_LENGTH;i++)com.comment[i]=' ';}
	strncpy(com.comment,c,piece);

	c+= piece;
	clen -= piece;
    }
    com.next = 0;
    if (piece!=COMMENT_LENGTH)
	com.comment[piece]='\0';
    write_comment(cur,0,com);

    return this_comment;
}

static void add_RD_comment(tag_id t_id, char *comment)
{
    tagRecord t;

    readtg_(&devils_.idevt,&t_id,&t.position,&t.length,&t.comment,&t.type.i,&t.next);
    t.comment = put_comment(comment);
    writtg_(&devils_.idevt,&t_id,&t.position,&t.length,&t.comment,&t.type.i,&t.next);

}

static void insert_CUT_tags(tag_id t_id, int pos, int length, char *type, char *comment)
{
    tagRecord t,new_t;
    tag_id next_id,new_id;

    new_t.length = 1;
    strncpy(new_t.type.c,type,4);
    new_t.comment = put_comment(comment);

    readtg_(&devils_.idevt,&t_id,&t.position,&t.length,&t.comment,&t.type.i,&t.next);
    next_id = t.next;
    new_id = get_free_tag();
    new_t.position = pos;
    new_t.next = next_id;
    writtg_(&devils_.idevt,&new_id,&new_t.position,&new_t.length,&new_t.comment,&new_t.type.i,&new_t.next);
    next_id = new_id;
    t.next=next_id;
    writtg_(&devils_.idevt,&t_id,&t.position,&t.length,&t.comment,&t.type.i,&t.next);

}

void insert_NEW_tag(tag_id t_id, int pos, int length, char *type, char *comment)
{
    tagRecord new_t;
    tagRecord t, next_t;
    tag_id next_id,new_id;

    new_t.position = pos;
    new_t.length = length;
    strncpy(new_t.type.c,type,4);
    if (comment)
	new_t.comment = put_comment(comment);
    else
	new_t.comment = 0;

    readtg_(&devils_.idevt,&t_id,&t.position,&t.length,&t.comment,&t.type.i,&t.next);
    next_id = t.next;
    if (next_id)
	readtg_(&devils_.idevt,&next_id,&next_t.position,&next_t.length,&next_t.comment,&next_t.type.i,&next_t.next);
    while(next_id && next_t.position <= pos) {
	t_id = next_id;
	t = next_t;
	next_id = next_t.next;
	if(next_id)
	    readtg_(&devils_.idevt,&next_id,&next_t.position,&next_t.length,&next_t.comment,&next_t.type.i,&next_t.next);
    }

    new_id = get_free_tag();
    new_t.next = next_id;
    writtg_(&devils_.idevt,&new_id,&new_t.position,&new_t.length,&new_t.comment,&new_t.type.i,&new_t.next);
    t.next = new_id;
    writtg_(&devils_.idevt,&t_id,&t.position,&t.length,&t.comment,&t.type.i,&t.next);
}

static void insert_INS_tags(tag_id t_id, int pos, int length)
{
    tagRecord t,new_t;
    tag_id next_id,new_id;
    int i;

    new_t.length = 1;
    strncpy(new_t.type.c,TAG_TYPE_INSERT,3);
    new_t.type.c[3]='*';
    new_t.comment = 0;

    readtg_(&devils_.idevt,&t_id,&t.position,&t.length,&t.comment,&t.type.i,&t.next);
    next_id = t.next;
    for (i=length-1;i>=0;i--) {
	new_id = get_free_tag();
	new_t.position = pos+i;
	new_t.next = next_id;
	writtg_(&devils_.idevt,&new_id,&new_t.position,&new_t.length,&new_t.comment,&new_t.type.i,&new_t.next);
	next_id = new_id;
    }
    t.next=next_id;
    writtg_(&devils_.idevt,&t_id,&t.position,&t.length,&t.comment,&t.type.i,&t.next);

}

static void myFstr2Cstr(char *Fstr, int_fl Flen,
	       char *Cstr, int_fl Clen)
/*
    Copy the significant characters of a blank padded Fortran string
    to a '\0' terminated C string, ignoring excess characters.

    This function works if the strings are distinct or coincident, but
    not if they overlap in any other way.
*/
{   int_fl FsigLen, i;

    /* Find the significant length of Fstr */
    FsigLen=Flen;
    while ((FsigLen > 0) && (Fstr[FsigLen-1] == ' '))
    {   FsigLen--;
    }

    /* Copy up to (Clen-1) significant characters */
    i=0;
    while ((i < FsigLen) && (i < (Clen-1)))
    {   Cstr[i] = Fstr[i];
        i++;
    }

    Cstr[i] = '\0';
}

/* C version of ENTRD */
void entrd_(int_f *IDEVG,int_f *IDEVT,int_f *IDEVC,char *NAMARC,int_f *NGEL,
       int_f *IOK, int_fl l_NAMARC)
{
    int LENR,LCUT,LENW;
    int RCUT;

#define fn_len 100
    char fn[fn_len];
#define l_line 100
    char line[l_line];
    char *result;
    char *cutbuff,*cb,*l;

    FILE *fp;

    /* blank tag rec */
    blank_tag_rec((tag_id) *NGEL);

    myFstr2Cstr(NAMARC,l_NAMARC,fn,(int_fl)fn_len);

    *IOK = 0;
    /* don't need this anymore
    saveState.idevt  = (int_f *) &devils_.idevt;
    saveState.idevc  = (int_f *) &devils_.idevc;
    saveState.idbsiz = (int_f *) &devils_.idbsiz;
    */
    /* Enters raw data info into database */
    if ( (fp = fopen(fn,"r")) != NULL ) {
	result = fgets(line,l_line,fp);
	if (result != NULL && line[0] == ';') { 
	    /* process header */
	    sscanf(line,";%6d%6d%6d%*s\n",&LENR,&LCUT,&LENW);
	    RCUT = LENR - LCUT - LENW;
            add_RD_comment(*NGEL, &line[1]);
	    result = fgets(line,l_line,fp);
	}

	if (result != NULL && strlen(line)>1 && strncmp(line,";<",2)==0) {
	    /* process left cut off */
	    /* starting with this line we need to read LCUT ditties */
	    cutbuff = (char *) TAG_MALLOC(LCUT+1);
	    cb = cutbuff;
	    while (result!=NULL && strlen(line)>1 && strncmp(line,";<",2)==0) {
		for(l=&line[2];*l;l++)if(strchr(" \n\t",*l)==NULL)*cb++=*l;
		result = fgets(line,l_line,fp);
	    }
	    *cb='\0';
            insert_CUT_tags(*NGEL, 0, 0, TAG_TYPE_LCUT, cutbuff);
	    TAG_FREE(cutbuff);
	}

	if (result != NULL && strlen(line)>1 && strncmp(line,";>",2)==0) {
	    /* process right cut off */
	    /* starting with this line we need to read RCUT ditties */
	    cutbuff = (char *) TAG_MALLOC(RCUT+1);
	    cb = cutbuff;
	    while (result != NULL && strlen(line)>1 && strncmp(line,";>",2)==0) {
		for(l=&line[2];*l;l++)if(strchr(" \n\t",*l)==NULL)*cb++=*l;
		result = fgets(line,l_line,fp);
	    }
	    *cb='\0';
            insert_CUT_tags(*NGEL, 0, 0, TAG_TYPE_RCUT, cutbuff);
	    TAG_FREE(cutbuff);
	}
	while (result != NULL && strlen(line)>1 && strncmp(line,";;",2)==0) {
	    int pos,len;
	    char *comment;
	    /* format ";;%4s %6d %6d %s\n",type,position,length,comment 
	       --or-- ";;%4s %6d %6d\n",   type,position,length
	    */
	    if (strlen(line)>21)
		comment = line+21;
	    else
		comment = NULL;
	    sscanf(line+7,"%6d %6d",&pos,&len);
            insert_NEW_tag((tag_id)*NGEL, pos, len, line+2, comment);
	    result = fgets(line,l_line,fp);
	}

	fclose(fp);
    }
}

int_f freecc_ (int_f *IDEVC)
{
    return get_free_comment();
}

void padtag_(int_f *llino_, int_f *k_, int_f *nc_, int_f *lngthg_)
/*
** Play around with tags when auto assembling
*/
{
    int seq;
    int pos;
    int length;
    int seq_length;
    tagRecord t;
    tag_id last,next;
    int last_pos;
    if (devils_.idevt>0) {
	/* fudge */
	/* don't need this anymore
	saveState.idevt  = (int_f *) &devils_.idevt;
	saveState.idbsiz = (int_f *) &devils_.idbsiz;
	*/
	seq = *llino_;
	length = *nc_;
	if (*lngthg_ < 0) {
	    seq_length = - *lngthg_;
	    pos = seq_length - *k_ - *nc_ + 2;
	} else {
	    seq_length = *lngthg_;
	    pos = *k_;
	}

	next = seq;
	last = 0;
	last_pos = 0;
	while (next) {
	    readtg_(&devils_.idevt,&next,&t.position,&t.length,&t.comment,&t.type.i,&t.next);
	    /*
	    ** Move tags accordingly
	    */
	    if (t.position >= pos)
	    {
		t.position+=length;
		writtg_(&devils_.idevt,&next,&t.position,&t.length,&t.comment,&t.type.i,&t.next);
	    }
	    else if (t.position + t.length > pos)
	    {
		t.length+=length;
		writtg_(&devils_.idevt,&next,&t.position,&t.length,&t.comment,&t.type.i,&t.next);
	    }

	    /*
	    ** Insert *INS tags
	    */
	    if (last_pos < pos && t.position >= pos)
		/*
		** Insert after last `length' tags
		*/
		insert_INS_tags(last,pos,length);
	    else if (t.position < pos && t.next==0)
		/*
		** Insert after next `length' tags
		*/
		insert_INS_tags(next,pos,length);

            last = next;
	    last_pos = t.position;
	    next = t.next;
	}

    }
}

void taggel_(int_f *ngels_, int_f *lngthg_, char *gel_, int_fl gel_length)
{
    int_f length = abs(*lngthg_);
    int_f one=1;
    int_f i,j;
    
    if (*lngthg_ > 0) {
	for (i=0,j=1; i<length; i++,j++)
	    if (gel_[i]=='*')
		padtag_(ngels_,&j,&one,lngthg_);
    } else {
	for (i=length-1,j=length; i>=0; i--,j--)
	    if (gel_[i]=='*')
		padtag_(ngels_,&j,&one,lngthg_);
    }

}

void movtag_ (int_f *from, int_f *to )
/*
** Move tag information of gel ``from'' to gel ``to'',
** and perform garbage collection on old gel ``to''
*/
{

    tagRecord freerec;
    tag_id this,next;

    /* don't do anything if tag files aren't open */
    if ( devils_.idevc < 0 || devils_.idevt < 0 ) return;

    /* Throw away ``to'' tag records */
    (void) read_tag((tag_id) *to,&freerec);

    if (freerec.comment)
	delete_comment(freerec.comment);

    next = freerec.next;
    while ( next ) {
        this = next;
	(void) read_tag( this , &freerec );
        next = freerec.next;
        delete_tag_rec (this);
    }

    /* copy ``from'' record to ``to'' record */
    (void) read_tag((tag_id) *from,&freerec);
    (void) write_tag((tag_id) *to,freerec);

    /* initialise the hole to blank */
    blank_tag_rec(*from);

}

char *get_comment(comment_id cp)
{
    int dummy;
    commentStruct c;
    int count;
    comment_id nc;
    char *com,*comptr;

    if (!cp) return NULL;
    /* determine how long string is */
    count = 1;
    nc=cp;
    read_comment(nc, &dummy, &c);
    while (c.next != 0) {
	nc = c.next;
	count++;
        read_comment(nc, &dummy, &c);
    }

    com = comptr = (char *)TAG_MALLOC(count * COMMENT_LENGTH+1);
    nc=cp;
    read_comment(nc, &dummy, &c);
    strncpy(com,c.comment,COMMENT_LENGTH); com+=COMMENT_LENGTH;
    while (c.next != 0) {
	nc = c.next;
	count++;
        read_comment(nc, &dummy, &c);
        strncpy(com,c.comment,COMMENT_LENGTH); com+=COMMENT_LENGTH;
    }
    *com = '\0';

    return comptr;
    
}

void getext_(int_f *gel, char *cutoff, int_f *lcutoff, int_f *ok,
	     int_fl l_cutoff)
/*
** Get right cutoff for lowly Fortran Users
** If a tag TAG_TYPE_IGNORE exists return with ok=0
*/
{
#define TAG_TYPE_IGNORE "IGN"

    tagRecord rec;
    tag_id next;

    (void) read_tag((tag_id) *gel,&rec);

    next = rec.next;

    *ok = 1;
    while ( next && ! rec.position && *ok == 1) {
	(void) read_tag( next , &rec );
        next = rec.next;

	if (strncmp(rec.type.c,TAG_TYPE_RCUT,4) == 0) {
	    if (rec.comment) {
	        char *c;
		int i;
		/* use this */
		c = get_comment(rec.comment);
		/* copy to fortran array */
		for (i=0; i<*lcutoff && c[i]; i++) cutoff[i] = c[i];
		/* set length of returned string */
		*lcutoff = i;
		for (; i<*lcutoff; i++) cutoff[i] = ' ';

		/* rec temporary string space */
		TAG_FREE(c);
		*ok = 0;
	    } else {
		return;
	    }

	} else if (strncmp(rec.type.c,TAG_TYPE_IGNORE,3) == 0) return;

    }

    if (*ok == 0) {
	/* check to see we should ignore this reading */
	/* assumption - IGN tag occurs pos >= 1 */
	while ( next && *ok == 0) {
	    (void) read_tag( next , &rec );
	    next = rec.next;
	    *ok = (strncmp(rec.type.c,TAG_TYPE_IGNORE,3) == 0);
	}
    }

}











/*************************************************************/
/**      Routines for Rodger's pad shuffling                 */
/*************************************************************/

static void insert_edit_tag(tag_id t_id, int pos, char base, char *type)
{
    tagRecord t,new_t;
    tag_id new_id;

    /* read previous tag */
    readtg_(&devils_.idevt,&t_id,&t.position,&t.length,&t.comment,
	    &t.type.i,&t.next);

    /* set up new record */
    strncpy(new_t.type.c,type,3);
    new_t.type.c[3]=base;
    new_t.length = 1;
    new_t.comment = 0;
    new_t.position = pos;
    new_t.next = t.next;

    /* write new record */
    new_id = get_free_tag();
    writtg_(&devils_.idevt,&new_id,&new_t.position,&new_t.length,&new_t.comment,&new_t.type.i,&new_t.next);

    /* update previous tag */
    t.next=new_id;
    writtg_(&devils_.idevt,&t_id,&t.position,&t.length,&t.comment,&t.type.i,&t.next);

}



void insedt_(int_f *seq_, int_f *pos_, char *base_, int_fl base_l)
/*
** Insert a tag for a base at position pos in gel
*/
{
    int seq;
    int pos;
    char base;

    tagRecord t;
    tag_id last,next;

    if (devils_.idevt>0) {

	seq = (int) *seq_;
	pos = (int) *pos_;
	base = base_[0];


	/* skip over header */
	last = next = seq;
	readtg_(&devils_.idevt,&next,&t.position,&t.length,&t.comment,&t.type.i,&t.next);
	next = t.next;
	if (next) readtg_(&devils_.idevt,&next,&t.position,&t.length,&t.comment,&t.type.i,&t.next);

	/* adjust lengths of all tags staring before pos */
	while (next && t.position < pos) {
	    if (t.position + t.length - 1 >= pos &&
		t.type.c[0] != '*') {
		t.length++;
		writtg_(&devils_.idevt,&next,&t.position,&t.length,&t.comment,&t.type.i,&t.next);
	    }
            last = next;
	    next = t.next;
	    if (next) readtg_(&devils_.idevt,&next,&t.position,&t.length,&t.comment,&t.type.i,&t.next);

	}

	/* insert a tag after last and before next */
	insert_edit_tag(last,pos,base,TAG_TYPE_INSERT);

	/* shift everything else right */
	while (next) {
	    t.position++;
	    writtg_(&devils_.idevt,&next,&t.position,&t.length,&t.comment,&t.type.i,&t.next);
	    next = t.next;
	    if (next) readtg_(&devils_.idevt,&next,&t.position,&t.length,&t.comment,&t.type.i,&t.next);
	}

    }
}





void deledt_(int_f *seq_, int_f *pos_, char *base_, int_fl base_l)
/*
** Delete a base at position pos in gel
*/
{
    int_f seq;
    int_f pos;
    char base;

    tagRecord t;
    tag_id last,next;

    if (devils_.idevt>0) {

	seq = *seq_;
	pos = *pos_;
	base = base_[0];


	/* skip over header */
	last = next = (tag_id)seq;
	readtg_(&devils_.idevt,&next,&t.position,&t.length,&t.comment,&t.type.i,&t.next);
	next = t.next;
	if (next) readtg_(&devils_.idevt,&next,&t.position,&t.length,&t.comment,&t.type.i,&t.next);

	/* adjust lengths of all tags staring before pos */
	while (next && t.position <= pos) {
	    if (t.position + t.length - 1 >= pos &&
		t.type.c[0] != '*') {
		t.length--;
		writtg_(&devils_.idevt,&next,&t.position,&t.length,&t.comment,&t.type.i,&t.next);
	    }
            last = next;
	    next = t.next;
	    if (next) readtg_(&devils_.idevt,&next,&t.position,&t.length,&t.comment,&t.type.i,&t.next);

	}

	/* insert a tag after last and before next */
	insert_edit_tag(last,pos,base,TAG_TYPE_DELETE);

	/* shift everything else right */
	while (next) {
	    t.position--;
	    writtg_(&devils_.idevt,&next,&t.position,&t.length,&t.comment,&t.type.i,&t.next);
	    next = t.next;
	    if (next) readtg_(&devils_.idevt,&next,&t.position,&t.length,&t.comment,&t.type.i,&t.next);
	}

    }
}










/**********************************************************************/


int modext(int gel, int shorten_by)
/*
** shorted an extension by 'shorten_by' characters
** Returns:
**    0 - modification successful
**  !=0 - an error has occurred occur
*/
{

    tagRecord rec;
    tag_id next;
    int ok;
    char *c;
    tag_id hit;
    comment_id cut_off;

    (void) read_tag((tag_id) gel,&rec);
    /* store cutoff comment id */
    cut_off = rec.comment;

    next = rec.next;

    ok = 0;
    while ( next && ! rec.position && ok == 0) {
	(void) read_tag( next , &rec );

	if (strncmp(rec.type.c,TAG_TYPE_RCUT,4) == 0) {
	    if (rec.comment) {
		hit = next;
		c = get_comment(rec.comment);
		ok = 1;
	    } else {
		ok = 2;
	    }

	} else if (strncmp(rec.type.c,TAG_TYPE_IGNORE,3) == 0) ok = 2;

        next = rec.next;

    }

    
    if (ok == 1) {
	/* check to see we should ignore this reading */
	/* assumption - IGN tag occurs pos >= 1 */
	ok = 0;
	while ( next && ok == 0) {
	    (void) read_tag( next , &rec );
	    next = rec.next;
	    ok = (strncmp(rec.type.c,TAG_TYPE_IGNORE,3) == 0);
	}
	if (!ok) {
	    /*
	     * we have found a cutoff and not found an IGN tag!
	     * we can now shorten
	     */
	    if (ok = (strlen(c) >= shorten_by)) {
		/* must have enough sequence */
		comment_id oldcid, newcid;
		(void) read_tag( hit , &rec );
		oldcid = rec.comment;
		newcid = put_comment(c+shorten_by);
		rec.comment = newcid;
		(void) write_tag( hit , rec );
		delete_comment(oldcid);

		/* adjust cutoff */
		if (cut_off){
		    commentStruct c;
		    int dummy;
		    read_comment(cut_off, &dummy, &c);
		    /*
		     * parse comment and adjust current sequence length
		     */
		    {
			int llen,lcut,wlen;
			char type[5];
			char name[FILE_NAME_LENGTH+1];
			sscanf(c.comment,"%6d%6d%6d%*s",&llen,&lcut,&wlen);
			strncpy(type,&c.comment[18],4);
			strncpy(name,&c.comment[22],FILE_NAME_LENGTH);
			wlen+=shorten_by;
			sprintf(c.comment,"%6d%6d%6d",llen,lcut,wlen);
			strncat(c.comment,type,4);
			strncat(c.comment,name,FILE_NAME_LENGTH);
		    }
		    write_comment(cut_off, dummy, c);
		    
		}
	    }
	    
	}

	TAG_FREE(c);
    }

    return (ok!=1);

}

