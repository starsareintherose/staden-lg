#include <stdio.h>
#define USER_DATA
#ifdef USER_DATA
#include <pwd.h>
#endif
#include "misc.h" /* IMPORT: is_file */

/*
** This is an incredible hack. The original is based on a csh script
** Beware of horrid goto's etc
*/
typedef char String[200];
String SUBCLONES; /* initialised in initialise() */
String methods[] = {
    "shear",
    "Sau3A",
    "Alu-1",
};
String vectors[] = {
    "M13",
    "pBS",
    "pEMBL9",
    "pUC118",
};
String sizes[] = {
    "1-2",
    "6-9",
    "6-9",
    "9-14",
};
int MATCH = 3;

/*
** Defaults
*/
String def_mtd;
String def_cosmid;
String def_method;
String def_vector;
String def_size;
String def_verify;

/*
** Answers
*/
String ans_mtd;
String ans_cosmid;
String ans_method;
String ans_vector;
String ans_size;
#define MAX_COMMENTS 10
String ans_comments[MAX_COMMENTS];
int comments;
String ans_verify;

static void initialise()
/*
** Initialise miscellaneous variables
*/
{
    char *subclones;

    /* clonelib = subclones:$SUBCLONES */
    if (is_file("subclones"))
	strcpy(SUBCLONES,"subclones");
    else {
	subclones = (char *) getenv("SUBCLONES");
	if (subclones == NULL)
	    SUBCLONES[0] = '\0';
	else if (is_file(subclones))
	    strcpy(SUBCLONES,subclones);
	else
	    SUBCLONES[0] = '\0';
    }
}

int set_defaults()
{ 
    FILE *f;
    String line;
    String last_cosmid;
    String max_mtd;
    String mtd;
    String alpha;
    int number;
    int i;

    /* Determine SUBCLONES */
    initialise();

    if ( (f = fopen(SUBCLONES,"r")) == NULL ) return 1;

    max_mtd[0] = '\0';
    while ( fgets(line,sizeof(line),f) ) {
	sscanf(line, "%s %s", mtd,last_cosmid);
	if (strcmp(max_mtd,mtd) < 0) strcpy(max_mtd,mtd);
    }

    if ( ! max_mtd[0] ) return 1;

    /*
    ** generate next number
    */
    for (i = 0;isalpha(max_mtd[i]);i++) alpha[i] = max_mtd[i];
    alpha[i] = '\0';
    number = atoi(&max_mtd[i]) + 1;

    sprintf(def_mtd,"%s%02d",alpha,number);
    strcpy(def_cosmid, last_cosmid);
    strcpy(def_method, methods[0]);
    strcpy(def_vector, vectors[0]);
    strcpy(def_size, sizes[0]);
    strcpy(def_verify, "Yes");

    return 0;
}



ask_mtd()
{
 ask:
    printf("Microtitre dish [%s] ? ",def_mtd);
    gets(ans_mtd);

    if (strcmp(ans_mtd,"?")==0) {
	printf("* Enter the name of the new microtitre dish to be added to the library.\n");
	printf("  A microtitre dish must be a letter followed by two digits. Eg a87\n");
	goto ask;
    } else if (ans_mtd[0]) {
	if (! islower(ans_mtd[0]) ||
	    (strlen(ans_mtd)==3 ?
	     ! isdigit(ans_mtd[1]) :
	     ! islower(ans_mtd[1])) ||
	    ! isdigit(ans_mtd[2]) ||
	    (strlen(ans_mtd)==3 ?
	     ans_mtd[3] :
	     ! isdigit(ans_mtd[3]) ||
	     ans_mtd[4])) {
	    printf("! Invalid microtitre dish name.\n");
	    printf("  A microtitre dish must be a letter followed by two digits. Eg a87\n");
	    goto ask;
	}
    } else {
	strcpy(ans_mtd,def_mtd);
    }

}




ask_cosmid()
{
 ask:

    printf ("Cosmid [%s] ? ",def_cosmid);
    gets(ans_cosmid);

    if ( strcmp(ans_cosmid,"?") == 0 ) {
	printf("* Enter the name of the cosmid from which the subclones was made.\n");
	printf("  NO check is made on the correctness of the value entered.\n");
	goto ask;
    } else if ( ans_cosmid[0] ){
	int i;
	for (i=0; i<strlen(ans_cosmid); i++) {
	    if (i) {
		if (! isupper(ans_cosmid[i]) &&
		    ! isdigit(ans_cosmid[i])) break;
	    } else {
		if (! isupper(ans_cosmid[i])) break;
	    }
	}
	if (i != strlen(ans_cosmid)) {
	    printf("! Invalid cosmid name.\n");
	    printf("  A cosmid must be an letter followed by alphanumeric characters only\n");
	    goto ask;
	}
    } else {
	strcpy(ans_cosmid,def_cosmid);
    }
}

#define N(A) ( sizeof(A) / sizeof(A[0]) )
ask_method()
{
 ask:
    printf("Method of fragmentation [%s] ? ",def_method);
    gets(ans_method);

    if ( strcmp(ans_method,"?") == 0) {
	int i;
	printf("* Enter the fragmentation method used to make the subclones\n");
	printf("  Enter one of:\n");
	printf("    ");
	for (i=0; i< N(methods); i++)
	    printf("%s ",methods[i]);
	printf("\n");
	goto ask;
    } else if (ans_method[0]) {
	int i;
	for (i=0; i<N(methods);i++) {
	    if ( strncmp(ans_method,methods[i],MATCH) == 0) {
		strcpy(ans_method,methods[i]);
		break;
	    }
	}
	if (i == N(methods)) {
	    printf("! Invalid fragmentation method\n");
	    printf("  A valid fragmentation method is one of:\n");
	    printf("    ");
	    for (i=0; i< N(methods); i++)
		printf("%s ",methods[i]);
	    printf("\n");
	    goto ask;
	}
    } else {
	strcpy(ans_method, def_method);
    }
}


ask_vector()
{
 ask:
    printf("Vector [%s] ? ",def_vector);
    gets(ans_vector);

    if ( strcmp(ans_vector,"?") == 0) {
	int i;
	printf("* Enter the vector being used for the subclones\n");
	printf("  Enter one of:\n");
	printf("    ");
	for (i=0; i< N(vectors); i++)
	    printf("%s ",vectors[i]);
	printf("\n");
	goto ask;
    } else if (ans_vector[0] ) {
	int i;
	for (i=0; i<N(vectors);i++) {
	    if ( strncmp(ans_vector,vectors[i],MATCH) == 0) {
		strcpy(ans_vector,vectors[i]);
		strcpy(def_size,sizes[i]);
		break;
	    }
	}
	if (i == N(vectors)) {
	    printf("! Invalid fragmentation method\n");
	    printf("  A valid fragmentation method is one of:\n");
	    printf("    ");
	    for (i=0; i< N(vectors); i++)
		printf("%s ",vectors[i]);
	    printf("\n");
	    goto ask;
	}
    } else {
	strcpy(ans_vector, def_vector);
    }
}




ask_size()
{
 ask:
    printf("Size range [%s] ? ",def_size);
    gets(ans_size);
    
    if (strcmp(ans_size,"?") == 0) {
	printf("* Enter the size of the insert in the vector\n");
	printf("  The size should be a range in kilobases\n");
	printf("  Eg 1-2\n");
	goto ask;
    } else if (ans_size[0]) {
	int i;
	for (i=0; isdigit(ans_size[i]); i++);
	if (ans_size[i] == '-') {
	    for (i++; isdigit(ans_size[i]); i++);
	}
	if (i != strlen(ans_size)) {
	    printf("! Invalid size range specification.\n");
            printf("  The size range should be a range in kilobases. Eg 1-2\n");
	    goto ask;
	}
    } else {
	strcpy(ans_size, def_size);
    }
}

ask_comments()
{
    int i;
    String ans_comment;
    for (i=0; i<MAX_COMMENTS; i++) ans_comments[i][0] = '\0';
    comments = 0;

 ask:
    printf("Comment #%d [] ? ",comments+1);
    gets(ans_comment);

    if (strcmp(ans_comment,"?") == 0) {
	printf("* Enter a one line comment.\n");
	printf("  Multiple comments can be entered. When you wish to have entered all the\n");
	printf("  comments you wish, press return to the comment prompt.\n");
	goto ask;
    } else if (ans_comment[0]) {
	strcpy(ans_comments[comments++],ans_comment);
	if (comments == MAX_COMMENTS) {
	    printf("! Maximum number if comments reached. Sorry.\n");
	} else
	    goto ask;
    }

}


ask_verify()
{
    int i;

    printf("\nHere are the values you entered:\n\n");
    printf("Microtitre dish number: %s\n", ans_mtd);
    printf("Cosmid:                 %s\n", ans_cosmid);
    printf("Fragmentation method:   %s\n", ans_method);
    printf("Vector:                 %s\n", ans_vector);
    printf("Size range:             %s\n", ans_size);
    for(i=0; i< comments; i++)
	printf("Comment: #%d             %s\n", i+1, ans_comments[i]);
    printf("\n");

 ask:
    printf("Are these values correct [%s] ? ", def_verify);
    gets(ans_verify);
    
    if (strcmp(ans_verify,"?") == 0 ) {
	printf("Enter `Yes' if these values are correct. They will then be appended to the\n");
	printf("subclone libraries file.\n");
	printf("Enter `No' to exit the program.\n");
	goto ask;
    } else if (ans_verify[0]) {
	if ( strchr("YyNn",ans_verify[0]) == NULL ) {
	    printf("Please enter `Yes' or `No'.\n");
	    goto ask;
	}
    } else {
	strcpy(ans_verify, def_verify);
    }
}



update()
{
    FILE *f;
    int i;
    String output;
#ifdef USER_DATA
    struct passwd *pw;
#endif
    if ( (f = fopen(SUBCLONES,"a")) == NULL) return 1;

    fprintf(f,"%s %s %s %s %s", ans_mtd, ans_cosmid, ans_size, ans_method, ans_vector);
    for (i=0; i< comments; i++)
	fprintf(f," (%s)", ans_comments[i]);

#ifdef USER_DATA
    pw = getpwuid(getuid());
    fprintf(f," # %s",pw->pw_name);
    shell_call("hostname",output, sizeof(output));
    fprintf(f," %s",output);
    shell_call("date",output, sizeof(output));
    fprintf(f," %s",output);
#endif

    fprintf(f,"\n");
    fclose(f);
    return 0;
}




main ()
{


    printf("Maintain subclone database\n\n");

    if ( set_defaults() ) {
	fprintf (stderr,"Cannot open file %s\n",SUBCLONES);
	exit(1);
    }

    printf("Database=%s\n\n",SUBCLONES);

    ask_mtd();
    ask_cosmid();
    ask_method();
    ask_vector();
    ask_size();
    ask_comments();

    ask_verify();

    if ( strchr("Yy",ans_verify[0]) != NULL) {
	printf("Updating file %s with new values...",SUBCLONES);
	update();
	printf("done\n");
    } else {
	printf("No update made\n");
    }


}
