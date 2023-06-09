#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include "list.h"
#include "process.h"
#include "misc.h"

char *types[] = {
    db_type_RS_flat_file,
    db_type_SD_flat_file,
    db_type_sap,
    db_type_early_xdap,
    db_type_middle_xdap,
    db_type_late_xdap,
};

char *vtypes[] = {
    db_vtype_RS_flat_file,
    db_vtype_SD_flat_file,
    db_vtype_sap,
    db_vtype_early_xdap,
    db_vtype_middle_xdap,
    db_vtype_late_xdap,
};

int all_types[] = {
    0, /* db_type_RS_flat_file */
    1, /* db_type_SD_flat_file */
    2, /* db_type_sap */
    3, /* db_type_early_xdap */
    4, /* db_type_middle_xdap */
    5, /* db_type_late_xdap */
};

int src_types[] = {
    1, /* db_type_SD_flat_file */
    4, /* db_type_middle_xdap */
    5, /* db_type_late_xdap */
};

int tgt_types[] = {
    1, /* db_type_SD_flat_file */
    5, /* db_type_late_xdap */
};


typedef char IOString[200];



static void get_db(char *prompt,
		   int *use_types,
		   int ntypes,
		   char *name,
		   char *version,
		   char *type)
{
    int i;
    IOString ctype;
    int itype;

    printf("%s\n",prompt);
    printf("Available types are:\n");

    for (i=0;i<ntypes;i++)
	printf("%d. %s\n",i,vtypes[use_types[i]]);

    printf("\n");
    do {
	printf("Database type? ");
	if ( gets(ctype) == NULL )
	    itype = -1;
	else
	    itype = atoi(ctype);
    } while (itype<0 || itype>=ntypes);

    strcpy(type,types[use_types[itype]]);

    printf("Database name? ");
    gets(name);

    printf("Database version? ");
    gets(version);

    printf("\n");
}





int main ()
{

    List *f;
    List *t;
    IOString src_name;
    IOString src_version;
    IOString src_type;
    IOString tgt_name;
    IOString tgt_version;
    IOString tgt_type;

    printf("Covert Project Database\nVersion 1.0, 18 November 1992\n");

#define NUMBER(A) ((int)(sizeof(A) / sizeof((A)[0])))
    get_db("Please enter database to convert:\n",src_types, NUMBER(src_types), src_name, src_version, src_type);
    get_db("Please enter database to create:\n",tgt_types, NUMBER(tgt_types), tgt_name, tgt_version, tgt_type);

    if (strcmp(src_type,tgt_type)==0)
	crash("Cannot convert a database to another of the same type\n");

    f = build_list (
		    atom_str(db_from),
		    build_list(
			       atom_str(db_name),
			       atom_str(src_name),
			       nil),
		    build_list(
			       atom_str(db_version),
			       atom_str(src_version),
			       nil),
		    build_list(
			       atom_str(db_type),
			       atom_str(src_type),
			       nil),
		    nil);

    t = build_list (
		    atom_str(db_to),
		    build_list(
			       atom_str(db_name),
			       atom_str(tgt_name),
			       nil),
		    build_list(
			       atom_str(db_version),
			       atom_str(tgt_version),
			       nil),
		    build_list(
			       atom_str(db_type),
			       atom_str(tgt_type),
			       nil),
		    nil);

    process (f,t);

    destroy_list(f);
    destroy_list(t);

    destroy_node_list(); /* garbage collect */

    printf("\nConversion completed\n");

    return 0;
}


