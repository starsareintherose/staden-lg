#ifndef _dapDB_h
#define _dapDB_h

#include "list.h"

extern void xdap_middle_open_for_read(List *l);
extern void xdap_middle_close(List *l);
extern List *xdap_middle_read_header();
extern List *xdap_middle_read_gel_data();
extern List *xdap_middle_read_contig_data();

#endif /* _dapDB_h */

