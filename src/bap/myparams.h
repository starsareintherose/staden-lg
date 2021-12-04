#ifndef _myparams_h
#define _myparams_h

#include <sys/types.h>

enum types {
    t_unknown, t_int, t_char, t_float
};


typedef struct _field_entry{
    char *field_name;
    char *field_value;
    int  field_type;
    size_t  field_size;
} Field_entry;




extern void change_params(Widget parentWid, char *title, Field_entry *field_list, int field_entries);




#endif /* _myparams_h */
