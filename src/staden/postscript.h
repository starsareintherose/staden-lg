#define clearg_x clearg_
#define movegr_x movegr_
#define drawgr_x drawgr_
#define pointg_x pointg_
#define writet_x writet_
#define openp_x openp_
#define closep_x closep_
#define clearp_x clearp_
#define movep_x movep_
#define drawp_x drawp_
#define pointp_x pointp_
#define writep_x writep_

#include "fort.h"

/* --- function prototypes --- */
int_f openp_x(char *FILE_p,
	      int_f *x1_p, int_f *y1_p, int_f *x2_p, int_f *y2_p,
	      int_f *width_p, int_f *lndscp_p, int_fl FILE_l);
void closep_x();
void clearp_x();
void movep_x(int_f *IX_p, int_f *IY_p);
void drawp_x(int_f *IX_p, int_f *IY_p);
void pointp_x(int_f *IX_p, int_f *IY_p);
void writep_x(int_f *IX_p, int_f *IY_p, char *TEXT_p, int_f *NCHAR_p,
	      int_fl TEXT_l);
