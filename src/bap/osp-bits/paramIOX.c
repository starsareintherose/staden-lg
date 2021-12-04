/* 
  Program Name: paramIOX
  File name: paramIOX.c
  Purpose: X windows portion of reading in and output constraints
  Last Update: Aug 6 1991
  Copyright 1991: LaDeana Hillier and Philip Green

  Change Log:
*/


/* ---- Includes ---- */
#include "defn.h"  /* macros and stdio */
#include "our_allo.h"
#include "Xmess.h"
#include "Xincludes.h"  /* global X variables and X libs etc */
#include "struct.h" /* global structure defns */
#include "paramIO.h" /* IMPORT ReadDef,write_params_file */


/* --------External variables--------*/

extern Prm prm;
extern int program_option;
extern int program_version;
static Prm *orig_params;

#ifdef XVERSION
extern Widget resultsWid;
extern int weights;
extern char *def_fn; /* default file name */
extern char *seq_filename;
#endif

extern void updateOtherFnWid();
void set_text_def();

#ifdef SUBVERSION
int weights=0; /* he may want to ask me to change the weights, then what? */
char def_fn[MAX_NAME_SIZE];
static char seq_filename[MAX_NAME_SIZE];
#endif


void create_osp_change_parameters_popup();
void check_param_validity();

/* Widgets global to paramIO.c */
Widget option1weight1Wid=NULL,option1weight0Wid=NULL,option3weight0Wid=NULL, option3weight1Wid=NULL;



/* ---- Exports ---- */

static void
updateOtherSeqFn(w,client_data, call_data)
/*callback when the user asks to input parameters from
  file, this updates the other sequence filename input
  box on the main form */
Widget w;
XtPointer client_data;
XtPointer call_data;
{
  /*this function is in osp.c and just writes
    the other fn to the main form widget*/
#ifdef XVERSION
  updateOtherFnWid();
#endif
  return;
}



static void changeWtAmbig(widget,client_data,call_data)
Widget widget;
XtPointer client_data;
XtPointer call_data;
{
   Arg arg[10];
   int n=0;
   Widget WtAmbigWid = (Widget) client_data;
   
   if (!strcmp("avg",prm.wt_ambig)) {
     sprintf(prm.wt_ambig,"full");
     XtSetArg(arg[n], XtNlabel, "full"); 
   }
   else {
     sprintf(prm.wt_ambig,"avg");
     XtSetArg(arg[n], XtNlabel, "avg "); 
   }
        n=1;
	XtSetValues(WtAmbigWid,arg,n);
 }



void osp_change_parameters(button,params,weight_or_param)
Widget button;
Prm *params;
int weight_or_param; /* a 0 if you want me to popup the change
			parameters window, a 1 if you want the window
			to change weights instead. */

{ int temp_weights;
  int temp_option;

    weights=weight_or_param;
    orig_params = params;
    prm=*params;

/* if they have not been popped up before, go ahead and create
   all the parameter and weight popups */

  if (option1weight1Wid==NULL && option1weight0Wid==NULL && option3weight0Wid==NULL && option3weight1Wid==NULL)
 {

/* temporarily story these so you can change them */
    temp_weights=weights;
    temp_option=program_option;

/* this creates all the necessary popup prompts */
    program_option=3;
    weights=0;
    create_osp_change_parameters_popup(button,prm);
    weights=1;
    create_osp_change_parameters_popup(button,prm);
  if (program_version!=1) {
    program_option=1;
    weights=0;
    create_osp_change_parameters_popup(button,prm);
    weights=1;
    create_osp_change_parameters_popup(button,prm);
  }

/* reset them back to their original values */
    weights=temp_weights;
    program_option=temp_option;
  }




  if (program_option==3 || program_option==4) {
    if (weights)
      set_text_def(option3weight1Wid);
    else
      set_text_def(option3weight0Wid);
  }
  else {
    if (weights)
      set_text_def(option1weight1Wid);
    else
      set_text_def(option1weight0Wid);
  }


  if (program_option==3 || program_option==4) {
    if (weights)
      XtPopup(option3weight1Wid, XtGrabExclusive);
    else
      XtPopup(option3weight0Wid, XtGrabExclusive);
  }
  else {
    if (weights)
      XtPopup(option1weight1Wid, XtGrabExclusive);
    else
      XtPopup(option1weight0Wid, XtGrabExclusive);
  }

    return;
 }

void 
popDownPopupPrompt(widget,client_data,call_data)
Widget widget;
XtPointer client_data;
XtPointer call_data;
{

  if (program_option==3 || program_option==4) {
    if (weights)
      XtPopdown(option3weight1Wid);
    else
      XtPopdown(option3weight0Wid);
  }
  else {
    if (weights)
      XtPopdown(option1weight1Wid);
    else
      XtPopdown(option1weight0Wid);
  }
}

void 
destroyParamPopupPrompts()
{
/*
  XtDestroyWidget(option3weight1Wid);
  XtDestroyWidget(option3weight0Wid);
  if (program_version!=3)  {
    XtDestroyWidget(option1weight1Wid);
    XtDestroyWidget(option1weight0Wid);
  }
*/
}


void create_osp_change_parameters_popup(button,prm)
Widget button;
Prm prm;
{ 
    Arg		args[5],arg[5];
    Widget	popup;
    Widget lprod_leng_min,lprod_leng_max,lprod_tm_min,lprod_tm_max,lprim_tm_max,lprim_tm_min;
    Widget lprod_gc_min,lprod_gc_max;
    Widget lprim_len_min,lprim_len_max,lprim_gc_min,lprim_gc_max;
    Widget lprimprodWid,lprimprimWid,lprimotherWid;
    Widget lprim_nucs,ldelta_tm;
    Widget lprimselfWid,lann_hmlg;
    Widget lATWeightWid,lCGWeightWid;
    Widget lWtAmbigWid;
    Widget cancelWid, confirmWid, defFnWid, saveDefWid,inputDefWid;
    Widget ann_labl;
    Position	x, y;
    Dimension	width, height;
    Cardinal	n;
    String str;
Widget paramForm,prod_form,prod_labl,prod_leng_min,prod_leng_max;
Widget prod_tm_min,prod_tm_max,prim_form,prim_labl,prim_len_min;
Widget prim_len_max,prim_gc_min,prim_gc_max,ann_hmlg_i,ann_hmlg_t;
Widget prim_nucs,prim_tm_min,prim_tm_max,delta_tm;
Widget ppann_hmlg_i,ppann_hmlg_t;
Widget prod_gc_min,prod_gc_max;
Widget primprodIWid,primprod3Wid,primotherIWid,primother3Wid;
Widget ATWeightWid,CGWeightWid,WtAmbigWid;
Widget ann_form;





#ifdef XVERSION
    button=resultsWid; /* this way the param popup will be most likely
			  to fit on the screen */
#endif

    /*
     * This will position the upper left hand corner of the popup at the
     * center of the widget which invoked this callback, which will also
     * become the parent of the popup. 
     */

    n = 0;
    XtSetArg(args[0], XtNwidth, &width); n++;
    XtSetArg(args[1], XtNheight, &height); n++;
    XtGetValues(button, args, n);
    XtTranslateCoords(button, (Position) (width / 2), (Position) (height / 2),
		      &x, &y);
    x -= 417; /*move it over by half of the param popups width, so its centered
		over the results wid window */
    y -= 240;
    
    n = 0;
    XtSetArg(args[n], XtNx, x);				n++;
    XtSetArg(args[n], XtNy, y);				n++;

    if (program_option==3) {
      if (weights) {
	option3weight1Wid = XtCreatePopupShell("params", 
		      transientShellWidgetClass, button, args, n);   
	popup=option3weight1Wid;
      }
      else {
	option3weight0Wid = XtCreatePopupShell("params", 
			  transientShellWidgetClass, button, args, n);   
	popup=option3weight0Wid;
      } 
    }
    else {
      if (weights) {
	option1weight1Wid = XtCreatePopupShell("params", 
			  transientShellWidgetClass, button, args, n);   
	popup=option1weight1Wid;
      }
      else {
	option1weight0Wid = XtCreatePopupShell("params", 
			  transientShellWidgetClass, button, args, n);   
	popup=option1weight0Wid;
      }
    }

    paramForm = XtCreateManagedWidget("paramForm", formWidgetClass, popup ,NULL, 0);

	prod_form = XtVaCreateManagedWidget("prod_form", formWidgetClass, 
					    paramForm, 
					NULL);
if (program_option != 3) {
	prod_labl = XtVaCreateManagedWidget("prod_labl",labelWidgetClass, 
					    prod_form,
					     XtNwidth, 280,
					     XtNlabel, "PRODUCT CONSTRAINTS",
					     NULL);


	lprod_leng_min = XtVaCreateManagedWidget("lprod_leng_min",
						 labelWidgetClass, prod_form,
					     XtNwidth, 200,
					     /* XtNjustify, XtJustifyLeft,*/
					     XtNfromVert, prod_labl,
					    NULL);

      } 
else {
	prod_labl = XtVaCreateManagedWidget("prod_labl",labelWidgetClass, 
					    prod_form,
					     XtNwidth, 280,
					     XtNlabel, "PRIMER LOCATION CONSTRAINTS",
					     NULL);
	lprod_leng_min = XtVaCreateManagedWidget("lprod_leng_min",
						 labelWidgetClass, prod_form,
					     XtNwidth, 200,
					     /* XtNjustify, XtJustifyLeft,*/
					     XtNfromVert, prod_labl,
					    NULL);



      }

    n = 0;
    if (weights) 
      XtSetArg(args[n], XtNlabel,     "Length:                    "); 
    else {
      if (program_option==3 || program_option==4)
	XtSetArg(args[n], XtNlabel,   "Length from end(bp):  Min ");
      else
	XtSetArg(args[n], XtNlabel,   "Product length(bp):   Min ");
    }
    n++;
    XtSetValues(lprod_leng_min, args, n);

	prod_leng_min = XtVaCreateManagedWidget("prod_leng_min",
						asciiTextWidgetClass, prod_form,
					     XtNwidth, 70,
					     XtNeditType,  XawtextEdit,
					     XtNfromVert, prod_labl,
					     XtNfromHoriz, lprod_leng_min,
					    NULL);


    if (!weights) {
	lprod_leng_max = XtVaCreateManagedWidget("lprod_leng_max",
						 labelWidgetClass, prod_form,
					     XtNwidth, 200,
	       	                             XtNlabel,  "                    Max",
					     XtNfromVert, prod_leng_min,
					     /* XtNjustify, XtJustifyLeft,*/
						NULL);
	prod_leng_max = XtVaCreateManagedWidget("prod_leng_max",
						asciiTextWidgetClass, prod_form,
					     XtNwidth, 70,
					     XtNeditType,  XawtextEdit,
					     XtNfromVert, prod_leng_min,
					     XtNfromHoriz, lprod_leng_max,
						NULL);
      }


if (program_option != 3) {
	lprod_gc_min = XtVaCreateManagedWidget("lprod_gc_min",
                                               labelWidgetClass, prod_form,
					       XtNwidth, 200,
					       /* XtNjustify, XtJustifyLeft,*/
					    NULL);

    n = 0;
    if (weights) {
      XtSetArg(args[n], XtNfromVert, prod_leng_min); n++;
      XtSetArg(args[n], XtNlabel, "G+C Content:               "); n++;
    }
    else {
      XtSetArg(args[n], XtNfromVert, prod_leng_max); n++;
      XtSetArg(args[n], XtNlabel, "G+C Content(%):       Min "); n++;	
    }
    XtSetValues(lprod_gc_min, args, n);


	prod_gc_min = XtVaCreateManagedWidget("prod_gc_min",
                                               asciiTextWidgetClass, prod_form,
					       XtNwidth, 70,
					       XtNeditType,  XawtextEdit,
					       XtNfromHoriz, lprod_gc_min,
					    NULL);

    n = 0;
    if (weights)
      XtSetArg(args[n], XtNfromVert, prod_leng_min); 
    else
      XtSetArg(args[n], XtNfromVert, prod_leng_max);
    n++;
    XtSetValues(prod_gc_min, args, n);


    if (!weights) {
	lprod_gc_max = XtVaCreateManagedWidget("lprod_gc_max",
					       labelWidgetClass, prod_form,
					       XtNwidth, 200,
					       XtNlabel,"                    Max",
					       XtNfromVert, prod_gc_min,
					       /* XtNjustify, XtJustifyLeft,*/
					    NULL);
	prod_gc_max = XtVaCreateManagedWidget("prod_gc_max",
					      asciiTextWidgetClass, prod_form,
					       XtNwidth,  70,
					       XtNeditType,  XawtextEdit,
					       XtNfromVert, prod_gc_min,
					       XtNfromHoriz, lprod_gc_max,
					    NULL);

      }



	lprod_tm_min = XtVaCreateManagedWidget("lprod_tm_min",
	                                     labelWidgetClass, prod_form,
					     XtNwidth, 200,
					     XtNeditType,  XawtextEdit,
					     /* XtNjustify, XtJustifyLeft,*/
					    NULL);

    n = 0;
    if (weights) {
      XtSetArg(args[n], XtNfromVert, lprod_gc_min); n++;
      XtSetArg(args[n], XtNlabel, "Tm:                        "); n++;
    }
    else {
      XtSetArg(args[n], XtNfromVert, lprod_gc_max); n++;
      XtSetArg(args[n], XtNlabel, "Tm(Celsius):          Min "); n++;	
    }
    XtSetValues(lprod_tm_min, args, n);


	prod_tm_min = XtVaCreateManagedWidget("prod_tm_min",
	                                     asciiTextWidgetClass, prod_form,
					     XtNwidth, 70,
					     XtNeditType,  XawtextEdit,
					     XtNfromHoriz, lprod_tm_min,
					    NULL);

    n = 0;
    if (weights) 
      XtSetArg(args[n], XtNfromVert, prod_gc_min); 
    else 
      XtSetArg(args[n], XtNfromVert, prod_gc_max);
    n++;
    XtSetValues(prod_tm_min, args, n);


      if (!weights) {
	lprod_tm_max = XtVaCreateManagedWidget("lprod_tm_max",
	                                     labelWidgetClass, prod_form,
					     XtNwidth, 200,
	 				     XtNlabel,"                    Max",
					     XtNfromVert, prod_tm_min,
					     /* XtNjustify, XtJustifyLeft,*/
					    NULL);
	prod_tm_max = XtVaCreateManagedWidget("prod_tm_max",
                                             asciiTextWidgetClass, prod_form,
					     XtNwidth, 70,
					     XtNeditType,  XawtextEdit,
					     XtNfromVert, prod_tm_min,
					     XtNfromHoriz, lprod_tm_max,
					    NULL);
      }
      
     }  /* if program_option != 3 */





	prim_form = XtVaCreateManagedWidget("prim_form", 
                                            formWidgetClass, paramForm, 
					    XtNfromHoriz, prod_form,
					    NULL);
        prim_labl = XtVaCreateManagedWidget("prim_labl",
                                            labelWidgetClass, prim_form,
					    XtNlabel, "PRIMER CONSTRAINTS",
					    XtNwidth, 305,
					    NULL);
					    
	lprim_len_min = XtVaCreateManagedWidget("lprim_len_min",
                                               labelWidgetClass, prim_form,
					       XtNwidth, 225,
						/* XtNjustify, XtJustifyLeft,*/
					       XtNfromVert, prim_labl,
					    NULL);


    n = 0;
    if (weights) 
      XtSetArg(args[n], XtNlabel, "Primer 1 Length:               "); 
    else 
      XtSetArg(args[n], XtNlabel, "Length(bases):        Min      ");
    n++;
    XtSetValues(lprim_len_min, args, n);


	prim_len_min = XtVaCreateManagedWidget("prim_len_min",
                                               asciiTextWidgetClass, prim_form,
					       XtNwidth, 70,
					       XtNeditType,  XawtextEdit,
					       XtNfromVert, prim_labl,
					       XtNfromHoriz, lprim_len_min,
					    NULL);
	lprim_len_max = XtVaCreateManagedWidget("lprim_len_max",
                                                labelWidgetClass, prim_form,
					       XtNwidth, 225,
					     /* XtNjustify, XtJustifyLeft,*/
					       XtNfromVert, prim_len_min,
					    NULL);


	prim_len_max = XtVaCreateManagedWidget("prim_len_max",
                                               asciiTextWidgetClass, prim_form,
					       XtNwidth, 70,
					       XtNeditType,  XawtextEdit,
					       XtNfromHoriz, lprim_len_max,
					       XtNfromVert, prim_len_min,
					    NULL);

    n = 0;
    if (weights) {
      if (program_option==3) {
	XtSetArg(args[n], XtNeditType, XawtextRead); n++;
	XtSetArg(args[n], XtNborderWidth, 0); n++;
	XtSetValues(prim_len_max, args, n);
	n=0;
	XtSetArg(args[n], XtNlabel, "---------"); n++;
      }
      else
	XtSetArg(args[n], XtNlabel, "Primer 2 Length:               "); n++;
      XtSetValues(lprim_len_max, args, n);
    }
    else {
      XtSetArg(args[n], XtNlabel,"                      Max      "); n++;
      XtSetValues(lprim_len_max, args, n);
    }

	lprim_gc_min = XtVaCreateManagedWidget("lprim_gc_min",
                                               labelWidgetClass, prim_form,
					       XtNwidth, 225,
					     /* XtNjustify, XtJustifyLeft,*/
					       XtNfromVert, prim_len_max,
					    NULL);
    n = 0;
    if (weights) 
      XtSetArg(args[n], XtNlabel, "Primer 1 G+C:                  "); 
    else 
      XtSetArg(args[n], XtNlabel, "G+C Content(%):       Min      ");
    n++;
    XtSetValues(lprim_gc_min, args, n);

	prim_gc_min = XtVaCreateManagedWidget("prim_gc_min",
                                               asciiTextWidgetClass, prim_form,
					       XtNwidth, 70,
					       XtNeditType,  XawtextEdit,
					       XtNfromVert, prim_len_max,
					       XtNfromHoriz, lprim_gc_min,
					    NULL);
	lprim_gc_max = XtVaCreateManagedWidget("lprim_gc_max",labelWidgetClass, 
					       prim_form,
					       XtNwidth, 225,
					     /* XtNjustify, XtJustifyLeft,*/
					       XtNfromVert, prim_gc_min,
					    NULL);

	prim_gc_max = XtVaCreateManagedWidget("prim_gc_max",
					      asciiTextWidgetClass, prim_form,
					       XtNwidth,  70,
					       XtNeditType,  XawtextEdit,
					       XtNfromVert, prim_gc_min,
					       XtNfromHoriz, lprim_gc_max,
					    NULL);
    n = 0;
    if (weights) {
      if (program_option==3) {
	XtSetArg(args[n], XtNeditType, XawtextRead); n++;
	XtSetArg(args[n], XtNborderWidth, 0); n++;
	XtSetValues(prim_gc_max, args, n);
	n=0;
	XtSetArg(args[n], XtNlabel, "---------"); n++;
      }
      else
	XtSetArg(args[n], XtNlabel, "Primer 2 G+C:                  "); n++;
      XtSetValues(lprim_gc_max, args, n);
    }
    else {
      XtSetArg(args[n], XtNlabel, "                      Max      ");    n++;
      XtSetValues(lprim_gc_max, args, n);
    }


        lprim_tm_min = XtVaCreateManagedWidget("lprim_tm_min",
					       labelWidgetClass,prim_form,
                                             XtNwidth, 225,
					     /* XtNjustify, XtJustifyLeft,*/
                                             XtNeditType,  XawtextEdit,
                                             XtNfromVert, lprim_gc_max,
                                            NULL);

    n = 0;
    if (weights) 
      XtSetArg(args[n], XtNlabel, "Primer 1 Tm:                   "); 
    else 
      XtSetArg(args[n], XtNlabel,"Tm(Celsius):          Min      ");
    n++;
    XtSetValues(lprim_tm_min, args, n);


       prim_tm_min = XtVaCreateManagedWidget("prim_tm_min",asciiTextWidgetClass, prim_form,
                                             XtNwidth, 70,
                                             XtNeditType,  XawtextEdit,
                                             XtNfromHoriz, lprim_tm_min,
                                             XtNfromVert, prim_gc_max,
                                            NULL);


        lprim_tm_max = XtVaCreateManagedWidget("lprim_tm_max",labelWidgetClass,prim_form,
                                             XtNwidth, 225,
					     /* XtNjustify, XtJustifyLeft,*/
                                             XtNfromVert, lprim_tm_min,
                                            NULL);
        prim_tm_max = XtVaCreateManagedWidget("prim_tm_max",asciiTextWidgetClass, prim_form,
                                             XtNwidth, 70,
                                             XtNeditType,  XawtextEdit,
                                             XtNfromVert, prim_tm_min,
                                             XtNfromHoriz, lprim_tm_max,
                                            NULL);
    n = 0;
    if (weights) {
      if (program_option==3) {
	XtSetArg(args[n], XtNeditType, XawtextRead); n++;
	XtSetArg(args[n], XtNborderWidth, 0); n++;
	XtSetValues(prim_tm_max, args, n); 
        n=0;
	XtSetArg(args[n], XtNlabel, "---------"); n++;
      }
      else 
	XtSetArg(args[n], XtNlabel, "Primer 2 Tm:                   "); n++;
      XtSetValues(lprim_tm_max, args, n); 
    }
    else {
      XtSetArg(args[n], XtNlabel,"                      Max      ");  n++;
      XtSetValues(lprim_tm_max, args, n); 
    }


if (program_option != 3) {

       ldelta_tm = XtVaCreateManagedWidget("ldelta_tm",labelWidgetClass, prim_form,
					       XtNwidth, 225,
					       XtNjustify, XtJustifyLeft,
					       XtNlabel, "Difference in Tm cutoff:   ",
					       XtNfromVert, prim_tm_max,
					    NULL);
       delta_tm = XtVaCreateManagedWidget("delta_tm",asciiTextWidgetClass, prim_form,
					       XtNwidth, 70,
					       XtNeditType,  XawtextEdit,
					       XtNfromVert, prim_tm_max,
					       XtNfromHoriz, ldelta_tm,	
					    NULL);
     }


if (!weights) {

       lprim_nucs = XtVaCreateManagedWidget("lprim_nucs",
					    labelWidgetClass, prim_form,
					       XtNwidth, 225,
					       XtNjustify, XtJustifyLeft,
                                   XtNlabel, "3' Nucleotides (S=G/C,W=A/T):",
					       XtNfromVert, primotherIWid,
					    NULL);

    n = 0;
    if (program_option==3) 
      XtSetArg(args[n], XtNfromVert, lprim_tm_max); 
    else 
      XtSetArg(args[n], XtNfromVert, ldelta_tm); 
    n++;
    XtSetValues(lprim_nucs, args, n);

       prim_nucs = XtVaCreateManagedWidget("prim_nucs",asciiTextWidgetClass, prim_form,
					       XtNwidth, 70,
					       XtNeditType,  XawtextEdit,
					       XtNfromHoriz, lprim_nucs,
					    NULL);
    n = 0;
    if (program_option==3) 
      XtSetArg(args[n], XtNfromVert, prim_tm_max); 
    else 
      XtSetArg(args[n], XtNfromVert, delta_tm); 
    n++;
    XtSetValues(prim_nucs, args, n);
     }

    /* form widget to hold all annealing constraints information */
	ann_form = XtVaCreateManagedWidget("ann_form", 
                                            formWidgetClass, paramForm, 
					    XtNfromHoriz, prim_form,
					    NULL);
        ann_labl = XtVaCreateManagedWidget("ann_labl",
                                            labelWidgetClass, ann_form,
					    XtNlabel, "ANNEALING CONSTRAINTS",
					    XtNwidth, 250,
					    NULL);
					    


    /* big annealing homology title */
	lann_hmlg = XtVaCreateManagedWidget("lann_hmlg",
					    labelWidgetClass, ann_form,
					       XtNwidth, 250,
					       XtNlabel, "Annealing Scores:  Internal  3'",
					       XtNfromVert, prim_labl,
					     NULL);
    /* label for primer-self annealing*/
    lprimselfWid  = XtVaCreateManagedWidget("lprimself",
					       labelWidgetClass, ann_form,
					       XtNwidth, 150,
					       XtNlabel, "Primer-self",
					       XtNfromVert, lann_hmlg,
					     NULL);
    if (program_option!=3) {
      /*These are only needed when looking for primer pairs*/
      /* primer-primer annealing label*/
	lprimprimWid = XtVaCreateManagedWidget("lprimprim",
						labelWidgetClass, ann_form,
					       XtNwidth, 150,
					       XtNlabel, "Primer-primer",
					       XtNfromVert, lprimselfWid,
					     NULL);

      /* primer-product annealing label*/	
	lprimprodWid = XtVaCreateManagedWidget("lprimprod",
					      labelWidgetClass, ann_form,
					       XtNwidth, 150,
					       XtNlabel, "Primer-product",
					       XtNfromVert, lprimprimWid,
					    NULL);
      } /*if program_option != 3*/

    /* label for primer-other sequence annealing, other-sequence includes
     rest of sequence, other than primer*/
	lprimotherWid = XtVaCreateManagedWidget("lprimother",
						labelWidgetClass, ann_form,
					       XtNwidth, 150,
					       XtNlabel, "Primer-other",
					     NULL);

    if (program_option==3) {
        n = 0;
        XtSetArg(args[n], XtNfromVert, lprimselfWid);  n++;
        XtSetValues(lprimotherWid, args, n);
      } 
    else {
        n = 0;
        XtSetArg(args[n], XtNfromVert, lprimprodWid);  n++;
        XtSetValues(lprimotherWid, args, n);
      }

/* primer-self annealing, internal */
	ann_hmlg_i = XtVaCreateManagedWidget("ann_hmlg_i",
                                         asciiTextWidgetClass, ann_form,
					       XtNwidth, 45,
					       XtNeditType,  XawtextEdit,
					       XtNfromVert, lann_hmlg,
                                               XtNfromHoriz, lprimselfWid,
					     NULL);

if (program_option != 3) {
  /*primer-primer internal annealing homology*/
	ppann_hmlg_i = XtVaCreateManagedWidget("ppann_hmlg_i",
                                         asciiTextWidgetClass, ann_form,
					       XtNwidth, 45,
					       XtNeditType,  XawtextEdit,
					       XtNfromVert, ann_hmlg_i,
                                               XtNfromHoriz, lprimprimWid,
					     NULL);
/* primer-primer three prime annealing homology*/
	ppann_hmlg_t = XtVaCreateManagedWidget("ppann_hmlg_t",
                                   asciiTextWidgetClass, ann_form,
					       XtNwidth, 45,
					       XtNeditType,  XawtextEdit,
					       XtNfromVert, ann_hmlg_i,
                                               XtNfromHoriz, ppann_hmlg_i,
					    NULL);
      }

/* primer-self annealing, three-prime*/
	ann_hmlg_t = XtVaCreateManagedWidget("ann_hmlg_t",
                               asciiTextWidgetClass, ann_form,
					       XtNwidth, 45,
					       XtNeditType,  XawtextEdit,
					       XtNfromVert, lann_hmlg,
					     XtNfromHoriz, ann_hmlg_i,
					    NULL);



    if (program_option != 3) {
      /*only look at these product annealings if looking for primer-pairs*/
    /* primer vs product, internal annealing */
	primprodIWid = XtVaCreateManagedWidget("primprodI",
                                              asciiTextWidgetClass, ann_form,
					       XtNwidth, 45,
					       XtNeditType,  XawtextEdit,
					       XtNfromVert, ppann_hmlg_i,
					       XtNfromHoriz, lprimprodWid,
					    NULL);


    /* primer-product, three-prime annealing */
	primprod3Wid = XtVaCreateManagedWidget("primprod3",
                                              asciiTextWidgetClass, ann_form,
					       XtNwidth, 45,
					       XtNeditType,  XawtextEdit,
					       XtNfromVert, ppann_hmlg_t,
					       XtNfromHoriz, primprodIWid,
					    NULL);
      }
    /* primer vs other sequence, internal annealing */
	primotherIWid = XtVaCreateManagedWidget("primotherI",
						asciiTextWidgetClass, ann_form,
					       XtNwidth, 45,
					       XtNeditType,  XawtextEdit,
					       XtNfromHoriz, lprimotherWid,
					    NULL);
    /* primer-other sequence, three-prime annealing */
	primother3Wid = XtVaCreateManagedWidget("primother3",
						asciiTextWidgetClass, ann_form,
					       XtNwidth, 45,
					       XtNeditType,  XawtextEdit,
					       XtNfromHoriz, primotherIWid,
					    NULL);


    if (program_option==3) {
        n = 0;
        XtSetArg(args[n], XtNfromVert, ann_hmlg_i);  n++;
        XtSetValues(primotherIWid, args, n);
        n = 0;
        XtSetArg(args[n], XtNfromVert, ann_hmlg_t);  n++;
        XtSetValues(primother3Wid, args, n);
      }
    else {
        n = 0;
        XtSetArg(args[n], XtNfromVert, primprodIWid);  n++;
        XtSetValues(primotherIWid, args, n);
        n = 0;
        XtSetArg(args[n], XtNfromVert, primprod3Wid);  n++;
        XtSetValues(primother3Wid, args, n);
      }


if (!weights) {
       lATWeightWid =  XtVaCreateManagedWidget("lATWeight",
					       labelWidgetClass, ann_form,
					       XtNwidth, 150,
					       XtNlabel, "A<->T Score",
					       XtNeditType,  XawtextEdit,
					       XtNfromVert, lprimotherWid,
					    NULL);

       ATWeightWid =  XtVaCreateManagedWidget("ATWeight",
					       asciiTextWidgetClass, ann_form,
					       XtNwidth, 45,
					       XtNeditType,  XawtextEdit,
					       XtNfromVert, primother3Wid,
					       XtNfromHoriz, lATWeightWid,
					    NULL);

       lCGWeightWid =  XtVaCreateManagedWidget("lCGWeight",
					       labelWidgetClass, ann_form,
					       XtNwidth, 150,
					       XtNlabel, "C<->G Score",
					       XtNeditType,  XawtextEdit,
					       XtNfromVert, lATWeightWid,
					    NULL);

       CGWeightWid =  XtVaCreateManagedWidget("CGWeight",
					       asciiTextWidgetClass, ann_form,
					       XtNwidth, 45,
					       XtNeditType,  XawtextEdit,
					       XtNfromVert, ATWeightWid,
					       XtNfromHoriz, lCGWeightWid,
					    NULL);

       lWtAmbigWid =  XtVaCreateManagedWidget("lWtAmbig",
					       labelWidgetClass, ann_form,
					       XtNwidth, 150,
					       XtNlabel, "Wt Ambig",
					       XtNfromVert, lCGWeightWid,
					    NULL);

       WtAmbigWid =  XtVaCreateManagedWidget("WtAmbig",
					       commandWidgetClass, ann_form,
					       XtNwidth, 49,
					       XtNfromVert, CGWeightWid,
					       XtNfromHoriz, lWtAmbigWid,
					    NULL);
     }


        confirmWid = XtVaCreateManagedWidget("confirm", commandWidgetClass,paramForm,
					 XtNfromVert, prim_form,
                                         XtNlabel, "CONFIRM",
                                      NULL);
        cancelWid = XtVaCreateManagedWidget("cancel", 
					 commandWidgetClass,paramForm,
					 XtNfromHoriz, confirmWid,
					 XtNfromVert, prim_form,
                                      NULL);

        inputDefWid = XtVaCreateManagedWidget("inputDef", commandWidgetClass,paramForm,
					 XtNfromVert, prim_form,
					 XtNfromHoriz, cancelWid,
					 XtNborderColor, XtDefaultBackground,
                                         XtNlabel, "INPUT CONSTRAINTS FROM FILE:",
                                      NULL);

        saveDefWid = XtVaCreateManagedWidget("saveDef", commandWidgetClass,paramForm,
					 XtNfromVert, prim_form,
					 XtNfromHoriz, inputDefWid,
					 XtNborderColor, XtDefaultBackground,
                                         XtNlabel, "SAVE CONSTRAINTS TO FILE:",
                                      NULL);

        defFnWid = XtVaCreateManagedWidget("defFn", 
					 asciiTextWidgetClass,paramForm,
				         XtNeditType, XawtextEdit,
					 XtNfromVert, prim_form,
					 XtNfromHoriz, saveDefWid,
					 XtNwidth, 250,
					 XtNborderColor, XtDefaultBackground,
                                      NULL);

    str = (char *)our_alloc(MAX_NAME_SIZE*sizeof(char));
    if (strlen(seq_filename)>(MAX_NAME_SIZE-4)) {
      message("Sequence Filename longer than MAX_NAME_SIZE\n");
      popUpErrorMessage();
    }
    sprintf(str,"%s.def",seq_filename);
    n=0;
    XtSetArg(arg[n], XtNstring, str); n++;
    XtSetValues(defFnWid,arg,n);



/* weights lets me know whether the widget that was clicked on 
   was for changing the actual parameters or changing the weights that
   are attached to the parameters, weights=0 then parameters if weights=1
   then its the weights I'm changing */

   our_free(str);


    XtAddCallback(saveDefWid, XtNcallback, GetParams, (XtPointer)popup);
    XtAddCallback(saveDefWid, XtNcallback, check_param_validity, NULL);
    XtAddCallback(saveDefWid, XtNcallback, SaveParams, (XtPointer)defFnWid);
    XtAddCallback(saveDefWid, XtNcallback, popDownPopupPrompt,(XtPointer)paramForm);
    XtAddCallback(inputDefWid, XtNcallback, inputParams, (XtPointer)defFnWid);
    XtAddCallback(inputDefWid, XtNcallback, updateOtherSeqFn, NULL);
    XtAddCallback(inputDefWid, XtNcallback, popDownPopupPrompt,(XtPointer)paramForm);
    XtAddCallback(inputDefWid, XtNcallback, check_param_validity, NULL);
    if (!weights)
      XtAddCallback(WtAmbigWid, XtNcallback, changeWtAmbig, (XtPointer)WtAmbigWid);

    XtAddCallback(confirmWid, XtNcallback, GetParams, (XtPointer)popup);
    XtAddCallback(confirmWid, XtNcallback, check_param_validity, NULL);
    XtAddCallback(confirmWid, XtNcallback, popDownPopupPrompt, (XtPointer)paramForm);
    XtAddCallback(cancelWid, XtNcallback, popDownPopupPrompt, (XtPointer)paramForm);

    
}



/* writes the values to the changeParamsPopup*/
void set_text_def(w)
Widget w;
/* if weights == 1, then we are changing the parameter weights,
   otherwise we are changing the parameters themselves */

/* initializes and/or refreshes the change parameter input windows*/
{ Arg arg[15];
  int n;
  char *temp;
  float gc;
  Widget paramForm,primForm,prodForm,annForm;
  Widget prod_leng_max,prod_leng_min;
  Widget prod_gc_min,prod_gc_max;
  Widget prod_tm_min,prod_tm_max;
  Widget prim_len_min,prim_len_max;
  Widget prim_gc_min,prim_gc_max;
  Widget prim_tm_min,prim_tm_max;
  Widget delta_tm;
  Widget prim_nucs;
  Widget ann_hmlg_i,ann_hmlg_t; /* primer-self */
  Widget ppann_hmlg_i, ppann_hmlg_t; /* primer-primer */
  Widget primotherIWid,primother3Wid;
  Widget primprodIWid, primprod3Wid;
  Widget ATWeightWid,CGWeightWid,WtAmbigWid; 


  /* get the names of all of the children of the current
     parameter popup */

  paramForm = XtNameToWidget(w,"paramForm");
  primForm = XtNameToWidget(paramForm,"prim_form");
  prodForm = XtNameToWidget(paramForm,"prod_form");
  annForm = XtNameToWidget(paramForm,"ann_form");
  prod_leng_min = XtNameToWidget(prodForm,"prod_leng_min");
  prod_leng_max = XtNameToWidget(prodForm,"prod_leng_max");
  prod_gc_min = XtNameToWidget(prodForm,"prod_gc_min");
  prod_gc_max = XtNameToWidget(prodForm,"prod_gc_max");
  prod_tm_min = XtNameToWidget(prodForm,"prod_tm_min");
  prod_tm_max = XtNameToWidget(prodForm,"prod_tm_max");
  prim_len_min = XtNameToWidget(primForm,"prim_len_min");
  prim_len_max = XtNameToWidget(primForm,"prim_len_max");
  prim_gc_min = XtNameToWidget(primForm,"prim_gc_min");
  prim_gc_max = XtNameToWidget(primForm,"prim_gc_max");
  prim_tm_min = XtNameToWidget(primForm,"prim_tm_min");
  prim_tm_max = XtNameToWidget(primForm,"prim_tm_max");
  delta_tm = XtNameToWidget(primForm,"delta_tm");
  prim_nucs = XtNameToWidget(primForm,"prim_nucs");
  ann_hmlg_i = XtNameToWidget(annForm,"ann_hmlg_i");
  ann_hmlg_t = XtNameToWidget(annForm,"ann_hmlg_t");
  ppann_hmlg_i = XtNameToWidget(annForm,"ppann_hmlg_i");
  ppann_hmlg_t = XtNameToWidget(annForm,"ppann_hmlg_t");
  primotherIWid = XtNameToWidget(annForm,"primotherI");
  primother3Wid = XtNameToWidget(annForm,"primother3");
  primprodIWid = XtNameToWidget(annForm,"primprodI");
  primprod3Wid = XtNameToWidget(annForm,"primprod3");
  ATWeightWid =  XtNameToWidget(annForm,"ATWeight");
  CGWeightWid =  XtNameToWidget(annForm,"CGWeight");
  WtAmbigWid =  XtNameToWidget(annForm,"WtAmbig");
  



  temp = (char *)our_alloc(MAX_WORD_SIZE * sizeof(char));

  

  if (weights)
  sprintf(temp,"%5.2f",prm.wt_prod_len);
  else
  sprintf(temp,"%d",prm.prod_len_low);
  n=0;
  XtSetArg(arg[n],XtNstring,temp); n++;
  XtSetValues(prod_leng_min,arg,n);
  n=0;
  XtSetArg(arg[n],XtNinsertPosition, strlen(temp)); n++;
  XtSetValues(prod_leng_min,arg,n);


  if (!weights) {
  sprintf(temp,"%d",prm.prod_len_high);
  n=0;
  XtSetArg(arg[n],XtNstring,temp); n++;
  XtSetValues(prod_leng_max,arg,n);
  n=0;
  XtSetArg(arg[n],XtNinsertPosition, strlen(temp)); n++;
  XtSetValues(prod_leng_max,arg,n);
  }

  
  if (program_option != 3) {

  if (weights) {
    gc = prm.wt_prod_gc;
    sprintf(temp,"%5.2f",gc);
  }
  else {
    if (prm.prod_gc_low < 1.0) gc = prm.prod_gc_low * 100.0;
    else gc = prm.prod_gc_low;
    sprintf(temp,"%2.0f",gc);
  }
  n=0;
  XtSetArg(arg[n],XtNstring,temp); n++;
  XtSetValues(prod_gc_min,arg,n);
  n=0;
  XtSetArg(arg[n],XtNinsertPosition, strlen(temp)); n++;
  XtSetValues(prod_gc_min,arg,n);


  if (!weights)
  {
    if (prm.prod_gc_high < 1.0) gc = prm.prod_gc_high * 100.0;
    else gc = prm.prod_gc_high;
  sprintf(temp,"%2.0f",gc);
  n=0;
  XtSetArg(arg[n],XtNstring,temp); n++;
  XtSetValues(prod_gc_max,arg,n);
  n=0;
  XtSetArg(arg[n],XtNinsertPosition, strlen(temp)); n++;
  XtSetValues(prod_gc_max,arg,n);
  }


  if (weights)
    sprintf(temp,"%5.2f",prm.wt_prod_tm);
  else
    sprintf(temp,"%4.1f",prm.prod_tm_low);
  n=0;
  XtSetArg(arg[n],XtNstring,temp); n++;
  XtSetValues(prod_tm_min,arg,n);
  n=0;
  XtSetArg(arg[n],XtNinsertPosition, strlen(temp)); n++;
  XtSetValues(prod_tm_min,arg,n);

  if (!weights) {
  sprintf(temp,"%4.1f",prm.prod_tm_high);
  n=0;
  XtSetArg(arg[n],XtNstring,temp); n++;
  XtSetValues(prod_tm_max,arg,n);
  n=0;
  XtSetArg(arg[n],XtNinsertPosition, strlen(temp)); n++;
  XtSetValues(prod_tm_max,arg,n);
 }
} 
  else {
    if (weights) {
      prm.wt_prod_tm=0.0;
    }
    else  {
      prm.prod_tm_low=0.0;prm.prod_tm_high=0.0;
    }
  }

  if (weights)
    sprintf(temp,"%5.2f",prm.wt_prim_s_len);
  else
    sprintf(temp,"%d",prm.min_prim_len);
  n=0;
  XtSetArg(arg[n],XtNstring,temp); n++;
  XtSetValues(prim_len_min,arg,n);
  n=0;
  XtSetArg(arg[n],XtNinsertPosition, strlen(temp)); n++;
  XtSetValues(prim_len_min,arg,n);

  if (weights){
    if (program_option==3)
      sprintf(temp," ");
    else
      sprintf(temp,"%5.2f",prm.wt_prim_a_len);
  }
  else
    sprintf(temp,"%d",prm.max_prim_len);
  n=0;
  XtSetArg(arg[n],XtNstring,temp); n++;
  XtSetValues(prim_len_max,arg,n);
  n=0;
  XtSetArg(arg[n],XtNinsertPosition, strlen(temp)); n++;
  XtSetValues(prim_len_max,arg,n);

  if (weights) {
    gc = prm.wt_prim_s_gc;
    sprintf(temp,"%5.2f",gc);
  }
  else {
    if (prm.prim_gc_low < 1.0) gc = prm.prim_gc_low * 100.0;
    else gc = prm.prim_gc_low;
    sprintf(temp,"%2.0f",gc);
  }
  n=0;
  XtSetArg(arg[n],XtNstring,temp); n++;
  XtSetValues(prim_gc_min,arg,n);
  n=0;
  XtSetArg(arg[n],XtNinsertPosition, strlen(temp)); n++;
  XtSetValues(prim_gc_min,arg,n);


  if (weights) {
    if (program_option==3) 
      sprintf(temp," ");
    else {
      gc=prm.wt_prim_a_gc;
      sprintf(temp,"%5.2f",gc);
    }
  }
  else {
    if (prm.prim_gc_high < 1.0) gc = prm.prim_gc_high * 100.0;
    else gc = prm.prim_gc_high;
    sprintf(temp,"%2.0f",gc);
  }
  n=0;
  XtSetArg(arg[n],XtNstring,temp); n++;
  XtSetValues(prim_gc_max,arg,n);
  n=0;
  XtSetArg(arg[n],XtNinsertPosition, strlen(temp)); n++;
  XtSetValues(prim_gc_max,arg,n);

  if (weights)
    sprintf(temp,"%5.2f",prm.wt_selfI_hmlg_cut);
  else
    sprintf(temp,"%4.1f",prm.selfI_hmlg_cut);
  n=0;
  XtSetArg(arg[n],XtNstring,temp); n++;
  XtSetValues(ann_hmlg_i,arg,n);
  n=0;
  XtSetArg(arg[n],XtNinsertPosition, strlen(temp)); n++;
  XtSetValues(ann_hmlg_i,arg,n);

  if (weights)
    sprintf(temp,"%5.2f",prm.wt_self3_hmlg_cut);
  else
    sprintf(temp,"%4.1f",prm.self3_hmlg_cut);
  n=0;
  XtSetArg(arg[n],XtNstring,temp); n++;
  XtSetValues(ann_hmlg_t,arg,n);
  n=0;
  XtSetArg(arg[n],XtNinsertPosition, strlen(temp)); n++;
  XtSetValues(ann_hmlg_t,arg,n);


if (program_option != 3) {
  if (weights)
    sprintf(temp,"%5.2f",prm.wt_ppI_hmlg_cut);
  else
    sprintf(temp,"%4.1f",prm.ppI_hmlg_cut);
  n=0;
  XtSetArg(arg[n],XtNstring,temp); n++;
  XtSetValues(ppann_hmlg_i,arg,n);
  n=0;
  XtSetArg(arg[n],XtNinsertPosition, strlen(temp)); n++;
  XtSetValues(ppann_hmlg_i,arg,n);

  if (weights)
    sprintf(temp,"%5.2f",prm.wt_pp3_hmlg_cut);
  else
    sprintf(temp,"%4.1f",prm.pp3_hmlg_cut);
  n=0;
  XtSetArg(arg[n],XtNstring,temp); n++;
  XtSetValues(ppann_hmlg_t,arg,n);
  n=0;
  XtSetArg(arg[n],XtNinsertPosition, strlen(temp)); n++;
  XtSetValues(ppann_hmlg_t,arg,n);


  if (weights)
    sprintf(temp,"%5.2f",prm.wt_primprodI_hmlg_cut);
  else
    sprintf(temp,"%4.1f",prm.primprodI_hmlg_cut);
  n=0;
  XtSetArg(arg[n],XtNstring,temp); n++;
  XtSetValues(primprodIWid,arg,n);
  n=0;
  XtSetArg(arg[n],XtNinsertPosition, strlen(temp)); n++;
  XtSetValues(primprodIWid,arg,n);

  if (weights)
    sprintf(temp,"%5.2f",prm.wt_primprod3_hmlg_cut);
  else
    sprintf(temp,"%4.1f",prm.primprod3_hmlg_cut);
  n=0;
  XtSetArg(arg[n],XtNstring,temp); n++;
  XtSetValues(primprod3Wid,arg,n);
  n=0;
  XtSetArg(arg[n],XtNinsertPosition, strlen(temp)); n++;
  XtSetValues(primprod3Wid,arg,n);
}

  if (weights)
    sprintf(temp,"%5.2f",prm.wt_primotherI_hmlg_cut);
  else
    sprintf(temp,"%4.1f",prm.primotherI_hmlg_cut);
  n=0;
  XtSetArg(arg[n],XtNstring,temp); n++;
  XtSetValues(primotherIWid,arg,n);
  n=0;
  XtSetArg(arg[n],XtNinsertPosition, strlen(temp)); n++;
  XtSetValues(primotherIWid,arg,n);

  if (weights)
    sprintf(temp,"%5.2f",prm.wt_primother3_hmlg_cut);
  else
    sprintf(temp,"%4.1f",prm.primother3_hmlg_cut);
  n=0;
  XtSetArg(arg[n],XtNstring,temp); n++;
  XtSetValues(primother3Wid,arg,n);
  n=0;
  XtSetArg(arg[n],XtNinsertPosition, strlen(temp)); n++;
  XtSetValues(primother3Wid,arg,n);


  if (!weights) {
    sprintf(temp,"%s",prm.end_nucs);
    n=0;
    XtSetArg(arg[n],XtNstring,temp); n++;
    XtSetValues(prim_nucs,arg,n);
    n=0;
    XtSetArg(arg[n],XtNinsertPosition, strlen(temp)); n++;
    XtSetValues(prim_nucs,arg,n);
  }

  
  if (weights)
    sprintf(temp,"%5.2f",prm.wt_prim_s_tm);
  else
    sprintf(temp,"%4.1f",prm.prim_tm_low);
  n=0;
  XtSetArg(arg[n],XtNstring,temp); n++;
  XtSetValues(prim_tm_min,arg,n);
  n=0;
  XtSetArg(arg[n],XtNinsertPosition, strlen(temp)); n++;
  XtSetValues(prim_tm_min,arg,n);

  if (weights) {
    if (program_option==3) 
      sprintf(temp," ");
   else
    sprintf(temp,"%5.2f",prm.wt_prim_a_tm);
  }
  else
    sprintf(temp,"%4.1f",prm.prim_tm_high);
  n=0;
  XtSetArg(arg[n],XtNstring,temp); n++;
  XtSetValues(prim_tm_max,arg,n);
  n=0;
  XtSetArg(arg[n],XtNinsertPosition, strlen(temp)); n++;
  XtSetValues(prim_tm_max,arg,n);


if (program_option != 3) {
  if (weights)
    sprintf(temp,"%5.2f",prm.wt_delta_tm_cut);
  else
    sprintf(temp,"%4.1f",prm.delta_tm_cut);
  n=0;
  XtSetArg(arg[n],XtNstring,temp); n++;
  XtSetValues(delta_tm,arg,n);
  n=0;
  XtSetArg(arg[n],XtNinsertPosition, strlen(temp)); n++;
  XtSetValues(delta_tm,arg,n);
}
else {
  if (weights)
    prm.wt_delta_tm_cut = 0;  
  else
    prm.delta_tm_cut = 0;  
}


  if (!weights) {
  sprintf(temp,"%4.1f",prm.AT_score);
  n=0;
  XtSetArg(arg[n],XtNstring,temp); n++;
  XtSetValues(ATWeightWid,arg,n);
  n=0;
  XtSetArg(arg[n],XtNinsertPosition, strlen(temp)); n++;
  XtSetValues(ATWeightWid,arg,n);
  }

  if (!weights) {
  sprintf(temp,"%4.1f",prm.CG_score);
  n=0;
  XtSetArg(arg[n],XtNstring,temp); n++;
  XtSetValues(CGWeightWid,arg,n);
  n=0;
  XtSetArg(arg[n],XtNinsertPosition, strlen(temp)); n++;
  XtSetValues(CGWeightWid,arg,n);
  }

  if (!weights) {
  sprintf(temp,"%s",prm.wt_ambig);
  n=0;
  XtSetArg(arg[n],XtNlabel,temp); n++;
  XtSetValues(WtAmbigWid,arg,n);
  }

  /* if a parameter equals zero, it 
     is interpreted as not being considered as a parameter -i.e.
     they do not care about a particular parameter */

  our_free(temp);
  return;
 }                                 


void GetParams(w,client_data,call_data)
Widget w;
XtPointer client_data;
XtPointer call_data;
    {
      Widget paramFormParent = (Widget) client_data;
      Arg args[15];
      String str;
      int k,kk;
      int temp_int;
      float temp_float;
      int stp,endp;
/*      int o_stp1,o_endp1,o_stp2,o_endp2;
      int endseq; /* last sequence indices */

  Widget paramForm,primForm,prodForm,annForm;
  Widget prod_leng_max,prod_leng_min;
  Widget prod_gc_min,prod_gc_max;
  Widget prod_tm_min,prod_tm_max;
  Widget prim_len_min,prim_len_max;
  Widget prim_gc_min,prim_gc_max;
  Widget prim_tm_min,prim_tm_max;
  Widget delta_tm;
  Widget prim_nucs;
  Widget ann_hmlg_i,ann_hmlg_t; /* primer-self */
  Widget ppann_hmlg_i, ppann_hmlg_t; /* primer-primer */
  Widget primotherIWid,primother3Wid;
  Widget primprodIWid,primprod3Wid;
  Widget ATWeightWid,CGWeightWid,WtAmbigWid; 


  /* get the names of all of the children of the current
     parameter popup */

  paramForm = XtNameToWidget(paramFormParent,"paramForm");
  primForm = XtNameToWidget(paramForm,"prim_form");
  prodForm = XtNameToWidget(paramForm,"prod_form");
  annForm = XtNameToWidget(paramForm,"ann_form");
  prod_leng_min = XtNameToWidget(prodForm,"prod_leng_min");
  prod_leng_max = XtNameToWidget(prodForm,"prod_leng_max");
  prod_gc_min = XtNameToWidget(prodForm,"prod_gc_min");
  prod_gc_max = XtNameToWidget(prodForm,"prod_gc_max");
  prod_tm_min = XtNameToWidget(prodForm,"prod_tm_min");
  prod_tm_max = XtNameToWidget(prodForm,"prod_tm_max");
  prim_len_min = XtNameToWidget(primForm,"prim_len_min");
  prim_len_max = XtNameToWidget(primForm,"prim_len_max");
  prim_gc_min = XtNameToWidget(primForm,"prim_gc_min");
  prim_gc_max = XtNameToWidget(primForm,"prim_gc_max");
  prim_tm_min = XtNameToWidget(primForm,"prim_tm_min");
  prim_tm_max = XtNameToWidget(primForm,"prim_tm_max");
  delta_tm = XtNameToWidget(primForm,"delta_tm");
  prim_nucs = XtNameToWidget(primForm,"prim_nucs");
  ann_hmlg_i = XtNameToWidget(annForm,"ann_hmlg_i");
  ann_hmlg_t = XtNameToWidget(annForm,"ann_hmlg_t");
  ppann_hmlg_i = XtNameToWidget(annForm,"ppann_hmlg_i");
  ppann_hmlg_t = XtNameToWidget(annForm,"ppann_hmlg_t");
  primotherIWid = XtNameToWidget(annForm,"primotherI");
  primother3Wid = XtNameToWidget(annForm,"primother3");
  primprodIWid = XtNameToWidget(annForm,"primprodI");
  primprod3Wid = XtNameToWidget(annForm,"primprod3");
  ATWeightWid =  XtNameToWidget(annForm,"ATWeight");
  CGWeightWid =  XtNameToWidget(annForm,"CGWeight");
  WtAmbigWid =  XtNameToWidget(annForm,"WtAmbig");
  



      XtSetArg(args[0],XtNstring, &str);
      XtGetValues(prod_leng_min, args, ONE);
      if (weights)
	sscanf(str,"%f",&prm.wt_prod_len);
      else
	sscanf(str,"%d",&prm.prod_len_low);

      if (!weights) {
      XtSetArg(args[0],XtNstring, &str);
      XtGetValues(prod_leng_max, args, ONE);
      sscanf(str,"%d",&prm.prod_len_high);
      }

      if (program_option!=3) {

      XtSetArg(args[0],XtNstring, &str);
      XtGetValues(prod_gc_min, args, ONE);
	sscanf(str,"%f",&temp_float);      
      if (weights)
	prm.wt_prod_gc = temp_float;
      else
	prm.prod_gc_low = temp_float/100.0;

      if (!weights) {
       XtSetArg(args[0],XtNstring, &str);
       XtGetValues(prod_gc_max, args, ONE);
       sscanf(str,"%f",&temp_float);  
       prm.prod_gc_high = temp_float/100.0;
      }

	XtSetArg(args[0],XtNstring, &str);
	XtGetValues(prod_tm_min, args, ONE);
	sscanf(str,"%f",&temp_float);
	if (weights)
	  prm.wt_prod_tm = temp_float;
	else
	  prm.prod_tm_low = temp_float;
	
      if (!weights) {
	XtSetArg(args[0],XtNstring, &str);
	XtGetValues(prod_tm_max, args, ONE);
	sscanf(str,"%f",&temp_float);
        prm.prod_tm_high = temp_float; 
      }
    } /* if (program_option!=3) */
      else {
	prm.prod_gc_low=0;
	prm.wt_prod_gc=0;
	prm.prod_gc_high=0;
	prm.prod_tm_low=0;
	prm.wt_prod_tm=0;
	prm.prod_tm_high=0;
      }

      XtSetArg(args[0],XtNstring, &str);
      XtGetValues(prim_len_min, args, ONE);
      if (weights)
	sscanf(str,"%f",&prm.wt_prim_s_len);
      else
	sscanf(str,"%d",&prm.min_prim_len);

      XtSetArg(args[0],XtNstring, &str);
      XtGetValues(prim_len_max, args, ONE);
      if (weights) {
	if (program_option!=3)
	  sscanf(str,"%f",&prm.wt_prim_a_len);
      }
      else
	sscanf(str,"%d",&prm.max_prim_len);


      XtSetArg(args[0],XtNstring, &str);
      XtGetValues(prim_gc_min, args, ONE);
      sscanf(str,"%f",&temp_float);      
      if (weights)
	prm.wt_prim_s_gc = temp_float;
      else
	prm.prim_gc_low = temp_float/100.0;

      XtSetArg(args[0],XtNstring, &str);
      XtGetValues(prim_gc_max, args, ONE);
      sscanf(str,"%f",&temp_float);      
      if (weights) {
	if (program_option!=3)
	  prm.wt_prim_a_gc = temp_float;
      }
      else 
	prm.prim_gc_high = temp_float/100.0;

      XtSetArg(args[0],XtNstring, &str);
      XtGetValues(ann_hmlg_i, args, ONE);
      if (weights)
	sscanf(str,"%f",&prm.wt_selfI_hmlg_cut);
      else
	sscanf(str,"%f",&prm.selfI_hmlg_cut);

      XtSetArg(args[0],XtNstring, &str);
      XtGetValues(ann_hmlg_t, args, ONE);
      if (weights)
	sscanf(str,"%f",&prm.wt_self3_hmlg_cut);
      else
	sscanf(str,"%f",&prm.self3_hmlg_cut);

      if (program_option==3) {
	if (!weights) {
	  prm.pp3_hmlg_cut = 0;
	  prm.ppI_hmlg_cut = 0;
	}
	else {
	  prm.wt_pp3_hmlg_cut = 0;
	  prm.wt_ppI_hmlg_cut = 0;
	}
      }
      else {
      XtSetArg(args[0],XtNstring, &str);
      XtGetValues(ppann_hmlg_i, args, ONE);
      if (weights)
	sscanf(str,"%f",&prm.wt_ppI_hmlg_cut);
      else
	sscanf(str,"%f",&prm.ppI_hmlg_cut);

      XtSetArg(args[0],XtNstring, &str);
      XtGetValues(ppann_hmlg_t, args, ONE);
      if (weights)
	sscanf(str,"%f",&prm.wt_pp3_hmlg_cut);
      else
	sscanf(str,"%f",&prm.pp3_hmlg_cut);

     
      XtSetArg(args[0],XtNstring, &str);
      XtGetValues(primprodIWid, args, ONE);
      if (weights)
	sscanf(str,"%f",&prm.wt_primprodI_hmlg_cut);
      else
	sscanf(str,"%f",&prm.primprodI_hmlg_cut);

      XtSetArg(args[0],XtNstring, &str);
      XtGetValues(primprod3Wid, args, ONE);
      if (weights)
	sscanf(str,"%f",&prm.wt_primprod3_hmlg_cut);
      else
	sscanf(str,"%f",&prm.primprod3_hmlg_cut);
    }


      XtSetArg(args[0],XtNstring, &str);
      XtGetValues(primotherIWid, args, ONE);
      if (weights)
	sscanf(str,"%f",&prm.wt_primotherI_hmlg_cut);
      else
	sscanf(str,"%f",&prm.primotherI_hmlg_cut);

      XtSetArg(args[0],XtNstring, &str);
      XtGetValues(primother3Wid, args, ONE);
      if (weights)
	sscanf(str,"%f",&prm.wt_primother3_hmlg_cut);
      else
	sscanf(str,"%f",&prm.primother3_hmlg_cut);



      XtSetArg(args[0],XtNstring, &str);
      XtGetValues(primotherIWid, args, ONE);
      if (weights)
	sscanf(str,"%f",&prm.wt_primotherI_hmlg_cut);
      else
	sscanf(str,"%f",&prm.primotherI_hmlg_cut);


      if (!weights) {
	XtSetArg(args[0],XtNstring, &str);
	XtGetValues(ATWeightWid, args, ONE);
	sscanf(str,"%f",&prm.AT_score);


	XtSetArg(args[0],XtNstring, &str);
	XtGetValues(CGWeightWid, args, ONE);
	sscanf(str,"%f",&prm.CG_score);

	XtSetArg(args[0],XtNstring, &str);
	XtGetValues(prim_nucs, args, ONE);
	if (strlen(str) > MAX_NAME_SIZE)
	  popupMessage("Length of PRIM_NUCS is larger than size, MAX_NAME_SIZE, allowable\nChoice of PRIM_NUCS ignored.");
	else
	  {
	    
	    kk = 0;
	    for (k = 0; str[k]; k++) {
	      prm.end_nucs[kk]=str[k];
	      if (!isupper(prm.end_nucs[kk])) toupper(prm.end_nucs[kk]);
	      kk++;
	    }
	    prm.end_nucs[kk]='\0';
	  }
      }

      XtSetArg(args[0],XtNstring, &str);
      XtGetValues(prim_tm_min, args, ONE);
      sscanf(str,"%f",&temp_float);
      if (weights)
	prm.wt_prim_s_tm = temp_float;
      else
	prm.prim_tm_low = temp_float;

      XtSetArg(args[0],XtNstring, &str);
      XtGetValues(prim_tm_max, args, ONE);
      sscanf(str,"%f",&temp_float);
      if (weights) {
	if (program_option!=3)
	  prm.wt_prim_a_tm = temp_float; 
      }
      else
	prm.prim_tm_high = temp_float; 

      if (program_option != 3) {
	XtSetArg(args[0],XtNstring, &str);
	XtGetValues(delta_tm, args, ONE);
	sscanf(str,"%f",&temp_float);      
	if (weights) 
	prm.wt_delta_tm_cut = temp_float;
	else
	prm.delta_tm_cut = temp_float;
      }
      else {
	prm.delta_tm_cut=0; /* 0 meaning not applicable */
	prm.wt_delta_tm_cut=0;
      }


       return;

    }

/* saves the values that Get Params has gotten from the constraints
   information changes in changeParamsPopup to a file */
void SaveParams(w,client_data,call_data)
     Widget w;
     XtPointer client_data;
     XtPointer call_data;
     
{ String str;
  char *fn;
  Widget apane1 = (Widget) client_data;
  Arg args[10];
  int n;
  FILE *gp;
  

  fn = (char *)our_alloc(100 *sizeof(char));
  
  XtSetArg(args[0],XtNstring, &str);
  XtGetValues(apane1, args, ONE);
  strcpy(fn,str);
  if (strlen(fn) > 100) {
    popupMessage("Filename too long\nFile could not be saved\n");
  }
  else {
    write_params_file(fn);
  }
  
  our_free(fn);
  return;
}

void check_param_validity(w,client_data,call_data)
     Widget w;
     XtPointer client_data;
     XtPointer call_data;
{
  check_params();
  *orig_params = prm;

  return;
}


/* reads the constraint file name from the changeParamsPopup */
void inputParams(w,  client_data,call_data)
Widget w;
XtPointer client_data;
XtPointer call_data;
 
    { Widget defFnWid = (Widget) client_data;
      Arg args[10];
      String str;


      XtSetArg(args[0],XtNstring, &str);
      XtGetValues(defFnWid, args, ONE);

      if (strlen(str) > MAX_NAME_SIZE) 
	printf("ERROR: Parameter filename longer than MAX_NAME_SIZE, %d, allowed.\nPlease use a shorter filename.\nParameter file was not read in.",MAX_NAME_SIZE);
      else {
	strcpy(def_fn,str);
	ReadDef(def_fn,1); /* the 1 indicates that X windows 
			    is up and running when you make this
			    call to the read def function*/
      }

/*      our_free(str);*/
      return;
    }





















