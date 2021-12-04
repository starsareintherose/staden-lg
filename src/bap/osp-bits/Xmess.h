#ifndef _Xmess_h
#define _Xmess_h

/* 
  Program Name: Xmess 
  File name: Xmess.h
  Purpose: send any message to either a popupWidget or the resultsWid
     from any of the other program modules -- can be warnings, or
     information or errors
  Copyright 1991: LaDeana Hillier and Philip Green

  Last Update: Fri Mar 22 1991

  Change Log:
*/

/* ---- Exports ---- */
extern void message();
/* void message(message)
 String message;
  put the String message in the results window */



extern void popupMessage();
/* void popupMessage(message_str)
 char *message_str;
  put the message_str in the popupMessage window */

extern void popUpErrorMessage();
/* pops up an error message if there has been overwriting
   of any variable in our_alloc, i.e. if I have not allocated
   enough space for some variable*/

#endif /* _Xmess_h */


