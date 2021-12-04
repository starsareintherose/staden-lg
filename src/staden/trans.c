#include <X11/Intrinsic.h>
/*
** We don't want to have all the textwidget translations
** Here are the list of the ones we want.
** This is taken from Xaw/TextTr.c source
*/
char *defaultTranslations = "\
<Key>Right:	forward-character() \n\
<Key>Left:	backward-character() \n\
<Key>Delete:	delete-previous-character() \n\
<Key>BackSpace:	delete-previous-character() \n\
<Key>:		insert-char() \n\
<FocusIn>:	focus-in() \n\
<FocusOut>:	focus-out() \n\
<Btn1Down>:	select-start() \n\
<Btn1Motion>:	extend-adjust() \n\
<Btn1Up>:	extend-end(PRIMARY, CUT_BUFFER0) \n\
<Btn2Down>:	insert-selection(PRIMARY, CUT_BUFFER0) \n\
<Btn3Down>:	extend-start() \n\
<Btn3Motion>:	extend-adjust() \n\
<Btn3Up>:	extend-end(PRIMARY, CUT_BUFFER0) \
";

void my_translations(Widget w)
/*
** Use my default translations
*/
{
    XtTranslations parsedTranslations;

    parsedTranslations = XtParseTranslationTable(defaultTranslations);

    XtUninstallTranslations(w);
    XtAugmentTranslations(w,parsedTranslations);
}
