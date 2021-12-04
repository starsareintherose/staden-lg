#ifndef _searchUtils_h
#define _searchUtils_h

#include <X11/Intrinsic.h>
#include "edUtils.h"


enum searchOptions {
  OptByGelName=1,
  OptByTagType,
  OptByAnnotation,
  OptBySequence,
  OptByPosition,
  OptByProblem,
  OptByQuality
};

extern void createSearchWidget(Widget parentWid);

extern int invokeSearchGeneric(EdStruct *xx);

extern int destroySearchWindow();

#endif /* _searchUtils_h */
