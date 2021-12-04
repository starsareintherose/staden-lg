      SUBROUTINE MENU(OPT,KOPT,MOPT,MAXOPT,MINMEN,KBIN,KBOUT,
     +IHELPS,IHELPE,HELPF,IDEVH)
      INTEGER IHELPS(0:MAXOPT),IHELPE(0:MAXOPT)
      INTEGER OPT
      CHARACTER HELPF*(*)
1     CONTINUE
      CALL CLEARV
      IF(MOPT.EQ.0) THEN
      WRITE(KBOUT,1000)
 1000 FORMAT(' Menus and their numbers are',/,
     +' m0 = This menu',/,
     +' m1 = General menu',/,
     +' m2 = Screen control menu',/,
     +' m3 = Dictionary analysis menu',/,
     +'  ? = Help',/,
     +'  ! = Quit')
      ELSE IF (MOPT.EQ.-1) THEN
        WRITE(KBOUT,1001)
1001    FORMAT(' General menu',/,
     +  '   0 = List of menus',/,
     +  '   ? = Help',/,
     +  '   ! = Quit'/,
     +  '   3 = Read new sequences',/,
     +  '   4 = Redefine active region',/,
     +  '   5 = List the sequences',/,
     +  '   6 = List text file',/,
     +  '   7 = Direct output to disk',/,
     +  '  17 = Search for strings',/,
     +  '  33 = Find inverted repeats')
       ELSE IF (MOPT.EQ.-2) THEN
        WRITE(KBOUT,1002)
1002    FORMAT(' Screen control menu',/,
     +  '   0 = List of menus',/,
     +  '   ? = Help',/,
     +  '   ! = Quit'/,
     +  '  10 = Clear graphics',/,
     +  '  11 = Clear text',/,
     +  '  12 = Draw ruler',/,
     +  '  13 = Use cross hair',/,
     +  '  14 = Reset margins',/,
     +  '  15 = Label diagram',/,
     +  '  16 = Draw map')
       ELSE IF (MOPT.EQ.-3) THEN
        WRITE(KBOUT,1003)
1003    FORMAT(' Dictionary analysis menu',/,
     +  '  18 = Set strand',/,
     +  '  19 = Set composition',/,
     +  '  20 = Set word mask',/,
     +  '  21 = Set number of mismatches',/,
     +  '  22 = Show settings',/,
     +  '  23 = Make dictionary Dw',/,
     +  '  24 = Make dictionary Ds',/,
     +  '  25 = Make fuzzy dictionary Dm from Dw',/,
     +  '  26 = Make fuzzy dictionary Dm from Ds',/,
     +  '  27 = Make fuzzy dictionary Dh from Dm',/,
     +  '  28 = Examine fuzzy dictionary Dm',/,
     +  '  29 = Examine fuzzy dictionary Dh',/,
     +  '  30 = Examine words in Dm',/,
     +  '  31 = Examine words in Dh',/,
     +  '  32 = Save or restore a dictionary')
      END IF
      WRITE(KBOUT,1004)
1004  FORMAT(' ? Menu or option number=',$)
      CALL GETOPT(KBIN,KOPT,OPT)
      IF((OPT.LT.MINMEN).OR.(OPT.GT.MAXOPT).OR.(KOPT.GT.2)) THEN
        MOPT = 0
        GO TO 1
      END IF
      IF((KOPT.EQ.2).AND.(OPT.GE.0)) THEN
        CALL HELP2(IHELPS(OPT),IHELPE(OPT),HELPF,IDEVH,KBIN,KBOUT)
        GO TO 1
      END IF
      IF(OPT.EQ.0) THEN
        MOPT = 0
        GO TO 1
      END IF
      IF(OPT.LT.0) THEN
        MOPT = OPT
        GO TO 1
       END IF
       END
