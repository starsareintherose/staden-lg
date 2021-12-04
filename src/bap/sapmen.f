      SUBROUTINE DBMENT(MENU,NOPT,KOPT,MAXOPT,
     +IHELPS,IHELPE,HELPF,IDEVH,KBIN,KBOUT)
      CHARACTER HELPF*(*)
      INTEGER IHELPS(0:MAXOPT),IHELPE(0:MAXOPT)
1     CONTINUE
      IF(MENU.EQ.0) THEN
        WRITE(KBOUT,5000)
5000    FORMAT(' Menus and their numbers are',/,
     +  ' m0 = This menu',/,
     +  ' m1 = General',/,
     +  ' m2 = Screen control',/,
     +  ' m3 = Modification',/,
     +  '  ? = Help',/,
     +  '  ! = Quit')
      ELSE IF(MENU.EQ.-1) THEN
      WRITE(KBOUT,1001)
1001  FORMAT(' General menu',/,
     +'  0 = List of menus',/,
     +'  ? = Help',/,
     +'  ! = Quit',/,
     +'  3 = Open a database',/,
     +'  5 = Display a contig',/,
     +'  6 = List a text file',/,
     +'  7 = Redirect output',/,
     +'  8 = Calculate a consensus',/,
     +' 17 = Screen against restriction enzymes',/,
     +' 18 = Screen against vector',/,
     +' 19 = Check database',/,
     +' 24 = Copy database',/,
     +' 25 = Show relationships',/,
     +' 27 = Set parameters',/,
     +' 28 = Highlight disagreements',/,
     +' 29 = Examine quality',/,
     +' 35 = Find internal joins')
      ELSE IF(MENU.EQ.-2) THEN
      WRITE(KBOUT,1002)
1002  FORMAT(' Screen control menu',/, 
     +'  0 = List of menus',/,
     +'  ? = Help',/,
     +'  ! = Quit',/,
     +' 10 = Clear graphics',/,
     +' 11 = Clear text',/,
     +' 12 = Draw ruler',/,
     +' 13 = Use cross hair',/,
     +' 14 = Change margins',/,
     +' 15 = Label diagram',/,
     +' 16 = Plot map',/,
     +' 33 = Plot single contig',/,
     +' 34 = Plot all contigs')
      ELSE IF(MENU.EQ.-3) THEN
      WRITE(KBOUT,1003)
1003  FORMAT(' Modification menu',/, 
     +'  0 = List of menus',/,
     +'  ? = Help',/,
     +'  ! = Quit',/,
     +'  4 = Edit contig',/,
     +'  9 = Screen edit',/,
     +' 20 = Auto assemble',/,
     +' 21 = Enter new gel reading',/,
     +' 22 = Join contigs',/,
     +' 23 = Complement a contig',/,
     +' 26 = Alter relationships',/,
     +' 30 = Auto edit a contig',/,
     +' 31 = Disassemble readings',/,
     +' 32 = Extract gel readings')
      END IF
      WRITE(KBOUT,1010)
      CALL GETOPT(KBIN,KOPT,NOPT)
      IF((NOPT.LT.-3).OR.(NOPT.GT.MAXOPT).OR.(KOPT.GT.2)) THEN
        MENU = 0
        GO TO 1
      END IF
      IF((KOPT.EQ.2).AND.(NOPT.GE.0))THEN
        CALL HELP2(IHELPS(NOPT),IHELPE(NOPT),HELPF,IDEVH,KBIN,KBOUT)
        GO TO 1
      END IF
      IF(NOPT.EQ.0) THEN
        MENU = 0
        GO TO 1
      END IF
      IF(NOPT.LT.0) THEN
        MENU = NOPT
        GO TO 1
      END IF
1010  FORMAT(
     +' ? Option number=',$)
      RETURN
      END
      SUBROUTINE DBMENU(MENU,NOPT,KOPT,IHELPS,IHELPE,HELPF,IDEVH,
     +KBIN,KBOUT)
      CHARACTER HELPF*(*)
      INTEGER IHELPS,IHELPE
1     CONTINUE
      IF(MENU.EQ.2)THEN
20    CONTINUE
      CALL BPAUSE(KBIN,KBOUT,IOK)
      WRITE(KBOUT,1002)
1002  FORMAT( ' Enter menu options are:',/,
     +'  ? = Help',/,
     +'  ! = Give up',/,
     +'  3 = Complete entry',/,
     +'  4 = Edit contig',/,
     +'  5 = Display',/,
     +'  6 = Edit new gel reading')
      WRITE(KBOUT,1010)
      CALL GETOPT(KBIN,KOPT,NOPT)
      MAXOPT = 6
      IF((NOPT.LT.0).OR.(NOPT.GT.MAXOPT).OR.(KOPT.GT.2)) THEN
        GO TO 20
      END IF
      IF((KOPT.EQ.2).AND.(NOPT.GE.0))THEN
        CALL HELP2(IHELPS,IHELPE,HELPF,IDEVH,KBIN,KBOUT)
        GO TO 20
      END IF
      ELSE IF(MENU.EQ.3)THEN
30    CONTINUE
      CALL BPAUSE(KBIN,KBOUT,IOK)
      WRITE(KBOUT,1003)
1003  FORMAT( ' Join menu options are:'/
     +'  ? = Help',/,
     +'  ! = Give up',/,
     +'  3 = Complete join',/,
     +'  4 = Edit left contig',/,
     +'  5 = Display join',/,
     +'  6 = Edit right contig',/,
     +'  7 = Move join')
      WRITE(KBOUT,1010)
      CALL GETOPT(KBIN,KOPT,NOPT)
      MAXOPT = 7
      IF((NOPT.LT.0).OR.(NOPT.GT.MAXOPT).OR.(KOPT.GT.2)) THEN
        GO TO 30
      END IF
      IF((KOPT.EQ.2).AND.(NOPT.GE.0))THEN
        CALL HELP2(IHELPS,IHELPE,HELPF,IDEVH,KBIN,KBOUT)
        GO TO 30
      END IF
      ELSE IF(MENU.EQ.4)THEN
40    CONTINUE
      CALL BPAUSE(KBIN,KBOUT,IOK)
      WRITE(KBOUT,1004)
1004  FORMAT(' Alter relationships menu options are:'/
     +'  ? = Help',/,
     +'  ! = Quit',/,
     +'  3 = Line change',/,
     +'  4 = Check logical consistency',/,
     +'  5 = Remove a contig',/,
     +'  6 = Shift',/,
     +'  7 = Move gel reading',/,
     +'  8 = Rename gel reading',/,
     +'  9 = Break a contig',/,
     +' 10 = Remove a gel reading')
      WRITE(KBOUT,1010)
      CALL GETOPT(KBIN,KOPT,NOPT)
      MAXOPT = 10
      IF((NOPT.LT.0).OR.(NOPT.GT.MAXOPT).OR.(KOPT.GT.2)) THEN
        GO TO 40
      END IF
      IF((KOPT.EQ.2).AND.(NOPT.GE.0))THEN
        CALL HELP2(IHELPS,IHELPE,HELPF,IDEVH,KBIN,KBOUT)
        GO TO 40
      END IF
      ELSE IF(MENU.EQ.5)THEN
50    CONTINUE
      CALL BPAUSE(KBIN,KBOUT,IOK)
      WRITE(KBOUT,1005)
1005  FORMAT(
     +' Edit options are:',/,
     +'  ? = Help',/,
     +'  ! = Quit',/,
     +'  3 = Insert',/,
     +'  4 = Delete',/,
     +'  5 = Change')
      WRITE(KBOUT,1010)
      CALL GETOPT(KBIN,KOPT,NOPT)
      MAXOPT = 5
      IF((NOPT.LT.0).OR.(NOPT.GT.MAXOPT).OR.(KOPT.GT.2)) THEN
        GO TO 50
      END IF
      IF((KOPT.EQ.2).AND.(NOPT.GE.0))THEN
        CALL HELP2(IHELPS,IHELPE,HELPF,IDEVH,KBIN,KBOUT)
        GO TO 50
      END IF
      END IF
1010  FORMAT(
     +' ? Option number=',$)
      IF(NOPT.LE.0) GO TO 1
      RETURN
      END
