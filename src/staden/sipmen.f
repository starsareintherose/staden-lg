      SUBROUTINE MENU(OPT,KOPT,MOPT,MAXOPT,MINMEN,KBIN,KBOUT,
     +IHELPS,IHELPE,HELPF,IDEVH)
C   AUTHOR: RODGER STADEN
      INTEGER OPT
      INTEGER IHELPS(0:MAXOPT),IHELPE(0:MAXOPT)
      CHARACTER HELPF*(*)
1     CONTINUE
      CALL CLEARV
      IF(MOPT.EQ.0)THEN
      WRITE(KBOUT,5000)
5000  FORMAT(' Menus and their numbers are'/
     +' m0 = This menu'/
     1' m1 = General'/
     2' m2 = Screen control'/
     5' m3 = Set parameters'/
     +' m4 = Comparison'/
     6'  ? = Help'/
     1'  ! = Quit')
1001  FORMAT(' ? Menu or option number=',$)
      ELSE IF(MOPT.EQ.-1)THEN
              WRITE(KBOUT,5001)
5001            FORMAT(' General menu'/
     +          '  0 = List of menus'/
     1          '  3 = Read new sequence'/
     1          '  4 = Redefine active region'/
     +          '  5 = List the sequences'/
     +          '  6 = List a text file'/
     +          '  7 = Direct output to disk'/
     +          '  8 = Write active sequence to disk'/
     +          '  9 = Edit the sequences'/
     +          ' 29 = Complement sequences')
      ELSE IF(MOPT.EQ.-2)THEN
                WRITE(KBOUT,5002)
5002            FORMAT(' Screen control menu'/
     +          '  0 = List of menus'/
     +          ' 10 = Clear graphics'/
     +          ' 11 = Clear text'/
     +          ' 12 = Draw a ruler'/
     +          ' 13 = Use cross hair'/
     +          ' 14 = Reposition plots'/
     +          ' 15 = Label diagram'/
     +          ' 16 = Display a map'/
     +          ' 27 = Draw a /')
      ELSE IF(MOPT.EQ.-3)THEN
                WRITE(KBOUT,5005)
5005            FORMAT(' Set parameters menu'/
     +          '  0 = List of menus'/
     +          ' 20 = Set span length'/
     +          ' 21 = Set proportional score'/
     +          ' 22 = Set identites score'/
     +          ' 23 = Calculate expected scores'/
     +          ' 24 = Calculate observed scores'/
     +          ' 25 = Show current parameter settings'/
     +          ' 30 = Switch main diagonal'/
     +          ' 31 = Switch identities'/
     +          ' 32 = Change score matrix'/
     +          ' 33 = Set number of sd for Quickscan'/
     +          ' 34 = Set gap penalties')
      ELSE IF(MOPT.EQ.-4)THEN
                WRITE(KBOUT,5006)
5006            FORMAT(' Comparison menu'/
     +          '  0 = List of menus'/
     +          ' 17 = Apply identities algorithm'/
     +          ' 18 = Apply proportional algorithm'/
     +          ' 19 = List matching spans'/
     +          ' 26 = Quick scan'/
     +          ' 28 = Align sequences')
      END IF
3     CONTINUE
      WRITE(KBOUT,1001)
      CALL GETOPT(KBIN,KOPT,OPT)
      IF((OPT.LT.MINMEN).OR.(OPT.GT.MAXOPT).OR.(KOPT.GT.2)) THEN
        MOPT = 0
        GO TO 1
      END IF
      IF((KOPT.EQ.2).AND.(OPT.GE.0)) THEN
        CALL HELP2(IHELPS(OPT),IHELPE(OPT),HELPF,IDEVH,KBIN,KBOUT)
        GO TO 1
      END IF
      IF(OPT.LE.0) THEN
        MOPT = OPT
        GO TO 1
      END IF
      END
