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
5000  FORMAT(' Menus and their numbers are '/
     +' m0 = This menu'/
     1' m1 = General'/
     2' m2 = Screen control'/
     5' m3 = Statistical analysis of content'/
     1' m4 = Structure'/
     3' m5 = Search'/
     6'  ? = Help'/
     1'  ! = Quit')
      ELSE IF(MOPT.EQ.-1)THEN
              WRITE(KBOUT,5001)
5001            FORMAT(' General menu'/
     +          '  0 = List of menus'/
     1          '  3 = Read new sequence'/
     1          '  4 = Redefine active region'/
     +          '  5 = List the sequence'/
     +          '  6 = List a text file'/
     +          '  7 = Direct output to disk'/
     +          '  8 = Write active sequence to disk'/
     +          '  9 = Edit the sequence'/
     +          ' 17 = Short sequence search'/
     +          ' 18 = Compare a sequence'/
     +          ' 19 = Compare a sequence using a score matrix'/
     +          ' 27 = Back translate to dna')
      ELSE IF(MOPT.EQ.-2)THEN
                WRITE(KBOUT,5002)
5002            FORMAT(' Screen control menu'/
     +          '  0 = List of menus'/
     +          ' 10 = Clear graphics'/
     +          ' 11 = Clear text'/
     +          ' 12 = Draw a ruler'/
     +          ' 13 = Use cross hair'/
     +          ' 14 = Reset margins'/
     +          ' 15 = Label diagram'/
     +          ' 16 = Display a map')
      ELSE IF(MOPT.EQ.-3)THEN
                WRITE(KBOUT,5005)
5005            FORMAT(' Statistical analysis of content'/
     +          '  0 = List of menus'/
     +          ' 21 = Count amino acid composition'/
     +          ' 22 = Plot hydrophobicity'/
     +          ' 23 = Plot charge',/
     +          ' 25 = Plot hydrophobic moment')
      ELSE IF(MOPT.EQ.-4)THEN
                WRITE(KBOUT,5007)
5007            FORMAT(' Structure menu'/
     +          '  0 = List of menus'/
     +          ' 22 = Plot hydrophobicity'/
     +          ' 23 = Plot charge'/,
     +          ' 24 = Plot robson prediction',/
     +          ' 25 = Plot hydrophobic moment',/
     +          ' 26 = Draw helix wheel')
      ELSE IF(MOPT.EQ.-5)THEN
                WRITE(KBOUT,5004)
5004            FORMAT(' Search menu'/
     +          '  0 = List of menus'/
     1          ' 17 = Search for short sequences'/
     2          ' 18 = Compare a sequence'/
     2          ' 19 = Compare a sequence using a score matrix'/
     2          ' 20 = Search for a sequence using a weight matrix'/
     2          ' 28 = Search for patterns of motifs')
      END IF
      WRITE(KBOUT,1001)
1001  FORMAT(' ? Menu or option number=',$)
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
