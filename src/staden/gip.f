C     GIP
C   SCREEN OF 24 LINES, 80 CHARACTERS ASSUMED
C
C
C   14 May 1991
C      Modified to work on Sparcstations - connect digitiser to RS232-C port B
C   24 Sept 1992
C      digitizer is now specified by environment variable DIGITIZER
C
      SUBROUTINE FMAIN()
      PARAMETER (IDMENU=30)
      INTEGER XLANE(4),DY,DXMIN,XPEN,YPEN,YMEAN
      INTEGER XLMENU(IDMENU),XRMENU(IDMENU)
      INTEGER YBMENU(IDMENU),YTMENU(IDMENU)
      CHARACTER CHARS(IDMENU),GEL(512),BASE(4),MENUE,GELNAM*40,ANS
      INTEGER MENUP(IDMENU),SEQROW,SEQCOL,SEQLEN
      INTEGER DEVNOS(6)
      INTEGER ESC
      CHARACTER*40 FILDIG,FILVT
      CHARACTER SPACE,CESC,CBELL
C   ALL SCREEN OUTPUT CONTROLLED USING CURSOR
C   SCREEN SIZE MAXROW, MAXCOL
C   DIRECTION FROM HERE IS INC (1 OR -1)
      INTEGER TITLER,TITLEC,CURER,CUREC,CURFR,CURFC
      COMMON /ESCCHR/ CESC
      COMMON /BELCHR/ CBELL
      PARAMETER (ESC=27,IBELL=7)
      PARAMETER (INC=1,MAXR=24)
      PARAMETER (TITLER=1,TITLEC=20,LORDR=3,LORDC=20,
     +IERRR=MAXR,IERRC=10,
     +INSTR1=MAXR-INC,INSTC1=10,INSTR2=MAXR-2*INC,INSTC2=10,
     +CURER=MAXR-4*INC,CUREC=1,CURFR=2,CURFC=20)
      PARAMETER (SEQROW=6,SEQCOL=10,SEQLEN=60)
C      PARAMETER (FILDIG='/dev/ttyb')
      PARAMETER (FILDIG='DIGITIZER')
      DATA XLMENU/0,
     +0,200,400,600,0,200,400,600,
     +0,200,400,600,0,200,400,600,
     +0,200,400,600,0,200,400,600,
     +0,400,0,0,0/
      DATA XRMENU/800,
     +200,400,600,800,200,400,600,800,
     +200,400,600,800,200,400,600,800,
     +200,400,600,800,200,400,600,800,
     +400,800,800,800,800/
      DATA YBMENU/000,
     +1800,1800,1800,1800,
     +1600,1600,1600,1600,
     +1400,1400,1400,1400,
     +1200,1200,1200,1200,
     +1000,1000,1000,1000,
     +800,800,800,800,
     +600,600,400,200,0/
      DATA YTMENU/2000,2000,2000,2000,2000,
     +1800,1800,1800,1800,
     +1600,1600,1600,1600,
     +1400,1400,1400,1400,
     +1200,1200,1200,1200,
     +1000,1000,1000,1000,
     +800,800,600,400,200/
      DATA CHARS/'W',
     +'T','C','G','A',
     +'2','1','4','3',
     +'V','D','H','B',
     +'L','K','N','M',
     +'R','Y','X','-',
     +'5','6','7','8',
     +'D','R','S','S','C'/
      DATA MENUP/0,1,2,3,4,5,6,7,8,9,10,
     +           11,12,13,14,15,16,17,18,19,20,
     +           21,22,23,24,50,51,53,52,56/
      DATA SPACE/' '/
      DATA BASE/'T','C','G','A'/
C   PROBLEM WITH DEFINING ESCAPE AND BELL SO DO IT HERE AND PUT IT COMMON
C      WRITE(CESC,2000)ESC
      CESC=CHAR(ESC)
2000  FORMAT(A1)
      CBELL=CHAR(IBELL)
      CALL UNITNO(KBIN,KBOUT,DEVNOS,6)
C   OPEN LOGICAL UNIT FOR SOME TERMINAL OUTPUT
      CALL OPENT(DEVNOS(6),FILVT)
C   CLEAR SCREEN
      CALL CLEAR
C   WRITE TITLE
      CALL CURSOR(TITLER,TITLEC)
      CALL WRITES('GIP v1.0 Author: Rodger Staden')
C   define allowed Y distance from current pen position
      DY=80
      CALL OPENRS(DEVNOS(1),FILDIG,IOK,LRECL,8)
      IF(IOK.NE.0)THEN
        CALL CURSOR(IERRR,1)
        CALL BLINE
        CALL CURSOR(IERRR,IERRC)
        CALL WRITES('******UNABLE TO OPEN DIGITIZER******')
          STOP
      END IF
C   FLUSH DIGITIZER
C      CALL FLUSH(DEVNOS(1))
      CALL CURSOR(INSTR1,1)
      CALL BLINE
      CALL CURSOR(INSTR1,INSTC1)
      CALL WRITES('The film must be firmly fixed to the light box')
5     CONTINUE
      CALL CURSOR(CURFR,1)
      CALL BLINE
      CALL CURSOR(CURFR,CURFC)
      CALL WRITES('? File of file names=')
      READ(KBIN,1000,ERR=5)GELNAM
1000  FORMAT(A)
      CALL OPENRS(DEVNOS(3),GELNAM,IOK,LRECL,1)
      IF(IOK.NE.0)THEN
        CALL CURSOR(IERRR,1)
        CALL BLINE
        CALL CURSOR(IERRR,IERRC)
        CALL WRITES('******ERROR OPENING FILE******')
        GO TO 5
      END IF
C   CLEAR ERROR LINE IN CASE ITS BEEN USED
      CALL CURSOR(IERRR,1)
      CALL BLINE
      CALL CURSOR(INSTR2,1)
      CALL BLINE
      CALL CURSOR(INSTR2,INSTC2)
      CALL WRITES(
     +'Hit device menu origin, program origin, then hit start')
90    CONTINUE
      CALL READPN(XPEN,YPEN,1,DEVNOS(1),KBOUT)
C   START?
      CALL INTERP(XLMENU,XRMENU,YBMENU,YTMENU,MENUP,CHARS,
     +IDMENU,XPEN,YPEN,MENU,MENUE,KBOUT)
      IF(MENU.NE.52)GO TO 90
100   CONTINUE
C   CLEAR ERROR LINE IN CASE ITS BEEN USED
      CALL CURSOR(IERRR,1)
      CALL BLINE
C   GET LANE ORDER
120   CONTINUE
C   GET LANE ORDER
      CALL LORDER(XLMENU,XRMENU,YBMENU,YTMENU,MENUP,
     +CHARS,MENUE,IDMENU,BASE,DEVNOS(1),KBOUT,INSTR2,INSTC2,
     +LORDR,LORDC)
C   CLEAR ERROR LINE IN CASE ITS BEEN USED
      CALL CURSOR(IERRR,1)
      CALL BLINE
C   GET LANE COORDS
      CALL GETLAN(XLANE,YMEAN,DXMIN,
     +XLMENU,XRMENU,YBMENU,YTMENU,MENUP,CHARS,MENUE,IDMENU,
     +DEVNOS(1),KBOUT,INSTR2,INSTC2)
C   CLEAR ERROR LINE IN CASE ITS BEEN USED
      CALL CURSOR(IERRR,1)
      CALL BLINE
C   POINT TO GEL
      IPGEL=0
******************************************************
200   CONTINUE
      CALL CURSOR(INSTR2,1)
      CALL BLINE
      CALL CURSOR(INSTR2,INSTC2)
      CALL WRITES('Hit start when ready to begin reading')
      CALL READPN(XPEN,YPEN,1,DEVNOS(1),KBOUT)
C   START?
      CALL INTERP(XLMENU,XRMENU,YBMENU,YTMENU,MENUP,CHARS,
     +IDMENU,XPEN,YPEN,MENU,MENUE,KBOUT)
      IF(MENU.EQ.52)THEN
        CALL CURSOR(INSTR2,1)
      CALL BLINE
      CALL CURSOR(INSTR2,INSTC2)
        CALL WRITES
     +  ('Hit bands, uncertainty codes, DELETE, RESET or STOP')
        CALL READG(
     +  XLMENU,XRMENU,YBMENU,YTMENU,MENUP,CHARS,
     +  IDMENU,XLANE,YMEAN,DY,DXMIN,BASE,GEL,IPGEL,
     +  SEQROW,SEQCOL,SEQLEN,DEVNOS(1),KBOUT,INSTR2,INSTC2)
        CALL CURSOR(INSTR2,INSTC2)
        IF(IPGEL.GT.0)THEN
50        CONTINUE
          CALL CURSOR(INSTR2,1)
          CALL BLINE
          CALL CURSOR(INSTR2,INSTC2)
          CALL WRITES('File name for this gel reading=')
          READ(KBIN,1000,ERR=50)GELNAM
          CALL OPENRS(DEVNOS(5),GELNAM,IOK,LRECL,1)
          IF(IOK.EQ.0)THEN
            CALL FMTDK(DEVNOS(5),GEL,IPGEL)
            CLOSE(UNIT=DEVNOS(5))
            WRITE(DEVNOS(3),1008)GELNAM
1008        FORMAT(A)
            CALL CURSOR(INSTR2,INSTC2)
          ELSE
            CALL CURSOR(IERRR,1)
            CALL BLINE
            CALL CURSOR(IERRR,IERRC)
            CALL WRITES('******ERROR OPENING FILE******')
            GO TO 50
          END IF
C   CLEAR GEL FROM SCREEN
          DO 441 I=1,IPGEL
            CALL WRITEG(' ',SEQCOL,SEQROW,SEQLEN,I)
441       CONTINUE
        END IF
        CALL CURSOR(INSTR2,1)
        CALL BLINE
        CALL CURSOR(INSTR2,INSTC2)
        CALL WRITES('? (y/n) (y) Read another sequence ')
        READ(KBIN,1010)ANS
1010    FORMAT(A1)
        IF((ANS.EQ.' ').OR.(ANS.EQ.'Y').OR.(ANS.EQ.'y'))GO TO 100
        CALL CLEAR
        STOP
      END IF
      GO TO 200
      END
C   DIGIT
C   AUTHOR RODGER STADEN
C   ROUTINES USED BY GELIN
C   NONSTANDARD: CARRIAGE CONTROL $ USED BY ROUTINE BELL
C   ROUTINES IN THIS LIB:
C   READG
C   INOUT
C   LANE
C   GETLAN
C   GETMEN
C   LORDER
C   WRITEG
C   INTERP
C   READPN
      SUBROUTINE READG(XLMENU,XRMENU,YBMENU,YTMENU,MENUP,CHARS,
     +IDMENU,XLANE,YMEAN,DY,DXMIN,BASE,GEL,IPGEL,SEQROW,SEQCOL,
     +SEQLEN,IDEVD,KBOUT,INSTR2,INSTC2)
C   AUTHOR RODGER STADEN
C   READS A SEQUENCE USING THE DIGITIZER UNTIL A STOP COMMAND IS GIVEN
C   LANE COORDINATES CAN BE RESET.
      INTEGER XLANE(4),DY,DXMIN,XPEN,YPEN,YMEAN
      INTEGER XLMENU(IDMENU),XRMENU(IDMENU)
      INTEGER YBMENU(IDMENU),YTMENU(IDMENU)
      CHARACTER CHARS(IDMENU),GEL(512),BASE(4),MENUE,LASTCH
      INTEGER MENUP(100),SEQCOL,SEQROW,SEQLEN,INSTR2,INSTC2
      EXTERNAL LANE
      LASTCH=' '
300   CONTINUE
      CALL READPN(XPEN,YPEN,0,IDEVD,KBOUT)
C   IN LANES?
C*****************************************************
      L=LANE(XLANE,YMEAN,DY,DXMIN,XPEN,YPEN)
      IF(L.NE.0)THEN
        CALL GBELL(1,KBOUT)
C   IN LANES
C   EXPECTING A BAND SO DEAL WITH IT
C   INCREMENT POINTER TO GEL CHARACTERS
        IPGEL=IPGEL+1
        GEL(IPGEL)=BASE(L)
C   PUT ON SCREEN ETC
        CALL WRITEG(GEL(IPGEL),SEQCOL,SEQROW,SEQLEN,IPGEL)
        LASTCH='C'
      ELSE
C   IN MENU?
        CALL INTERP(XLMENU,XRMENU,YBMENU,YTMENU,MENUP,CHARS,
     +          IDMENU,XPEN,YPEN,MENU,MENUE,KBOUT)
C   UNCERTAINTY CODE?
        IF((MENU.GT.0).AND.(MENU.LT.25))THEN
C   UNCERTAINTY CODE
          CALL GBELL(1,KBOUT)
          IPGEL=IPGEL+1
          GEL(IPGEL)=MENUE
          LASTCH='C'
          CALL WRITEG(GEL(IPGEL),SEQCOL,SEQROW,SEQLEN,IPGEL)
C   DELETE?
        ELSE IF(MENU.EQ.50)THEN
C   DELETE
C   IF LAST CHAR A BASE THEN NEED TO REMOVE IT FROM GEL
          IF((LASTCH.EQ.'C').AND.(IPGEL.GT.0))THEN
            CALL GBELL(1,KBOUT)
            CALL WRITEG(' ',SEQCOL,SEQROW,SEQLEN,IPGEL)
            IPGEL=IPGEL-1
          END IF
C   RESET LANE CENTRES?
        ELSE IF(MENU.EQ.51)THEN
          CALL GBELL(1,KBOUT)
          CALL GETLAN(XLANE,YMEAN,DXMIN,
     +    XLMENU,XRMENU,YBMENU,YTMENU,
     +    MENUP,CHARS,MENUE,IDMENU,
     +    IDEVD,KBOUT,INSTR2,INSTC2)
          CALL CURSOR(INSTR2,1)
      CALL BLINE
      CALL CURSOR(INSTR2,INSTC2)
          CALL WRITES
     +    ('Hit bands, uncertainty codes, DELETE, RESET or STOP')
C   STOP?
        ELSE IF(MENU.EQ.53)THEN
          CALL GBELL(1,KBOUT)
          RETURN
C   FOR NOW NOT FOUND SO IGNORE
       END IF
      END IF
C   NOT IN MENU
C   NOT IN MENU OR LANES SO IGNORE
      GO TO 300
      END
C   INOUT
C
C   FUNCTION TO RETURN VALUE 1 IF XPEN,YPEN IN BOX, 0 ELSE
C   AUTHOR RODGER STADEN
      INTEGER FUNCTION INOUT(X1,X2,Y1,Y2,XPEN,YPEN)
      IMPLICIT INTEGER (A-Z)
      INOUT=0
      IF((XPEN.LT.X1).OR.(XPEN.GT.X2))RETURN
      IF((YPEN.LT.Y1).OR.(YPEN.GT.Y2))RETURN
      INOUT=1
      RETURN
      END
C   LANEORDER
C   GETS LANE ORDER FROM TABLET
      SUBROUTINE LORDER(XLMENU,XRMENU,YBMENU,YTMENU,MENUP,
     +CHARS,MENUE,IDMENU,BASE,IDEVD,KBOUT,INSTR2,INSTC2,
     +LORDR,LORDC)
C   AUTHOR RODGER STADEN
      INTEGER  XLMENU(IDMENU),XRMENU(IDMENU)
      INTEGER YBMENU(IDMENU),YTMENU(IDMENU)
      INTEGER MENUP(IDMENU),INSTR2,INSTC2
      CHARACTER BASE(4),BASES(4),CHARS(IDMENU),MENUE,SPACE
      CHARACTER BASET*4
      PARAMETER (IERRR=24,IERRC=10)
      SAVE BASES,SPACE
      DATA SPACE/' '/
      DATA BASES/'T','C','G','A'/
C
10    CONTINUE
      DO 5 I=1,4
        BASET(I:I)=BASE(I)
5     CONTINUE
C
      CALL CURSOR(LORDR,1)
      CALL BLINE
      CALL CURSOR(LORDR,LORDC)
      CALL WRITES('Lane order is ')
      CALL WRITES(BASET)
      CALL CURSOR(INSTR2,1)
      CALL BLINE
      CALL CURSOR(INSTR2,INSTC2)
      CALL WRITES('If lane order correct hit CONFIRM, else hit RESET')
      CALL READPN(XPEN,YPEN,1,IDEVD,KBOUT)
      CALL INTERP(XLMENU,XRMENU,YBMENU,YTMENU,MENUP,CHARS,
     +IDMENU,XPEN,YPEN,MENU,MENUE,KBOUT)
      IF(MENU.EQ.56)THEN
C   CLEAR ERROR LINE IN CASE ITS BEEN USED
        CALL CURSOR(IERRR,1)
        CALL BLINE
        RETURN
      END IF
      IF(MENU.EQ.51)THEN
      CALL CURSOR(INSTR2,1)
      CALL BLINE
        CALL CURSOR(INSTR2,INSTC2)
        CALL WRITES('Define lane order, left to right, using menu')
C   COUNT POINTS
        IP=1
20      CONTINUE
        IF(IP.LE.4)THEN
           CALL READPN(XPEN,YPEN,1,IDEVD,KBOUT)
           CALL INTERP(XLMENU,XRMENU,YBMENU,YTMENU,MENUP,
     +     CHARS,IDMENU,XPEN,YPEN,MENU,MENUE,KBOUT)
           DO 30 I=1,4
              IF(MENUE.EQ.BASES(I))THEN
                 BASE(IP)=MENUE
                 GO TO 31
              END IF
30         CONTINUE
31         CONTINUE
           IP=IP+1
           GO TO 20
        END IF
C     ALL DIFFERENT?
        DO 40 I=1,4
          DO 41 J=I+1,4
          IF(BASE(I).EQ.BASE(J))THEN
C           SAME!
            CALL CURSOR(INSTR2,1)
      CALL BLINE
      CALL CURSOR(IERRR,IERRC)
            CALL WRITES('*****DUPLICATION IN LANE ORDER*****')
            DO 35 K=1,4
               BASE(K)=BASET(K:K)
35          CONTINUE
            GO TO 10
          END IF
41       CONTINUE
40      CONTINUE
      END IF
      GO TO 10
      END
C   INTERPXY
C
C   INTERPRETS X,Y COORDS FROM DIGITIZER
      SUBROUTINE INTERP(XL,XR,YB,YT,MENUP,CHARS,
     +IDMENU,XPEN,YPEN,MENU,MENUE,KBOUT)
C   AUTHOR RODGER STADEN
      INTEGER XL(IDMENU),XR(IDMENU),YB(IDMENU),YT(IDMENU)
      INTEGER MENUP(IDMENU),XPEN,YPEN
      CHARACTER CHARS(IDMENU),MENUE
      EXTERNAL INOUT
      PARAMETER (IERRR=24,IERRC=10)
      MENU=0
      MENUE=CHARS(1)
C   IN MENU AT ALL?
      IF(INOUT(XL(1),XR(1),YB(1),YT(1),XPEN,YPEN).EQ.0)RETURN
      DO 100 I=2,IDMENU
        IF(INOUT(XL(I),XR(I),YB(I),YT(I),XPEN,YPEN).NE.1)GO TO 100
C       MATCH
        MENU=MENUP(I)
        MENUE=CHARS(I)
        RETURN
100   CONTINUE
C   ERROR IN MENU
      MENU=0
      MENUE=CHARS(1)
      CALL CURSOR(IERRR,1)
      CALL BLINE
      CALL CURSOR(IERRR,IERRC)
      CALL WRITES('******ERROR IN MENU******')
      RETURN
      END
C
C   READPEN GETS COORDS FROM DIGITIZER
      SUBROUTINE READPN(XPEN,YPEN,IBELL,IDEVD,KBOUT)
C   AUTHOR RODGER STADEN
      CHARACTER INPUT*11,INPUTX*5,INPUTY*5
      INTEGER XPEN,YPEN
C      EQUIVALENCE (INPUT(2:6),INPUTX),(INPUT(7:11),INPUTY)
C  NOTE ON IBM NEXT LINE EQUIVALENCE USED
      EQUIVALENCE (INPUT(1:5),INPUTX),(INPUT(6:10),INPUTY)
      PARAMETER (IERRR=24,IERRC=10)
1000  FORMAT(A)
1     CONTINUE
      READ(IDEVD,1000,ERR=1)INPUT
1002  FORMAT(I5)
      READ(INPUTX,1002,ERR=100)XPEN
      READ(INPUTY,1002,ERR=100)YPEN
      IF(IBELL.EQ.1)CALL GBELL(1,KBOUT)
C READ 2nd NEWLINE ON SUN
      READ(IDEVD,1000,ERR=1)INPUT
      RETURN
100   CONTINUE
      CALL CURSOR(IERRR,1)
      CALL BLINE
      CALL CURSOR(IERRR,IERRC)
      CALL WRITES('*********ERROR READING PEN********')
      RETURN
      END
C   WRITEGEL
      SUBROUTINE WRITEG(CHAR,STARTX,STARTY,LENGTH,IPGEL)
C   AUTHOR RODGER STADEN
C   WRITE OUT A CHARACTER
      CHARACTER CHAR
      INTEGER STARTX,STARTY,ROW,COL
      ROW=(IPGEL-1)/LENGTH
      COL=MOD(IPGEL,LENGTH)
      IF(COL.EQ.0)COL=LENGTH
      ROW=STARTY+ROW
      COL=STARTX+COL
      CALL CURSOR(ROW,COL)
      CALL WRITES(CHAR)
      RETURN
      END
C
C   GETLANE
      SUBROUTINE GETLAN(XLANE,YMEAN,DXMIN,
     +XLMENU,XRMENU,YBMENU,YTMENU,MENUP,CHARS,MENUE,IDMENU,
     +IDEVD,KBOUT,INSTR2,INSTC2)
C   AUTHOR RODGER STADEN
C   GETS LANE ORDER
      INTEGER XLANE(4)
      INTEGER YTEMP(4)
      INTEGER XLMENU(IDMENU),XRMENU(IDMENU)
      INTEGER YBMENU(IDMENU),YTMENU(IDMENU)
      INTEGER MENUP(IDMENU),INSTR2,INSTC2
      INTEGER XPEN,YPEN,YMEAN,DXMIN
      CHARACTER CHARS(IDMENU),MENUE,SEPAR*10
      PARAMETER (IERRR=24,IERRC=10)
      INSTR3=4
      INSTC3=20
100   CONTINUE
      CALL CURSOR(INSTR2,1)
      CALL BLINE
      CALL CURSOR(INSTR2,INSTC2)
      CALL WRITES
     +('Hit START, then, left to right,')
      CALL WRITES(' the start centres of this clones lanes')
      CALL READPN(XPEN,YPEN,1,IDEVD,KBOUT)
      CALL INTERP(XLMENU,XRMENU,YBMENU,YTMENU,MENUP,CHARS,
     +IDMENU,XPEN,YPEN,MENU,MENUE,KBOUT)
C   START?
      IF(MENU.NE.52)GO TO 100
C   GOT START, EXPECT LANES NOW
200   CONTINUE
      DO 300 I=1,4
      CALL READPN(XPEN,YPEN,1,IDEVD,KBOUT)
C   IN MENU?
      CALL INTERP(XLMENU,XRMENU,YBMENU,YTMENU,MENUP,CHARS,
     +IDMENU,XPEN,YPEN,MENU,MENUE,KBOUT)
      IF(MENU.NE.0)GO TO 100
C   NOT IN MENU SO RECORD POSITION
      XLANE(I)=XPEN
      YTEMP(I)=YPEN
300   CONTINUE
      YMEAN=(YTEMP(1)+YTEMP(2)+YTEMP(3)+YTEMP(4))/4
C   FIND LANE SEPARATION
      DXMIN=(XLANE(4)-XLANE(1))/3
      WRITE(SEPAR,1011)DXMIN
      CALL CURSOR(INSTR3,1)
      CALL BLINE
      CALL CURSOR(INSTR3,INSTC3)
      CALL WRITES('Mean lane separation = ')
      CALL WRITES(SEPAR)
      CALL CURSOR(INSTR2,1)
      CALL BLINE
      CALL CURSOR(INSTR2,INSTC2)
      CALL WRITES(
     +'If separation ok hit CONFIRM, else hit RESET')
1011  FORMAT(I6)
      CALL READPN(XPEN,YPEN,1,IDEVD,KBOUT)
C   IN MENU?
      CALL INTERP(XLMENU,XRMENU,YBMENU,YTMENU,MENUP,CHARS,
     +IDMENU,XPEN,YPEN,MENU,MENUE,KBOUT)
C
      IF(MENU.NE.56)GO TO 100
C   SET MINIMUM LANE CENTRE CLOSENESS TO 50% OF SEPARATION
      DXMIN=DXMIN/2
C   CLEAR ERROR LINE IN CASE ITS BEEN USED
      CALL CURSOR(IERRR,1)
      CALL BLINE
      RETURN
      END
C   LANE
C   RETURNS THE LANE NUMBER FOR GELIN
      INTEGER FUNCTION LANE(XLANE,YMEAN,DY,DXMIN,XPEN,YPEN)
C   AUTHOR RODGER STADEN
      INTEGER XLANE(4),YMEAN,D(4)
      INTEGER XPEN,YPEN,DXMIN,DY,X1,X2,Y1,Y2,DMIN
      EXTERNAL INOUT
      LANE=0
C   IS THE POINT IN THE LANES?
      X1=XLANE(1)-DXMIN
      X2=XLANE(4)+DXMIN
      Y1=YMEAN-DY
      Y2=YMEAN+DY
      IF(INOUT(X1,X2,Y1,Y2,XPEN,YPEN).EQ.0)RETURN
C
C   IN LANES
C  WHICH IS NEAREST XLANE VALUE
      DO 10 I=1,4
        D(I)=ABS(XLANE(I)-XPEN)
10    CONTINUE
C   WHICH IS LEAST?
      DMIN=D(1)
      ICLOSE=1
      DO 20 I=2,4
      IF(D(I).GT.DMIN)GO TO 20
C  CLOSER
      ICLOSE=I
      DMIN=D(I)
20    CONTINUE
C   CLOSEST TO ICLOSE SO SET THIS CHAR AND SHIFT THIS LANE CENTRE
      XLANE(ICLOSE)=XPEN
      YMEAN=YPEN
      LANE=ICLOSE
C   MAKE SURE LANES ARE NOT TOO CLOSE
      DO 30 I=2,4
        IF(ABS(XLANE(I-1)-XLANE(I)).LT.DXMIN)LANE=0
30    CONTINUE
C  RESET LANE SEPARATION
      DXMIN=(XLANE(4)-XLANE(1))/6
      RETURN
      END
C   BELL
C       SUBROUTINE TO RING BELL N TIMES
      SUBROUTINE GBELL(N,KBOUT)
C   AUTHOR RODGER STADEN
      CHARACTER CBELL
      COMMON /BELCHR/ CBELL
      CALL WRITES(CBELL)
      RETURN
      END
        SUBROUTINE OPENT(IDEV,FILNAM)
C   AUTHOR: RODGER STADEN
        CHARACTER FILNAM*(*)
        COMMON /TERM/IDEVT
      SAVE /TERM/
      PARAMETER (IERRR=24,IERRC=10)
      IDEVT=IDEV
      CALL OPENRS(IDEV,FILNAM,IOK,LRECL,10)
      IF(IOK.EQ.0)RETURN
      CALL CURSOR(IERRR,1)
      CALL BLINE
      CALL CURSOR(IERRR,IERRC)
      CALL WRITES('****ERROR OPENING SCREEN FOR OUTPUT****')
      RETURN
      END
      SUBROUTINE CLEAR
C   AUTHOR: RODGER STADEN
       CHARACTER STRING*4,CESC
      COMMON /ESCCHR/ CESC
      DATA STRING(2:4)/'[2J'/
      STRING(1:1)=CESC
C   CLEAR SCREEN
      CALL WRITES(STRING)
      RETURN
      END
      SUBROUTINE WRITES(CHARS)
C   AUTHOR: RODGER STADEN
      CHARACTER CHARS*(*)
      INTEGER NUL
        COMMON /TERM/IDEVT
      SAVE /TERM/
      DATA NUL/0/
C        WRITE(IDEVT,1000)NUL,CHARS
C1000  FORMAT(A1,A)
        WRITE(IDEVT,1000)CHARS
1000  FORMAT(A,$)
      CALL FLUSH(IDEVT)
      RETURN
      END
        SUBROUTINE CURSOR(LINE,COLUMN)
C   AUTHOR: RODGER STADEN
C   SETS CURSOR TO LINE AND COLUMN
      INTEGER COLUMN
       CHARACTER BLIN*2,BCOL*2,SPACE,ZERO,STRING*8,CESC
      COMMON /ESCCHR/ CESC
      EQUIVALENCE (STRING(3:4),BLIN),(STRING(6:7),BCOL)
      SAVE STRING,ZERO,SPACE
      DATA STRING(2:2)/'['/,STRING(8:8)/'H'/
      DATA STRING(5:5)/';'/
      DATA SPACE/' '/,ZERO/'0'/
      STRING(1:1)=CESC
      WRITE(BCOL,1002,ERR=100)COLUMN
      WRITE(BLIN,1002,ERR=100)LINE
1002  FORMAT(I2.2)
C  NEED TO SET SPACES TO ZEROS
C      IF(BCOL(1:1).EQ.SPACE)BCOL(1:1)=ZERO
C      IF(BLIN(1:1).EQ.SPACE)BLIN(1:1)=ZERO
      CALL WRITES(STRING)
      RETURN
100   CONTINUE
      WRITE(*,*)'ERROR IN CURSOR SUBROUTINE'
      RETURN
      END
      SUBROUTINE BLINE
      CHARACTER BLANK*79
      SAVE BLANK
      DATA BLANK/' '/
      CALL WRITES(BLANK)
      RETURN
      END
      SUBROUTINE FMTDK(IDEV,SEQNCE,IDIM)
C   AUTHOR: RODGER STADEN
      CHARACTER SEQNCE(IDIM)
C   SET POINTERS TO FIRST AND LAST ELEMENTS ONE WRITE
      JS=1
      JE=60
10    CONTINUE
C   SET JE TO LAST ELEMENT IF NECESSARY
      IF(JE.GT.IDIM)JE=IDIM
      WRITE(IDEV,1002)(SEQNCE(I),I=JS,JE)
1002  FORMAT(' ',60A1)
C   TEST FOR END
      IF(JE.EQ.IDIM)GO TO 20
C   INCREMENT FIRST AND LAST POINTERS
      JS=JE+1
      JE=JE+60
      GO TO 10
20    CONTINUE
      RETURN
      END
