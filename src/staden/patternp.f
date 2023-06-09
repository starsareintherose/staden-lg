C 22-7-91 fixed bug in dsplay: graphics did not work
C 2-3-92 set filnam = ' ' for some calls to openf1
      SUBROUTINE PATTEN(SEQ,IDSEQ,STRING,MAXSTR,
     +LENGTH,CLASS,RELMOT,RANGES,RANGEL,RANGET,RANGEM,IENTRY,
     +START2,IEND2,WTSTR,START,IEND,MATCHQ,RELEND,MATCHP,
     +STRNGS,LAST5,LAST3S,LAST3E,MATCHS,CUTOFF,WEIGHT,FILNAM,
     +MAXMOT,MAXWTS,MATRIX,
     +IDEV1,IDEV2,IDEV3,
     +MARGL,MARGR,MARGB,MARGT,ISXMAX,ISYMAX,KSTART,KBIN,KBOUT,
     +IDEV4,IDM,COMBIN,MAT1,NAMSAV,KEYNS,CHRSET,
     +IHELPS,IHELPE,HELPF,IDEVH,FOFNAM,IDEVFF)
      INTEGER LENGTH(MAXMOT),CLASS(MAXMOT),RELMOT(MAXMOT)
      INTEGER RANGES(MAXMOT),RANGEL(MAXMOT)
      INTEGER RANGET(MAXMOT),RANGEM(MAXMOT),IENTRY(MAXMOT)
      INTEGER START2(MAXMOT),IEND2(MAXMOT)
      CHARACTER SEQ(IDSEQ),STRING(MAXSTR)
      INTEGER WTSTR(MAXMOT),START(MAXMOT),IEND(MAXMOT)
      INTEGER MATCHQ(MAXMOT),RELEND(MAXMOT)
      INTEGER MATCHP(MAXMOT),STRNGS(MAXMOT)
      INTEGER LAST5(MAXMOT),LAST3S(MAXMOT),LAST3E(MAXMOT)
      INTEGER MAT1(IDM,IDM),MATRIX(IDM,IDM)
      REAL WEIGHT(MAXWTS),CUTOFF(MAXMOT),MATCHS(MAXMOT)
      REAL MINSCR,MAXSCR
      CHARACTER FILNAM*(*),HELPF*(*),FOFNAM*(*),TITLE*80
      CHARACTER COMBIN(MAXMOT),CHRSET(IDM)
      CHARACTER*(*) NAMSAV(MAXMOT),KEYNS(MAXMOT)
      PARAMETER (MAXCLS = 6)
      PARAMETER (SMALL=1.0E-5)
      PARAMETER (MAXPRM = 30)
      CHARACTER PROMPT(4)*(MAXPRM)
      WRITE(KBOUT,*)'  Pattern searcher'
      JDEV = KBIN
      PROMPT(1) = 'Use keyboard'
      PROMPT(2) = 'Use pattern file'
      PROMPT(3) = 'Use file of pattern file names'
      IN = 1
      CALL RADION('Select pattern definition mode',PROMPT,3,IN,
     +IHELPS,IHELPE,HELPF,IDEVH,KBIN,KBOUT)
      IF(IN.LT.1) RETURN
      IF(IN.EQ.2)THEN
        FILNAM = ' '
        CALL OPENF1(IDEV3,FILNAM,0,IOK,KBIN,KBOUT,
     +  'Pattern definition file',
     +  IHELPS,IHELPE,HELPF,IDEVH)
        IF(IOK.NE.0)RETURN
        JDEV = IDEV3
      ELSE IF(IN.EQ.3) THEN
        FOFNAM = ' '
        CALL OPENF1(IDEVFF,FOFNAM,0,IOK,KBIN,KBOUT,
     +  'File of pattern file names',
     +  IHELPS,IHELPE,HELPF,IDEVH)
        IF(IOK.NE.0)RETURN
        JDEV = IDEV3
      END IF
      IOPT = 1
      PROMPT(1) = 'Motif by motif'
      PROMPT(2) = 'Inclusive'
      PROMPT(3) = 'Graphical'
      PROMPT(4) = 'EMBL feature table'
      CALL RADION('Select results display mode',PROMPT,4,IOPT,
     +IHELPS,IHELPE,HELPF,IDEVH,KBIN,KBOUT)
      IF(IOPT.LT.1) GO TO 201
      IF((IN.EQ.3).AND.(IOPT.EQ.3)) IOPT = 2
      PMINC = 1.
      CUTSCR = -99999.
      NPAT = 0
      IPROB = 0
      NTOTAL = 0
      JTOTAL = 0
      CALL SETCMP(SEQ,IDSEQ,IDM)
      IF(IN.EQ.3) THEN
        IPROB = 1
        CALL BUSY(KBOUT)
      END IF
10    CONTINUE
      IF(IN.EQ.3) THEN
        READ(IDEVFF,1000,ERR=100,END=200)FOFNAM
        CALL OPENRS(IDEV3,FOFNAM,IOK,LRECL,2)
        IF(IOK.NE.0) GO TO 200
        NPAT = NPAT + 1
      END IF
1000  FORMAT(A)
        CALL FILLI(LENGTH,MAXMOT,0)
        CALL FILLI(CLASS,MAXMOT,0) 
        CALL FILLI(RELMOT,MAXMOT,0)
        CALL FILLI(RANGES,MAXMOT,0)
        CALL FILLI(RANGEL,MAXMOT,0)
        CALL FILLI(RANGET,MAXMOT,0)
        CALL FILLI(RANGEM,MAXMOT,0)
        CALL FILLI(IENTRY,MAXMOT,0)
        CALL FILLI(START2,MAXMOT,0)
        CALL FILLI(IEND2,MAXMOT,0) 
        CALL FILLI(WTSTR,MAXMOT,0) 
        CALL FILLI(START,MAXMOT,0) 
        CALL FILLI(IEND,MAXMOT,0)  
        CALL FILLI(MATCHQ,MAXMOT,0)
        CALL FILLI(RELEND,MAXMOT,0)
        CALL FILLI(MATCHP,MAXMOT,0)
        CALL FILLI(STRNGS,MAXMOT,0)
        CALL FILLI(LAST5,MAXMOT,0) 
        CALL FILLI(LAST3S,MAXMOT,0)
        CALL FILLI(LAST3E,MAXMOT,0)
        CALL FILLR(CUTOFF,MAXMOT,0.0)
        CALL FILLR(MATCHS,MAXMOT,0.0)
        CALL FILLR(WEIGHT,MAXWTS,0.0)
        CALL FILLC(COMBIN,MAXMOT,'A')
        DO 1 I = 1,MAXMOT
          NAMSAV(I) = 'FILENOTUSED'
C          WRITE(*,*)NAMSAV(I)
1       CONTINUE        
        ITOTAL = 0
C
C
C
C  GET MOTIF DEFINITIONS
C
C  RETURN STRING LENGTH FOR COMPATIBILITY WITH LIB SEARCH
      NSTRNG = MAXSTR
      IF(IN.EQ.1) THEN
        CALL GETMK(KBIN,KBOUT,STRING,NSTRNG,ISTRNG,
     +  LENGTH,MAXMOT,CLASS,RELMOT,RANGES,RANGEL,
     +  RANGET,RANGEM,
     +  STRNGS,NMOT,WEIGHT,MAXWTS,CUTOFF,IDEV2,
     +  WTSTR,JDEV,IOK,RELEND,IDSEQ,IDEV4,IDM,COMBIN,
     +  MAXCLS,MATRIX,MAT1,
     +  PMINT,PMAXT,PROBT,EXPTT,CHRSET,
     +  IHELPS,IHELPE,HELPF,IDEVH,KEYNS,NAMSAV,FILNAM,TITLE)
      ELSE
        CALL GETMF(KBIN,KBOUT,STRING,NSTRNG,ISTRNG,
     +  LENGTH,MAXMOT,CLASS,RELMOT,RANGES,RANGEL,
     +  RANGET,RANGEM,
     +  STRNGS,NMOT,WEIGHT,MAXWTS,CUTOFF,IDEV2,
     +  WTSTR,JDEV,IOK,RELEND,IDSEQ,IDEV4,IDM,COMBIN,
     +  MAXCLS,MATRIX,MAT1,
     +  PMINT,PMAXT,PROBT,EXPTT,CHRSET,
     +  IHELPS,IHELPE,HELPF,IDEVH,KEYNS,NAMSAV,FILNAM,IPROB,TITLE)
      END IF
      IF(IOK.NE.0) GO TO 201
      IF(NMOT.LT.1) GO TO 201
C
C
C  DISPLAY THE SIGNAL DESCRIPTION
C
      RANGES(1) = 1
      IF(IN.NE.3) THEN
        CALL DESSIG(
     +            KBOUT,STRING,MAXSTR,
     +            LENGTH,CLASS,RELMOT,RANGES,RANGEL,
     +            RANGET,RANGEM,
     +            STRNGS,NMOT,WEIGHT,MAXWTS,CUTOFF,
     +            WTSTR,RELEND,COMBIN,KEYNS,TITLE)
C
C
        WRITE(KBOUT,2003)PROBT
2003    FORMAT(' Probability of finding pattern = ',E10.4)
        WRITE(KBOUT,2004)EXPTT
2004    FORMAT(' Expected number of matches  = ',E10.4)
        CALL GETRL(0.,1.,1.0,'Maximum pattern probability',
     +  XP,KBIN,KBOUT,IHELPS,IHELPE,HELPF,IDEVH,IOK)
        IF(IOK.NE.0) GO TO 201
        PMINC = XP
        CALL GETRL(-9999.,9999.,-9999.,'Minimum pattern score',
     +  XP,KBIN,KBOUT,IHELPS,IHELPE,HELPF,IDEVH,IOK)
        IF(IOK.NE.0) GO TO 201
        CUTSCR = XP
        MINSCR = 9999999.
        MAXSCR = -9999999.
        YMIN = LOG(1.-PMINT)
        YMAX = LOG(1.-PMAXT)
        YDEL = ABS(YMIN - YMAX)
        IF(YDEL.LT.SMALL)THEN
          YMAX = YMAX + SMALL
          YMIN = YMIN - SMALL
        END IF
        YDEL = ABS(YMIN - YMAX) * 0.1
        YMIN = YMIN - YDEL
        YMAX = YMAX + YDEL
        CALL BUSY(KBOUT)
      END IF
      IF(IOPT.EQ.3)THEN
        XMAX = IDSEQ
        XMIN = KSTART
        CALL VECTOM
        CALL FRAME(MARGL,MARGR,MARGB,MARGT,ISXMAX,ISYMAX)
        CALL CLEARV
      END IF
C
C  DO THE SEARCH
C
      CALL SRCSIG(KBIN,KBOUT,WTSTR,LENGTH,CLASS,
     +RANGES,RANGEL,START,IEND,RELMOT,MATCHP,STRNGS,WEIGHT,
     +MAXWTS,CUTOFF,MATCHS,NMOT,STRING,MAXSTR,SEQ,IDSEQ,
     +RANGET,RANGEM,IENTRY,START2,IEND2,MATRIX,MATCHQ,RELEND,
     +IDEV1,LAST5,LAST3S,LAST3E,MARGL,MARGR,MARGB,MARGT,ISXMAX,
     +ISYMAX,IOPT,XMAX,XMIN,YMAX,YMIN,ITOTAL,KSTART,IDM,COMBIN,
     +CUTSCR,MINSCR,MAXSCR,MAT1,PMINT,PMAXT,PROBT,PMINC,KEYNS)
      CALL VT100M
      IF(IN.EQ.3) THEN
        IF(ITOTAL.NE.0) THEN
          WRITE(IDEV1,1003)TITLE
 1003      FORMAT(' ',A)
          WRITE(IDEV1,1001)FOFNAM,ITOTAL
1001      FORMAT(' ',A,' ',I6,/)
          NTOTAL = NTOTAL + ITOTAL
          JTOTAL = JTOTAL + 1
        END IF
        GO TO 10
      END IF
      WRITE(KBOUT,1002)ITOTAL
1002  FORMAT(' Total matches found',I7)
      IF((ITOTAL.GT.0).AND.(IN.NE.3))WRITE(KBOUT,1006)MINSCR,MAXSCR
1006  FORMAT(' Minimum and maximum observed scores',2F12.2)
      RETURN
100   CONTINUE
      WRITE(KBOUT,*)'Error reading file of file names'
200   CONTINUE
      WRITE(KBOUT,*)'Of the ',NPAT,' patterns processed'
      WRITE(KBOUT,*)JTOTAL,' gave a total of',NTOTAL,' matches'
201   CONTINUE
      CLOSE(UNIT=IDEVFF)
      END
      SUBROUTINE SRCSIG(KBIN,KBOUT,WTSTR,LENGTH,CLASS,
     +RANGES,RANGEL,START,IEND,RELMOT,MATCHP,STRNGS,WEIGHT,
     +MAXWTS,CUTOFF,MATCHS,NMOT,STRING,MAXSTR,SEQ,IDSEQ,
     +RANGET,RANGEM,IENTRY,START2,IEND2,MATRIX,MATCHQ,RELEND,
     +IDEVOT,LAST5,LAST3S,LAST3E,MARGL,MARGR,MARGB,MARGT,ISXMAX,
     +ISYMAX,IOPT,XMAX,XMIN,YMAX,YMIN,ITOTAL,KSTART,IDM,COMBIN,
     +CUTSCR,MINSCR,MAXSCR,MAT1,PMINT,PMAXT,PROBT,PMINC,KEYNS)
C  ROUTINE TO SEARCH FOR SIGNALS COMPOSED OF MOTIFS
C  WEIGHT  = WEIGHTS FOR MATRICES
C  CUTOFF  = CUTOFF SCORES
C  LENGTH  = MOTIF LENGTHS
C  CLASS   = MOTIF CLASS
C  COMBIN  = LOGICAL COMBINATION A, O, N
C  WTSTR   = POINTER TO WEIGHT STARTS
C  RANGES  = RANGES START
C  RANGEL  = RANGE LENGTH (A DISTANCE MEASURED FROM RANGES)
C  RELMOT  = MOTIF NUMBER THAT A RANGE IS RELATIVE TO IE THE
C            FIRST MOTIF'S RANGE IS RELATIVE TO MOTIF 0, BUT
C            ANY OTHER MOTIF MAY HAVE TO BE DEFINED RELATIVE
C            TO ANY OTHER. THE MOST COMMON WOULD BE THE FIRST
C            MOTIF OR THE LAST ONE SEARCHED FOR.
C  RELEND  = IS A SPECIAL CASE FLAG FOR STEMS. IT ALLOWS OTHER
C            MOTIFS TO HAVE THEIR POSITIONS RELATIVE TO THE 3' SIDE
C            OF A STEM. IT IS 5 FOR THE 5 PRIME SIDE, 3 FOR 3' SIDE
C  START   = RANGE START DURING SEARCH (SOME POSITIONS MAY HAVE
C            BEEN TRIED)
C  IEND    = RANGE END POSITION FOR CURRENT INITIAL START (WHEN IT
C            IS RESET DEPENDS ON WHETHER IT IS DEFINED RELATIVE TO 
C            THE FIRST OR THE PREVIOUS MOTIF. IF IT IS DEFINED RELATIVE
C            TO THE FIRST MOTIF IT IS RESET WHEN WE FIND A MATCH FOR THE
C            FIRST MOTIF. IF IT IS DEFINED RELATIVE TO THE PREVIOUS MOTIF
C            WE MUST RESET WHEN WE MOVE FORWARD ONE MOTIF. I THINK THIS
C            CAN BE TAKEN CARE OF BY UPDATING ALL THOSE MOTIFS THAT ARE
C            DEFINED TO THE CURRENT MOTIF EVERY TIME WE MOVE FORWARD 
C            ONE MOTIF (IE IT INCLUDES THE FIRST MOTIF SO IT IS NOT A
C            SPECIAL CASE). OTHERWISE WE UPDATE POSITIONS WHEN WE FIND
C            A MATCH FOR THEM (WE SET TO THE MATCH POSITION PLUS 1)
C  MATCHP  = LIST OF CURRENT MATCH POSITIONS FOR EACH MOTIF
C  MATCHS  = LIST OF CURRENT MATCH SCORES FOR EACH MOTIF
C  IFOUND  = A FLAG TO INDICATE SUCCESS OR FAILURE OF A SEARCH ROUTINE
C            1 = SUCCESS, 0 = FAIL
C  STRNGS  = POINTER TO STRING STARTS IN CHARACTER ARRAY STRING
C  TEMPORARY VALUES ARE:
C  MOTIF   = ACTUAL MOTIF NUMBER
C  ICLASS  = CLASS
C  ILEN    = LENGTH OF MOTIF
C  CUT     = CUTOFF
C  WT      = START OF WEIGHTS FOR THIS MOTIF
C  ISTRST  = START OF STRING
C  RANGET  = START OF 3' RANGE FOR STEM SEARCHES
C  RANGEM  = END OF 3' RANGE FOR STEM SEARCHES
C  IENTRY  = FLAG TO SIGNIFY MORE 3' STEM POSITIONS FOR LAST 5' START
C            0 = NONE, ON RETURN FROM MOTIF6 IT CONTAINS THE 3' MATCH 
C            POSITION
C  MATCHQ  = MATCH POSITION FOR STEM SEARCH
C  COMB    = LOGICAL COMBINATION A, O, N
      INTEGER WTSTR(NMOT),LENGTH(NMOT),CLASS(NMOT)
      INTEGER RANGES(NMOT),RANGEL(NMOT),START(NMOT),IEND(NMOT)
      INTEGER RELMOT(NMOT),MATCHP(NMOT),STRNGS(NMOT)
      INTEGER RANGET(NMOT),RANGEM(NMOT),IENTRY(NMOT),RELEND(NMOT)
      INTEGER START2(NMOT),IEND2(NMOT),MATRIX(IDM,IDM),MATCHQ(NMOT)
      INTEGER LAST5(NMOT),LAST3S(NMOT),LAST3E(NMOT),MAT1(IDM,IDM)
      REAL WEIGHT(MAXWTS),CUTOFF(NMOT),MATCHS(NMOT)
      REAL MINSCR,MAXSCR
      CHARACTER SEQ(IDSEQ),STRING(MAXSTR)
      CHARACTER COMBIN(NMOT),COMB
      CHARACTER*(*) KEYNS(NMOT)
C
C
C  INITIALIZE
      JMOT = 0
      IRET = 0
5     CONTINUE
      JMOT = JMOT + 1
      IF(JMOT.LE.NMOT)THEN
        IF(RELMOT(JMOT).EQ.0)THEN
          START(JMOT) = RANGES(1)
          IEND(JMOT) = RANGES(1) + RANGEL(1) -1
          GO TO 5
        END IF
      END IF
      MOTIF  = 1
      ICLASS = CLASS(1)
      ILEN   = LENGTH(1)
      CUT    = CUTOFF(1)
      IWT    = WTSTR(1)
      ISTRST = STRNGS(1)
      IENTRY(1) = 0
      COMB   = COMBIN(1)
      DO 10 I = 1,NMOT
        MATCHP(I) = 0
10    CONTINUE
C
C
C
C
C
100   CONTINUE
C
C
C  THIS A CLASS CLASS MOTIF, PERFORM THE APPROPRIATE SEARCH IF THE START 
C  POSITION IS >0. (IF IT IS NOT THE CURRENT MOTIF IS A NOT THAT HAS
C  ALREADY BEEN SEARCHED FOR
C
      IFOUND = 0
      IF(START(MOTIF).GT.0)THEN
C
C
      IF(ICLASS.EQ.1)THEN
        CALL MOTIF1(SEQ,IDSEQ,STRING(ISTRST),ILEN,START(MOTIF),
     +  IEND(MOTIF),MATCHP(MOTIF),MATCHS(MOTIF),IFOUND,
     +  CUTOFF(MOTIF),0)
      ELSE IF(ICLASS.EQ.2)THEN
        CALL MOTIF2(SEQ,IDSEQ,STRING(ISTRST),ILEN,START(MOTIF),
     +  IEND(MOTIF),CUT,MATCHP(MOTIF),MATCHS(MOTIF),IFOUND)
      ELSE IF(ICLASS.EQ.3)THEN
        CALL MOTIF3(SEQ,IDSEQ,STRING(ISTRST),ILEN,START(MOTIF),
     +  IEND(MOTIF),CUT,MATCHP(MOTIF),MATCHS(MOTIF),IFOUND,MATRIX,IDM)
      ELSE IF(ICLASS.EQ.4)THEN
        CALL MOTIF4(SEQ,IDSEQ,ILEN,START(MOTIF),
     +  IEND(MOTIF),WEIGHT(IWT),CUT,MATCHP(MOTIF),MATCHS(MOTIF),
     +  IFOUND,IDM)
      ELSE IF(ICLASS.EQ.5)THEN
      CALL MOTIF8(SEQ,IDSEQ,MATRIX,LENGTH(MOTIF),START(MOTIF),
     +            IEND(MOTIF),RANGET(MOTIF),RANGEM(MOTIF),
     +            CUTOFF(MOTIF),MATCHP(MOTIF),MATCHS(MOTIF),
     +            IENTRY(MOTIF),IFOUND,MATCHQ(MOTIF),
     +            LAST5(MOTIF),LAST3S(MOTIF),LAST3E(MOTIF),IDM)
      ELSE IF(ICLASS.EQ.6)THEN
        CALL MOTIF4(SEQ,IDSEQ,ILEN,START(MOTIF),
     +  IEND(MOTIF),WEIGHT(IWT),CUT,MATCHP(MOTIF),MATCHS(MOTIF),
     +  IFOUND,IDM)
C        CALL MOTIF6(SEQ,IDSEQ,STRING(ISTRST),ILEN,START(MOTIF),
C     +  IEND(MOTIF),MATCHP(MOTIF),MATCHS(MOTIF),IFOUND,
C     +  CUTOFF(MOTIF),0)
      ELSE
        WRITE(KBOUT,*)'UNKNOWN CLASS!!'
      END IF
C
C
      END IF
C
C
C   MATCH FOUND WHEN MATCH WANTED ?
C
C
C
      IF(((IFOUND.EQ.0).AND.(COMB.NE.'N')).OR.
     +   ((IFOUND.GT.0).AND.(COMB.EQ.'N')))THEN
C
C   NO SO GO BACK OR SIDEWAYS ONE MOTIF
C
C
        CALL BAKSID(CLASS,LENGTH,CUTOFF,STRNGS,NMOT,
     +  MOTIF,ICLASS,ILEN,CUT,IWT,ISTRST,WTSTR,
     +  RELMOT,START,IEND,MATCHQ,RANGES,RANGEL,RELEND,IRET,MATCHP,
     +  COMBIN,COMB)
C
C
C   IF CANT GO BACK ANY FURTHER QUIT
        IF(IRET.NE.0)RETURN
C
C
      ELSE
C
C
C   MATCH FOUND. 
C
C
C   PREPARE FOR NEXT SEARCH THIS MOTIF BY INCREMENTING POINTER
C   TO SEARCH RANGE (NOT FOR CLASS 6 WHICH IS HANDLED BY MOTIF6)
C
C
       IF(COMB.EQ.'N')THEN
         START(MOTIF) = -9
       ELSE
         IF(ICLASS.NE.5) START(MOTIF) = MATCHP(MOTIF) + 1
       END IF
C
C   TRY GOING FORWARD ONE MOTIF
C
C
      CALL FORWAD(CLASS,LENGTH,CUTOFF,NMOT,
     +  MOTIF,ICLASS,ILEN,CUT,IWT,RELMOT,START,IEND,
     +  RANGES,RANGEL,STRNGS,ISTRST,WTSTR,IDSEQ,IENTRY,
     +  RANGET,RANGEM,START2,IEND2,MATCHQ,RELEND,MATCHP,IDSPLY,
     +  COMBIN,COMB)
C
C
C
C   IS THIS THE LAST MOTIF? IF SO DISPLAY THE MATCH
C
C
        IF(IDSPLY.EQ.1)THEN
          ITOTAL = ITOTAL + 1
          CALL DSPLAY(MATCHP,LENGTH,NMOT,SEQ,IDSEQ,IDEVOT,
     +    CLASS,MATCHQ,MARGL,MARGR,MARGB,MARGT,ISXMAX,ISYMAX,IOPT,
     +    XMAX,XMIN,YMAX,YMIN,KSTART,MATCHS,CUTSCR,MINSCR,MAXSCR,
     +    MATRIX,MAT1,IDM,PMINT,PMAXT,PROBT,
     +    WEIGHT,MAXWTS,WTSTR,CUTOFF,PMINC,KEYNS)
C
C
C  HORRIBLE SPECIAL CASE - IF LAST MOTIF IS NOTTED WE MUST MOVE BACK AFTER
C  DISPLAY
          IF(COMB.EQ.'N')THEN
            CALL BAKSID(CLASS,LENGTH,CUTOFF,STRNGS,NMOT,
     +      MOTIF,ICLASS,ILEN,CUT,IWT,ISTRST,WTSTR,
     +      RELMOT,START,IEND,MATCHQ,RANGES,RANGEL,RELEND,IRET,MATCHP,
     +      COMBIN,COMB)
C
C
C   IF CANT GO BACK ANY FURTHER QUIT
            IF(IRET.NE.0)RETURN
          END IF
        END IF
C
C
      END IF
C
C
C   GO BACK FOR NEXT SEARCH
C
C
      GO TO 100
      END
      SUBROUTINE GETMK(KBIN,KBOUT,STRING,MAXSTR,ISTRNG,
     +LENGTH,MAXMOT,CLASS,RELMOT,RANGES,RANGEL,RANGET,RANGEM,
     +STRNGS,NMOT,WEIGHT,MAXWTS,CUTOFF,IDEV,WTSTR,JDEV,IOK,
     +RELEND,IDSEQ,IDEVSV,IDM,COMBIN,MAXCLS,MATRIX,MAT1,
     +PMINT,PMAXT,PROBT,EXPTT,CHRSET,
     +IHELPS,IHELPE,HELPF,IDEVH,KEYNS,NAMSAV,FILNAM,TITLE)
C  MAT1 SIMPLE IDENTITY
C  MAT2 IUB SCORES 0-1
C  MAT3 IUB SCORES 0-36
C  MAT4 INVERTED REPEAT
      INTEGER LENGTH(MAXMOT),CLASS(MAXMOT),RELMOT(MAXMOT)
      INTEGER RANGES(MAXMOT),RANGEL(MAXMOT),STRNGS(MAXMOT)
      INTEGER WTSTR(MAXMOT),RANGET(MAXMOT),RANGEM(MAXMOT)
      INTEGER RELEND(MAXMOT),MATRIX(IDM,IDM),MAT1(IDM,IDM)
      CHARACTER STRING(MAXSTR),FILNAM*(*),HELPF*(*)
      REAL WEIGHT(MAXWTS),CUTOFF(MAXMOT)
      CHARACTER COMBIN(MAXMOT),TCLASS,CHRSET(IDM)
      CHARACTER*(*) KEYNS(MAXMOT)
      CHARACTER*(*) NAMSAV(MAXMOT),TITLE
      PARAMETER (MAXPRM = 31)
      CHARACTER PROMPT(7)*(MAXPRM)
      EXTERNAL PSCORE
C   GETS PATTERN DEFINITIONS IN TERMS OF MOTIFS
      PMINT = 1.0
      PMAXT = 1.0
      PROBT = 1.0
      EXPTS = 0.0
      PROBS = 0.0
      PMINS = 0.0
      PMAXS = 0.0
      EXPTT = 1.0
      IOK = 0
      IPROB = 0
      DO 10 I=1,MAXMOT
        RELEND(I) = 5
        COMBIN(I) = 'A'
10    CONTINUE
      RANGES(1) = 0
      RANGEL(1) = IDSEQ
      IREL = 0
C   COUNT MOTIFS
      NMOT = 0
      MOTIF = 0
C   SET POINTER TO SEARCH STRINGS ARRAY
      ISTRNG = 1
C   SET POINTER TO WEIGHT ARRAY
      IWT = 1
      TCLASS = 'A'
C   SET CLASS TO EXACT MATCH
      ICLASS = 1
100   CONTINUE
      IOK = 1
      PROMPT(1) = 'Exact match'
      PROMPT(2) = 'Percentage match'
      PROMPT(3) = 'Cut-off score and score matrix'
      PROMPT(4) = 'Cut-off score and weight matrix'
      PROMPT(5) = 'Direct repeat'
      PROMPT(6) = 'Membership of set'
      PROMPT(7) = 'Pattern complete'
      CALL RADION('Select motif definition mode',PROMPT,7,ICLASS,
     +IHELPS,IHELPE,HELPF,IDEVH,KBIN,KBOUT)
      IF(ICLASS.LT.1) RETURN
      IF(ICLASS.EQ.7)GO TO 900
101   CONTINUE
      LKEY = 0
      CALL GTSTR('Motif name',' ',KEYNS(MOTIF+1),LKEY,
     +KBOUT,KBIN,INFLAG)
      IF(INFLAG.EQ.2) RETURN
      IF(INFLAG.EQ.1) THEN
        CALL HELP2(IHELPS,IHELPE,HELPF,IDEVH,KBIN,KBOUT)
        GO TO 101
      END IF
      IF(MOTIF.GT.0) THEN
        IF(COMBIN(MOTIF).NE.'N') THEN
          IC = 1
          PROMPT(1) = 'And'
          PROMPT(2) = 'Or'
          PROMPT(3) = 'Not'
          CALL RADION('Select logical operator',PROMPT,3,IC,
     +    IHELPS,IHELPE,HELPF,IDEVH,KBIN,KBOUT)
          IF(IC.LT.1) RETURN
          IF(IC.EQ.1) TCLASS = 'A'
          IF(IC.EQ.2) TCLASS = 'O'
          IF(IC.EQ.3) TCLASS = 'N'
        ELSE
          IC = 1
          PROMPT(1) = 'And'
          PROMPT(2) = 'Not'
          CALL RADION('Select logical operator',PROMPT,2,IC,
     +    IHELPS,IHELPE,HELPF,IDEVH,KBIN,KBOUT)
          IF(IC.LT.1) RETURN
          IF(IC.EQ.1) TCLASS = 'A'
          IF(IC.EQ.2) TCLASS = 'N'
        END IF
      END IF
      NMOT = NMOT + 1
      MOTIF = MOTIF + 1
      IF(NMOT.GT.MAXMOT)THEN
        WRITE(KBOUT,*)'Maximum number of motifs exceeded'
        GO TO 901
      END IF
      CLASS(MOTIF) = ICLASS
      COMBIN(MOTIF) = TCLASS
C  IF NOT THE FIRST MOTIF, AND AN ANDED MOTIF OR THE FIRST IN A LIST OF ORS
C  GET ITS RANGE ETC
      IOK = 0
      IF((MOTIF.GT.1).AND.(COMBIN(MOTIF).NE.'O'))THEN
C        WRITE(KBOUT,1002)
C1002    FORMAT(' RANGES ARE DEFINED RELATIVE TO OTHER MOTIFS',/,
C     +' AND BY STARTS AND DISTANCES.')
        IOK = 1
        MININ = 1
        MAXIN = MOTIF
211     CONTINUE
        MAXIN = MAXIN - 1
        IF(COMBIN(MAXIN).NE.'A') GO TO 211
        IREL = MAXIN
        CALL GETINT(MININ,MAXIN,IREL,'Number of reference motif',
     +  IVAL,KBIN,KBOUT,IHELPS,IHELPE,HELPF,IDEVH,IOK)
        IF(IOK.NE.0) RETURN
        IREL = IVAL
        RELMOT(MOTIF) = IREL
        IOK = 1
        RELEND(MOTIF) = 5
        MININ = -1000
        MAXIN = 1000
        ID = LENGTH(IREL) + 1
        IF(CLASS(IREL).EQ.5) ID = 1
        CALL GETINT(MININ,MAXIN,ID,'Relative start position',
     +  IVAL,KBIN,KBOUT,IHELPS,IHELPE,HELPF,IDEVH,IOK)
        IF(IOK.NE.0) RETURN
        ID = IVAL
        RANGES(MOTIF) = ID - 1
        MININ = 0
        MAXIN = 1000
        IR = 0
        CALL GETINT(MININ,MAXIN,IR,'Number of extra positions',
     +  IVAL,KBIN,KBOUT,IHELPS,IHELPE,HELPF,IDEVH,IOK)
        IF(IOK.NE.0) RETURN
        IR = IVAL
        RANGEL(MOTIF) = IR
      ELSE IF(COMBIN(MOTIF).EQ.'O')THEN
C  NEED TO SET RANGES TO THOSE OF THE FIRST IN A SET OF ORED MOTIFS
C  SET TO THOSE OF THE PREVIOUS MOTIF BECAUSE IT MUST BE THE SAME
        RANGES(MOTIF) = RANGES(MOTIF-1)
        RANGEL(MOTIF) = RANGEL(MOTIF-1)
        RELEND(MOTIF) = RELEND(MOTIF-1)
        RELMOT(MOTIF) = IREL
      END IF
      CALL GETMC(KBIN,KBOUT,STRING,MAXSTR,ISTRNG,
     +LENGTH,MAXMOT,CLASS,RELMOT,RANGES,RANGEL,RANGET,RANGEM,
     +STRNGS,NMOT,WEIGHT,MAXWTS,CUTOFF,IDEV,WTSTR,JDEV,IOK,
     +RELEND,IDSEQ,IDEVSV,IDM,COMBIN,MAXCLS,MATRIX,MAT1,
     +PMINT,PMAXT,PROBT,EXPTT,EXPTS,PROBS,PMINS,PMAXS,CHRSET,
     +IHELPS,IHELPE,HELPF,IDEVH,MOTIF,IWT,NAMSAV,FILNAM,IPROB)
      IF(IOK.EQ.0) GO TO 100
901   CONTINUE
      WRITE(KBOUT,*)' Error in pattern definition'
      IOK = 1
      RETURN
900   CONTINUE
      EXPTT = EXPTT * EXPTS
      PROBT = PROBT * PROBS
      PMINT = PMINT * PMINS
      PMAXT = PMAXT * PMAXS
      CALL YESNO(IN,'Save pattern in a file',
     +IHELPS,IHELPE,HELPF,IDEVH,KBIN,KBOUT)
      IF(IN.LT.0)RETURN
      IF(IN.EQ.0)THEN
        FILNAM = ' '
        CALL OPENF1(IDEVSV,FILNAM,1,IOK,KBIN,KBOUT,
     +  'Pattern definition file',
     +  IHELPS,IHELPE,HELPF,IDEVH)
        IF(IOK.NE.0)RETURN
        CALL SAVSIG(
     +  IDEVSV,STRING,MAXSTR,
     +  LENGTH,CLASS,RELMOT,RANGES,RANGEL,
     +  RANGET,RANGEM,
     +  STRNGS,NMOT,WEIGHT,MAXWTS,CUTOFF,
     +  WTSTR,RELEND,NAMSAV,COMBIN,KEYNS,IDEV,FILNAM,
     +  KBIN,KBOUT,IHELPS,IHELPE,HELPF,IDEVH,CHRSET,IDM,TITLE)
        CLOSE(UNIT=IDEVSV)
      END IF
      DO 899 I = 2,NMOT
        IF(CLASS(I).EQ.5) THEN
          RANGEL(I) = RANGEL(I) + 1
        ELSE
          RANGEL(I) = RANGEL(I) + LENGTH(I)
        END IF
899   CONTINUE
C  RETURN STRING LENGTH
      MAXSTR = ISTRNG - 1
      IOK = 0
      END
      SUBROUTINE DSPLAY(MATCHP,LENGTH,NMOT,SEQ,IDSEQ,IDEV,
     +CLASS,MATCHQ,MARGL,MARGR,MARGB,MARGT,ISXMAX,ISYMAX,IOPT,
     +XMAX,XMIN,YMAX,YMIN,KSTART,MATCHS,CUTSCR,MINSCR,MAXSCR,
     +MATRIX,MAT1,IDM,PMINT,PMAXT,PROBT,
     +WEIGHT,MAXWTS,WTSTR,CUTOFF,PMINC,KEYNS)
      INTEGER MATCHP(NMOT),LENGTH(NMOT),CLASS(NMOT)
      INTEGER MATCHQ(NMOT)
      CHARACTER SEQ(IDSEQ)
      REAL MATCHS(NMOT),MINSCR,MAXSCR
      REAL WEIGHT(MAXWTS),CUTOFF(NMOT)
      INTEGER MAT1(IDM,IDM),MATRIX(IDM,IDM)
      INTEGER WTSTR(NMOT)
      CHARACTER*(*) KEYNS(NMOT)
      EXTERNAL PSCORE
C  ADD SCORES
      T = 0.
      DO 10 I = 1,NMOT
        IF(MATCHP(I).NE.0) T = T + MATCHS(I)
10    CONTINUE
      POBS = 1.0
      IF((PMINC.LT.1.0).OR.(IOPT.EQ.3))THEN
C  CALC PROB
        DO 20 I = 1,NMOT
          IF(MATCHP(I).NE.0)THEN
            CALL GETP(CLASS(I),SEQ(MATCHP(I)+KSTART-1),LENGTH(I),
     +      IDM,MATRIX,MAT1,WEIGHT(MAX(1,WTSTR(I))))
            PROB = PSCORE(MATCHS(I))
            POBS = POBS * PROB
          END IF
20      CONTINUE
        IF((PMINC.LT.1.0).AND.(POBS.GT.PMINC))RETURN
      END IF
C
      IF(T.GT.MAXSCR) MAXSCR = T
      IF(T.LT.MINSCR) MINSCR = T
      IF(T.LT.CUTSCR) RETURN
C
      IF(IOPT.EQ.1)THEN
      WRITE(IDEV,1001)
1001  FORMAT(' Match')
      DO 100 I=1,NMOT
        J = I
C  CHECK FOR NO MATCH (NEEDED FOR ORED MOTIFS)
        IF(MATCHP(J).NE.0)THEN
        WRITE(IDEV,1000)MATCHP(J)+KSTART-1,KEYNS(I)
        WRITE(IDEV,1002)(SEQ(K),K=MATCHP(J),MATCHP(J)+LENGTH(J)-1)
C  REPEAT ?
        IF(CLASS(J).EQ.5)THEN
          WRITE(IDEV,1002)(SEQ(K),K=MATCHQ(J),MATCHQ(J)+LENGTH(J)-1)
          WRITE(IDEV,1000)MATCHQ(J)+KSTART-1
        END IF
        END IF
100   CONTINUE
1000  FORMAT(' ',I7,' ',A8)
1002  FORMAT('       ',60A1)
      IF(PMINC.LT.1.0)WRITE(IDEV,1004)POBS
1004  FORMAT(' Probability =',E10.4)
      RETURN
      END IF
      IF(IOPT.EQ.3)THEN
C       MARK THE POSITION OF THE FIRST NON-ZERO MATCH ONLY
        DO 200 I = 1,NMOT
          IF(MATCHP(I).NE.0)THEN
            X = MATCHP(I) + KSTART - 1
            Y = LOG(1.-POBS)
            CALL VECTOM
            CALL LINE(X,X,YMIN,Y,XMAX,XMIN,YMAX,YMIN,
     +      MARGL,MARGR,MARGB,MARGT,ISXMAX,ISYMAX)
            CALL VT100M
            GO TO 201
          END IF
200     CONTINUE
201     CONTINUE
      END IF
      IF(IOPT.EQ.2)THEN
        MINP = 999999
        MAXP = -999999
        DO 300 I = 1, NMOT
          K = MATCHP(I)
          IF(K.NE.0)THEN
            IF(K.LT.MINP)MINP = K
            K = K + LENGTH(I) - 1
C  REPEAT ?
            IF(CLASS(I).EQ.5) K = MATCHQ(I) + LENGTH(I) - 1
            IF(K.GT.MAXP)MAXP = K
          END IF
300     CONTINUE
        WRITE(IDEV,1000)MINP+KSTART-1
        WRITE(IDEV,1002)
     +  ((SEQ(K1),K1=K2,MIN(K2+59,MAXP)),K2=MINP,MAXP,60)
        IF(PMINC.LT.1.0)WRITE(IDEV,1004)POBS
        RETURN
      END IF
      IF(IOPT.EQ.4)THEN
        MINP = 999999
        MAXP = -999999
        K1 = 0
        DO 400 I = 1, NMOT
          K = MATCHP(I)
          IF(K.NE.0)THEN
            IF(K1.EQ.0) INAM = I
            K1 = 1
            IF(K.LT.MINP)MINP = K
            K = K + LENGTH(I) - 1
C           REPEAT ?
            IF(CLASS(I).EQ.5) K = MATCHQ(I) + LENGTH(I) - 1
            IF(K.GT.MAXP)MAXP = K
          END IF
400     CONTINUE
        CALL EMBOUT(KEYNS(INAM),MINP+KSTART-1,MAXP+KSTART-1,' ',
     +  'Program',IDEV)
      END IF
      END
      SUBROUTINE MOTIF3(SEQ,IDIM1,STRING,IDIM2,ISTART,IEND,CUTOFF,
     +MATCHP,MATCHS,IFOUND,MATRIX,IDM)
      CHARACTER SEQ(IDIM1),STRING(IDIM2)
      INTEGER MATRIX(IDM,IDM)
      REAL MATCHS
      IFOUND = 0
      IF(ISTART.LT.1)ISTART=1
      IF(ISTART.GT.IDIM1)RETURN
      CALL SQFIT5(SEQ,IDIM1,STRING,IDIM2,ISTART,IEND,CUTOFF,MATCHS,
     +IFOUND,MATRIX,IDM)
      IF(IFOUND.EQ.0)RETURN
C  SAVE MATCH POSITION
      MATCHP = IFOUND
      RETURN
      END
C*********************************************************************
      SUBROUTINE SQFIT5(SEQ,IDIM1,STRING,IDIM2,
     1IS,IE,MINSC,MATCHS,IFOUND,MATRIX,IDM)
C   AUTHOR: RODGER STADEN
      CHARACTER SEQ(IDIM1),STRING(IDIM2)
      REAL MATCHS,MINSC
      INTEGER MATRIX(IDM,IDM),DTONUM
      EXTERNAL DTONUM
      MINSCR = MINSC
C
      IDIF=(IE-IS+2)-IDIM2
C   IDIF IS THE NUMBER OF POSNS TO TRY
C   IPSTR GOES FROM 1 TO IDIM2 IDIF TIMES
C   TRY ALL POSSIBLE POSITIONS FOR MATCHING AND SCORE FOR EACH
C   POINT TO ARRAY ELEMENT CORRESPONDING TO FIRST BASE
      IPSEQ=IS
      DO 200 I=1,IDIF
        NTOT=0
        IP=IPSEQ
        DO 100 J=1,IDIM2
          NTOT = NTOT + MATRIX(DTONUM(SEQ(IP)),DTONUM(STRING(J)))
          IP=IP+1
100     CONTINUE
C   END OF COUNTING FOR THIS POSITION.IS TOTAL HIGH ENOUGH?
        IF(NTOT.GE.MINSCR)THEN
          MATCHS = NTOT
          IFOUND = IP-IDIM2
          RETURN
        END IF
        IPSEQ=IPSEQ+1
200   CONTINUE
      IFOUND = 0
      RETURN
      END
C*********************************************************************
      SUBROUTINE MOTIF4(SEQ,IDIM1,LENGTH,ISTART,IEND,
     +WEIGHT,CUTOFF,MATCHP,MATCHS,IFOUND,IDM)
      REAL WEIGHT(IDM,LENGTH)
      REAL MATCHS
      CHARACTER SEQ(IDIM1)
      IFOUND = 0
      IF(ISTART.LT.1)ISTART=1
      L1 = IEND-ISTART+1
      IF(ISTART.GT.IDIM1)RETURN
      IF(L1.LT.LENGTH)RETURN
      CALL FMOT4(SEQ(ISTART),L1,WEIGHT,LENGTH,CUTOFF,SCORE,IFOUND,IDM)
      IF(IFOUND.EQ.0)RETURN
C  SAVE MATCH POSITION
      MATCHP = ISTART+IFOUND-1
      MATCHS = SCORE
      END
C*********************************************************************
      SUBROUTINE FMOT4(SEQ,IDIM,WT,LENGTH,CUTOFF,SUM,IFOUND,IDM)
C   AUTHOR: RODGER STADEN
      CHARACTER SEQ(IDIM)
      REAL WT(IDM,LENGTH)
      INTEGER CTONUM
      EXTERNAL CTONUM
      DO 10 I=1,IDIM-LENGTH+1
        SUM=0.
        K=0
        DO 5 J=I,I+LENGTH-1
          K=K+1
          IP = CTONUM(SEQ(J))
          SUM=SUM+WT(IP,K)
5       CONTINUE
        IF(SUM.GE.CUTOFF)THEN
        IFOUND = I
        RETURN
        END IF
10    CONTINUE
      IFOUND = 0
      END
C*********************************************************************
      SUBROUTINE MOTIF8(SEQ,IDSEQ,MATRIX,LENGTH,I5STAR,I5END,
     +                  I3STAR,I3END,CUTOFF,MATCHP,MATCHS,
     +                  IENTRY,IFOUND,MATCHQ,
     +                  LAST5,LAST3S,LAST3E,IDM)
C   AUTHOR: RODGER STADEN
      CHARACTER SEQ(IDSEQ)
      INTEGER MATRIX(IDM,IDM),REPEAT
      REAL MATCHS
      EXTERNAL REPEAT
C  WE HAVE A START POSITION FOR THE 5' END OF THE 5' END OF
C  A POTENTIAL REPEAT I5STAR AND AN END DEFINED BY A RANGE I5END
C  WE HAVE A REPEAT LENGTH LENGTH
C  WE HAVE A RANGE OF POSITIONS FOR THE 3' STEM TO START
C  I3STAR TO I3END
C  TRY THE TIGHTEST LOOPS FIRST
C  BUT FIRST WE MAY HAVE TO FINISH A PREVIOUS SEARCH
C  THIS IS DENOTED BY IENTRY NE 0.
C  NOTE IENTRY IS ALSO USED TO RETURN THE 3' MATCH POSITION
      ICUT = CUTOFF
C      WRITE(*,*)'ICUT',ICUT
      IFOUND = 0
      JENTRY = IENTRY
      IENTRY = 0
      IF(I5STAR.LT.1)I5STAR=1
      IF((I5STAR+I3STAR+LENGTH-2).GT.IDSEQ)RETURN
      IF(JENTRY.NE.0)THEN
        I1 = LAST5
C        WRITE(*,*)'I1,LAST3S,LAST3E',I1,LAST3S,LAST3E
        DO 50 J=LAST3S+1,LAST3E
          J1 = J
          ISUM = REPEAT(SEQ,IDSEQ,MATRIX,LENGTH,I1,J1,IDM)
C  RETURN IF GOOD ENOUGH
          IF(ISUM.GE.ICUT)THEN
            MATCHP = I1
            IENTRY = J1
            MATCHQ = J1
            MATCHS = ISUM
            IFOUND = MATCHP
            LAST3S = J1
            RETURN
          END IF
50      CONTINUE
C  NOW MOVE 5' STEM START POSITION (WE HAVE JUST FINISHED THE LAST)
C  TO THE LAST MATCH + 1
        I5STAR = MATCHP + 1
      END IF
C  SET ENTRY FLAG TO ZERO TO SIGNIFY LAST SEARCH NOW COMPLETED
      IENTRY = 0
      ISUM = 0
      LOOPI1 = I5STAR
      IF((I5STAR+I3STAR+LENGTH-2).GT.IDSEQ)RETURN
      LOOPI2 = MIN(IDSEQ-2*LENGTH+1,I5END)
C      WRITE(*,*)'IDSEQ,LENGTH,I5STAR,I5END',
C     +IDSEQ,LENGTH,I5STAR,I5END
C
C  TRY ALL STEM STARTS FROM 5' START TO 5' END
C
C
      DO 200 I = LOOPI1,LOOPI2
C
C
        I1 = I
C
C   TRY ALL LOOPS FROM 3' START TO 3' END
C
        LOOPJ1 = I + I3STAR -1
        IF((LOOPJ1+LENGTH-1).GT.IDSEQ)RETURN
        LOOPJ2 = MIN(IDSEQ-LENGTH+1,I+I3END-1)
C      WRITE(*,*)'I3STAR,I3END',I3STAR,I3END
C
C
C
        DO 100 J = LOOPJ1,LOOPJ2
C
C
C
         J1 = J
C  IN REPEAT NOTE THAT
C  THE 5' END POINTER I1 GOES FORWARDS
C  THE 3' END POINTER J1 GOES FORWARDS
C
         ISUM = REPEAT(SEQ,IDSEQ,MATRIX,LENGTH,I1,J1,IDM)
C  RETURN IF GOOD ENOUGH
C         WRITE(*,*)ISUM
         IF(ISUM.GE.ICUT)THEN
           MATCHP = I1
           IENTRY = J1
           MATCHQ = J1
           MATCHS = ISUM
           IFOUND = MATCHP
C  SAVE CURRENT POSITION FOR LATER ENTRIES
           LAST5  = I1
           LAST3S = J1
           LAST3E = LOOPJ2
           RETURN
         END IF
100    CONTINUE
200   CONTINUE
      END
C*********************************************************************
      INTEGER FUNCTION REPEAT(SEQ,IDSEQ,MATRIX,LENGTH,I5P,I3P,IDM)
      CHARACTER SEQ(IDSEQ)
      INTEGER MATRIX(IDM,IDM),CTONUM
      EXTERNAL CTONUM
C  THE 5' END POINTER GOES FORWARDS
C  THE 3' END POINTER GOES FORWARDS
      L=0
      I5=I5P-1
      I3=I3P-1
      DO 100 I=1,LENGTH
       I5 = I5 + 1
       I3 = I3 + 1
C      WRITE(*,*)'I5,I3',I5,I3
       L5 = CTONUM(SEQ(I5))
       L3 = CTONUM(SEQ(I3))
       L = L + MATRIX(L5,L3)
100   CONTINUE
      REPEAT = L
      END
      SUBROUTINE MOTIF6(SEQ,IDIM1,STRING,IDIM2,ISTART,IEND,
     +MATCHP,MATCHS,IFOUND,CUT,JOB)
      CHARACTER SEQ(IDIM1),STRING(IDIM2)
      REAL MATCHS
      INTEGER CUT
      INTEGER INSET1
      EXTERNAL INSET1
      IFOUND = 0
      ISTEP = 1
C      IF(JOB.EQ.1)ISTEP = CUT
      IF(ISTART.LT.1)ISTART=1
      L1 = IEND-ISTART+1
      IF(ISTART.GT.IDIM1)RETURN
      IF(L1.LT.1)RETURN
      IFOUND = INSET1(SEQ(ISTART),L1,STRING,IDIM2,ISTEP)
      IF(IFOUND.EQ.0)RETURN
C  SAVE MATCH POSITION
      MATCHP = ISTART+IFOUND-1
      MATCHS = 1.
      END
      INTEGER FUNCTION INSET1(SEQ,IDIM,STRING,IDS,INC)
      CHARACTER SEQ(IDIM),STRING(IDS)
      INTEGER INSET
      EXTERNAL INSET
      INSET1 = 0
      ISEQ = 1 - INC
10    CONTINUE
        ISEQ = ISEQ + INC
        IF(ISEQ.GT.IDIM)RETURN
        IMATCH = INSET(SEQ(ISEQ),STRING,IDS)
        IF(IMATCH.EQ.1)THEN
          INSET1 = ISEQ
          RETURN
        END IF
        GO TO 10
      END
      INTEGER FUNCTION INSET(SEQ,STRING,IDS)
      CHARACTER SEQ,STRING(IDS)
      INTEGER CTONUM
      EXTERNAL CTONUM
      INSET = 1
      KSEQ = CTONUM(SEQ)
      DO 20 I = 1,IDS
        IF(KSEQ.EQ.CTONUM(STRING(I)))RETURN
20    CONTINUE
      INSET = 0
      END
      SUBROUTINE SETCMP(SEQ,IDIM,IDM)
      PARAMETER (MAXCHR = 26)
      CHARACTER SEQ(IDIM)
      INTEGER DTONUM
      EXTERNAL DTONUM
      COMMON /COMPC/COMP(MAXCHR)
      SAVE /COMPC/
      DO 10 I = 1,IDM
        COMP(I) = 0.0
10    CONTINUE
      DO 20 I = 1,IDIM
        J = DTONUM(SEQ(I))
        COMP(J) = COMP(J) + 1.
20    CONTINUE
      T = MAX(1,IDIM)
      DO 30 I = 1,IDM
        COMP(I) = COMP(I) / T
30    CONTINUE
      END
