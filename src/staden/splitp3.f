C  Routine to translate prosite motif library into patterns
C  assumes only exact match, membership of set and NOT
C  assumes no errors in library
C  assumes gaps up to 40
C  assumes no variation in length of motif: only variation in
C  their separation ie [fred](2,3) is not dealt with (but why should it exist)
C  but x(2,3) causes a new motif to be started starting 2 away with 1 extra
C  position.
C  the translation is crude: all go to membership of set
C  nots go to membership of set for the rest of the character set
C  fixed gaps are included in "weight matrices" and so will slow searches
C  18-7-91 Added titles to pattern files
C  3-2-92 Changed program so it writes pattern and weight files
C         to current directory. All file names in the fofn and
C         pattern files are full path names.
      SUBROUTINE FMAIN()
      INTEGER AP,RP,SCORE
      PARAMETER (MAXSTR = 255,MAXLEN = 120, IDM = 26, MAXDEV = 5)
      INTEGER WTSMAT(IDM,MAXLEN)
      INTEGER DEVNOS(MAXDEV)
      CHARACTER AMOS*(MAXSTR),RS*(MAXSTR),CACIDS*20,COMMAS*40
      CHARACTER AAMOS*(MAXSTR)
      CHARACTER*60 PATNAM,WTSNAM,HELPF,FILNAM
      CHARACTER CSET*26,TITLE*80
      LOGICAL ACID,NUMBER
      EQUIVALENCE (AAMOS(6:),AMOS)
      EXTERNAL ACID,NUMBER,INTFC
      DATA COMMAS/',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'/
      DATA CSET/'ABCDEFGHIJKLMNOPQRSTUVWXYZ'/
      CALL INITLU(IDM)
      CALL UNITNO(KBIN,KBOUT,DEVNOS,MAXDEV)
      WRITE(KBOUT,*)
     +' SPLITP3 v3.0 Feb 1992, author Rodger Staden'
      WRITE(KBOUT,*)
     +' Splits Prosite motif library into Staden pattern files'
      WRITE(KBOUT,*)
     +' and creates a file of file names. The pattern files and'
      WRITE(KBOUT,*)
     +' weight matrices are written to the current directory but'
      WRITE(KBOUT,*)
     +' full path names are included in the file of file names'
      IDEV1 = DEVNOS(1)
      IDEV2 = DEVNOS(2)
      IDEV3 = DEVNOS(3)
      IDEV4 = DEVNOS(4)
      NVAR = 0
      NMOTIF = 0
      FILNAM = ' '
      CALL OPENF1(DEVNOS(1),FILNAM,0,IOK,KBIN,KBOUT,
     +'Prosite library file',
     +IHELPS,IHELPE,HELPF,DEVNOS(5))
      IF(IOK.NE.0) STOP
      FILNAM = ' '
      CALL OPENF1(DEVNOS(2),FILNAM,1,IOK,KBIN,KBOUT,
     +'Name for file of pattern file names',
     +IHELPS,IHELPE,HELPF,DEVNOS(5))
      IF(IOK.NE.0) STOP
3     CONTINUE 
      LIN = 0
      CALL GTSTR('Path name of motif directory',' ',PATNAM,LIN,
     +KBOUT,KBIN,INFLAG)
      IF(INFLAG.EQ.1) THEN
        CALL HELP2(IHELPS,IHELPE,HELPF,DEVNOS(5),KBIN,KBOUT)
        GO TO 3
      END IF
      IF(INFLAG.EQ.2) STOP
      IDIR = MAX(1,INDEX(PATNAM,' '))
C
C add final slash if not present
      IF(PATNAM(IDIR-1:IDIR-1).NE.'/') THEN
        PATNAM(IDIR:IDIR) = '/'
        IDIR = IDIR + 1
      END IF
5     CONTINUE
      READ(IDEV1,1000,ERR=100,END=200)AAMOS
1000  FORMAT(A)
      IF(AAMOS(1:2).NE.'AC') GO TO 5
        NAMLEN = INDEX(AMOS(1:),';') - 1
        PATNAM(IDIR:) = AMOS(1:NAMLEN)
        WTSNAM = PATNAM
        PATNAM = PATNAM(1:NAMLEN+IDIR-1)//'.PAT'
      CALL OPENRS(IDEV3,PATNAM(IDIR:),IOK,LRECL,1)
      IF(IOK.NE.0) WRITE(*,*)'SCREAM, FILE OPENING'
6       CONTINUE
        READ(IDEV1,1000,ERR=100,END=200)AAMOS
        IF(AAMOS(1:2).NE.'DE') GO TO 6
        TITLE = AAMOS(6:)
        WRITE(IDEV3,1002)TITLE
9       CONTINUE
        READ(IDEV1,1000,ERR=100,END=200)AAMOS
C Check for empty definition ie no pa line
        IF(AAMOS(1:2).EQ.'//') THEN
          WRITE(KBOUT,*)TITLE
          WRITE(KBOUT,*)'No PA line!'
          CLOSE(UNIT=IDEV3)
          GO TO 5
        END IF
        IF(AAMOS(1:2).NE.'PA') GO TO 9
      IF(INDEX(AMOS,'.').EQ.0) THEN
        J = INDEX(AMOS,' ')
        READ(IDEV1,1001,ERR=100,END=200)AMOS(J:)
1001    FORMAT(5X,A)
      END IF
      LAMOS = INDEX(AMOS,'.')
C      WRITE(KBOUT,*)AMOS(1:LAMOS)
      NMOTIF = NMOTIF + 1
      AP = 0
      RP = 0
      SCORE = 0
      MOTIF = 1
      WRITE(IDEV3,1002)'A6           '//
     +PATNAM(IDIR+2:IDIR+6)//'          Class'
1002  FORMAT(A)
10    CONTINUE
      AP = AP + 1
      IF(AP.LT.LAMOS) THEN
        IF(AMOS(AP:AP).EQ.'-') AP = AP + 1
        IF(ACID(AMOS(AP:))) THEN
          RP = RP + 1
          RS(RP:RP) = AMOS(AP:AP)
          LASTS = RP
          RP = RP + 1
          RS(RP:RP) = ','
          SCORE = SCORE + 1
        ELSE IF(AMOS(AP:AP).EQ.'[') THEN
          AP = AP + 1
          LSET = INDEX(AMOS(AP:),']') - 1
          RP = RP + 1
          RS(RP:RP+LSET-1) = AMOS(AP:AP+LSET-1)
          LASTS = RP
          RP = RP + LSET
          AP = AP + LSET
          RS(RP:RP) = ','
          AP = AP + 1
          SCORE = SCORE + 1
          IF(AMOS(AP:AP).EQ.'(') THEN
            AP = AP + 1
            LSET = INDEX(AMOS(AP:),')') - 1
            ICOMMA = INDEX(AMOS(AP:),',')
            IF((ICOMMA.GT.0).AND.(ICOMMA.LT.LSET)) THEN
              WRITE(KBOUT,*)'Variable gap in []'
              NVAR = NVAR + 1
              GO TO 5
            END IF
            NREP = INTFC(AMOS(AP:),LSET)
            LREP = RP - LASTS
            RP = RP + 1
            CALL REPST(RS(LASTS:),RS(RP:),LREP,NREP)
            RP = RP + (NREP - 1) * (LREP + 1) - 1
            AP = AP + LSET
            SCORE = SCORE + NREP - 1
          END IF
        ELSE IF(AMOS(AP:AP).EQ.'{') THEN
          AP = AP + 1
          LSET = INDEX(AMOS(AP:),'}') - 1
          RP = RP + 1
          LSETS = LSET
          CALL CACID(AMOS(AP:),CACIDS,LSET)
          RS(RP:RP+LSET-1) = CACIDS(1:LSET)
          LASTS = RP
          RP = RP + LSET
          AP = AP + LSETS
          RS(RP:RP) = ','
          AP = AP + 1
          SCORE = SCORE + 1
          IF(AMOS(AP:AP).EQ.'(') THEN
            AP = AP + 1
            LSET = INDEX(AMOS(AP:),')') - 1
            ICOMMA = INDEX(AMOS(AP:),',')
            IF((ICOMMA.GT.0).AND.(ICOMMA.LT.LSET)) THEN
              WRITE(*,*)'Variable gap in {}'
              NVAR = NVAR + 1
              GO TO 5
            END IF
            NREP = INTFC(AMOS(AP:),LSET)
            LREP = RP - LASTS
            RP = RP + 1
            CALL REPST(RS(LASTS:),RS(RP:),LREP,NREP)
            RP = RP + (NREP - 1) * (LREP + 1) - 1
            AP = AP + LSET
            SCORE = SCORE + NREP - 1
          END IF
        ELSE IF(AMOS(AP:AP).EQ.'x') THEN
          AP = AP + 1
          IF(AMOS(AP:AP).EQ.'(') THEN
            AP = AP + 1
            LSET = INDEX(AMOS(AP:),')') - 1
            ICOMMA = INDEX(AMOS(AP:),',')
            IF((ICOMMA.GT.0).AND.(ICOMMA.LT.LSET)) THEN
              IF(RS(RP:RP).EQ.',') RP = RP - 1
      WTSNAM = WTSNAM(1:NAMLEN+IDIR-1)//'.WTS'//CSET(MOTIF:MOTIF)
      WRITE(IDEV3,1002)WTSNAM(1:NAMLEN+IDIR+4)
      CALL INTRP7(RS,RP,SCORE,WTSMAT,IDM,MAXLEN,
     +CUTOFF,IOK)
      IF(IOK.NE.0) WRITE(*,*)'SCREAM'
      CALL OPENRS(IDEV4,WTSNAM(IDIR:),IOK,LRECL,1)
      IF(IOK.NE.0) WRITE(*,*)'SCREAM, FILE OPENING'
      TOP = SCORE
      CALL WRTSCP(TITLE,SCORE,0,CUTOFF,TOP,IDM,WTSMAT,IDEV4)
              NCOMMA = INTFC(AMOS(AP:),ICOMMA-1)
              JCOMMA = INTFC(AMOS(AP+ICOMMA:),LSET-ICOMMA)
      WRITE(IDEV3,1002)'A6           '//
     +PATNAM(IDIR+2:IDIR+6)//'          Class'
              WRITE(IDEV3,1008)MOTIF
1008          FORMAT(I7,'      Relative motif')
              WRITE(IDEV3,1003)SCORE+NCOMMA+1
1003              FORMAT(I7,'      Relative start position')
              WRITE(IDEV3,1004)JCOMMA-NCOMMA
1004          FORMAT(I7,'      Number of extra positions')
              AP = AP + LSET
              RP = 0
              SCORE = 0
              MOTIF = MOTIF + 1
            ELSE
              NCOMMA = INTFC(AMOS(AP:),LSET)
              RP = RP + 1
              RS(RP:RP+NCOMMA-1) = COMMAS(1:NCOMMA)
              RP = RP + NCOMMA - 1
              AP = AP + LSET
              SCORE = SCORE + NCOMMA
            END IF
          ELSE
            RP = RP + 1
            RS(RP:RP+1) = ','
            SCORE = SCORE + 1
          END IF
        END IF
        GO TO 10
      END IF
      RP = RP - 1
      IF(RS(RP:RP).EQ.',') RP = RP - 1
      WTSNAM = WTSNAM(1:NAMLEN+IDIR-1)//'.WTS'//CSET(MOTIF:MOTIF)
      WRITE(IDEV3,1002)WTSNAM(1:NAMLEN+IDIR+4)
      CALL INTRP7(RS,RP,SCORE,WTSMAT,IDM,MAXLEN,
     +CUTOFF,IOK)
      IF(IOK.NE.0) WRITE(*,*)'SCREAM'
      CALL OPENRS(IDEV4,WTSNAM(IDIR:),IOK,LRECL,1)
      IF(IOK.NE.0) WRITE(*,*)'SCREAM, FILE OPENING'
      TOP = SCORE
      CALL WRTSCP(TITLE,SCORE,0,CUTOFF,TOP,IDM,WTSMAT,IDEV4)
      WRITE(IDEV2,1002)PATNAM
      GO TO 5
100   CONTINUE
      WRITE(*,*)'READ ERROR'
      STOP
200   CONTINUE
      WRITE(KBOUT,*)'Number of patterns',NMOTIF
      WRITE(KBOUT,*)'Number of variable gaps',NVAR
      END
      SUBROUTINE REPST(S1,S2,L,N)
      CHARACTER S1*(*),S2*(*)
      J = 1
      DO 10 I = 1,N-1
        S2(J:J+L-1) = S1(1:L)
        S2(J+L:J+L) = ','
        J = J + L + 1
10    CONTINUE
      END
      INTEGER FUNCTION INTFC(STRING,LS)
      CHARACTER TEMP*10,STRING*(*)
      TEMP = STRING(1:LS)
      CALL RJST(TEMP)
      READ(TEMP,1000,ERR=10)INTFC
1000  FORMAT(I10)
      RETURN
10    CONTINUE
      INTFC = 0
      WRITE(*,*)'Error in encode'
      END
      SUBROUTINE CACID(ACID,REST,LACID)
      CHARACTER ACID*(*),REST*20,ACIDS*20
      SAVE ACIDS
      DATA ACIDS/'QWERTYIPASDFGHKLCVNM'/
      J = 0
      DO 10 I = 1,20
        DO 5 K = 1,LACID
          IF(ACID(K:K).EQ.ACIDS(I:I)) GO TO 9
5       CONTINUE
        J = J + 1
        REST(J:J) = ACIDS(I:I)
9       CONTINUE
10    CONTINUE
      LACID = J
      END
      LOGICAL FUNCTION ACID(CHAR)
      LOGICAL ONEOF
      CHARACTER ACIDS*(20),CHAR
      SAVE ACIDS
      EXTERNAL ONEOF
      DATA ACIDS/'QWERTYIPASDFGHKLCVNM'/
      ACID = ONEOF(ACIDS,CHAR)
      END
      LOGICAL FUNCTION NUMBER(CHAR)
      LOGICAL ONEOF
      CHARACTER DIGITS*(10),CHAR
      SAVE DIGITS
      EXTERNAL ONEOF
      DATA DIGITS/'1234567890'/
      NUMBER = ONEOF(DIGITS,CHAR)
      END
      LOGICAL FUNCTION ONEOF(CHARS,CHAR)
      CHARACTER CHARS*(*),CHAR
      ONEOF = .FALSE.
      IF(INDEX(CHARS,CHAR).NE.0) ONEOF = .TRUE.
      END
      SUBROUTINE INTRP7(STRING,ISEND,LENGTH,WT,MAXCHR,MAXLEN,
     +CUTOFF,IOK)
      CHARACTER STRING*(*),TERM
      INTEGER WT(MAXCHR,MAXLEN)
      INTEGER CTONUM
      EXTERNAL CTONUM
      PARAMETER (TERM = ',')
      IOK = 0
C  POINT TO STRING
      IS = 1
      ICOL = 1
C  COUNT FILLED COLUMNS
      CUTOFF = 0.
10    CONTINUE
      CALL FILLI(WT(1,ICOL),MAXCHR,0)
      CUTOFF = CUTOFF + 1.
20    CONTINUE
      IF(IS.LE.ISEND)THEN
        IF(STRING(IS:IS).NE.TERM)THEN
          IROW = CTONUM(STRING(IS:IS))
          WT(IROW,ICOL) = 1
          IS = IS + 1
          GO TO 20
        END IF
        NC = 1
30      CONTINUE
        IF(IS.LE.ISEND)THEN
          IF(STRING(IS:IS).EQ.TERM)THEN
            ICOL = ICOL + 1
            CALL FILLI(WT(1,ICOL),MAXCHR,0)
            NC = NC + 1
            IS = IS + 1
            GO TO 30
          END IF
C
C  END OF TERMINATORS
C
C          IF(ICOL.GT.1) GO TO 10
C  ERROR TERMINATOR BEFORE ANY GOOD COLUMNS
C          IOK = 1
C          RETURN
          GO TO 10
        END IF
C  STRING ENDED WITH TERMINATOR
        LENGTH = ICOL - NC + 1
        RETURN
      END IF
C
C  END REACHED WITH NO TERMINATOR (NORMAL)
      IF(ICOL.GT.0)THEN
        LENGTH = ICOL
        IOK = 0
        RETURN
      END IF
      IOK = 1
      END
      SUBROUTINE WRTSCP(TITLE,LENGTH,MIDDLE,BOT,TOP,IDM,
     +SUM,IDEV)
      INTEGER TOT(120)
      INTEGER SUM(IDM,LENGTH)
      CHARACTER CHRSET*22,TITLE*(*)
      SAVE CHRSET
      DATA CHRSET/'CSTPAGNDEQBZHRKMILVFYW'/
C   PROTEIN MATRICES DONT WRITE ROWS FOR -X? AND SPACE SO SET DIMENSION
C   TO IDM-4
      CALL FILLI(TOT,120,0)
      WRITE(IDEV,1018)TITLE
1018  FORMAT(' ',A)
1019  FORMAT(' P',20I4)
1020  FORMAT(' N',20I4)
1021  FORMAT(' ',A,20I4)
1022  FORMAT(' ',2I6,2F10.3)
      WRITE(IDEV,1022)LENGTH,MIDDLE,BOT,TOP
      NLINES=1+(LENGTH-1)/20
      K1=1
      DO 400 J=1,NLINES
        K2=MIN((K1+19),LENGTH)
        WRITE(IDEV,1019)(K,K=K1-MIDDLE,K2-MIDDLE)
        WRITE(IDEV,1020)(TOT(K),K=K1,K2)
        DO 390 I=1,IDM-4
          WRITE(IDEV,1021)CHRSET(I:I),(SUM(I,K),K=K1,K2)
390     CONTINUE
        K1=K1+20
        IF(K1.GT.LENGTH)K1=LENGTH
400   CONTINUE
      CLOSE(UNIT=IDEV)
      END
