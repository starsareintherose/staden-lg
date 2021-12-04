      SUBROUTINE FMAIN()
C
C rep compares a set of sequence readings against a library of alu
C sequences in both orientations. It creates 3 output files:
C a list of files that pass
C a list of files that fail
C a log file which contains the highest score found for each reading
C WARNING: rep also modifies all files in which it finds a match
C by writing a record that is translated by the assembly program into
C a tag to mark alu sequences. (First it copies the original file, say
C called fred, to fred.ALU then it modifies the original).
C The list of matching sequences includes:
C the file name, the top score, the top score for the other end of the read (0
C if not above the cutoff), and the number of bases that does not appear to 
C contain ALU.
C One assembly strategy would assemble the file of passes, and then sort the
C fail file on the last column so that the reads with the most nonALU sequence
C are at the top of the list, then assemble those.
C The log file is simply for checking the sensitivity of the program.
C
      PARAMETER (MAXSEQ = 50000,
     +           MAXRD = 1000,
     +           MAXWLN = 4,
     +           LCONST = 4*MAXWLN,
     +           MAXWRD = 4**MAXWLN,
     +           MAXFIL = 150,
     +           MAXRSQ = 400,
     +           MAXCF  = 4000,
     +           MAXDEV = 7,
     +           NAMLEN = 80)
C
C MAXFIL MAX NUMBER REPEAT FILES
C MAXRSQ MAX LENGTH OF EACH REPEAT FILE
C MAXCF  MAX COMPARISON LENGTH
C
      CHARACTER SEQV(MAXSEQ),SEQG(MAXRD)
      INTEGER WORDP(MAXWRD,MAXFIL),POSN(MAXRSQ,MAXFIL)
      REAL HIST(-MAXCF:MAXCF)
      CHARACTER*(NAMLEN) NAMES(MAXFIL)
      INTEGER STARTS(MAXFIL),MATRIX(5,5),RC,BESTRS(MAXRD),BESTRC(MAXRD)
      REAL BESTSC(MAXRD)
      CHARACTER BESTD(MAXRD)
      INTEGER CONSTS(0:LCONST),DEVNOS(MAXDEV)
      INTEGER SEQVI(MAXSEQ),SEQGI(MAXRD),SEQR(MAXRD)
      CHARACTER*(NAMLEN) FILNAM,HELPF,NAME,TNAME,VNAME
      INTEGER GNFFOF
      EXTERNAL GNFFOF
      PARAMETER (VNAME='.ALU')
      ICG = 0
      ICB = 0
      ICB2 = 0
      IDM = 5
      CALL INITLU(IDM)
      CALL UNITNO(KBIN,KBOUT,DEVNOS,MAXDEV)
      CALL MAKEID(MATRIX,IDM)
      IDEVNI = DEVNOS(1)
      IDEVV = DEVNOS(2)
      IDEVNO = DEVNOS(3)
      IDEVNF = DEVNOS(5)
      IDEVNR = DEVNOS(6)
      IDEVNL = DEVNOS(7)
      WRITE(KBOUT,*)'rep v1.1: repeat examination program. April 93'
      WRITE(KBOUT,*)'Author: Rodger Staden'
      WRITE(KBOUT,*)'Copyright: Medical Research Council, UK'
      FILNAM = ' '
      CALL OPENF1(IDEVNI,FILNAM,0,IOK,KBIN,KBOUT,
     +'Input file of gel reading file names',
     +IHELPS,IHELPE,HELPF,IDEVH)
      IF(IOK.NE.0) STOP
      FILNAM = ' '
      CALL OPENF1(IDEVNR,FILNAM,0,IOK,KBIN,KBOUT,
     +'Input file of repeat file names',
     +IHELPS,IHELPE,HELPF,IDEVH)
      IF(IOK.NE.0) STOP
      FILNAM = ' '
      CALL OPENF1(IDEVNO,FILNAM,1,IOK,KBIN,KBOUT,
     +'Output file of passed file names',
     +IHELPS,IHELPE,HELPF,IDEVH)
      IF(IOK.NE.0) STOP
      FILNAM = ' '
      CALL OPENF1(IDEVNF,FILNAM,1,IOK,KBIN,KBOUT,
     +'Output file of failed file names',
     +IHELPS,IHELPE,HELPF,IDEVH)
      IF(IOK.NE.0) STOP
      FILNAM = ' '
      CALL OPENF1(IDEVNL,FILNAM,1,IOK,KBIN,KBOUT,
     +'Log file name',
     +IHELPS,IHELPE,HELPF,IDEVH)
      IF(IOK.NE.0) STOP
      MN = 2
      MX = MAXWLN
      LENGTH = 4
C      CALL GETINT(MN,MX,LENGTH,
C     +'Word length',
C     +IVAL,KBIN,KBOUT,
C     +IHELPS,IHELPE,HELPF,IDEVH,IOK)
C      IF(IOK.NE.0) STOP
C      LENGTH = IVAL
      MN = 1
      MX = 11
      LW = 7
C      CALL GETINT(MN,MX,LW,
C     +'Number of diagonals to combine',
C     +IVAL,KBIN,KBOUT,
C     +IHELPS,IHELPE,HELPF,IDEVH,IOK)
C      IF(IOK.NE.0) STOP
C      LW = IVAL
      XMN = 0.0
      XMX = LW*1.0
      XMX = 1.
      CUT = 0.6
      CALL GETRL(XMN,XMX,CUT,
     +'Cutoff score',
     +VAL,KBIN,KBOUT,
     +IHELPS,IHELPE,HELPF,IDEVH,IOK)
      IF(IOK.NE.0) STOP
      CUT = VAL
C
C set initial values so we hash the first vector
C
      ICSITT = 0
      IPSITT = 0
      ICSITE = ICSITT
      IPSITE = IPSITT
      IDE = (IDM-1)**LENGTH
      CALL SETCN(CONSTS,LENGTH,IDM,LCONST)
      CALL GETSQS(IDEVNR,IDEVV,SEQV,MAXSEQ,STARTS,NAMES,
     +FILNAM,NFILE,MAXFIL,MAXNAM,KBIN,KBOUT,
     +IHELPS,IHELPE,HELPF,IDEVH)
      IDIMV = STARTS(NFILE+1)-1
      NFILES = NFILE + 1
      CALL CONNUM(SEQV,SEQVI,IDIMV)
      DO 3 NSEQ = 1,NFILE
        IS = STARTS(NSEQ)
        IE = STARTS(NSEQ+1) - 1
        IDIMV = IE - IS + 1
C        WRITE(*,*)(SEQVI(K),K=IS,IE)
        CALL ENCONC(SEQVI(IS),IDIMV,POSN(1,NSEQ),WORDP(1,NSEQ),
     +  IDE,IDM,CONSTS,LENGTH,
     +  LCONST)
 3    CONTINUE
      CALL BLATA(WORDP,IDE,MAXFIL,LENGTH,NSEQ,CONSTS,LCONST)
C      CALL LISTN(SEQV,MAXSEQ,STARTS,NFILE,60,1,200,IDEV,KBOUT,
C     +NAMES,MAXFIL)
      IDEVE = 0
      IDEVG = IDEVV
      MINR = 20
      JGEL = 0
 10   CONTINUE
C Get next experiment file name
      IOK = GNFFOF(IDEVNI,NAME)
      IF(IOK.EQ.1) THEN
        WRITE(KBOUT,*)
     + 'Finished after processing',JGEL,' files and finding'
        WRITE(KBOUT,*)ICG,' free of the repeat'
        WRITE(KBOUT,*)ICB,' contain the repeat'
        WRITE(KBOUT,*)ICB2,' contain the repeat at both ends'
        STOP
      ELSE IF(IOK.EQ.2) THEN
        CALL ERROM(KBOUT,'Empty line in file of file names')
        GO TO 10
      ELSE IF(IOK.EQ.3) THEN
        CALL ERROM(KBOUT,'Error reading file of file names')
        GO TO 10
      END IF
      IDIMG = MAXRD
      CALL OPENRS(IDEVG,NAME,IOK,LRECL,2)
      CALL ARRFIM(IDEVG,SEQG,IDIMG,KBOUT)
      IF (IDIMG.GT.MINR) THEN
        JGEL = JGEL + 1
        CALL CONNUM(SEQG,SEQGI,IDIMG)
        CALL VCUTT(SEQR,
     +    POSN,WORDP,IDE,MAXRSQ,MAXFIL,SEQGI,IDIMG,CONSTS,
     +    LENGTH,IDM,LCONST,HIST,MAXCF,KBOUT,CUT,LC,RC,
     +    STARTS,NFILES,LW,
     +    IDEVNF,IDEVNO,IDEVNL,
     +    NAME,NAMES,MATRIX,SEQVI,MAXSEQ,SCORE,ICG,ICB,
     +    ICB2,
     +    BESTRS,BESTRC,BESTSC,BESTD,MAXRD,KCUT,LCUT,EMAX)
C        write(*,*)lc,rc
        IF(LC.EQ.0) THEN
            CLOSE(UNIT=IDEVG)
        ELSE
          CALL VPOUTR(IDEVG,IDEVNR,NAME,LC,RC,
     +    SCORE,VNAME,TNAME,KCUT,LCUT,EMAX,IOK)
          IF(IOK.NE.0) STOP
        END IF
      END IF
      GO TO 10
      END
      SUBROUTINE SQCOMI(SEQ,ID)
      INTEGER SEQ(ID),COMP(5)
      SAVE COMP
      DATA COMP/3,4,1,2,5/
      DO 10 I=1,ID
       SEQ(I) = COMP(SEQ(I))
 10   CONTINUE
      END
      SUBROUTINE SQREVI(SEQ,ID)
      INTEGER SEQ(ID)
      IEND = ID/2
      DO 100 I=1,IEND
       J = SEQ(I)
       SEQ(I) = SEQ(ID+1-I)
       SEQ(ID+1-I) = J
 100  CONTINUE
      END
      SUBROUTINE VCUTT(SEQR,
     +POSN,WORDP,IDE,MAXRSQ,MAXFIL,SEQH,IDIMH,CONSTS,
     +LENGTH,IDM,LCONST,HIST,MAXSEQ,KBOUT,CUT,ICUT,JCUT,
     +STARTS,NFILES,LW,
     +IDEVNF,IDEVNO,IDEVNL,
     +NAME,NAMES,MATRIX,SEQV,MAXVSQ,DMAX,ICG,ICB,ICB2,
     +BESTRS,BESTRC,BESTSC,BESTD,MAXRD,KCUT,LCUT,EMAX)
      INTEGER SEQH(IDIMH),STARTS(NFILES),SEQR(IDIMH),RC,MATRIX(IDM,IDM)
      INTEGER POSN(MAXRSQ,MAXFIL),WORDP(IDE,MAXFIL),CONSTS(0:LCONST)
      REAL HIST(-MAXSEQ:MAXSEQ)
      INTEGER BESTRS(MAXRD),BESTRC(MAXRD),SEQV(MAXVSQ)
      REAL BESTSC(MAXRD)
      CHARACTER BESTD(MAXRD)
      CHARACTER NAME*(*),ORIEN*9
      CHARACTER*(*) NAMES(NFILES)
C      WRITE(*,*)IDIMH
      CALL COPYI(SEQH,SEQR,IDIMH)
      CALL SQREVI(SEQR,IDIMH)
      CALL SQCOMI(SEQR,IDIMH)
      CALL FILLI(BESTRS,IDIMH,0)
      CALL FILLI(BESTRC,IDIMH,0)
      CALL FILLR(BESTSC,IDIMH,0)
      CALL FILLC(BESTD,IDIMH,'F')
      DMAX = -1.
C      WRITE(*,*)NFILES
      IBLANK = INDEX(NAME,' ')
      DO 30 NSEQ = 1,NFILES-1
        IS = STARTS(NSEQ)
        IE = STARTS(NSEQ+1) - 1
        IDIMV = IE - IS + 1
        IDIMHT = IDIMH
          IBLANK = INDEX(NAME,' ')
          JBLANK = INDEX(NAMES(NSEQ),' ')
        CALL VCUT(IDIMV,POSN(1,NSEQ),WORDP(1,NSEQ),MAXRSQ,
     +  IDE,SEQH,IDIMHT,CONSTS,
     +  LENGTH,IDM,LCONST,HIST,MAXSEQ,KBOUT,CUT,LC,RC,LW,DSCORE,
     +  SDS,MATRIX,SEQV(STARTS(NSEQ)))
C        WRITE(*,*)CUT,DMAX,DSCORE
        IF(DSCORE.GT.CUT) THEN
          IF(DSCORE.GT.BESTSC(LC)) THEN
            BESTSC(LC) = DSCORE
            BESTRC(LC) = RC
            BESTRS(LC) = NSEQ
          END IF
        END IF
        IF(DSCORE.GT.DMAX) THEN
          DMAX = DSCORE
          ORIEN = 'Left'
          NBSEQ = NSEQ
          ICUT = LC
          JCUT = RC
        END IF
        CALL VCUT(IDIMV,POSN(1,NSEQ),WORDP(1,NSEQ),MAXRSQ,
     +  IDE,SEQR,IDIMHT,CONSTS,
     +  LENGTH,IDM,LCONST,HIST,MAXSEQ,KBOUT,CUT,LC,RC,LW,DSCORE,
     +  SDS,MATRIX,SEQV(STARTS(NSEQ)))
C        WRITE(*,*)DMAX,DSCORE
        IF(DSCORE.GT.CUT) THEN
          JCUT = IDIMH - LC + 1
          ICUT = IDIMH - RC + 1
          LC = ICUT
          RC = JCUT
          IF(DSCORE.GT.BESTSC(LC)) THEN
            BESTSC(LC) = DSCORE
            BESTRC(LC) = RC
            BESTRS(LC) = NSEQ
            BESTD(LC) = 'R'
          END IF
        END IF
        IF(DSCORE.GT.DMAX) THEN
          DMAX = DSCORE
          ORIEN = 'Rev'
          NBSEQ = NSEQ
          JCUT = IDIMH - LC + 1
          ICUT = IDIMH - RC + 1
        END IF
 30   CONTINUE
      IF(DMAX.LT.CUT) THEN
        EMAX = 0.
        JBLANK = INDEX(NAMES(NBSEQ),' ')
        WRITE(IDEVNO,1001)NAME(1:IBLANK),DMAX,EMAX,IDIMH
 1001   FORMAT(A,2F5.2,I5)
        WRITE(IDEVNL,1000)NAME(1:IBLANK),NAMES(NBSEQ)(1:JBLANK),
     +  ORIEN,ICUT,JCUT,IDIMH,DMAX
        ICG = ICG + 1
        ICUT = 0
        RETURN
      END IF
C
C find max value
C
      DMAX = CUT
      DO 40 I=1,IDIMH
        IF(BESTSC(I).GT.DMAX) THEN
          DMAX = BESTSC(I)
          ICUT = I
          JCUT = BESTRC(I)
          NBSEQ = BESTRS(I)
          IF(BESTD(I).EQ.'F') THEN
            ORIEN = 'Left'
          ELSE
            ORIEN = 'Rev'
          END IF
        END IF
 40   CONTINUE
      JBLANK = INDEX(NAMES(NBSEQ),' ')
        WRITE(IDEVNL,1000)NAME(1:IBLANK),NAMES(NBSEQ)(1:JBLANK),
     +  ORIEN,ICUT,JCUT,IDIMH,DMAX
 1000   FORMAT(A,' ',A,' ',A,3I6,F7.3)
        ICB = ICB + 1
C
C now find best value in biggest bit thats left
C
      EMAX = CUT
      IF(ICUT.GT.IDIMH-JCUT) THEN
        LEFT = ICUT - 1
        DO 50 I=1,ICUT-1
          IF((BESTSC(I).GT.EMAX).AND.(BESTRC(I).LT.ICUT)) THEN
            EMAX = BESTSC(I)
            KCUT = I
            LCUT = BESTRC(I)
            MBSEQ = BESTRS(I)
            LEFT = KCUT - 1
            IF(BESTD(I).EQ.'F') THEN
              ORIEN = 'Left'
            ELSE
              ORIEN = 'Rev'
            END IF
          END IF
 50     CONTINUE
      ELSE
        LEFT = IDIMH - JCUT
        DO 60 I=JCUT,IDIMH
          IF(BESTSC(I).GT.EMAX) THEN
            EMAX = BESTSC(I)
            KCUT = I
            LCUT = BESTRC(I)
            MBSEQ = BESTRS(I)
            LEFT = IDIMH - LCUT
            IF(BESTD(I).EQ.'F') THEN
              ORIEN = 'Left'
            ELSE
              ORIEN = 'Rev'
            END IF
          END IF
 60     CONTINUE
      END IF
      IF(EMAX.GT.CUT) THEN
        JBLANK = INDEX(NAMES(MBSEQ),' ')
        WRITE(IDEVNL,1000)NAME(1:IBLANK),NAMES(MBSEQ)(1:JBLANK),
     +  ORIEN,KCUT,LCUT,IDIMH,EMAX
        WRITE(IDEVNF,1001)NAME(1:IBLANK),DMAX,EMAX,LEFT
        ICB2 = ICB2 + 1
      ELSE
        EMAX = 0.
        WRITE(IDEVNF,1001)NAME(1:IBLANK),DMAX,EMAX,LEFT
        KCUT = 0
      END IF
      END
      SUBROUTINE VCUT(IDIMV,
     +POSN,WORDP,MAXRSQ,IDE,SEQH,IDIMH,CONSTS,
     +LENGTH,IDM,LCONST,HIST,MAXSEQ,KBOUT,CUT,LC,RC,LW,DSCORE,
     +SDS,MATRIX,SEQV)
      INTEGER SEQH(IDIMH),RC,MATRIX(IDM,IDM),SEQV(IDIMV)
      INTEGER POSN(MAXRSQ),WORDP(IDE),CONSTS(0:LCONST)
      REAL HIST(-MAXSEQ:MAXSEQ)
      EXTERNAL NCODEA
C      WRITE(*,*)'VCUT'
C        WRITE(*,*)(SEQH(KKK),KKK=1,IDIMH)
C        WRITE(*,*)(SEQV(KKK),KKK=1,IDIMV)
C
C when we get here idimh is the length we want to screen
C
        CALL FILLR(HIST(LENGTH-IDIMV),IDIMH+IDIMV+1,0.)
        DO 20 I = 1,IDIMH-LENGTH+1
          J = NCODEA(SEQH(I),LENGTH,CONSTS,IDM,LCONST)
          IF(J.NE.0)THEN
            J1 = WORDP(J)
C            write(*,*)i,seqh(i),j,j1
            IF(J1.NE.0)THEN
              K = I - J1
              HIST(K) = HIST(K) + 1.
10            CONTINUE
              J2 = J1
              J1 = POSN(J2)
              IF(J1.NE.0)THEN
                K = I - J1
                HIST(K) = HIST(K) + 1.
                GO TO 10
              END IF
            END IF
          END IF
20      CONTINUE
C        I1 = LENGTH - IDIMV
C        I2 = IDIMH - LENGTH
C        DO 55 IJK=I1,I2
C          WRITE(33,*)IJK,HIST(IJK)
C 55       CONTINUE
        CALL PHIST(HIST,IDIMV,IDIMH,LENGTH,MAXSEQ)
C        DO 56 IJK=I1,I2
C          WRITE(33,*)IJK,HIST(IJK)
C 56    CONTINUE
        CALL FCUT(HIST,IDIMV,IDIMH,SEQV,SEQH,LENGTH,
     +  MAXSEQ,CUT,LC,RC,LW,DSCORE,SDS,MATRIX,IDM)
      END
      SUBROUTINE PHIST(HIST,IDIMV,IDIMH,LENGTH,MAXSEQ)
      REAL HIST(-MAXSEQ:MAXSEQ)
C
C modified 29-3-93 to correct for diagonal length being shorter by length -1
C
      IF(IDIMV.GE.IDIMH) THEN
        D  = 1
        DO 10 I=LENGTH-IDIMV,IDIMH-IDIMV-1
          HIST(I) = HIST(I)/D
          D = D + 1
 10       CONTINUE
        D  = IDIMH - LENGTH + 1
        DO 20 I=IDIMH-IDIMV,0
          HIST(I) = HIST(I)/D
 20       CONTINUE  
        D  = IDIMH - 1 - LENGTH + 1
        DO 30 I=1,IDIMH-LENGTH
          HIST(I) = HIST(I)/D
          D = D - 1
 30       CONTINUE
      ELSE
        D  = 1
        DO 40 I=LENGTH-IDIMV,-1
          HIST(I) = HIST(I)/D
          D = D + 1
 40       CONTINUE
        D  = IDIMV - LENGTH + 1
        DO 50 I=0,IDIMH-IDIMV
          HIST(I) = HIST(I)/D
 50       CONTINUE
        D  = IDIMV - 1 - LENGTH + 1
        DO 60 I=IDIMH-IDIMV+1,IDIMH-LENGTH
          HIST(I) = HIST(I)/D
          D = D - 1
 60       CONTINUE
      END IF
      END
      SUBROUTINE FCUT(HIST,IDIMV,IDIMH,SEQV,SEQH,LENGTH,
     + MAXSEQ,CUT,LC,RC,LW,D,SDS,MATRIX,IDM)
      REAL HIST(-MAXSEQ:MAXSEQ)
      INTEGER RC,SEQH(IDIMH),MATRIX(IDM,IDM),SEQV(IDIMV)
      PARAMETER (MINDIA = 25)
      REAL DSCAN
      EXTERNAL DSCAN
C
C dont look within mindia of edges
C
C and rotate over a window of length lw
C
      LB = 1 + LW/2
      LF = LW/2
      I1 = LENGTH - IDIMV + MINDIA
      I2 = IDIMH - LENGTH - MINDIA
C first we do the first window length
      DT = 0.
      DO 1 I=I1-LB,I1+LF-1
        DT = DT + HIST(I)
 1    CONTINUE
      LC = 0
      RC = 0
      DMAX = 0.
      ID = 0
C      WRITE(*,*)'LOOKING AT ',I1,I2
C      RMSQ = 0.
C      RM = 0.
C      write(*,*)i1,i2
      DO 10 I=I1,I2
        DT = DT - HIST(I-LB) + HIST(I+LF)
C        DT = HIST(I)
C        WRITE(*,*)I,DT
C        RM = RM + DT
C        RMSQ = RMSQ + DT*DT
        IF(DT.GT.DMAX) THEN
          DMAX = DT
          ID = I
C          write(*,*)'i,dt',i,dt
        END IF
 10     CONTINUE
C      N = I2 - I1
C      RM = RM / N
C      RMSQ = RMSQ / N
C      RM2 = RM * RM
C      SD = 0.
C      T = RMSQ - RM2
C      IF (T.GT.0.) SD = SQRT(T)
C      SDS = (DMAX - RM ) / SD
C      D = DMAX
C      WRITE(*,*)'Best diagonal, score and local sum',ID,DMAX,D
C      WRITE(*,1000)ID,DMAX,D
C 1000 FORMAT(I6,2F10.3)
C      IF(D.LT.CUT) RETURN
      IF(IDIMV.GE.IDIMH) THEN
        IF(ID.GE.0) THEN
          LC = ID + 1
          RC = IDIMH
        ELSE
          LC = 1
          RC = MIN(IDIMH,IDIMV + ID)
        END IF
      ELSE
        IF(ID.GE.0) THEN
          LC = ID + 1
          RC = MIN(ID+IDIMV,IDIMH)
        ELSE
          LC = 1
          RC = ID + IDIMV
        END IF
      END IF
C      WRITE(*,*)' Discard ',LC, ' to ',RC
      LS = 5
      MS = 4
      CALL DIAGO(ID,IX,IY)
C      write(*,*)'ix,iy',ix,iy
      D = DSCAN(SEQH,IDIMH,IX,SEQV,IDIMV,IY,LW,LS,MATRIX,IDM,MS)
C      WRITE(*,*)IX,IY,D
      END
      INTEGER FUNCTION GNFFOF(IDEV,NAME)
      CHARACTER NAME*(*)
      EXTERNAL NOTLR
C
C routine to read a file of file names and return a name
C deals with leading spaces and trims names at first space
C after name: eg '  fred is a bum' is returned as 'fred'
C needed because file names can contain spaces (not our file names!)
C and the open statement expects the names to match precisely
C
C return 0 = ok, 2 = empty line in file, 3 = error in read, 1 = end of file
C
      READ(IDEV,1000,ERR=100,END=200)NAME
 1000 FORMAT(A)
C
C get first non space position
C
      LENGTH = LEN(NAME)
      I = NOTLR(NAME,LENGTH,' ')
C empty line ?
      IF(I.EQ.0) THEN
        GNFFOF = 2
        RETURN
      END IF
C now want first space after I
      J = INDEX(NAME(I+1:),' ')
      IF(J.EQ.0) THEN
        J = LENGTH
      ELSE
        J = J + I - 1
      END IF
      CALL SHFTLS(NAME,I,1,J)
      NAME(J-I+2:) = ' '
      GNFFOF = 0
      RETURN
 100  CONTINUE
      GNFFOF = 3
      RETURN
 200  CONTINUE
      GNFFOF = 1
      END
      SUBROUTINE SHFLCA(STRING,MAXAR,FROMS,TO,FROME)
      CHARACTER STRING(MAXAR)
      INTEGER FROMS,FROME,TO
C
C  shift left from from to to
C
      J = TO
      DO 10 I=FROMS,FROME
        STRING(J) = STRING(I)
        J = J + 1
 10   CONTINUE
      END
      SUBROUTINE AERROR(IDEVS,IDEVF,NAME,IERR)
      CHARACTER NAME*(*)
C
C handle errors for assembly
C
C errors are:
C 0 file not found
C 1 read too short
C 2 failed to align and not entered
C 3 failed on entry
C 4 failed to align but entered
      WRITE(IDEVF,1000)NAME(1:INDEX(NAME,' ')),IERR
 1000 FORMAT(A,I2)
      CALL ERROM(IDEVS,'Failed reading written to error file')
      END
      SUBROUTINE GETSQS(IDEV1,IDEV2,SEQ,MAXSEQ,STARTS,NAMES,
     +FILNAM,NFILE,MAXFIL,MAXNAM,KBIN,KBOUT,
     +IHELPS,IHELPE,HELPF,IDEVH)
      CHARACTER SEQ(MAXSEQ),HELPF*(*)
      CHARACTER*(*) FILNAM,NAMES(MAXFIL)
      INTEGER STARTS(MAXFIL)
C
C concatenate sequences into seq
C number of them is nfile
C sequence i starts at starts(i) and has file name names(i)
C and starts(nfile+1) is the position of the first unused element
      NFILE = 0
      IEND = 0
      FILNAM = ' '
C      CALL OPENF1(IDEV1,FILNAM,0,IOK,KBIN,KBOUT,
C     +'File of file names',
C     +IHELPS,IHELPE,HELPF,IDEVH)
2     CONTINUE
      IF (NFILE.LT.MAXFIL) THEN
        READ(IDEV1,1000,END=3)FILNAM
 1000   FORMAT(A)
C        WRITE(*,*)NFILE,IEND,FILNAM
        NFILE = NFILE + 1
        STARTS(NFILE) = IEND + 1
        NAMES(NFILE) = FILNAM
        IDIM2 = MAXSEQ - IEND
        IF (IDIM2.GT.0) THEN
          CALL OPENRS(IDEV2,FILNAM,IOK,LRECL,2)
          CALL ARRFIL(IDEV2,SEQ(STARTS(NFILE)),IDIM2,KBOUT)
          CLOSE(UNIT=IDEV2)
          IEND = IEND + IDIM2
          GO TO 2
        END IF
C
C if we get here weve run out of array space
C
        NFILE = NFILE - 1
        WRITE(KBOUT,*)'Only ',NFILE,' files read: no more memory'
        CLOSE(UNIT=IDEV1)
        RETURN
      ELSE
C
C if we get here weve got too many files
C
        NFILE = NFILE - 1
        CLOSE(UNIT=IDEV1)
        WRITE(KBOUT,*)'Maximum files ',NFILE,' read'
        RETURN
      END IF
 3    CONTINUE
      STARTS(NFILE+1) = IEND + 1
      CLOSE(UNIT=IDEV1)
      END
      SUBROUTINE LISTN(SEQ,MAXCHR,LENSEQ,NFILE,LINLEN,I1,I2,
     +IDEV,KBOUT,NAMES,MAXFIL)
      CHARACTER*(*) NAMES(MAXFIL)
      PARAMETER (MAXLIN = 120)
      CHARACTER SEQ(MAXCHR),LINE*(MAXLIN)
      INTEGER LENSEQ(MAXFIL)
      EXTERNAL NOTIRL
C     SET WIDTH FOR LAST PAGE
      LPAGE=MOD(I2-I1+1,LINLEN)
C     HOW MANY PAGE WIDTHS?
      NPAGE=1+(I2-I1+1)/LINLEN
      IF(MOD(I2-I1+1,LINLEN).EQ.0)THEN
        NPAGE=NPAGE-1
        LPAGE=LINLEN
      END IF
      ISTART=I1-LINLEN
      DO 50 I=1,NPAGE
        ISTART=ISTART+LINLEN
        IF(I.EQ.NPAGE)LINLEN=LPAGE
        WRITE(IDEV,1006)(K,K=ISTART+9,ISTART+LINLEN-1,10)
        DO 40 J=1,NFILE
          KF = ISTART + LENSEQ(J) - 1
          KT = MIN(KF+LINLEN,LENSEQ(J+1)) - 1
          LINE(1:) = ' '
          WRITE(LINE,1003,ERR=60)(SEQ(K),K=KF,KT)
1003      FORMAT(' ',60A1)
          LINE(KT-KF+4:) = NAMES(J)(1:15)
          WRITE(IDEV,1004,ERR=60)LINE(1:NOTIRL(LINE,MAXLIN,' '))
 1004     FORMAT(A)
1006      FORMAT(' ',10I10)
40      CONTINUE
        WRITE(IDEV,1008)
1008    FORMAT( )
50    CONTINUE
      RETURN
60    CONTINUE
      WRITE(KBOUT,*)' Error writing file'
      END
C   SUBROUTINE TO READ CHARACTER DATA FROM IDEV, REMOVE SPACES, FILL
C   ARRAY AND RETURN NUMBER OF ELEMENTS USED. ANY LINES STARTING WITH
C   A ; ARE TREATED AS COMMENTS
      SUBROUTINE ARRFIM(IDEV,SEQNCE,J,KBOUT)
C 14-8-91 Added err= option to read, and set length to 0 if error found
C   AUTHOR: RODGER STADEN
      CHARACTER TEMP(80),SEQNCE(J)
      CHARACTER SPACE,ENDCHR,TITCHR
      SAVE ENDCHR,SPACE,TITCHR
      DATA ENDCHR/'@'/
      DATA SPACE/' '/
      DATA TITCHR/';'/
      IDMX=J
      J=0
1     CONTINUE
      READ(IDEV,1001,END=30,ERR=40)TEMP
1001  FORMAT(80A1)
      IF(TEMP(1).EQ.TITCHR)THEN
C        WRITE(KBOUT,1003)(TEMP(K),K=2,80)
C1003    FORMAT(' ',79A1)
        GO TO 1
      END IF
10    CONTINUE
      DO 20 I=1,80
        IF(TEMP(I).NE.SPACE)THEN
          IF(TEMP(I).EQ.ENDCHR)RETURN
          IF(J.EQ.IDMX)THEN
            WRITE(KBOUT,1002)IDMX
1002        FORMAT(
     +             ' Too much data. Maximum possible',
     +             ' =',I6,', input stopped there')
            RETURN
          END IF
          J=J+1
          SEQNCE(J)=TEMP(I)
        END IF
20    CONTINUE
      GO TO 1
30    CONTINUE
      RETURN
 40   CONTINUE
      CALL ERROM(KBOUT,'Error reading file')
      J = 0
      END
      SUBROUTINE BLATA(WORDP,IDE,MAXFIL,LENGTH,NSEQ,CONSTS,LCONST)
      INTEGER WORDP(IDE,MAXFIL)
      INTEGER SEQA(4),CONSTS(0:LCONST)
      EXTERNAL NCODEA
C
C fudge: set xyxy to 0
C
      DO 10 I = 1,4
      DO 10 J = 1,4
        SEQA(1) = I
        SEQA(3) = I
        SEQA(2) = J
        SEQA(4) = J
        JA = NCODEA(SEQA,LENGTH,CONSTS,5,LCONST)
        DO 5 K = 1,NSEQ
          WORDP(JA,K) = 0
 5      CONTINUE
 10   CONTINUE
      END
      SUBROUTINE MAKEID(MATRIX,IDM)
      INTEGER MATRIX(IDM,IDM)
C
C set main diagonal to 1 except idm,idm
C
      DO 10 I=1,IDM
        DO 10 J=1,IDM
          IF(I.EQ.J) THEN
            MATRIX(I,J) = 1
          ELSE
            MATRIX(I,J) = 0
          END IF
 10   CONTINUE
      MATRIX(IDM,IDM) = 0
      END
      SUBROUTINE DIAGO(D,I,J)
      INTEGER D
      IF(D.GE.0) THEN
        J = 1
        I = D + 1
      ELSE
        I = 1 
        J = ABS(D) + 1
      END IF
      END
      REAL FUNCTION DSCAN(SX,LX,PXIN,SY,LY,PYIN,NDD,
     +LW,SCORES,MAXC,MS)
C seqs sx,sy length lx,ly positions px,py
C number of diagonals nd
C length of windw lw
C min score ms
      PARAMETER (MAXD = 21)
      INTEGER SX(LX),PXIN,PYIN,SCORES(MAXC,MAXC),SY(LY)
      INTEGER PX,PY,CS(-MAXD:MAXD),CX(-MAXD:MAXD),CY(-MAXD:MAXD)
      PX = PXIN
      PY = PYIN
C      WRITE(*,*)'PXIN,PYIN,ND,LW,MS,MAXC,MAXW',
C     +PXIN,PYIN,ND,LW,MS,MAXC
C
C new scheme for choosing number of diagonals and reducing chnace
C of finding matches close to edge. Let the number of diagonals nd
C depend on the length of the diagonal we are going to scan along.
C <29 nd = 3, >269 = 15
C
      L = MIN(LX-PX,LY-PY)
      IF(L.LT.31) THEN
        ND = 3
      ELSE
        ND = MIN(MAXD,3+6*((L-30)/40))
      END IF
C      WRITE(*,*)L,ND
      ND2 = ND/2
C
C get start safely positioned by shifting the original p's
C
      I = 0
      IF(PX.LT.ND2+1) I = ND2 - PX + 1
      PX = PX + I
      PY = PY + I
C      WRITE(*,*)PX,PY
C
C set current x
C
      DO 10 I=-ND2,ND2
        CX(I) = PX + I
        CY(I) = PY
 10   CONTINUE
C      WRITE(*,*)(CX(K),CY(K),K=-ND2,ND2)
      DO 33 I=-MAXD,MAXD
        CS(I) = 0
 33     CONTINUE
C      CALL FILLI(CS(-ND2),ND,0)
C
C do the first window length
C
C      WRITE(*,*)(SY(K),K=1,10)
      DO 30 J=0,LW-1
        DO 20 I=-ND2,ND2
C       WRITE(*,*)'J,I,I+J,CX,CY,SX,SY',J,I,I+J,CX(I)+J,CY(I)+J,
C     + SX(CX(I)+J),SY(CY(I)+J)
C          WRITE(*,*)CX(I)+J,CY(I)+J
C          WRITE(*,*)SX(CX(I)+J),SY(CY(I)+J)
          CS(I) = CS(I) + SCORES(SX(CX(I)+J),SY(CY(I)+J))
 20     CONTINUE
 30   CONTINUE
C      WRITE(*,*)CS
      IDSCAN = 0
      IS = IMAXA(CS(-ND2),ND)
      IF(IS.GE.MS) DSCAN = DSCAN + 1
      NSTEPS = MIN(LX - (PX+LW-1+ND2),LY - (PY+LW-1+ND2))
C      NSTEPS = MIN(LX - (PX+LW-1),LY - (PY+LW-1+ND2))
C      WRITE(*,*)'NSTEPS',NSTEPS
      DO 50 J=1,NSTEPS
        DO 40 I=-ND2,ND2
          ICX = CX(I)
          ICY = CY(I)
          CS(I) = CS(I) - SCORES(SX(ICX),SY(ICY))
     +                  + SCORES(SX(ICX+LW),SY(ICY+LW))
C          CS(I) = CS(I) - SCORES(SX(CX(I)),CTONUM(SY(CY(I))))
C     +                  + SCORES(SX(CX(I)+LW),CTONUM(SY(CY(I)+LW)))
C          WRITE(*,*)'I,CX(I),CY(I)',I,CX(I),CY(I)
          CX(I) = CX(I) + 1
          CY(I) = CY(I) + 1
 40     CONTINUE
C      WRITE(*,*)J,CX(0),CY(0),(CS(K),K=-ND2,ND2)
      IS = IMAXA(CS(-ND2),ND)
      IF(IS.GE.MS) IDSCAN = IDSCAN + 1
C      WRITE(*,*)IS,IDSCAN
50    CONTINUE
      DSCAN = REAL(IDSCAN)/NSTEPS
      END
      INTEGER FUNCTION IMAXA(I,N)
      INTEGER I(N)
      IMAXA = I(1)
      DO 10 J=2,N
        IMAXA = MAX(IMAXA,I(J))
 10     CONTINUE
      END
      SUBROUTINE VPOUTR(IDEVO,IDEVN,NAME,LC,RC,
     +SCORE,VNAME,TNAME,KCUT,LCUT,EMAX,IOK)
      CHARACTER LINE*80,NAME*(*),VNAME*(*),TNAME*(*)
      INTEGER RC
      EXTERNAL NOTRL
C
C IDEVO original file
C IDEVN new reading file
C IDEVNG fofn for good data
C
 1005   FORMAT(A)
C
C Some ALU, so rename original file, and use old name for new file
C
      TNAME = NAME
      CLOSE(IDEVO)
      K = INDEX(NAME,' ')
      NAME(K:) = VNAME
      IOK = NCFRS(TNAME,NAME)
      IF(IOK.NE.0) THEN
        WRITE(*,*)'Error renaming',TNAME
        RETURN
      END IF
C
C Reopen the file with its new name
C
      CALL OPENRS(IDEVO,NAME,IOK,LRECL,2)
      IF(IOK.NE.0) THEN
        WRITE(*,*)'Error opening file',NAME
        RETURN
      END IF
C
C Open the new file with the original name
C
      CALL OPENRS(IDEVN,TNAME,IOK,LRECL,1)
      IF(IOK.NE.0) THEN
        WRITE(*,*)'Error opening file',NAME
        RETURN
      END IF
 5    CONTINUE
      READ(IDEVO,1005,ERR=6,END=7)LINE
      IF(LINE(1:1).EQ.';') THEN
        WRITE(IDEVN,1005)LINE(1:NOTRL(LINE,80,' '))
        GO TO 5
      END IF
 7    CONTINUE
      WRITE(IDEVN,1000)LC,RC-LC+1,SCORE
      IF(KCUT.GT.0) THEN
        WRITE(IDEVN,1000)KCUT,LCUT-KCUT+1,EMAX
      END IF
 1000 FORMAT(';;ALUS',2I7,F10.3)
      WRITE(IDEVN,1005,ERR=6)LINE(1:NOTRL(LINE,80,' '))
 8    CONTINUE
      READ(IDEVO,1005,ERR=6,END=9)LINE
      WRITE(IDEVN,1005,ERR=6)LINE(1:NOTRL(LINE,80,' '))
      GO TO 8
 9    CONTINUE
      CLOSE(IDEVO)
      CLOSE(IDEVN)
      IOK = 0
      RETURN
 6    CONTINUE
      WRITE(*,*)'Error in VPOUTR'
      IOK = 1
      END
      INTEGER FUNCTION NCFRS(FROM,TO)
      CHARACTER FROM*(*),TO*(*)
      INTEGER RENAME
      EXTERNAL RENAME
C
C rename file using system call to RENAME
C
      IFD = 0
      ITD = 0
      IF(INDEX(FROM,'/').EQ.0) THEN
        FROM = './'//FROM
        IFD = 1
      END IF
      IF(INDEX(TO,'/').EQ.0) THEN
        TO = './'//TO
        ITD = 1
      END IF
      NCFRS = RENAME(FROM,TO)
      IF(IFD.EQ.1) FROM = FROM(3:)
      IF(ITD.EQ.1) TO = TO(3:)
      END
