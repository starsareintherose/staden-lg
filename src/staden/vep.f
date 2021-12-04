      SUBROUTINE FMAIN()
      PARAMETER (MAXSEQ = 50000,
     +           MAXWLN = 6,
     +           LCONST = 4*MAXWLN,
     +           MAXWRD = 4**MAXWLN,
     +           MAXDEV = 5,
     +           NAMLEN = 60)
      CHARACTER SEQV(MAXSEQ*2),SEQG(MAXSEQ),SEQC(MAXSEQ)
      INTEGER WORDP(MAXWRD),POSN(MAXSEQ)
      REAL HIST(-MAXSEQ:MAXSEQ)
      INTEGER CONSTS(0:LCONST),DEVNOS(MAXDEV)
      INTEGER SEQVI(MAXSEQ),SEQGI(MAXSEQ),RC
      CHARACTER*(NAMLEN) FILNAM,HELPF,NAME,TNAME,VNAME
      EXTERNAL LASTO
C
C 5-2-92 replaced parts of the output routines vpout, vpouts
C with routines fmtc and fmtg
C
C This routine prepares a reading for the assembly program.
C It compares the sequence against vectors and clips off any found
C Vector clipping is of 4 types:
C 1) find 5' cloning site
C 2) look for 3' cloning site
C 3) test for insert being all vector
C 4) look for cosmid "vector"
C The first 3 can all be performed using one sequence, and are made easier
C by telling the program exactly where the cloning site is, and which of
C the 3 it is performing. Types 1,2 and 3 look only in one orientation,
C whereas 4 should check both strands. Additionally they
C differ in their outcomes: 1 and 2 write a new file with the clippoints
C marked, 3 scrubs the reading (does not add it to a file of file names),
C 4 writes out a new file if the reading contains some non vector sequence.
C The clip should be marked differently for cosmid vector, just so we know where
C it is (it can be tagged).
C Clipping off crap should probably be done first by looking at the traces.
C What do we need to tell the program?
C 1) name of vector file
C 2) position of cloning site
C 3) position of primer
C It would be helpful to have a standard orientation for vector sequences
C for example Cloning site------ ... ------etis gninolC for ? strand
C
C producing the output files will be much simpler when we move to expt
C file format for the sequence and its annotation
C
C
C   Initialise help - currently none
C      CALL INTHLP('vep', TOPOPT)
      ICG = 0
      ICB = 0
      IPG = 0
      IDM = 5
      CALL INITLU(IDM)
      CALL UNITNO(KBIN,KBOUT,DEVNOS,MAXDEV)
      IDEVNI = DEVNOS(1)
      IDEVN = DEVNOS(5)
      IDEVO = DEVNOS(2)
      IDEVNG = DEVNOS(3)
      IDEVNB = DEVNOS(4)
      WRITE(KBOUT,*)'VEP v2.1: vector excising program'
      FILNAM = ' '
      CALL OPENF1(DEVNOS(1),FILNAM,0,IOK,KBIN,KBOUT,
     +'Input file of file names',
     +IHELPS,IHELPE,HELPF,IDEVH)
      IF(IOK.NE.0) STOP
      FILNAM = 'm13mp18.seq'
      CALL OPENF1(DEVNOS(2),FILNAM,0,IOK,KBIN,KBOUT,
     +'File name of vector sequence',
     +IHELPS,IHELPE,HELPF,IDEVH)
      IF(IOK.NE.0) STOP
C
C simplify vector name to last field
C
      VNAME = FILNAM(1+LASTO(FILNAM,'/'):)
      IDIMV = MAXSEQ
      CALL ARRFIL(DEVNOS(2),SEQV,IDIMV,KBOUT)
      CLOSE(UNIT=DEVNOS(2))
C  check for contig header (should not be there)
      IF(SEQV(20).EQ.'>') THEN
        CALL SHFLCA(SEQV,MAXSEQ,21,1,IDIMV)
        IDIMV = IDIMV - 20
      END IF
      WRITE(KBOUT,*)'Vector length =',IDIMV
C cloning site used to distinguish search for cloning site
C from search for sequencing vector. A value of 0 signifies
C we are searching for cosmid vector
      MN = 0
      MX = IDIMV
      ICSITE = MIN(IDIMV,6249)
      CALL GETINT(MN,MX,ICSITE,
     +'3 prime side of cloning site (0 signifies cosmid vector)',
     +IVAL,KBIN,KBOUT,
     +IHELPS,IHELPE,HELPF,IDEVH,IOK)
      IF(IOK.NE.0) STOP
      ICSITE = IVAL
      IPSITE = 1
      IF(ICSITE.NE.0) THEN
        MN = -IDIMV
        MX = IDIMV
        IPSITE = 41
        CALL GETINT(MN,MX,IPSITE,
     +  'Relative position of 3 prime end of primer site',
     +  IVAL,KBIN,KBOUT,
     +  IHELPS,IHELPE,HELPF,IDEVH,IOK)
        IF(IOK.NE.0) STOP
        IPSITE = IVAL
C make cloning site end of seq, then start of seq is icsite + 1
        CALL SQCOPY(SEQV(1),SEQV(IDIMV+1),ICSITE)
C if forward primer then need to complement vector
        IF(IPSITE.GT.0) THEN
          CALL SQREV(SEQV(ICSITE+1),IDIMV)
          CALL SQCOM(SEQV(ICSITE+1),IDIMV)
        END IF
      END IF
      MN = 2
      MX = MAXWLN
      LENGTH = 4
      CALL GETINT(MN,MX,LENGTH,
     +'Word length',
     +IVAL,KBIN,KBOUT,
     +IHELPS,IHELPE,HELPF,IDEVH,IOK)
      IF(IOK.NE.0) STOP
      LENGTH = IVAL
      MN = 1
      MX = 11
      LW = 7
      CALL GETINT(MN,MX,LW,
     +'Number of diagonals to combine',
     +IVAL,KBIN,KBOUT,
     +IHELPS,IHELPE,HELPF,IDEVH,IOK)
      IF(IOK.NE.0) STOP
      LW = IVAL
      XMN = 0.1
      XMX = 1.0
      CUT = 0.35
      CALL GETRL(XMN,XMX,CUT,
     +'Cutoff score',
     +VAL,KBIN,KBOUT,
     +IHELPS,IHELPE,HELPF,IDEVH,IOK)
      IF(IOK.NE.0) STOP
      CUT = VAL
      FILNAM = ' '
      CALL OPENF1(DEVNOS(3),FILNAM,1,IOK,KBIN,KBOUT,
     +'Output file of passed file names',
     +IHELPS,IHELPE,HELPF,IDEVH)
      IF(IOK.NE.0) STOP
      IF(ICSITE.EQ.0) THEN
        FILNAM = ' '
        CALL OPENF1(DEVNOS(4),FILNAM,1,IOK,KBIN,KBOUT,
     +  'Output file of failed file names',
     +  IHELPS,IHELPE,HELPF,IDEVH)
        IF(IOK.NE.0) STOP
      END IF
      IDE = (IDM-1)**LENGTH
      CALL SETCN(CONSTS,LENGTH,IDM,LCONST)
      CALL CONNUM(SEQV(ICSITE+1),SEQVI,IDIMV)
      CALL ENCONC(SEQVI,IDIMV,POSN,WORDP,IDE,IDM,CONSTS,LENGTH,
     +LCONST)
 10   CONTINUE
      READ(IDEVNI,1002,END=100)NAME
      CALL OPENRS(IDEVO,NAME,IOK,LRECL,2)
      IF(IOK.NE.0) THEN
        WRITE(KBOUT,*)'Error opening file',NAME
        GO TO 10
      END IF
      IDIMGI = MAXSEQ
      CALL ARRFIM(IDEVO,SEQG,IDIMGI,KBOUT)
      JGEL = JGEL + 1
      WRITE(KBOUT,*)'>>>> Read number',JGEL,' length',IDIMGI,' ',NAME
1002  FORMAT(A)
C  LONG ENOUGH ?
      IF(IDIMGI.LT.LENGTH)THEN
        WRITE(KBOUT,*)' Gel reading too short to compare'
        GO TO 10
      END IF
      IDIMG = IDIMGI
      IF(ICSITE.GT.0) THEN
C
C   Sequencing vector clipping
C
        CALL CONNUM(SEQG,SEQGI,IDIMG)
        CALL VCUT(SEQVI,IDIMV,POSN,WORDP,IDE,SEQGI,IDIMG,CONSTS,
     +  LENGTH,IDM,LCONST,HIST,MAXSEQ,KBOUT,CUT,LW,LC,RC,ICSITE,
     +  IPSITE)
C        WRITE(*,*)'LC,RC',LC,RC
        CALL VPOUTS(IDEVO,NAME,LC,RC,SEQG,IDIMG,IDEVNG,IDEVN,
     +  VNAME,TNAME,ICB,ICG,IPG,IOK)
        GO TO 10
      END IF
C
C   Cosmid clipping
C
      CALL CONNUM(SEQG,SEQGI,IDIMG)
      CALL VCUT(SEQVI,IDIMV,POSN,WORDP,IDE,SEQGI,IDIMG,CONSTS,
     +  LENGTH,IDM,LCONST,HIST,MAXSEQ,KBOUT,CUT,LW,LC,RC,ICSITE,
     +  IPSITE)
      IF(LC.NE.0) THEN
C
C Match found so do output
C
        CALL VPOUT(IDEVO,NAME,LC,RC,SEQG,IDIMG,IDEVNG,IDEVNB,IDEVN,
     +  VNAME,TNAME,ICB,ICG,IPG,IOK)
        GO TO 10
      ELSE
C
C Try other strand
C
        CALL SQCOPY(SEQG,SEQC,IDIMG)
        CALL SQREV(SEQC,IDIMG)
        CALL SQCOM(SEQC,IDIMG)
        CALL CONNUM(SEQC,SEQGI,IDIMG)
        CALL VCUT(SEQVI,IDIMV,POSN,WORDP,IDE,SEQGI,IDIMG,CONSTS,
     +  LENGTH,IDM,LCONST,HIST,MAXSEQ,KBOUT,CUT,LW,LC,RC,ICSITE,
     +  IPSITE)
        IF(LC.NE.0) THEN
          LC1 = IDIMG - RC + 1
          RC = IDIMG - LC + 1
          LC = LC1
        END IF
        CALL VPOUT(IDEVO,NAME,LC,RC,SEQG,IDIMG,IDEVNG,IDEVNB,IDEVN,
     +  VNAME,TNAME,ICB,ICG,IPG,IOK)
      END IF
      GO TO 10
 100  CONTINUE
      WRITE(KBOUT,*)
     +'Finished after processing',JGEL,' files and finding'
      WRITE(KBOUT,*)ICB,' completely vector'
      WRITE(KBOUT,*)IPG,' partly vector'
      WRITE(KBOUT,*)ICG,' free of vector'
      END
      INTEGER FUNCTION LASTO(STRING,C)
      CHARACTER STRING*(*),C
C find last occurrence of C in STRING
      J1 = 1
      J = 0
      L = LEN(STRING)
 10   CONTINUE
      J1 = J1 + J
      J = INDEX(STRING(J1:L),C)
      IF(J.NE.0) GO TO 10
      LASTO = J1 - 1
      END
      SUBROUTINE VPOUT(IDEVO,NAME,LC,RC,SEQ,IDIMG,IDEVNG,IDEVNB,IDEVN,
     +VNAME,TNAME,ICB,ICG,IPG,IOK)
      CHARACTER LINE*80,NAME*(*),MTYPE*4,SEQ(IDIMG),VNAME*(*),TNAME*(*)
      CHARACTER*2 GOODS,LCLIPS,RCLIPS
      PARAMETER (GOODS='  ',LCLIPS=';<',RCLIPS=';>')
      INTEGER RC,FMTC
      EXTERNAL NCFRS,FMTC
C
C IDEVO original file
C IDEVN new reading file
C IDEVNG fofn for good data
C IDEVNB fofn for bad data
C      IDEVN = 6
C      WRITE(*,*)'LC,RC vpout',LC,RC
C
C Only handles one pair of clip points and assumes we discard lc to rc
C Possibilities and outcomes:
C 1. no vector: write name to fofn for good files
C 2. some vector: write name to fofn for good files, rename old file
C                to oldname.vector name, rewrite sequence file with new
C                clip points
C 3. all vector: write name to fofn for bad files
C
C
C If no vector:
C
      IF(LC.EQ.0) THEN
        CLOSE(IDEVO)
        WRITE(IDEVNG,1005)NAME
        ICG = ICG + 1
        RETURN
      END IF
C
C If all vector:
C
      IF(RC-LC+1.GE.IDIMG) THEN
        CLOSE(IDEVO)
        WRITE(IDEVNB,1005)NAME
 1005   FORMAT(A)
        ICB = ICB + 1
        RETURN
      END IF
C
C Some vector, so rename original file, and use old name for new file
C
      TNAME = NAME
      CLOSE(IDEVO)
      K = INDEX(NAME,' ')
      NAME(K:K) = '.'
      NAME(K+1:) = VNAME
      IOK = NCFRS(TNAME,NAME)
      IF(IOK.NE.0) THEN
        WRITE(*,*)'Error renaming',TNAME
        RETURN
      END IF
C
C Add the file name to the good file of file names
C
      WRITE(IDEVNG,1005)TNAME
      IPG = IPG + 1
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
      READ(IDEVO,1000,ERR=100)LOR,LCOR,LLOR,MTYPE,NAME
 1000   FORMAT(1X,3I6,A,A)
      IF(LC.EQ.1) THEN
        LCOR = LCOR + (RC - LC + 1)
        LLOR = LLOR - (RC - LC + 1)
      ELSE
        LLOR = LC - 1
      END IF
      WRITE(IDEVN,1006,ERR=100)LOR,LCOR,LLOR,MTYPE,
     +NAME(1:INDEX(NAME,' ')-1)
 1006 FORMAT(';',3I6,A,A)
      IF(FMTC(IDEVO,IDEVN,LINE,LCLIPS).NE.0) GO TO 100
C
C now add left clip if required (lc=1)
C
      IF(LC.EQ.1) THEN
        K2 = 1
        CALL FMTV(SEQ,IDIMG,K2,RC,LINE,LCLIPS,IDEVN)
C
C now add in the right clip info
C
        REWIND(IDEVO)
        IF(FMTC(IDEVO,IDEVN,LINE,RCLIPS).NE.0) GO TO 100
C
C Now do seq between RC and end as good seq
C
        IF(RC.LT.IDIMG) THEN
          K2 = RC + 1
          CALL FMTV(SEQ,IDIMG,K2,IDIMG,LINE,GOODS,IDEVN)
        END IF
      ELSE
C
C do seq in between LC and RC  as a right clip (lc/=1)
C
        K2 = LC
        CALL FMTV(SEQ,IDIMG,K2,RC,LINE,RCLIPS,IDEVN)
C
C now add in the extant right clip info
C
        REWIND(IDEVO)
        IF(FMTC(IDEVO,IDEVN,LINE,RCLIPS).NE.0) GO TO 100
C
C Do data up to lc as good seq
C
        K2 = 1
        CALL FMTV(SEQ,IDIMG,K2,LC-1,LINE,GOODS,IDEVN)
      END IF
      CLOSE(IDEVO)
      CLOSE(IDEVN)
      IOK = 0
      RETURN
 100  CONTINUE
      WRITE(*,*)'Error in VPOUT'
      IOK = 1
      END
      SUBROUTINE FMTV(SEQ,IDIMG,LC,RC,LINE,LT,IDEVN)
      CHARACTER LINE*(*),SEQ(IDIMG),LT*(*)
      INTEGER RC
C      WRITE(*,*)'LC,RC IN FMTV',LC,RC
C
C  write out seq from lc to rc with line type lt
C
      K2 = LC - 1
 10   CONTINUE
      K1 = K2 + 1
      IF (K2.NE.RC) THEN
        K2 = MIN(K1+49,RC)
        WRITE(IDEVN,1000)LT,(SEQ(K),K=K1,K2)
        GO TO 10
      END IF
 1000 FORMAT(A,50A1)
      END
      INTEGER FUNCTION FMTC(IDEVO,IDEVN,LINE,LT)
      CHARACTER LINE*(*),LT*(*)
 10   CONTINUE
      READ(IDEVO,1000,ERR=100,END=200)LINE
      IF(LINE(1:2).EQ.LT) THEN
        WRITE(IDEVN,1000,ERR=100)LINE
      END IF
      GO TO 10
 100  CONTINUE
      FMTC = 1
      RETURN
 200  CONTINUE
      FMTC = 0
 1000 FORMAT(A)
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
      SUBROUTINE VPOUTS(IDEVO,NAME,LC,RC,
     +SEQ,IDIMG,IDEVNG,IDEVN,
     +VNAME,TNAME,ICB,ICG,IPG,IOK)
      CHARACTER LINE*80,NAME*(*),MTYPE*4,SEQ(IDIMG),VNAME*(*),TNAME*(*)
      CHARACTER*2 GOODS,LCLIPS,RCLIPS
      PARAMETER (GOODS='  ',LCLIPS=';<',RCLIPS=';>')
      INTEGER RC,FMTC
      EXTERNAL NCFRS,FMTC
C
C IDEVO original file
C IDEVN new reading file
C IDEVNG fofn for good data
C      IDEVN = 6
C       WRITE(*,*)'LC,RC',LC,RC
C
C handles 2 pairs of clip points
C Possibilities and outcomes:
C 1. no vector: write name to fofn for good files
C 2. some vector: write name to fofn for good files, rename old file
C                to oldname.vector name, rewrite sequence file with new
C                clip points
C 3. all vector: do nothing
C
C keep lc+1 to rc-1 ie discard 1 to lc and rc to end
C if no right cut we come in with rc = idimg+1
C if no left cut lc = 0
C
C If no vector:
C
      IF((LC.EQ.0).AND.(RC.EQ.IDIMG+1)) THEN
        CLOSE(IDEVO)
        WRITE(IDEVNG,1005)NAME
        ICG = ICG + 1
        RETURN
      END IF
C
C If all vector: or < 20 bases left
C
      IF(RC-LC.LT.20) THEN
        CLOSE(IDEVO)
 1005   FORMAT(A)
        ICB = ICB + 1
        RETURN
      END IF
C
C Some vector, so rename original file, and use old name for new file
C
      TNAME = NAME
      CLOSE(IDEVO)
      K = INDEX(NAME,' ')
      NAME(K:K) = '.'
      NAME(K+1:) = VNAME
      IOK = NCFRS(TNAME,NAME)
      IF(IOK.NE.0) THEN
        WRITE(*,*)'Error renaming',TNAME
        RETURN
      END IF
C
C Add the file name to the good file of file names
C
      WRITE(IDEVNG,1005)TNAME
      IPG = IPG + 1
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
      READ(IDEVO,1000,ERR=5)LOR,LCOR,LLOR,MTYPE,NAME
 1000   FORMAT(1X,3I6,A,A)
      GO TO 6
 5    CONTINUE
C
C  assume no header present (say scanner data)
C
      LOR = IDIMG
      LCOR = 0
      LLOR = IDIMG
      MTYPE = 'PLN '
      NAME = ' '
 6    CONTINUE
      LCOR = LCOR + LC
      LLOR = LLOR - (LC + IDIMG-RC+1)
      WRITE(IDEVN,1006,ERR=100)LOR,LCOR,LLOR,MTYPE,
     +NAME(1:MAX(1,INDEX(NAME,' ')-1))
 1006 FORMAT(';',3I6,A,A)
      IF(FMTC(IDEVO,IDEVN,LINE,LCLIPS).NE.0) GO TO 100
C
C now add left clip if required (lc=1)
C
      IF(LC.NE.0) THEN
        K2 = 1
        CALL FMTV(SEQ,IDIMG,K2,LC,LINE,LCLIPS,IDEVN)
      END IF
C
C now add new right clip if required
C
      IF(RC.NE.IDIMG+1) THEN
        K2 = RC
        CALL FMTV(SEQ,IDIMG,K2,IDIMG,LINE,RCLIPS,IDEVN)
      END IF
C
C now add in the extant right clip info
C
        REWIND(IDEVO)
        IF(FMTC(IDEVO,IDEVN,LINE,RCLIPS).NE.0) GO TO 100
C
C Now do seq between LC+1 and RC-1 as good seq
C
      K2 = LC + 1
      CALL FMTV(SEQ,IDIMG,K2,RC-1,LINE,GOODS,IDEVN)
      CLOSE(IDEVO)
      CLOSE(IDEVN)
      IOK = 0
      RETURN
 100  CONTINUE
      WRITE(*,*)'Error in VPOUTS'
      IOK = 1
      END
      SUBROUTINE VCUT(SEQV,IDIMV,POSN,WORDP,IDE,SEQH,IDIMH,CONSTS,
     +LENGTH,IDM,LCONST,HIST,MAXSEQ,KBOUT,CUT,LW,LC,RC,ICSITE,
     +IPSITE)
      INTEGER SEQV(IDIMV),SEQH(IDIMH)
      INTEGER POSN(IDIMV),WORDP(IDE),CONSTS(0:LCONST)
      INTEGER RC
      REAL HIST(-MAXSEQ:MAXSEQ)
      EXTERNAL NCODEA
C      CALL BUSY(KBOUT)
      CALL FILLR(HIST(LENGTH-IDIMV),IDIMH+IDIMV+1,0.)
      DO 20 I = 1,IDIMH-LENGTH+1
        J = NCODEA(SEQH(I),LENGTH,CONSTS,IDM,LCONST)
        IF(J.NE.0)THEN
          J1 = WORDP(J)
          IF(J1.NE.0)THEN
            K = I - J1
            HIST(K) = HIST(K) + 1.
10          CONTINUE
            J2 = J1
            J1 = POSN(J2)
            IF(J1.NE.0)THEN
              K = I - J1
              HIST(K) = HIST(K) + 1.
              GO TO 10
            END IF
          END IF
        END IF
20    CONTINUE
      CALL PHIST(HIST,IDIMV,IDIMH,LENGTH,MAXSEQ)
      IF(ICSITE.NE.0) THEN
C
C  look for primer region
C
        CALL FCUT(HIST,IDIMV,IDIMH,LENGTH,
     +  MAXSEQ,CUT,LW,LC,RC,IPSITE,1)
C        WRITE(*,*)'LC',LC
C        WRITE(*,*)'RC',RC
        IF (LC.EQ.0) THEN
C          WRITE(KBOUT,*)' ***** No primer site found ********'
        ELSE
          LC = RC
        END IF
C
C  look for running into vector at cloning site
C
        CALL FCUT(HIST,IDIMV,IDIMH,LENGTH,
     +  MAXSEQ,CUT,LW,LCR,IRCR,IPSITE,2)
C
C set right cut to 1 past the end of the sequence
C
C        WRITE(*,*)'LCR',LCR
        RC = IDIMH + 1
        IF(LCR.GT.0) THEN
          RC = LCR
        END IF
      ELSE
C
C  look for cosmid vector
C
        CALL FCUT(HIST,IDIMV,IDIMH,LENGTH,
     +  MAXSEQ,CUT,LW,LC,RC,IPSITE,3)
        IF(LC.GT.0) THEN
C          WRITE(KBOUT,*)
C     +'>>>>>>>>>>>>>>>>>>>>>>>>diagonal found'
         RETURN
        END IF
      END IF
      END
      SUBROUTINE PHIST(HIST,IDIMV,IDIMH,LENGTH,MAXSEQ)
      REAL HIST(-MAXSEQ:MAXSEQ)
      IF(IDIMV.GE.IDIMH) THEN
        D  = LENGTH
        DO 10 I=LENGTH-IDIMV,IDIMH-IDIMV-1
          HIST(I) = HIST(I)/D
          D = D + 1
 10       CONTINUE
        D  = IDIMH
        DO 20 I=IDIMH-IDIMV,0
          HIST(I) = HIST(I)/D
 20       CONTINUE  
        D  = IDIMH - 1
        DO 30 I=1,IDIMH-LENGTH
          HIST(I) = HIST(I)/D
          D = D - 1
 30       CONTINUE
      ELSE
        D  = LENGTH
        DO 40 I=LENGTH-IDIMV,-1
          HIST(I) = HIST(I)/D
          D = D + 1
 40       CONTINUE
        D  = IDIMV
        DO 50 I=0,IDIMH-IDIMV
          HIST(I) = HIST(I)/D
 50       CONTINUE
        D  = IDIMV - 1
        DO 60 I=IDIMH-IDIMV+1,IDIMH-LENGTH
          HIST(I) = HIST(I)/D
          D = D - 1
 60       CONTINUE
      END IF
      END
      SUBROUTINE FCUT(HIST,IDIMV,IDIMH,LENGTH,
     + MAXSEQ,CUT,LW,LC,RC,PSITE,JOB)
      REAL HIST(-MAXSEQ:MAXSEQ)
      INTEGER RC,PSITE
C PSITE is primer site
      LC = 0
      RC = 0
      DMAX = 0.
C If job = 1 look for cloning site from psite to the end of the vector
C if job = 2 look for vector in the rest of the sequence
C If job = 3 look for cosmid vector in whole of sequence
C We discard lc to rc inclusive, lc=0 means discard nothing
      IF(JOB.EQ.1) THEN
        I1 = LENGTH - IDIMV
        I2 =  -ABS(PSITE)
      ELSE IF(JOB.EQ.2) THEN
        I1 = -ABS(PSITE)
        I2 = IDIMH - LENGTH
      ELSE IF(JOB.EQ.3) THEN
        I1 = LENGTH - IDIMV
        I2 = IDIMH - LENGTH
      ELSE
        WRITE(*,*)'Error in FCUT'
        RETURN
      END IF
C      WRITE(*,*)'LOOKING AT ',I1,I2
      DO 10 I=I1,I2
        DT = HIST(I)
        IF(DT.GT.DMAX) THEN
          DMAX = DT
          ID = I
        END IF
 10     CONTINUE
      D = 0.
      DO 35 I=MAX(ID-LW/2,LENGTH-IDIMV),
     +        MIN(ID+LW/2,IDIMH-LENGTH)
        D = D + HIST(I)
 35   CONTINUE
C      WRITE(*,*)'Best diagonal, score and local sum',ID,DMAX,D
C      WRITE(*,1000)ID,DMAX,D
 1000 FORMAT(I6,2F10.3)
      IF(D.LT.CUT) RETURN
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
      WRITE(*,*)' Discard ',LC, ' to ',RC
      END
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
