C 18 Feb 1993 forced cloning vector clipped regions to be >MINVEC long
C 20 October 1992
C   IDEVE was being set to 0 before EXPKIL() and so the C files
C   were never being closed.
C
      PARAMETER (MAXSEQ = 50000,
     +           MAXWLN = 6,
     +           LCONST = 4*MAXWLN,
     +           MAXWRD = 4**MAXWLN,
     +           MAXDEV = 5,
     +           NAMLEN = 80)
      CHARACTER SEQV(MAXSEQ*2),SEQG(MAXSEQ),SEQC(MAXSEQ)
      INTEGER WORDP(MAXWRD),POSN(MAXSEQ)
      REAL HIST(-MAXSEQ:MAXSEQ)
      INTEGER CONSTS(0:LCONST),DEVNOS(MAXDEV)
      INTEGER SEQVI(MAXSEQ),SEQGI(MAXSEQ),RC,POORR,POORL
      CHARACTER*(NAMLEN) FILNAM,HELPF,NAME,FILNMV
      PARAMETER (MAXPRM = 22)
      CHARACTER PROMPT(2)*(MAXPRM)
      INTEGER GNFFOF,REXGEL,REXSVF,REXSVC,REXSVP,REXPDP,REXCVF
      INTEGER WEXSVP,WEXCVP,SVLCLP,SVRCLP,EXPOPN,REXSVQ
      EXTERNAL GNFFOF,REXGEL,REXSVF,REXSVC,REXSVP,REXPDP,REXCVF
      EXTERNAL WEXSVP,WEXCVP,EXPOPN,REXSVQ
C
C  new experiment file version of vep (vepe)
C
C
C
C This routine prepares a reading for the assembly program.
C It compares the sequence against vectors and marks any found
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
C NOTE that vector file should not have <---name.0001----> at the start
C
C error values written to file of failed names
C
C 1 couldnt open expt file
C 2 couldnt get reading from expt file
C 3 reading too short
C 4 couldnt find vector filename in expt file
C 5 couldnt find cloning site in expt file
C 6 couldnt find primer site in expt file
C 7 couldnt open vector file
C 8 failed to write to expt file
C 9 completely vector
C
      ICG = 0
      ICB = 0
      IPG = 0
      IDM = 5
      CALL INITLU(IDM)
      CALL UNITNO(KBIN,KBOUT,DEVNOS,MAXDEV)
      IDEVNI = DEVNOS(1)
      IDEVV = DEVNOS(2)
      IDEVNO = DEVNOS(3)
      IDEVNF = DEVNOS(5)
      WRITE(KBOUT,*)'vepe v2.0: vector excising program. June 92'
      PROMPT(1) = 'Mark sequencing vector'
      PROMPT(2) = 'Mark cloning vector'
      JOB = 1
      CALL RADION('Select task',PROMPT,2,JOB,
     +IHELPS,IHELPE,HELPF,IDEVH,KBIN,KBOUT)
      IF(JOB.LT.1) STOP
      FILNAM = ' '
      CALL OPENF1(IDEVNI,FILNAM,0,IOK,KBIN,KBOUT,
     +'Input file of file names',
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
C
C set initial values so we hash the first vector
C
      FILNMV = ' '
      ICSITT = 0
      IPSITT = 0
      FILNAM = 'UNLIKELY FILE NAME'
      ICSITE = ICSITT
      IPSITE = IPSITT
      IDE = (IDM-1)**LENGTH
      CALL SETCN(CONSTS,LENGTH,IDM,LCONST)
C
      IDEVE = 0
 10   CONTINUE
C
C main loop: get the info we need from the experiment file
C            for sequencing vector (job=1) we need:
C            the reading
C            its poor data pointers
C            the vector
C            the cloning site
C            the primer position
C if poor data pointers are zero assume all ok
C if vector, cloning site, primer site not found skip
C
C            for cloning vector (job=2) we need:
C            the reading
C            its sequencing vector pointers
C            its poor data pointers
C            the vector
C
C if sequencing vector pointers are zero use poor data pointers
C if poor data pointers are zero assume all ok
C if vector not found skip
C
C For both jobs shuffle sequence so we only process the unmarked segment
C then add on marker values at end
C Only hash vector if its different from the last one.
C
C Output: markers relative to the left end of the reading and name to file of
C         passed file names. Plus a summary at the end.
C
C Make sure the experiment file is closed
      CALL EXPKIL(IDEVE)
C Get next experiment file name
      IOK = GNFFOF(IDEVNI,NAME)
      IF(IOK.EQ.1) THEN
        WRITE(KBOUT,*)
     + 'Finished after processing',JGEL,' files and finding'
        WRITE(KBOUT,*)ICB,' completely vector'
        WRITE(KBOUT,*)IPG,' partly vector'
        WRITE(KBOUT,*)ICG,' free of vector'
        STOP
      ELSE IF(IOK.EQ.2) THEN
        CALL ERROM(KBOUT,'Empty line in file of file names')
        GO TO 10
      ELSE IF(IOK.EQ.3) THEN
        CALL ERROM(KBOUT,'Error reading file of file names')
        GO TO 10
      END IF
      IDEVE = EXPOPN(NAME)
      IF(IDEVE.EQ.0) THEN
        CALL AERROR(KBOUT,IDEVNF,NAME,1)
        CALL ERROM(KBOUT,'Error opening experiment file')
        GO TO 10
      END IF
      IDIMGI = MAXSEQ
      IOK = REXGEL(IDEVE,SEQG,IDIMGI)
      IF(IOK.NE.0) THEN
        CALL AERROR(KBOUT,IDEVNF,NAME,2)
        CALL ERROM(KBOUT,'Error getting gel reading')
        GO TO 10
      END IF
      JGEL = JGEL + 1
      WRITE(KBOUT,*)'>>>> Read number',JGEL,' length',IDIMGI,' ',NAME
C  LONG ENOUGH ?
      IF(IDIMGI.LT.LENGTH)THEN
        CALL AERROR(KBOUT,IDEVNF,NAME,3)
        CALL ERROM(KBOUT,'Gel reading too short to compare')
        GO TO 10
      END IF
      IDIMG = IDIMGI
C
C
      IF(JOB.EQ.1) THEN
C
C   Sequencing vector clipping
C
C   get vector name, csite (cloning site), ipsite (primer site),
C   poorl (poor data left mark), poorr
C
       IOK = REXSVF(IDEVE,FILNMV)
       IF(IOK.NE.0) THEN
         CALL AERROR(KBOUT,IDEVNF,NAME,4)
         CALL ERROM(KBOUT,'Error reading vector file name')
         CALL ERROM(KBOUT,FILNMV)
         GO TO 10
       END IF
       IOK = REXSVC(IDEVE,ICSITT)
       IF(IOK.NE.0) THEN
         CALL AERROR(KBOUT,IDEVNF,NAME,5)
         CALL ERROM(KBOUT,'Error reading cloning site')
         GO TO 10
       END IF
       IOK = REXSVQ(IDEVE,IPSITT)
       IF(IOK.NE.0) THEN
         CALL AERROR(KBOUT,IDEVNF,NAME,6)
         CALL ERROM(KBOUT,'Error reading primer site')
         GO TO 10
       END IF
       IOK = REXPDP(IDEVE,POORL,POORR)
       IF(IOK.NE.0) THEN
C         CALL ERROM(KBOUT,
C     +   'Error reading poor data positions, zero assumed')
       END IF
C
C   Decided to screen for primer in the poor data at the left end even
C   though the program expects to ignore it: set poorl=0
      POORL = 0
C
C  IF ANY OF THESE ARE MISSING DO SOMETHING SENSIBLE !!!!!!!!!!!!!
C
C   if filnam != current or icsitt != icsite or ipsitt != icsite
C      then reorganise vector
C
      IF((FILNMV.NE.FILNAM).OR.
     +   (ICSITT.NE.ICSITE).OR.
     +   (IPSITT.NE.IPSITE))    THEN
        CALL OPENRS(IDEVV,FILNMV,IOK,LRECL,2)
        IF(IOK.NE.0) THEN
          CALL AERROR(KBOUT,IDEVNF,NAME,7)
          CALL ERROM(KBOUT,'Error opening vector file')
          CALL ERROM(KBOUT,FILNMV)
          GO TO 10
        END IF
        FILNAM = FILNMV
        ICSITE = ICSITT
        IPSITE = IPSITT
        IDIMV  = MAXSEQ
        CALL ARRFIL(IDEVV,SEQV,IDIMV,KBOUT)
        CLOSE(UNIT=IDEVV)
C  check for contig header (should not be there)
          IF(SEQV(20).EQ.'>') THEN
            CALL SHFLCA(SEQV,MAXSEQ,21,1,IDIMV)
            IDIMV = IDIMV - 20
          END IF
        WRITE(KBOUT,*)'Vector length =',IDIMV
C make cloning site end of seq, then start of seq is icsite + 1
        CALL SQCOPY(SEQV(1),SEQV(IDIMV+1),ICSITE)
C if forward primer then need to complement vector
        IF(IPSITE.GT.0) THEN
          CALL SQREV(SEQV(ICSITE+1),IDIMV)
          CALL SQCOM(SEQV(ICSITE+1),IDIMV)
        END IF
        CALL CONNUM(SEQV(ICSITE+1),SEQVI,IDIMV)
        CALL ENCONC(SEQVI,IDIMV,POSN,WORDP,IDE,IDM,CONSTS,LENGTH,
     +  LCONST)
      END IF
C
C now hash the reading
C
C clip reading so only the good data is processed
C
C        WRITE(*,*)'POORL,POORR',POORL,POORR
C        WRITE(*,*)(SEQG(K),K=1,IDIMG)
C        WRITE(*,*)'IDIMG',IDIMG
        IF(POORR.EQ.0) POORR = IDIMG + 1
        IF(POORL.GT.0) THEN
          CALL SHFLCA(SEQG,MAXSEQ,POORL+1,1,POORR-1)
        END IF
        IDIMG = POORR - POORL - 1
C        WRITE(*,*)'IDIMG',IDIMG
C        WRITE(*,*)(SEQG(K),K=1,IDIMG)
        CALL CONNUM(SEQG,SEQGI,IDIMG)
        CALL VCUT(SEQVI,IDIMV,POSN,WORDP,IDE,SEQGI,IDIMG,CONSTS,
     +  LENGTH,IDM,LCONST,HIST,MAXSEQ,KBOUT,CUT,LW,LC,RC,ICSITE,
     +  IPSITE)
C
C 1 nothing found either end   lc = 0, rc = idimg+1
C 2 primer only found          lc = right end of primer, rc = idimg+1
C 3 run into vector only       lc = 0  rc = start of vector
C 4 primer found and vector run into lc = right end of primer, 
C                                    rc = start of vector
CC
C if rc = idimg+1 theres no vector at the right end of the reading
C
C        WRITE(*,*)'LC,RC',LC,RC
        IF(RC-LC+1.LT.20) THEN
          CALL AERROR(KBOUT,IDEVNF,NAME,9)
          ICB = ICB + 1
          GO TO 10
        END IF
C
C  note below we need to add on what weve cutoff
C
        IF((LC.EQ.0).AND.(RC.EQ.IDIMG+1)) THEN
C
C No sequencing vector found
C
          LCO = 0
          IRCO = IDIMGI + 1
          IOK = WEXSVP(IDEVE,LCO,IRCO)
          IF(IOK.NE.0) THEN
            CALL AERROR(KBOUT,IDEVNF,NAME,8)
            GO TO 10
          END IF
          ICG = ICG + 1
        ELSE IF((LC.NE.0).AND.(RC.NE.IDIMG+1)) THEN
C
C Primer found and run into sequencing vector
C
          LCO = LC + POORL
          IRCO = RC + POORL
          IOK = WEXSVP(IDEVE,LCO,IRCO)
          IF(IOK.NE.0) THEN
            CALL AERROR(KBOUT,IDEVNF,NAME,8)
            GO TO 10
          END IF
          IPG = IPG + 1
        ELSE IF(LC.EQ.0) THEN
C
C Run into vector only
C
          LCO = 0
          IRCO = RC + POORL
          IOK = WEXSVP(IDEVE,LCO,IRCO)
          IF(IOK.NE.0) THEN
            CALL AERROR(KBOUT,IDEVNF,NAME,8)
            GO TO 10
          END IF
          IPG = IPG + 1
        ELSE IF(RC.EQ.IDIMG+1) THEN
C
C Primer only found
C
          LCO = LC + POORL
          IRCO = IDIMGI + 1
          IOK = WEXSVP(IDEVE,LCO,IRCO)
          IF(IOK.NE.0) THEN
            CALL AERROR(KBOUT,IDEVNF,NAME,8)
            GO TO 10
          END IF
          IPG = IPG + 1
        END IF
        WRITE(IDEVNO,1005)NAME
 1005   FORMAT(A)
C
C
      ELSE IF (JOB.EQ.2) THEN
C
C   Cosmid clipping
C
C   get vector file name, poorl (poor data left mark), poorr,
C   svlclp (sequencing vector left clip position), svrclp
C
       IOK = REXCVF(IDEVE,FILNMV)
       IF(IOK.NE.0) THEN
         CALL AERROR(KBOUT,IDEVNF,NAME,4)
         CALL ERROM(KBOUT,'Error reading vector file name')
         CALL ERROM(KBOUT,FILNMV)
         GO TO 10
       END IF
       IOK = REXPDP(IDEVE,POORL,POORR)
       IF(IOK.NE.0) THEN
C         CALL ERROM(KBOUT,
C     +   'Error reading poor data positions, zero assumed')
       END IF
       IOK = REXSVP(IDEVE,SVLCLP,SVRCLP)
       IF(IOK.NE.0) THEN
C         CALL ERROM(KBOUT,
C     +   'Error reading sequencing vector clip points, zero assumed')
       END IF
C
C   if filnam != current
C      then reorganise vector
C
        IF(FILNMV.NE.FILNAM) THEN
          CALL OPENRS(IDEVV,FILNMV,IOK,LRECL,2)
          IF(IOK.NE.0) THEN
            CALL AERROR(KBOUT,IDEVNF,NAME,7)
            CALL ERROM(KBOUT,'Error opening vector file')
            CALL ERROM(KBOUT,FILNMV)
            GO TO 10
          END IF
          FILNAM = FILNMV
          IDIMV  = MAXSEQ
          CALL ARRFIL(IDEVV,SEQV,IDIMV,KBOUT)
          CLOSE(UNIT=IDEVV)
C  check for contig header (should not be there)
          IF(SEQV(20).EQ.'>') THEN
            CALL SHFLCA(SEQV,MAXSEQ,21,1,IDIMV)
            IDIMV = IDIMV - 20
          END IF
          WRITE(KBOUT,*)'Vector length =',IDIMV
          CALL CONNUM(SEQV,SEQVI,IDIMV)
          CALL ENCONC(SEQVI,IDIMV,POSN,WORDP,IDE,IDM,CONSTS,LENGTH,
     +    LCONST)
        END IF
C
C now hash the reading
C
C clip reading so only the good data is processed
C and set poorl and poorr to endpoints
          POORL = MAX(POORL,SVLCLP)
          I = IDIMG + 1
          IF(SVRCLP.GT.0) I = SVRCLP
          IF(POORR.GT.0) I = MIN(I,POORR)
          POORR = I
C
C        WRITE(*,*)'POORL,POORR',POORL,POORR
C        WRITE(*,*)(SEQG(K),K=1,IDIMG)
C        WRITE(*,*)'IDIMG',IDIMG
          IF(POORL.GT.0) THEN
            CALL SHFLCA(SEQG,MAXSEQ,POORL+1,1,POORR-1)
          END IF
          IDIMG = POORR - POORL - 1
C        WRITE(*,*)'IDIMG',IDIMG
C        WRITE(*,*)(SEQG(K),K=1,IDIMG)
          CALL CONNUM(SEQG,SEQGI,IDIMG)
          CALL VCUT(SEQVI,IDIMV,POSN,WORDP,IDE,SEQGI,IDIMG,CONSTS,
     +    LENGTH,IDM,LCONST,HIST,MAXSEQ,KBOUT,CUT,LW,LC,RC,0,0)
          IF(LC.NE.0) THEN
            IF(RC-LC+1.GE.IDIMG) THEN
              ICB = ICB + 1
              CALL AERROR(KBOUT,IDEVNF,NAME,9)
            ELSE
              IPG = IPG + 1
              WRITE(IDEVNO,1005)NAME
            END IF
            LCO = LC + POORL
            IRCO = RC + POORL
            IOK = WEXCVP(IDEVE,LCO,IRCO)
            IF(IOK.NE.0) THEN
              CALL AERROR(KBOUT,IDEVNF,NAME,8)
C              CALL ERROM(KBOUT,
C     +        'Error writing cosmid vector positions')
              GO TO 10
            END IF
            GO TO 10
          END IF
C
C Try other strand
C
          CALL SQCOPY(SEQG,SEQC,IDIMG)
          CALL SQREV(SEQC,IDIMG)
          CALL SQCOM(SEQC,IDIMG)
          CALL CONNUM(SEQC,SEQGI,IDIMG)
          CALL VCUT(SEQVI,IDIMV,POSN,WORDP,IDE,SEQGI,IDIMG,CONSTS,
     +    LENGTH,IDM,LCONST,HIST,MAXSEQ,KBOUT,CUT,LW,LC,RC,0,0)
          IF(LC.NE.0) THEN
            LC1 = IDIMG - RC + 1
            RC = IDIMG - LC + 1
            LC = LC1
            IF(RC-LC+1.GE.IDIMG) THEN
              ICB = ICB + 1
              CALL AERROR(KBOUT,IDEVNF,NAME,9)
            ELSE
              WRITE(IDEVNO,1005)NAME
              IPG = IPG + 1
            END IF
            LCO = LC + POORL
            IRCO = RC + POORL
            IOK = WEXCVP(IDEVE,LCO,IRCO)
            IF(IOK.NE.0) THEN
              CALL AERROR(KBOUT,IDEVNF,NAME,8)
C              CALL ERROM(KBOUT,
C     +          'Error writing cosmid vector positions')
              GO TO 10
            END IF
            GO TO 10
          END IF
          ICG = ICG + 1
          WRITE(IDEVNO,1005)NAME
          GO TO 10
        ELSE
C
C   unknown job !!!!!!!!
C
        WRITE(*,*)'COCKUP'
      END IF
      GO TO 10
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
        IF (LC.EQ.0) THEN
C          WRITE(KBOUT,*)' ***** No primer site found ********'
        ELSE
C
C  if primer found we want to know where it ends so set lc=rc
C
          LC = RC
        END IF
C
C  look for running into vector at cloning site
C
        CALL FCUT(HIST,IDIMV,IDIMH,LENGTH,
     +  MAXSEQ,CUT,LW,LCR,IRCR,IPSITE,2)
C
C set right cut to 1 past the end of the sequence
C if some vector found we set rc to where it starts
C 
        RC = IDIMH + 1
        IF(LCR.GT.0) THEN
          RC = LCR
        END IF
C
C so the outcomes are:
C 1 nothing found either end   lc = 0, rc = idimg+1
C 2 primer only found          lc = right end of primer, rc = idimg+1
C 3 run into vector only       lc = 0  rc = start of vector
C 4 primer found and vector run into lc = right end of primer, 
C                                    rc = start of vector
C
C
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
      PARAMETER (MINVEC = 6)
C MINVEC is minimum length of cloning vector to be reported
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
        I1 = LENGTH - IDIMV + MINVEC
        I2 = IDIMH - LENGTH - MINVEC
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
      SUBROUTINE SHFTLS(STRING,FROMS,TO,FROME)
      CHARACTER STRING*(*)
      INTEGER FROMS,TO,FROME
C
C shift a string left from froms to to
C
      J = TO
      DO 10 I=FROMS,FROME
        STRING(J:J) = STRING(I:I)
        J = J + 1
 10   CONTINUE
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
