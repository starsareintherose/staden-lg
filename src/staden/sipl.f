C  SIPL
C  AUTHOR RODGER STADEN
C  14-11-90 Replaced radio by radion
C   11-12-90 Changed library handling: added filnll and paramter
C  12-12-90 Changed max word length for dna from 4 to 6 and max 
C           word length for protein to 3
C  25-2-92 changed call to rdseq (uses the stack array for temp
C          storage so call to begina is also changed)
C  2-3-92 set filnam = ' ' for calls to openf1
      SUBROUTINE FMAIN()
      PARAMETER (NAMLEN = 60)
      CHARACTER*(NAMLEN) FILNAM,FILEP,HELPF
      CHARACTER*(NAMLEN) LIBLF
      INTEGER STKREC
      PARAMETER (
     +           MAXEN = 10000,
     +           MAXSEQ  = 20000,
     +           NMAX  = 20000,
     +           STKREC = 11,
     +           MAXSTK = NMAX*3*STKREC,
     +           MAXDEV=10,
     +           MAXK = 3,
     +           IDMM = 26,
     +           LCONST = MAXK*(IDMM-1),
     +           FILEP='PROTMAT',
     +           LIBLF = 'SEQUENCELIBRARIES')
      INTEGER MATRIX(IDMM,IDMM),DEVNOS(MAXDEV)
      INTEGER WTS(0:IDMM,0:IDMM)
      CHARACTER SEQ1(MAXSEQ),SEQ2(MAXSEQ)
      CHARACTER SEQ1A(NMAX),SEQ2A(NMAX)
      INTEGER SEQ1N(NMAX),SEQ2N(NMAX),SEQS(MAXSEQ)
      INTEGER STACK(0:MAXSTK)
      INTEGER CC(0:NMAX+1),DD(0:NMAX+1)
      INTEGER RR(0:NMAX+1),SS(0:NMAX+1)
      INTEGER SOP(0:2*NMAX)
      INTEGER MATDNA(5,5),SPAN,SPANCT
      INTEGER SCORE(MAXEN),PENTRY,PSCORE(MAXEN)
      INTEGER HIST(-MAXSEQ:MAXSEQ),WORDP(IDMM**MAXK)
      INTEGER POSN(MAXSEQ),CONSTS(0:LCONST),MAIND(IDMM)
      CHARACTER CHRSET(IDMM)
      CHARACTER TITLE*60,NAMIN*10,ENAMEL*10,LTYPE
      CHARACTER ENAME(MAXEN)*10,TITLES(MAXEN)*60
      PARAMETER (MAXDIV = 15)
      INTEGER DIVDEV(MAXDIV),RSIZEN
      EQUIVALENCE (MATRIX,MATDNA)
      DATA MATDNA/
     +1,0,0,0,0,
     +0,1,0,0,0,
     +0,0,1,0,0,
     +0,0,0,1,0,
     +0,0,0,0,0/
C   GET DEVICE NUMBERS
      CALL UNITNO(KBIN,KBOUT,DEVNOS,MAXDEV)
      IDEV1 = DEVNOS(1)
      IDEVH = DEVNOS(2)
      IDEVR = DEVNOS(3)
      IDM = IDMM
      NASD = 0
      NDIAGT = 0
      RNSD = 3.
      IDEVNL = DEVNOS(4)
      IDEVLL = DEVNOS(5)
      IDEVEN = DEVNOS(6)
      IDEVAN = DEVNOS(7)
      IDEVDL = DEVNOS(8)
      IDEVLF = DEVNOS(9)
      IDEVD = DEVNOS(MAXDEV)
      CALL BEGINA(KBIN,KBOUT,MAXSEQ,SEQ1,SEQ1N,
     +IDEV1,IDEVR,IDEVH,
     +FILNAM,HELPF,IALIGN,MINLEN,MINSCR,NSHOW,MAXEN,
     +FILEP,MATRIX,WTS,IDM,CHRSET,IGAPG,IGAPH,IDIM1,KTUP,
     +MAIND,RNSD,SPAN,SPANCT,
     +IDEVLL,IDEVEN,IDEVAN,IDEVDL,
     +IDEVLF,LIBLF,LIBNO,
     +STACK,MAXSTK,IOK)
      IF(IOK.NE.0) STOP
C  OPEN LIBRARY
      LIBIN = 1
      IF(IDM.EQ.26) LIBIN = 2
      CALL RDLIBL(FILNAM,KBIN,KBOUT,
     +IHELPS,IHELPE,HELPF,IDEVH,IDEVLL,IDEVEN,IDEVNL,
     +LIBLF,LIBIN,DIVDEV,MAXDIV,IDEVD,
     +LIST,ENAMEL,LIBTYP,LTYPE,NDIV,RSIZEN,NRECEN,IOK)
      IF(IOK.NE.0)STOP
      IDE = IDM**KTUP
      CALL SETCN(CONSTS,KTUP,IDM,LCONST)
      CALL ENCONC(SEQ1N,IDIM1,POSN,WORDP,IDE,IDM,CONSTS,KTUP,LCONST)
      CALL WDSCR(SEQ1N,SEQS,IDIM1,KTUP,MAIND,IDM)
      NENTRY = 0
      PENTRY = 0
       CALL ACALC1(SEQ1N,IDIM1,SEQ2N,MAXSEQ,SCORE,PENTRY,IDM,
     + SEQ2,TITLE,ENAME,TITLES,MAXEN,MINLEN,
     + NENTRY,LIST,FILNAM,IDEVR,
     + DIVDEV,NDIV,ENAMEL,NAMIN,IDEVEN,RSIZEN,NRECEN,IDEVNL,LIBTYP,
     + LTYPE,MINSCR,KBOUT,WORDP,IDE,POSN,CONSTS,LCONST,HIST,KTUP,
     + MATRIX,SEQS,RNSD,SPAN,SPANCT,NASD,NDIAGT)
900   CONTINUE
      WRITE(IDEVR,1009)NENTRY,PENTRY
1009  FORMAT(
     +' ',I7,' entries processed,',I6,' above cutoff, sorting now')
      IF(PENTRY.LT.1)GO TO 921
      WRITE(IDEVR,*)'Entries exceeding sd cutoff=',NASD
      WRITE(IDEVR,*)
     +'Mean number of diagonals above span cutoff',REAL(NDIAGT)/NASD
      N = MIN(MAXEN,PENTRY)
      INCDEC = -1
      CALL SHLSRT(SCORE,N,PSCORE,INCDEC)
      J = MIN(PENTRY,NSHOW)
      WRITE(IDEVR,*)'List in score order'
      DO 910 I = 1,J
         WRITE(IDEVR,1015)
     +   SCORE(PSCORE(I)),ENAME(PSCORE(I)),TITLES(PSCORE(I))
1015     FORMAT(' ',I6,' ',A,' ',A)
910   CONTINUE
C      IF(1.EQ.1)STOP
      IF(IALIGN.EQ.0) THEN
        WRITE(KBOUT,*)'Aligning'
        CALL ACALC2(SEQ1,IDIM1,SEQ2,MAXSEQ,SEQ1A,SEQ2A,
     +  SEQ1N,SEQ2N,SCORE,PENTRY,IDM,STACK,MAXSTK,STKREC,
     +  CC,DD,RR,SS,SOP,NMAX,WTS,
     +  TITLE,ENAME,TITLES,MAXEN,
     +  NENTRY,IGAPG,IGAPH,IDEVR,
     +  NAMIN,ENAMEL,IDEVEN,RSIZEN,NRECEN,IDEVNL,DIVDEV,NDIV,
     +  ICREC,IFINEX,LIBTYP,LTYPE,
     +  PSCORE,NSHOW,KBOUT)
      END IF
921   CONTINUE
      CLOSE(UNIT=IDEVR)
      GO TO 923
999   CONTINUE
      WRITE(KBOUT,1010)NENTRY
1010  FORMAT(' Error reading names file',I7,' entries processed')
      CLOSE(UNIT=IDEVR)
923   CONTINUE
      END
      SUBROUTINE BEGINA(KBIN,KBOUT,MAXSEQ,SEQ,SEQN,
     +IDEV1,IDEVR,IDEVH,
     +FILNAM,HELPF,IALIGN,MINLEN,MINSCR,NSHOW,MAXEN,
     +FILEP,MATRIX,WTS,IDM,CHRSET,IGAPG,IGAPH,IDIM1,MINPER,
     +MAIND,RNSD,LENGTH,MINPRO,
     +IDEVLL,IDEVEN,IDEVAN,IDEVDL,
     +IDEVLF,LIBLF,LIBNO,
     +WORKI,MAXWIR,IOK)
      INTEGER MATRIX(IDM,IDM),WTS(0:IDM,0:IDM),SEQN(MAXSEQ),MAIND(IDM)
      INTEGER WORKI(MAXWIR)
      CHARACTER SEQ(MAXSEQ),CHRSET(IDM)
      CHARACTER FILNAM*(*),FILEP*(*),HELPF*(*)
      CHARACTER*(*) LIBLF
      EXTERNAL NORP
C      IOK = 1
      WRITE(KBOUT,1000)
1000  FORMAT(/,
     +' SIPL (Similarity investigation program (Library))',
     +' V4.0 Feb 1992',/,
     +' Author: Rodger Staden',/,
     +' Compares a probe protein or nucleic acid',/,
     +' sequence against a library of sequences',/)
      IDEV=KBOUT
      IDIMT = 0
      IDIM1 = MAXSEQ
      IFORNO = 0
      LIBNO = 2
2     CONTINUE
      WRITE(KBOUT,*)'Select probe sequence'
      CALL RDSEQ(
     +SEQ,MAXSEQ,IDIMT,J1,J2,ISTART,IEND,IDIM1,IDIMB,
     +IDEV1,FILNAM,KBIN,KBOUT,
     +IHELPS,IHELPE,HELPF,IDEVH,IDEV,IFORNO,
     +IDEVLL,IDEVEN,IDEVAN,IDEVDL,
     +IDEVLF,LIBNO,LIBLF,WORKI,MAXWIR,IOK)
      IF(IOK.NE.0) RETURN
      IDM = NORP(SEQ,IDIMT)
      CALL INITLU(IDM)
C   GIVE COMPOSITION AS A CHECK
      IF(IDM.EQ.26)THEN
       IF(IDIM1.GT.0)CALL MWCALC(SEQ,IDIM1,
     +J1-ISTART+1,J2-ISTART+1,KBOUT,KBOUT)
      ELSE
        IF(IDIM1.GT.0)
     +    CALL BCOMP(SEQ,IDIM1,J1-ISTART+1,J2-ISTART+1,ISTART,KBOUT)
      END IF
      CALL CONNUM(SEQ,SEQN,IDIM1)
      IF(IDM.EQ.26)THEN
C   READ IN THE SCORE MATRIX (USUALLY MDM78)
        CALL GETMAT(IDEV1,FILEP,MATRIX,IDM,CHRSET,KBOUT,IOK)
        IF(IOK.NE.0)RETURN
      END IF
C   NEED TO KNOW LARGEST SCORE IN MATRIX FOR STATS
      MATMAX=0
      DO 20 I=1,IDM
        DO 20 J=1,IDM
          IF(MATMAX.LT.MATRIX(I,J))MATMAX=MATRIX(I,J)
20    CONTINUE
      MATMAX=MATMAX+1
C   SET AVERAGE SCORE FOR DEFAULTS
        AVSCOR=0.75
        IF(IDM.EQ.26)AVSCOR=AVSCOR*16.
      FILNAM = ' '
      CALL OPENF1(IDEVR,FILNAM,1,IOK,KBIN,KBOUT,
     +'Results file',
     +IHELPS,IHELPE,HELPF,IDEVH)
      IF(IOK.NE.0)RETURN
      CALL YESNO(IALIGN,'Display alignments',
     +IHELPS,IHELPE,HELPF,IDEVH,KBIN,KBOUT)
      IF(IALIGN.LT.0) THEN
       IOK = 1
       RETURN
      END IF
      MN = 10
      MX = MAXSEQ
      MINLEN = INT(IDIM1*0.8)
      CALL GETINT(MN,MX,MINLEN,
     +'Minimum library sequence length',
     +IVAL,KBIN,KBOUT,
     +IHELPS,IHELPE,HELPF,IDEVH,IOK)
      IF(IOK.NE.0) RETURN
      MINLEN = IVAL
      MN = 1
      MX = MAXEN
      NSHOW = 20
      CALL GETINT(MN,MX,NSHOW,
     +'Maximum number of scores to list',
     +IVAL,KBIN,KBOUT,
     +IHELPS,IHELPE,HELPF,IDEVH,IOK)
      IF(IOK.NE.0) RETURN
      NSHOW = IVAL
      CALL DP22L(IDM,MINPER,
     +KBIN,KBOUT,IHELPS,IHELPE,HELPF,IDEVH,IOK)
      IF(IOK.NE.0) RETURN
      CALL DP33(RNSD,
     +KBIN,KBOUT,IHELPS,IHELPE,HELPF,IDEVH,IOK)
      IF(IOK.NE.0) RETURN
      MN = 1
      MX = 31
      LENGTH = 11
      CALL GETINT(MN,MX,LENGTH,'Odd span length',
     +  IVAL,KBIN,KBOUT,IHELPS,IHELPE,HELPF,IDEVH,IOK)
      IF(IOK.NE.0) RETURN
      IF(MOD(IVAL,2).NE.1) IVAL = LENGTH
      LENGTH = IVAL
      MINPRO = INT(AVSCOR*LENGTH)
      CALL DP21(MATMAX,AVSCOR,LENGTH,MINPRO,
     +KBIN,KBOUT,IHELPS,IHELPE,HELPF,IDEVH,IOK)
      IF(IOK.NE.0) RETURN
      MINPRO = MINPRO - 1
      MX = IDIM1 * AVSCOR * LENGTH * 2
      MN = 1
      MINSCR = MX / 40
      CALL GETINT(MN,MX,MINSCR,'Minimum global score',
     +  IVAL,KBIN,KBOUT,IHELPS,IHELPE,HELPF,IDEVH,IOK)
      IF(IOK.NE.0) RETURN
      MINSCR = IVAL
      CALL GETGAP(KBIN,KBOUT,IGAPG,IGAPH,
     +IHELPS,IHELPE,HELPF,IDEVH,IDM,IOK)
        IF(IOK.NE.0)RETURN
      CALL PAMDIS(MATRIX,WTS,IDM)
      DO 30 I = 1,IDM
        MAIND(I) = MATRIX(I,I)
30    CONTINUE
      END
      SUBROUTINE DP22L(IDM,MINPER,
     +KBIN,KBOUT,IHELPS,IHELPE,HELPF,IDEVH,IOK)
      CHARACTER HELPF*(*)
      MN = 1
      IF(IDM.EQ.5) THEN
        MINPER = 6
        MX = 6
      ELSE
        MINPER = 1
        MX = 3
      END IF
      CALL GETINT(MN,MX,MINPER,'Identity score',
     +  IVAL,KBIN,KBOUT,IHELPS,IHELPE,HELPF,IDEVH,IOK)
      IF(IOK.EQ.0) MINPER = IVAL
      END
      SUBROUTINE ACALC1(SEQ1N,IDIM1,SEQ2N,MAXSEQ,SCORE,PENTRY,IDM,
     +SEQ2,TITLE,ENAME,TITLES,MAXEN,MINLEN,
     +NENTRY,LIST,FILNAM,IDEVR,
     +DIVDEV,NDIV,ENAMEL,NAMIN,IDEVEN,RSIZEN,NRECEN,IDEVNL,LIBTYP,
     +LTYPE,
     +MINSCR,KBOUT,WORDP,IDE,POSN,CONSTS,LCONST,HIST,KTUP,MATRIX,
     +SEQS,RNSD,SPAN,SPANCT,NASD,NDIAGT)
      INTEGER WORDP(IDE),POSN(IDIM1),CONSTS(0:LCONST)
      INTEGER HIST(-MAXSEQ:MAXSEQ),SPAN,SPANCT
      INTEGER SCORE(MAXEN),PENTRY,SEQ1N(IDIM1),SEQ2N(MAXSEQ)
      INTEGER MATRIX(IDM,IDM),SEQS(IDIM1)
      CHARACTER SEQ2(MAXSEQ),FILNAM*(*),LTYPE
      CHARACTER TITLE*60,NAMIN*10,ENAMEL*10
      CHARACTER ENAME(MAXEN)*10,TITLES(MAXEN)*60
      INTEGER RSIZEN,DIVDEV(NDIV),ENTRYN
      IFINEX = 0
      ICREC = 0
      ENTRYN = 0
 20   CONTINUE
      IDIM2 = MAXSEQ
      IF(LIBTYP.EQ.1) THEN
        CALL CDROML(LIST,NAMIN,ENAMEL,
     +  IDEVEN,RSIZEN,NRECEN,IDEVNL,SEQ2,IDIM2,
     +  DIVDEV,NDIV,ICREC,IFINEX,TITLE,KBOUT,LTYPE,IOK)
        IF(IOK.NE.0) RETURN
        FILNAM = NAMIN
      ELSE IF(LIBTYP.EQ.2) THEN
        CALL RDPIRA(SEQ2,IDIM2,
     +  IDEVEN,KBOUT,TITLE,FILNAM,LIST,ENAMEL,IDEVNL)
      ELSE IF(LIBTYP.EQ.3) THEN
        CALL RDFASA(SEQ2,IDIM2,
     +  IDEVEN,KBOUT,TITLE,FILNAM,LIST,ENAMEL,IDEVNL,ENTRYN)
      ELSE
        RETURN
      END IF
      IF(IDIM2.LT.1)GO TO 900
      NENTRY = NENTRY + 1
      IF(IDIM2.LT.MINLEN) GO TO 20
C  CONVERT TO INTEGER
      IF(IDIM2.GT.0)CALL CONNUM(SEQ2,SEQ2N,IDIM2)
C
C  DO THE SEARCH
        CALL QICKSL(SEQ1N,IDIM1,POSN,WORDP,IDE,SEQ2N,IDIM2,CONSTS,
     +  KTUP,IDM,LCONST,HIST,MAXSEQ,MATRIX,SEQS,
     +  SPAN,SPANCT,RNSD,MSCORE,NASD,NDIAGT)
      IF(MSCORE.GE.MINSCR)THEN
        IF(PENTRY.LT.MAXEN)THEN
          PENTRY = PENTRY + 1
          SCORE(PENTRY) = MSCORE
          ENAME(PENTRY) = FILNAM(1:10)
          TITLES(PENTRY) = TITLE
        ELSE
        WRITE(KBOUT,*)'Maximum number of entries (',MAXEN,') exceeded'
          RETURN
        END IF
      END IF
      GO TO 20
C
900   CONTINUE
      END
      SUBROUTINE ACALC2(SEQ1,IDIM1,SEQ2,MAXSEQ,SEQ1A,SEQ2A,
     +SEQ1N,SEQ2N,SCORE,PENTRY,IDM,STACK,MAXSTK,STKREC,
     +CC,DD,RR,SS,SOP,NMAX,WTS,
     +TITLE,ENAME,TITLES,MAXEN,
     +NENTRY,IGAPG,IGAPH,IDEVR,
     +NAMIN,ENAMEL,IDEVEN,RSIZEN,NRECEN,IDEVNL,DIVDEV,NDIV,
     +ICREC,IFINEX,LIBTYP,LTYPE,
     +PSCORE,NSHOW,KBOUT)
      INTEGER STACK(0:MAXSTK),STKREC
      INTEGER WTS(0:IDM,0:IDM)
      CHARACTER SEQ1(MAXSEQ),SEQ2(MAXSEQ)
      CHARACTER SEQ1A(NMAX),SEQ2A(NMAX)
      INTEGER SEQ1N(NMAX),SEQ2N(NMAX)
      INTEGER CC(0:NMAX+1),DD(0:NMAX+1)
      INTEGER RR(0:NMAX+1),SS(0:NMAX+1)
      INTEGER SOP(0:2*NMAX)
      INTEGER SCORE(MAXEN),PENTRY,PSCORE(MAXEN)
      CHARACTER TITLE*60,NAMIN*10,ENAMEL*10,LTYPE
      CHARACTER ENAME(MAXEN)*10,TITLES(MAXEN)*60
      INTEGER DIVDEV(NDIV)
      LIST = 2
      DO 20 I = 1,MIN(NSHOW,PENTRY)
        J = PSCORE(I)
        NAMIN = ENAME(J)
        IDIM2 = MAXSEQ
        IF(LIBTYP.EQ.1) THEN
          CALL CDROML(LIST,NAMIN,ENAMEL,
     +    IDEVEN,RSIZEN,NRECEN,IDEVNL,SEQ2,IDIM2,
     +    DIVDEV,NDIV,ICREC,IFINEX,TITLE,KBOUT,LTYPE,IOK)
          IF(IOK.NE.0) RETURN
        ELSE IF(LIBTYP.EQ.2) THEN
          REWIND(IDEVEN)
          CALL RDPIRD(SEQ2,IDIM2,
     +    IDEVEN,KBOUT,TITLE,NAMIN)
        ELSE IF(LIBTYP.EQ.3) THEN
          REWIND(IDEVEN)
          CALL RDFASD(SEQ2,IDIM2,
     +    IDEVEN,KBOUT,TITLE,NAMIN)
        ELSE
          RETURN
        END IF
        IF(IDIM2.LT.1) GO TO 20
        CALL CONNUM(SEQ2,SEQ2N,IDIM2)
        WRITE(IDEVR,1000)NAMIN,TITLE
1000    FORMAT(' ',A,' ',A)
        CALL ALIGNL(SEQ1N,SEQ2N,IDIM1,IDIM2,CC,DD,RR,SS,SOP,
     +NMAX,MAXSEQ,WTS,IDM,ISCORE,IGAPG,IGAPH,
     +IDEVR,SEQ1,SEQ2,SEQ1A,SEQ2A,
     +STACK,MAXSTK,STKREC,IOK)
      IF(IOK.NE.0) RETURN
20    CONTINUE
      END
      SUBROUTINE DIFFD(SA,SB,M,N,CC,DD,NMAX,
     +W,IDM,SCORE,G,H)
C  RETURNS ALIGNMENT COST (SCORE) USING MYERS AND MILLERS
C  VERSION OF GOTOH ALGORITHM
      IMPLICIT INTEGER(A-Z)
      INTEGER CC(0:NMAX+1),DD(0:NMAX+1)
      INTEGER W(0:IDM,0:IDM)
      INTEGER SA(NMAX),SB(NMAX)
      CC(0) = 0
      T = G
      DO 200 J=1,N
        T = T + H
        CC(J) = T
        DD(J) = T + G
200   CONTINUE
      T = G
      DO 400 I = 1,M
        S = CC(0)
        T = T + H
        C = T
        CC(0) = C
        E = T + G
        DO 300 J = 1,N
          E = MIN(E,C+G) + H
          DD(J) = MIN(DD(J),(CC(J)+G)) + H
          C = MIN(DD(J),E,S+W(SA(I),SB(J)))
          S = CC(J)
          CC(J) = C
300     CONTINUE
400   CONTINUE
      SCORE = CC(N)
      END
      SUBROUTINE ALIGNL(SEQ1N,SEQ2N,IDIM1,IDIM2,CC,DD,RR,SS,SOP,
     +NMAX,MAXSEQ,WTS,IDM,ISCORE,IG,IH,
     +KBOUT,SEQ1,SEQ2,SEQ1A,SEQ2A,
     +STACK,MAXSTK,STKREC,IOK)
      INTEGER CC(0:NMAX+1),DD(0:NMAX+1),RR(0:NMAX+1),SS(0:NMAX+1)
      INTEGER WTS(0:IDM,0:IDM),SOP(0:2*NMAX),R1,R2
      INTEGER SEQ1N(NMAX),SEQ2N(NMAX),STKREC,STACK(0:MAXSTK)
      CHARACTER SEQ1(MAXSEQ),SEQ2(MAXSEQ),SEQ1A(NMAX),SEQ2A(NMAX)
      CHARACTER PAD
      SAVE PAD
      DATA PAD/','/
      LW1 = MIN(IDIM1,NMAX)
      LW2 = MIN(IDIM2,NMAX)
      N = MAX(LW1,LW2)
      CALL DIFF(SEQ1N,SEQ2N,LW1,LW2,
     +CC,DD,RR,SS,SOP,N,
     +WTS,IDM,ISCORE,IG,IH,KBOUT,STACK,MAXSTK,STKREC,IOK)
      IF(IOK.NE.0) RETURN
      CALL ALIGND(SEQ1,SEQ2,SOP,
     +SEQ1A,SEQ2A,N,MAXSEQ,R1,R2,PAD,NP1,NP2,IDIM1,IDIM2)
      KPOUT = MAX(R1,R2)
      PC = PCON(SEQ1A,SEQ2A,KPOUT,PAD)
      CALL FMT2(KBOUT,SEQ1A,SEQ2A,KPOUT,1,1)
      WRITE(KBOUT,1001)PC
1001  FORMAT(' Conservation ',F5.1,'%')
      WRITE(KBOUT,1002)NP1,NP2
1002  FORMAT(' Number of padding characters inserted',I6,' and',I6)
      END
      SUBROUTINE QICKSL(SEQ1,IDIM1,POSN,WORDP,IDE,SEQ2,IDIM2,CONSTS,
     +LENGTH,IDM,LCONST,HIST,MAXSEQ,MATRIX,SEQS,
     +SPAN,MINPRO,RNSD,MSCORE,NASD,NDIAGT)
      INTEGER SEQ1(IDIM1),SEQ2(IDIM2),SEQS(IDIM1),SPAN
      INTEGER POSN(IDIM1),WORDP(IDE),CONSTS(0:LCONST)
      INTEGER ESCORE
      INTEGER HIST(-MAXSEQ:MAXSEQ)
      PARAMETER (MAXDIA = 20)
      INTEGER TOPD(MAXDIA),TOPI(MAXDIA),TOPJ(MAXDIA),MATRIX(IDM,IDM)
      EXTERNAL NCODEA,ESCORE
      CALL FILLI(HIST(-IDIM1),IDIM2+IDIM1+1,0)
      DO 20 I = 1,IDIM2-LENGTH+1
        J = NCODEA(SEQ2(I),LENGTH,CONSTS,IDM,LCONST)
        IF(J.NE.0)THEN
          J1 = WORDP(J)
          IF(J1.NE.0)THEN
            K = I - J1
            HIST(K) = HIST(K) + SEQS(J1)
10          CONTINUE
            J2 = J1
            J1 = POSN(J2)
            IF(J1.NE.0)THEN
              K = I - J1
              HIST(K) = HIST(K) + SEQS(J1)
              GO TO 10
            END IF
          END IF        
        END IF
20    CONTINUE
      NDIAG = MAXDIA
      CALL MHIST(HIST,IDIM1,IDIM2,TOPD,TOPI,TOPJ,
     +NDIAG,MAXSEQ,RNSD)
      IF(NDIAG.GT.0) THEN
        NASD = NASD + 1
        NDIAGT = NDIAGT + NDIAG
      END IF
      MSCORE = 0
      DO 40 I = 1,NDIAG
        IF(TOPD(I).NE.0) THEN
          L = MIN(IDIM1-TOPJ(I),IDIM2-TOPI(I)) + 1
          M = ESCORE(SEQ1(TOPJ(I)),SEQ2(TOPI(I)),L,MATRIX,IDM,
     +    SPAN,MINPRO)
          MSCORE = MSCORE + M
        END IF
40    CONTINUE
C      WRITE(*,*)'SCORE',MSCORE
      END
      SUBROUTINE SHLSRT(KEY,N,IPOINT,INCDEC)
C     SHELL SORT
C     REFERENCES:  D.L. SHELL, CACM 2, 32 (JULY 1959)
C                  D.E. KNUTH, TAOCP III, SECT. 5.2.1
C     CALLING SEQUENCE:
C
C     KEY    IS AN ARRAY OF KEYS ON WHICH TO SORT
C     N      IS THE NUMBER OF ITEMS
C     IPOINT IS THE ARRAY OF POINTERS
C            (ONLY THE POINTERS WILL MOVE)
C     INCDEC .GE. 0 FOR SORTING INTO INCREASING ORDER;
C            .LT. 0 FOR SORTING INTO DECREASING ORDER
C
      INTEGER KEY(N),IPOINT(N),K
      INTEGER H,S,T
      IPOINT(1) = 1
      IF(N.LT.2)RETURN
C     INITIALIZE POINTER ARRAY
      DO 2 I = 2,N
        IPOINT(I) = I
    2 CONTINUE
C
C     CHOICE OF SEQUENCE OF INCREMENTS SUGGESTED
C     BY KNUTH III, EQ. 8, P. 95.   HIS FORMULA
C     IS EQUIVALENT TO:
C
C            H(S) = (3**S - 1)/2
C            INITIAL VALUE OF S IS MINIMAL INTEGER
C              SUCH THAT H(S+2) .GE. N
C
C
C     SMAX = (ALOG(2N + 1)/ALOG(3)) - 2 + 1
      S = INT( (ALOG(FLOAT(2*N+1))/1.09861229) - 0.95 )
      S = MAX(S,1)
      H = (3**S - 1)/2
      DO 7 T = 1,S
        JMIN = H + 1
        DO 6 J = JMIN,N
          I = J - H
          JJ = IPOINT(J)
          K = KEY(JJ)
          IPT = IPOINT(J)
   3      CONTINUE
          II = IPOINT(I)
          IF((K-KEY(II)).LE.0)THEN
            IPLUSH = I + H
            IPOINT(IPLUSH) = IPOINT(I)
            I = I - H
            IF(I.GT.0) GO TO 3
          END IF
          IPLUSH = I + H
          IPOINT(IPLUSH) = IPT
    6   CONTINUE
C     CHANGE INCREMENT
        IF(H.LT.2) GO TO 8
        H = (H-1)/3
7     CONTINUE
C      CHECK INCDEC: IF NEGATIVE, SWITCH POINTER ARRAY
8     CONTINUE
      IF(INCDEC.LT.0)THEN
        M = N/2
        NP1MI = N
        DO 10 I = 1,M
          NTEMP = IPOINT(I)
          IPOINT(I) = IPOINT(NP1MI)
          IPOINT(NP1MI) = NTEMP
          NP1MI = NP1MI - 1
  10    CONTINUE
      END IF
      END
      INTEGER FUNCTION ESCORE(SEQ1,SEQ2,L,MATRIX,IDM,SPAN,MINSCR)
      INTEGER SEQ1(L),SEQ2(L),MATRIX(IDM,IDM),SPAN,FRONT,BACK
C 8-6-91 fixed bug which allowed span to be > L
      M = 0
      MTOT = 0
      FRONT = SPAN
      BACK = 0
      DO 10 I = 1,MIN(SPAN,L)
        M = M + MATRIX(SEQ1(I),SEQ2(I))
10    CONTINUE
      IF(M.GT.MINSCR) MTOT = M
      DO 20 I = 2,L-SPAN+1
        FRONT = FRONT + 1
        BACK = BACK + 1
        MM = MATRIX(SEQ1(BACK),SEQ2(BACK))
        MP =  MATRIX(SEQ1(FRONT),SEQ2(FRONT))
        M = M - MM + MP
        IF(M.GT.MINSCR) MTOT = MTOT + M
20    CONTINUE
      ESCORE = MTOT
      END
