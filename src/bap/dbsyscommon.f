C  DBSYS ROUTINES COMMON TO PRE AND POST .RD PROGRAMS
C
C 25-8-92 NOTE at the end of the file are new versions of getln2 (getln3)
C              and gelid (gelidn) and a new routine nameno, that should
C              replace getln2 and gelid.
C
C  Further sap routines are in dbsysold.f (pre .rd) and dbsysnew.f (post)
C  the split was made by rs 23-1-91
C 15-6-92 added fasta format output from consen
C 15-4-92 added in all the speedup changes ive been making and made 
C         screnv compatible
C 13-4-92 changed autocn to use a new hashing routine encof and inite
C  2-4-92 Added new dbauto related routines and changed gtconc
C  2-4-92 Added filnam = ' ' and brought uptodate with dap
C  4-5-90 Change to getreg to allow escape
C  4-5-90 addition of graphics routines and changes to menus
C  9-5-90 added default gel reading: many changes
C  17-5-90 Fixed 3 bugs in screen editing: 1) rightjustified names
C          caused problems; 2) beginnings of sequences starting at far
C          right of lines where not seen; 3) lines with no numbers at
C          the end of a contig (i.e. with <10 chars) were flagged
C          as errors. Changes to ltype for 1, linlen for 2, dsplay for 3
C  9-7-90  removed menu routines
C  20-8-90 changed gelid to add / to reading name because xsap did
C          not return the INFLAG = 3 for the default
C  23-8-90 Changes to dbauto and autocn to deal with failures better
C          Plus addition of calls to BUSY
C  9-11-90 Replaced call to radio with call to radion
C  19-11-90 Changed max match length in dbauto to maxglm+1  (was 50)
C  25-11-90 Very important bug fix in tpchek. Old versions could
C           duplicate bits of working versions.
C  28-11-90 Modified slider to receive maxpg and maxpc and to allow exactly
C           the requested number of matches at each end of the two 
C           sequences. 
C           Added two new options to dbauto: all gels to new contigs, all 
C           gels to contig 1; plus resurrected forbidding joins to allow
C           sequences to be entered only into the contig the overlap best.
C           Changed autocn to sort overlaps into order based on % mismatch
C           (previously it saved the best two in any order)
C           Minor change to dbstar
C 3-1-91   Discovered bug in dbopen: incorrect call to getint when the
C          database is very old and needs values for the current format
C 21-1-91  GELID allowed illegal gel numbers to be returned! Fixed it.
C 22-1-91  Modified autocn, adism4,adism3 to give more info about
C          overlaps, and to allow 10 overlaps. Modified dbopen to
C          return version number, ditto dbstar
C 23-1-91  Split into dbsyscommon, dbsysold, dbsysnew
C 26-2-91  Improved overflow check in padcop
C 28-7-91 added extra parameter to quality calc: mxgood is the maximum
C         reading length in which we have confidence, so only add this
C         many chars from the start of each reading. Also changed the
C         quality calc to make it the same as the consensus one. Made
C         all characters have nonzero score and made lowercase = 100
C 21-8-91 Changed arrfil to arrfim which does not display comments
C 22-8-91 Added routine to find contig line number given left gel (CLINNO)
C 
C
C 12-11-91 BIG CHANGE: made database handle 99,999 readings and 16 char names
C
C
C          Also added routine to make aedit take strandedness into account
C          (SUMSS).
C          Also added fmt4lp which is used by find internal joins and
C          could be used to advantage by others that call fmt4ln.
C 18-11-91 New routine GETLN2 with returns gel number specified
C
C     enconn
C   routine to store positions of words in posns and first occurences
C   in wordp and number of occurences in wordn
C   each number is a value representing one of the le4 possible
C   words of length length made up of 4 characters
C   words in posns are numbers from 1 to 4**length
      SUBROUTINE ENCONN(POSNS,IDIM,WORDP,WORDN,LE4,LENGTH,START)
C   AUTHOR: RODGER STADEN
      INTEGER WORDP(LE4),POSNS(IDIM)
      INTEGER WORDN(LE4),START
C   number of words of length length
      IDIM1 = IDIM - (LENGTH-1)
      IF (START.EQ.1) THEN
        DO 10 I=1,LE4
          WORDN(I) = 0
10      CONTINUE
      END IF
C   loop for each word
      DO 100 I=START,IDIM1
        N = POSNS(I)
        IF(N.NE.0) THEN
          NW = WORDN(N)
C   is their already an entry for this word?
          IF(NW.EQ.0) THEN
C  first entry, put in wordp
            WORDP(N) = I
            WORDN(N) = NW + 1
          ELSE
            WORDN(N) = NW + 1
            POSNS(I) = WORDP(N)
            WORDP(N) = I
          END IF
        END IF
100   CONTINUE
      END
      SUBROUTINE ENCOF(SEQ,IDSEQ,CONST,CSTART,LENGTH,POSNS)
      CHARACTER SEQ(IDSEQ)
      INTEGER CONST(LENGTH),CSTART,POSNS(IDSEQ),HASH
      INTEGER CTONUM,CONSTL
      EXTERNAL CTONUM
C
C new hashing routine. hash = k1 + k2
C
C hash = k1.c1 + k2.c2 + ... + kn.cn - cstart
C now c1=1, c2=4*c1, c3=4*c2,...
C
C find length bases in a row, then do first word base by base,
C for rest only change what is necessary
C
      DO 1 I=1,IDSEQ
        POSNS(I) = 0
 1    CONTINUE
      CONSTL = CONST(LENGTH)
      LM1 = LENGTH - 1
      IDSQML = IDSEQ - LENGTH
      IS = 1
      I  = 1
      IP = 1
      HASH = 0
 10   CONTINUE
C
C end approaching ?
C
      IF (IS.GT.IDSQML) RETURN
C
C at least a words length of characters left
C
 11   CONTINUE
      K = CTONUM(SEQ(I))
      IF (K.EQ.5) THEN
C
C start a new word
C
        IS = I + 1
        I = IS
        IP = 1
        HASH = 0
        GO TO 10
      END IF
      HASH = HASH + CONST(IP) * K
      IF (IP.NE.LENGTH) THEN
        I = I + 1
        IP = IP + 1
        GO TO 11
      END IF
C
C word finished
C
C save the hash value and the 
C
 20   CONTINUE
      POSNS(IS) = HASH + CSTART
C      K1 = CONST(1) * CTONUM(SEQ(IS)) note const(1) = 1
      K1 = CTONUM(SEQ(IS))
      K2 = (HASH - K1) / 4
      IS = IS + 1
      IF (IS.GT.IDSQML) RETURN
      K = CTONUM(SEQ(IS+LM1))
      IF (K.EQ.5) THEN
        IS = IS + 1
        I = IS
        IP = 1
        HASH = 0
        GO TO 10
      END IF
C      HASH = K2 + K * CONST(LENGTH) note this is a constant constant
      HASH = K2 + K * CONSTL
      GO TO 20
      END
      SUBROUTINE INITE(CONST,CSTART,LENGTH)
      INTEGER CONST(LENGTH),CSTART
      CSTART = 1
      DO 1 I=1,LENGTH
C      WRITE(*,*)I
        CONST(I) = 4**(I-1)
        CSTART = CSTART - CONST(I)
 1    CONTINUE
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
C      ABEDIN
C
C   ROUTINE TO EDIT THE DB USING A PADDED SEQ
C   HAVE AN ARRAY SEQC2 LENGTH IDC OF PADDED SECTION OF CONTIG LINCON
C  THE LEFT END OF THE PADDED CONTIG STARTS AT X
C   THERE ARE ITOTPC PADS TO MAKE
C
      SUBROUTINE ABEDIN(RELPG,LNGTHG,LNBR,RNBR,NGELS,NCONTS,
     +GEL,LINCON,X,SEQC2,ITOTPC,IDC,IDBSIZ,KBOUT,IDEVR,IDEVW,
     +MAXGEL)
C   AUTHOR: RODGER STADEN
      INTEGER  RELPG(IDBSIZ),X,POSN
      INTEGER LNGTHG(IDBSIZ),LNBR(IDBSIZ),RNBR(IDBSIZ)
      CHARACTER SEQC2(IDC),GEL(MAXGEL),P
      SAVE P
      DATA P/','/
C
C   POINT TO CONTIG
      POSN=X-1
C   POINT TO SEQC2
      IAT=0
C   COUNT PADS DONE
      IDONE=0
C   LOOP FOR ALL SEQC2
      DO 100 J=1,IDC
      POSN=POSN+1
      IAT=IAT+1
      IPAD=0
C   IS THIS A PADDING CHAR?
      IF(SEQC2(IAT).NE.P)GO TO 100
50    CONTINUE
C   COUNT PADS
      IPAD=IPAD+1
      IAT=IAT+1
      IF(SEQC2(IAT).EQ.P)GO TO 50
C   END OF THIS STRETCH OF PADS,DO INSERT
C   HAVE IPAD INSERTS TO MAKE AT POSN
      CALL PADCON(RELPG,LNGTHG,LNBR,RNBR,NGELS,NCONTS,
     +GEL,LINCON,POSN,IPAD,IDBSIZ,IDEVR,IDEVW,MAXGEL,KBOUT)
C   MOVE POINTER TO CONTIG
      POSN=POSN+IPAD
C   COUNT PADS DONE
      IDONE=IDONE+IPAD
C   ANY MORE TO DO?
      IF(IDONE.EQ.ITOTPC)GO TO 101
100   CONTINUE
C   ERROR SHOULD HAVE DONE ALL PADS
      WRITE(KBOUT,1000)
1000  FORMAT(' Problem: some pads were not done!')
101   CONTINUE
      END
      SUBROUTINE ADDTIT(SEQ1,NAMPRO,NGELS,IDIM1)
C   AUTHOR: RODGER STADEN
      CHARACTER SEQ1(20),NAMPRO*(*)
      CHARACTER NUMS(10)
C Set maximum number of digits in reading number
      PARAMETER (MAXGD = 5)
      SAVE NUMS
      DATA NUMS/'0','1','2','3','4','5','6','7','8','9'/
      CALL FILLC(SEQ1(2),18,'-')
      SEQ1(1) = '<'
      SEQ1(20) = '>'
      IEND = INDEX(NAMPRO,'.')
      N=NGELS
      K=IEND+MAXGD
      DO 10 J=1,MAXGD
        N=MOD(N,10)+1
        NAMPRO(K:K)=NUMS(N)
        N=NGELS/(10**J)
        K=K-1
10    CONTINUE
      K = 18-IEND
      K=K/2
      DO 20 I=1,IEND+MAXGD
        SEQ1(K)=NAMPRO(I:I)
        K=K+1
20    CONTINUE
      IDIM1=IDIM1+20
      END
      SUBROUTINE ADISM1(SEQ,IDIM,GEL,IDIMG,SAVPS,SAVPG,IDSAV,
     +CENDS,NENDS,IDCEND,MAXCON,ILEFTS,ILC,IPOSC,IPOSG,ISENSE,
     +LLINO,IMATC,
     +ISTRAN,KBOUT,MATCH)
C   AUTHOR: RODGER STADEN
C  NEW PARMS
      INTEGER ILEFTS(2),ILC(2),IPOSC(2),IPOSG(2),ISENSE(2),LLINO(2)
CCCCCCCCCCCC
      INTEGER CENDS(MAXCON)
      INTEGER NENDS(MAXCON)
      INTEGER SAVPS(IDSAV),SAVPG(IDSAV)
      CHARACTER SEQ(IDIM),GEL(IDIMG),MATCH(IDIMG)
C
C   EDITED 07-02-83 TO ALLOW FOR CASE WHERE A GEL OVERLAPS ADJACENT
C   CONTIGS WITHIN THE LENGTH OF THE GEL. USE PARM THAT CONTAINS
C   THE POSITION OF THE LEFT END OF THE NEXT CONTIG. SET TO VERY HIGH
C   VALUE TO START
      NEXTC=IDIM+1
C   SORT THE MATCHING WORDS INTO ASCENDING ORDER ON POSITION IN SEQ
      CALL BUB2AS(SAVPS,SAVPG,IDSAV)
C   LOOK FOR SEPERATE MATCHES
      LEND=IDIMG-SAVPG(1)+SAVPS(1)
C  COUNT NUMBER OF MATCHING CONTIGS
      IMATC=IMATC+1
      CALL ADISM2(SEQ,IDIM,GEL,IDIMG,SAVPS(1),
     1SAVPG(1),CENDS,NENDS,IDCEND,MAXCON,
     1ILEFTS,ILC,IPOSC,IPOSG,ISENSE,LLINO,IMATC,ISTRAN,NEXTC,KBOUT,
     2MATCH)
      DO 10 I=2,IDSAV
      IF((SAVPS(I).LT.LEND).AND.(SAVPS(I).LT.NEXTC))GO TO 10
C   NEW MATCH, DISPLAY IT
C   COUNT NUMBER OF MATCHING CONTIGS
      IMATC=IMATC+1
      CALL ADISM2(SEQ,IDIM,GEL,IDIMG,SAVPS(I),
     1SAVPG(I),CENDS,NENDS,IDCEND,MAXCON,
     1ILEFTS,ILC,IPOSC,IPOSG,ISENSE,LLINO,IMATC,ISTRAN,NEXTC,KBOUT,
     2MATCH)
C
C   RESET LEND
      LEND=IDIMG-SAVPG(I)+SAVPS(I)
10    CONTINUE
      RETURN
      END
C
C       ADISM2
C   ROUTINE TO DISPLAY MATCHES
      SUBROUTINE ADISM2(SEQ,IDIM1,GEL,IDIMG,ISAVPS,SAVPG,CENDS,NENDS,
     +IDCEND,MAXCON,ILEFTS,ILC,IPOSC,IPOSG,ISENSE,LLINO,IMATC,ISTRAN,
     +NEXTC,KBOUT,MATCH)
C   AUTHOR: RODGER STADEN
C   NEW PARMS
      INTEGER ILEFTS(2),ILC(2),IPOSC(2),IPOSG(2),ISENSE(2),LLINO(2)
CCCCCCCCCCC
      CHARACTER SEQ(IDIM1),GEL(IDIMG),MATCH(IDIMG)
      INTEGER SAVPS,SAVPG,CENDS(MAXCON)
      INTEGER NENDS(MAXCON)
C   EDITED 07-02-83 FOR NEXTC. SEE ADISM1.
C   DELETE 20 FROM END OF CONSENSUS MATCH
      SAVPS=ISAVPS-19
C   FIND CONTIG CONSENSUS ENDS
      JJ=1
      DO 5 J=2,IDCEND
      IF(SAVPS.GT.CENDS(J))GO TO 5
C   GONE PAST SO LAST IS THE ONE
      JJ=J-1
      GO TO 6
5     CONTINUE
      JJ=IDCEND
6     CONTINUE
C   SUBTRACT 1 FROM END
      SAVPS=SAVPS-1
C   LENGTH FROM MATCH TO LEFT OF CONTIG
      LCL=SAVPS-CENDS(JJ)
C   RIGHT
      LCR=CENDS(JJ+1)-ISAVPS-1
C   LEFT GEL
      LGL=SAVPG-1
      LGR=IDIMG-SAVPG
C   NEED MIN OF EACH PAIR
      LL=MIN(LCL,LGL)
      LR=MIN(LCR,LGR)
C   LENGTH OF OVERLAP
      LM=LR+LL+1
C  DISPLAY STARTS
      ICL=ISAVPS-LL
      IGL=SAVPG-LL
      WRITE(KBOUT,1000)NENDS(JJ)
1000  FORMAT(' Match found with contig number =',I6)
      CALL SQMTCH(SEQ(ICL),GEL(IGL),MATCH,LM)
      L=ICL-CENDS(JJ)-19
      CALL FMT4LN(SEQ(ICL),GEL(IGL),MATCH,LM,L,IGL,KBOUT)
C   UPDATE END OF NEXT CONTIG
      NEXTC=CENDS(JJ+1)+20
      IF(IMATC.GT.2)RETURN
      ILEFTS(IMATC)=CENDS(JJ)+20
      ILC(IMATC)=LCL+LCR+1
      IPOSC(IMATC)=LCL+1
      IPOSG(IMATC)=SAVPG
      LLINO(IMATC)=NENDS(JJ)
      ISENSE(IMATC)=1
      IF(ISTRAN.EQ.2)ISENSE(IMATC)=-1
      RETURN
      END
      SUBROUTINE ADISM3(ISAVPS,SAVPG,CENDS,NENDS,
     +IDCEND,MAXCON,ILEFTS,ILC,IPOSC,IPOSG,ISENSE,LLINO,IMATC,ISTRAN,
     +NEXTC,MAXC,KBOUT)
C   AUTHOR: RODGER STADEN
      INTEGER ILEFTS(MAXC),ILC(MAXC),IPOSC(MAXC),IPOSG(MAXC)
      INTEGER ISENSE(MAXC),LLINO(MAXC)
      INTEGER SAVPS,SAVPG,CENDS(MAXCON)
      INTEGER NENDS(MAXCON)
      SAVPS=ISAVPS-19
      JJ=1
      DO 5 J=2,IDCEND
        IF(SAVPS.GT.CENDS(J))GO TO 5
        JJ=J-1
        GO TO 6
5     CONTINUE
      JJ=IDCEND
6     CONTINUE
      SAVPS=SAVPS-1
      LCL=SAVPS-CENDS(JJ)
      LCR=CENDS(JJ+1)-ISAVPS-1
      NEXTC=CENDS(JJ+1)+20
      IF(IMATC.LE.MAXC) THEN
        ILEFTS(IMATC)=CENDS(JJ)+20
        ILC(IMATC)=LCL+LCR+1
        IPOSC(IMATC)=LCL+1
        IPOSG(IMATC)=SAVPG
        LLINO(IMATC)=NENDS(JJ)
        ISENSE(IMATC)=1
        IF(ISTRAN.EQ.2)ISENSE(IMATC)=-1
        WRITE(KBOUT,1000)LLINO(IMATC),IPOSC(IMATC),ISTRAN,
     +  IPOSG(IMATC)
 1000   FORMAT
     +  (' Contig',I5,' position',I6,' matches strand',I2,
     +  ' at position',I5)
      ELSE
        CALL ERROM(KBOUT,'Warning: too many overlaps')
      END IF
      END
      SUBROUTINE ADISM4(IDIM,IDIMG,SAVPS,SAVPG,IDSAV,
     +CENDS,NENDS,IDCEND,MAXCON,ILEFTS,ILC,IPOSC,IPOSG,ISENSE,
     +LLINO,IMATC,ISTRAN,MAXC,KBOUT)
C   AUTHOR: RODGER STADEN
      INTEGER ILEFTS(MAXC),ILC(MAXC),IPOSC(MAXC),IPOSG(MAXC)
      INTEGER ISENSE(MAXC),LLINO(MAXC)
      INTEGER CENDS(MAXCON)
      INTEGER NENDS(MAXCON)
      INTEGER SAVPS(IDSAV),SAVPG(IDSAV)
      NEXTC=IDIM+1
      CALL BUB2AS(SAVPS,SAVPG,IDSAV)
        IMATC=IMATC+1
        CALL ADISM3(SAVPS(1),SAVPG(1),CENDS,NENDS,IDCEND,MAXCON,
     +  ILEFTS,ILC,IPOSC,IPOSG,ISENSE,LLINO,IMATC,ISTRAN,NEXTC,MAXC,
     +  KBOUT)
      LEND=IDIMG-SAVPG(1)+SAVPS(1)
      DO 10 I=2,IDSAV
        IF((SAVPS(I).LT.LEND).AND.(SAVPS(I).LT.NEXTC))GO TO 10
        IMATC=IMATC+1
        CALL ADISM3(SAVPS(I),SAVPG(I),CENDS,NENDS,IDCEND,MAXCON,
     +  ILEFTS,ILC,IPOSC,IPOSG,ISENSE,LLINO,IMATC,ISTRAN,NEXTC,MAXC,
     +  KBOUT)
        LEND=IDIMG-SAVPG(I)+SAVPS(I)
10    CONTINUE
      IMATC = MIN(IMATC,MAXC)
      RETURN
      END
      SUBROUTINE AEDIT(RELPG,LNGTHG,LNBR,RNBR,IDBSIZ,LGEL,NCONT,
     +GEL,MAXGEL,CON,IDC,IDEVW,IDEVR,LREG,RREG,KBOUT)
      INTEGER RELPG(IDBSIZ),LNGTHG(IDBSIZ),LNBR(IDBSIZ),RNBR(IDBSIZ)
      CHARACTER GEL(MAXGEL),CON(IDC)
      INTEGER RREG,PC,PCA,PG
      NG = LGEL
      PG = RELPG(NG)
      PC = LREG
      NTT = 0
      NCT = 0
      NDT = 0
10    CONTINUE
C      WRITE(*,*)'GEL',NG
      CALL READW(IDEVW,NG,GEL,MAXGEL)
      LG = ABS(LNGTHG(NG))
      IF(PC.LT.LREG) PC = LREG
      PCA = PC - LREG + 1
      IG = PC - PG + 1
      LC = MIN(LG,RREG-PC+1)
C      WRITE(*,*)'PC,PG,IG,LG,PCA,LC',PC,PG,IG,LG,PCA,LC
      CALL ET(GEL(IG),LG,CON(PCA),LC,NE)
      NTT = NTT + NE
      CALL EC(GEL(IG),LG,CON(PCA),LC,NE)
      NCT = NCT + NE
      CALL ED(GEL(IG),LG,CON(PCA),LC,ND)
      NDT = NDT + ND
      CALL WRITEW(IDEVW,NG,GEL,MAXGEL)
      IF(ND.GT.0) THEN
        K = LNGTHG(NG)
        LNGTHG(NG) = ABS(LNGTHG(NG)) - ND
        LNGTHG(NG) = SIGN(LNGTHG(NG),K)
        CALL WRITER(IDEVR,NG,RELPG(NG),LNGTHG(NG),LNBR(NG),RNBR(NG))
      END IF
      IF(RNBR(NG).NE.0) THEN
        NG = RNBR(NG)
        PG = RELPG(NG)
        PC = PG
        IF(PG.LE.RREG) GO TO 10
      END IF
      CALL EDR(RELPG,LNGTHG,LNBR,RNBR,IDBSIZ,LGEL,NCONT,
     +CON,IDC,IDEVW,IDEVR,LREG)
      WRITE(KBOUT,1000)NTT
1000  FORMAT(' Number of transpositions=',I6)
      WRITE(KBOUT,1001)NCT
1001  FORMAT(' Number of changes       =',I6)
      WRITE(KBOUT,1002)NDT
1002  FORMAT(' Number of deletions     =',I6)
      END
C    AJOIN2
C   COMPLETES JOIN AND RETURNS LENGTH OF NEW CONTIG IN LLINOR
      SUBROUTINE AJOIN2(RELPG,LNGTHG,LNBR,RNBR,NGELS,NCONTS,IDBSIZ,
     +RELX,LLINOL,LLINOR,LNCONL,LNCONR,IDEVR)
C   AUTHOR: RODGER STADEN
      INTEGER RELPG(IDBSIZ)
      INTEGER LNBR(IDBSIZ),RNBR(IDBSIZ),LNGTHG(IDBSIZ)
      INTEGER RELX
C   RELX IS THE POSITION OF THE JOINT
C   LLINOL IS THE LEFT GEL NUMBER OF THE LEFT CONTIG
C   LLINOR IS THE LEFT GEL OF THE RIGHT CONTIG
C   LNCONL IS THE LEFT CONTIG LINE NUMBER
C   LNCONR IS THE RIGHT CONTIG LINE NUMBER
C
C   ADJUST ALL RELATIVE POSITIONS IN RIGHT CONTIG
      N=LLINOR
      RELPG(N)=RELX
50    CONTINUE
      IF(RNBR(N).EQ.0)GO TO 60
      N=RNBR(N)
      RELPG(N)=RELPG(N)+RELX-1
      GO TO 50
60    CONTINUE
C
C   FIX UP NEW GEL LINE FOR OLD LEFT OF RIGHT CONTIG
      LNBR(LLINOR)=RNBR(LNCONL)
C   FIX UP RIGHT GEL OF LEFT CONTIG
      N=RNBR(LNCONL)
      RNBR(N)=LLINOR
C   MERGE WILL SORT OUT THE CORRECT NEIGHBOURS
C
      CALL MERGE(RELPG,LNGTHG,LNBR,RNBR,LNCONL,IDBSIZ)
C   MERGE DOES NOT WRITE TO DISK
      N=LNBR(LNCONL)
65    CONTINUE
C      WRITE(IDEVR,REC=N)RELPG(N),LNGTHG(N),LNBR(N),RNBR(N)
      CALL WRITER(IDEVR,N,RELPG(N),LNGTHG(N),LNBR(N),RNBR(N))
      N=RNBR(N)
      IF(N.NE.0)GO TO 65
C   CONTIG LINES
      X=RELPG(LNCONR)+RELX-1
C   LENGTH MAY NOT HAVE INCREASED!
      IF(X.GT.RELPG(LNCONL))RELPG(LNCONL)=X
C   SAVE LENGTH OF NEW CONTIG
      RELX=RELPG(LNCONL)
C      WRITE(IDEVR,REC=LNCONL)RELPG(LNCONL),LNGTHG(LNCONL),LNBR(LNCONL),
C     1RNBR(LNCONL)
      CALL WRITER(IDEVR,LNCONL,RELPG(LNCONL),LNGTHG(LNCONL),
     +LNBR(LNCONL),RNBR(LNCONL))
C
C   NOW MOVE ALL DATA DOWN TO DELETE OLD RIGHT END
      N=IDBSIZ-NCONTS
      M=LNCONR-N
      IF(M.EQ.0)GO TO 80
      K=LNCONR
      J=LNCONR-1
      DO 70 I=1,M
      RELPG(K)=RELPG(J)
      LNGTHG(K)=LNGTHG(J)
      LNBR(K)=LNBR(J)
      RNBR(K)=RNBR(J)
C      WRITE(IDEVR,REC=K)RELPG(K),LNGTHG(K),LNBR(K),RNBR(K)
      CALL WRITER(IDEVR,K,RELPG(K),LNGTHG(K),LNBR(K),RNBR(K))
      K=K-1
      J=J-1
70    CONTINUE
80    CONTINUE
      NCONTS=NCONTS-1
C      WRITE(IDEVR,REC=IDBSIZ)NGELS,NCONTS
      CALL WRITER(IDEVR,IDBSIZ,NGELS,NCONTS,NGELS,NCONTS)
90    CONTINUE
      RETURN
      END
C     SUBROUTINE AJOIN3
      SUBROUTINE AJOIN3(RELPG,IDBSIZ,LINCON,ITYPE,ISENSE,JOINT,IDIM22,
     +KLASS,IOVER,KBOUT,PL,PR)
C   AUTHOR: RODGER STADEN
      INTEGER RELPG(IDBSIZ),LINCON(2),IDIM22(2)
      INTEGER ITYPE(2),ISENSE(2),JOINT(2),PL(2),PR(2)
C
C   CALC POSITIONS OF CONTIGS RELATIVE TO FIXED GEL
      DO 20 I=1,2
C   R+
      IF((ITYPE(I).NE.-1).OR.(ISENSE(I).NE.1))GO TO 11
      PL(I)=-1*JOINT(I)+2
      PR(I)=PL(I)+RELPG(LINCON(I))-1
      GO TO 20
C   L+
11    CONTINUE
      IF((ITYPE(I).NE.1).OR.(ISENSE(I).NE.1))GO TO 12
      PL(I)=JOINT(I)
      PR(I)=PL(I)+RELPG(LINCON(I))-1
      GO TO 20
C   R-
12    CONTINUE
      IF((ITYPE(I).NE.-1).OR.(ISENSE(I).NE.-1))GO TO 13
      PR(I)=JOINT(I)+IDIM22(I)-1
      PL(I)=PR(I)-RELPG(LINCON(I))+1
      GO TO 20
C   L-
13    CONTINUE
      PR(I)=IDIM22(I)-JOINT(I)+1
      PL(I)=PR(I)-RELPG(LINCON(I))+1
20    CONTINUE
C  LENGTH OF OVERLAP
      IOVER=MIN(PR(1),PR(2))-MAX(PL(1),PL(2))+1
      WRITE(KBOUT,1002)IOVER
1002  FORMAT(' Length of overlap between the contigs=',I6)
C
C  CLASS NUMBER 1-16
      KLASS=1
      IF(ITYPE(1).EQ.1)KLASS=KLASS+8
      IF(ISENSE(1).EQ.-1)KLASS=KLASS+4
      IF(ITYPE(2).EQ.1)KLASS=KLASS+2
      IF(ISENSE(2).EQ.-1)KLASS=KLASS+1
C      WRITE(KBOUT,1001)KLASS
C1001  FORMAT(' CLASS OF JOIN=',I6)
      RETURN
      END
C      ALINE
C
C    ROUTINE TO LINE UP 2 SEQS.
C   IT SLIDES,REMOVES OVERLAPPING MATCHES,
C   SORTS MATCHES INTO ASCENDING ORDER, THEN DOES DOES A TOPOLOGICAL
C   CHECK, AND THEN PRODUCES 2 LINED UP SEQS WITH PADDING CHARS
C   VARIABLES
C       SEQ1 CONSENSUS
C       SEQ2 GEL ORIGINAL IN CORRECT ORIENTATION
C       SEQG2 ALIGNED GEL
C       SEQC2 ALIGNED CONSENSUS
C       SEQ3 SAVED GEL RAW DATA
C       ISAV1,2,3 STORE MATCHES AND POSITIONS
C       IDSAV NUMBER ISAV'S
C       IDC LENGTH OF INPUT SEQ1
C       IDIM2 LENGTH OF INPUT SEQ2
C       IDOUT LENGTH OF OUTPUT ALIGNED SEQ1
C       IDIM2 LENGTH OF SEQ2 ON OUTPUT AFTER ALIGNMENT
C       MINSLI MIN MATCH FOR SLIDING
C       IFAIL FLAG TO SHOW IF ALIGNMENT FAILED DUE TO TOO
C   MANY MISMATCHES OR TOPOLIGICAL CHECK OR TOO MANY OR TOO MANY
C   PADDING CHARS. 1=FAIL,0=PASS
C
      SUBROUTINE ALINE(SEQ1,SEQ2,SEQG2,SEQC2,ISAV1,ISAV2,ISAV3,
     +IDSAV,IDC,IDIM2,IDOUT,IC1,IG1,MINSLI,JOINT,
     +ITOTPC,ITOTPG,IFAIL,ITYPE,MAXPC,MAXPG,PERMAX,KBOUT,SEQ3,MAXGEL,
     +PERCM,LENO,ISHOW)
C   AUTHOR: RODGER STADEN
      CHARACTER SEQ1(IDC),SEQ2(IDIM2),SEQG2(IDOUT),SEQC2(IDOUT)
      CHARACTER SEQ3(MAXGEL)
      INTEGER ISAV1(IDSAV),ISAV2(IDSAV),ISAV3(IDSAV)
      MINSLT=MINSLI
C   SAVE SEQ2
      CALL SQCOPY(SEQ2,SEQ3,IDIM2)
      CALL MSTLKL(SEQ3,IDIM2)
      IFAIL=1
C   FIND MATCHES
      IPP=IDSAV
      CALL SLIDER(SEQ1,IDC,SEQ3,IDIM2,IC1,IG1,MAXPG,MAXPC,MINSLT,
     +ISAV1,ISAV2,ISAV3,IPP)
      IF(IPP.GT.IDSAV)RETURN
      IF(IPP.LT.1)RETURN
      CALL REMOVL(ISAV2,ISAV3,ISAV1,IPP)
      CALL BUB3AS(ISAV2,ISAV3,ISAV1,IPP)
C   DO TOPOLOGICAL CHECK
      CALL TPCHEK(ISAV2,ISAV3,ISAV1,IPP)
C
C added next routine 27-2-93
C
      CALL UPCHEK(ISAV2,ISAV3,ISAV1,IPP)
      CALL LINEUP(SEQ2,SEQ1,SEQG2,SEQC2,IDC,IDIM2,IDOUT,ISAV3,ISAV2,
     +ISAV1,IPP,ITOTPC,ITOTPG,JOINT,ITYPE,KBOUT,MAXGEL,IFAIL)
      IF(ITOTPC.GT.MAXPC)IFAIL=1
      IF(ITOTPG.GT.MAXPG)IFAIL=1
      IF(IFAIL.NE.0)RETURN
C   IDIM2 IS NOW LENGTH OF ALIGNED GEL
      CALL DALIGN(SEQC2,SEQG2,SEQ3,MAXGEL,IDOUT,IDIM2,JOINT,
     +ITYPE,PERCM,KBOUT,IFAIL,LENO,PERMAX,ISHOW)
      IF(IFAIL.NE.0)RETURN
      IF(ISHOW.EQ.1) THEN
        WRITE(KBOUT,1052)PERCM,ITOTPC,ITOTPG
1052    FORMAT(' Percent mismatch=',F4.1,', pads in contig=',I3,
     +  ', pads in gel=',I3)
      END IF
      END
      SUBROUTINE ARCSER(RELPG,LNGTHG,LNBR,RNBR,NGELS,NCONTS,
     +IDBSIZ,KBIN,KBOUT,IDEVN,
     +IHELPS,IHELPE,FILEH,IDEVH)
      CHARACTER FILEH*(*)
C   AUTHOR: RODGER STADEN
C   SEARCHES FOR ARCHIVE NAMES
      INTEGER RELPG(IDBSIZ)
      INTEGER LNGTHG(IDBSIZ),LNBR(IDBSIZ),RNBR(IDBSIZ)
      CHARACTER NAME1*16,NAME2*16
10    CONTINUE
      L = 0
      CALL GTSTR('Archive name',' ',NAME1,L,KBOUT,KBIN,INFLAG)
      IF(L.EQ.0) RETURN
      CALL CCASE(NAME1,1)
      IF(INFLAG.EQ.2) RETURN
      IF(INFLAG.EQ.3) RETURN
      IF(NAME1(1:1).EQ.' ') RETURN
      IF(INFLAG.EQ.1) THEN
        CALL HELP2(IHELPS,IHELPE,FILEH,IDEVH,KBIN,KBOUT)
        GO TO 10
      END IF
      DO 100 I=1,NGELS
        CALL READN(IDEVN,I,NAME2)
        IF(NAME1.EQ.NAME2) THEN
          WRITE(KBOUT,1003)NAME2,I,RELPG(I),LNGTHG(I),LNBR(I),RNBR(I)
1003      FORMAT(' ',A,2X,I4,2X,I6,2X,I6,2X,I6,2X,I6/)
          GO TO 10
        END IF
100   CONTINUE
      WRITE(KBOUT,1004)NAME1
1004  FORMAT(' ',A,' Not in database')
      GO TO 10
      END
      SUBROUTINE AUTOCN(SEQ1,IDIM,GEL,IDIMG,ILEFTS,ILC,IPOSC,
     +IPOSG,ISENSE,LLINO,IMATC,IFCOMP,MINMAT,POSNS,WORDP,WORDN,
     +CONST,LENGTH,LPOWRC,KBOUT,MATCH,MAXGEL,MAXGLM,GELCOP,GELN,
     +SAVPS,SAVPG,SAVL,MAXSAV,CENDS,NENDS,MAXCON,
     +SEQG2,SEQC2,SEQ4,IDOUT,IDIM22,ITOTPG,ITOTPC,JOINT,IFAIL,
     +ITYPE,MAXPC,MAXPG,PERMAX,MINSLI,SEQG3,SEQC3,KFAIL,CSTART,
     +JOBC,PERMIS,LENO,ISHOW)
C   AUTHOR: RODGER STADEN
C   changed 29-11-90 to make first in list of alignments the best
      INTEGER ILEFTS(2),ILC(2),IPOSC(2),IPOSG(2),ISENSE(2),LLINO(2)
      INTEGER POSNS(IDIM),GELN(MAXGLM),WORDP(LPOWRC),SAVPS(MAXSAV)
      INTEGER SAVPG(MAXSAV),SAVL(MAXSAV)
      INTEGER WORDN(LPOWRC)
      CHARACTER GELCOP(MAXGLM),MATCH(MAXGLM)
      INTEGER CENDS(MAXCON),NENDS(MAXCON)
      INTEGER CONST(LENGTH)
      CHARACTER SEQ1(IDIM),GEL(MAXGLM)
C
      CHARACTER SEQG2(MAXGLM,2),SEQC2(MAXGLM,2),SEQ4(MAXGLM)
      INTEGER IDOUT(2),IDIM22(2),ITOTPG(2),ITOTPC(2),JOINT(2)
      INTEGER IFAIL(2),ITYPE(2)
      PARAMETER (MAXC = 100)
      CHARACTER SEQG3(MAXGLM),SEQC3(MAXGLM)
      INTEGER JLEFTS(MAXC),JLC(MAXC),JPOSC(MAXC),JPOSG(MAXC)
      INTEGER JSENSE(MAXC),JLLINO(MAXC),CSTART,START
      REAL PERMIS(2)
C
C jobc tells how to update the hash tables:
C 0 means dont do anything because the consensus hasnt changed
C 1 means add the last contig because a new one has been stuck on the end
C 2 means do the whole consensus
C
      IFAIL(1) = 1
      IFAIL(2) = 1
      KFAIL = 0
C  23-8-90 Need to deal with failures in a better way. Problem is
C          case where overlaps are found but fail to align. In future
C          signal them with new variable KFAIL which will be nonzero
C          if any alignment fails.
C  29-11-90 Changed sorting of overlaps so that the best is first in the 
C           list returned to caller.
C   SAVE GEL
      CALL SQCOPY(GEL,GELCOP,IDIMG)
C  COUNT NUMBER OF CONTIGS THAT MATCH
      IMATC=0
      IDCEND=MAXCON
      CALL BUSY(KBOUT)
      CALL FNDCON(SEQ1,IDIM,CENDS,NENDS,IDCEND,MAXCON,KBOUT)
      IF (JOBC.NE.0) THEN
        START = 1
        IF(JOBC.EQ.1) START = CENDS(IDCEND)
        CALL ENCOF(SEQ1(START),IDIM-START+1,CONST,CSTART,LENGTH,
     +POSNS(START))
        CALL ENCONN(POSNS,IDIM,WORDP,WORDN,LPOWRC,LENGTH,START)
      END IF
1     CONTINUE
      ISTRAN=1
2     CONTINUE
      CALL MSTLKL(GEL,IDIMG)
      CALL ENCOF(GEL,IDIMG,CONST,CSTART,LENGTH,GELN)
      IDSAV=MAXSAV
      CALL CFGEL(GELN,IDIMG,POSNS,IDIM,WORDP,WORDN,LENGTH,LPOWRC,
     +SAVPG,SAVPS,SAVL,
     +IDSAV,SEQ1,GEL,MINMAT,IFCOMP,KBOUT)
      IF(IFCOMP.NE.0)RETURN
      IF(IDSAV.NE.0)THEN
        CALL ADISM4(IDIM,IDIMG,SAVPS,SAVPG,IDSAV,CENDS,NENDS,
     +  IDCEND,MAXCON,JLEFTS,JLC,JPOSC,JPOSG,JSENSE,JLLINO,
     +  IMATC,ISTRAN,MAXC,KBOUT)
      END IF
      ISTRAN=ISTRAN+1
      IF(ISTRAN.EQ.2) THEN
        CALL SQCOPY(GELCOP,GEL,IDIMG)
        CALL SQREV(GEL,IDIMG)
        CALL SQCOM(GEL,IDIMG)
        GO TO 2
      END IF
      CALL SQCOPY(GELCOP,GEL,IDIMG)
      KSENSE = 0
      WRITE(KBOUT,*)'Total matches found',IMATC
      IF(IMATC.EQ.0) THEN
        IFAIL(1) = 0
        RETURN
      END IF
      JMATC = 0
      DO 100 I = 1,IMATC
        IF(JSENSE(I).EQ.-1) THEN
          IF(KSENSE.EQ.0) THEN 
            CALL SQREV(GEL,IDIMG)
            CALL SQCOM(GEL,IDIMG)
            KSENSE = 1
          END IF
        END IF
        JDIM22 = IDIMG
        JDOUT = MAXGEL
        IDSAV = MAXSAV
        WRITE(KBOUT,*)'Trying to align with contig',JLLINO(I)
        CALL ALINE(SEQ1(JLEFTS(I)),GEL,SEQG3,SEQC3,
     +  SAVPS,SAVPG,SAVL,IDSAV,JLC(I),JDIM22,JDOUT,
     +  JPOSC(I),JPOSG(I),MINSLI,JJOINT,JTOTPC,JTOTPG,
     +  JFAIL,JTYPE,MAXPC,MAXPG,PERMAX,KBOUT,SEQ4,MAXGEL,PERMS,LENO,
     +  ISHOW)
        IF(JFAIL.EQ.0) THEN
          JMATC = JMATC + 1
          IF(JMATC.EQ.1) THEN
C    Save in elements 1
             CALL COPYM(JLEFTS(I),ILEFTS(1),JLC(I),ILC(1),
     +          JPOSC(I),IPOSC(1),JSENSE(I),ISENSE(1),
     +          JLLINO(I),LLINO(1),JJOINT,JOINT(1),JTOTPC,
     +          ITOTPC(1),JTOTPG,ITOTPG(1),JTYPE,ITYPE(1),
     +          JDOUT,IDOUT(1),JDIM22,IDIM22(1),
     +          SEQG3,SEQG2(1,1),SEQC3,SEQC2(1,1),
     +          PERMS,PERMIS(1))
            IFAIL(1) = 0
          ELSE IF(JMATC.EQ.2) THEN
            IF(PERMS.LT.PERMIS(1)) THEN
C    Better match so save in elements 1, so copy 1 to 2 first
              CALL COPYM(ILEFTS(1),ILEFTS(2),ILC(1),ILC(2),
     +          IPOSC(1),IPOSC(2),ISENSE(1),ISENSE(2),
     +          LLINO(1),LLINO(2),JOINT(1),JOINT(2),ITOTPC(1),
     +          ITOTPC(2),ITOTPG(1),ITOTPG(2),ITYPE(1),ITYPE(2),
     +          IDOUT(1),IDOUT(2),IDIM22(1),IDIM22(2),
     +          SEQG2(1,1),SEQG2(1,2),SEQC2(1,1),SEQC2(1,2),
     +          PERMIS(1),PERMIS(2))
                IFAIL(2) = 0
C    Now save in 1
                CALL COPYM(JLEFTS(I),ILEFTS(1),JLC(I),ILC(1),
     +          JPOSC(I),IPOSC(1),JSENSE(I),ISENSE(1),
     +          JLLINO(I),LLINO(1),JJOINT,JOINT(1),JTOTPC,
     +          ITOTPC(1),JTOTPG,ITOTPG(1),JTYPE,ITYPE(1),
     +          JDOUT,IDOUT(1),JDIM22,IDIM22(1),
     +          SEQG3,SEQG2(1,1),SEQC3,SEQC2(1,1),
     +          PERMS,PERMIS(1))
            ELSE
C    Save in element 2
                CALL COPYM(JLEFTS(I),ILEFTS(2),JLC(I),ILC(2),
     +          JPOSC(I),IPOSC(2),JSENSE(I),ISENSE(2),
     +          JLLINO(I),LLINO(2),JJOINT,JOINT(2),JTOTPC,
     +          ITOTPC(2),JTOTPG,ITOTPG(2),JTYPE,ITYPE(2),
     +          JDOUT,IDOUT(2),JDIM22,IDIM22(2),
     +          SEQG3,SEQG2(1,2),SEQC3,SEQC2(1,2),
     +          PERMS,PERMIS(2))
              IFAIL(2) = 0
            END IF
          ELSE
            IF(PERMS.LT.PERMIS(1)) THEN
C    Better match so save in elements 1, so copy 1 to 2 first
              CALL COPYM(ILEFTS(1),ILEFTS(2),ILC(1),ILC(2),
     +          IPOSC(1),IPOSC(2),ISENSE(1),ISENSE(2),
     +          LLINO(1),LLINO(2),JOINT(1),JOINT(2),ITOTPC(1),
     +          ITOTPC(2),ITOTPG(1),ITOTPG(2),ITYPE(1),ITYPE(2),
     +          IDOUT(1),IDOUT(2),IDIM22(1),IDIM22(2),
     +          SEQG2(1,1),SEQG2(1,2),SEQC2(1,1),SEQC2(1,2),
     +          PERMIS(1),PERMIS(2))
                IFAIL(2) = 0
C    Now save in 1
                CALL COPYM(JLEFTS(I),ILEFTS(1),JLC(I),ILC(1),
     +          JPOSC(I),IPOSC(1),JSENSE(I),ISENSE(1),
     +          JLLINO(I),LLINO(1),JJOINT,JOINT(1),JTOTPC,
     +          ITOTPC(1),JTOTPG,ITOTPG(1),JTYPE,ITYPE(1),
     +          JDOUT,IDOUT(1),JDIM22,IDIM22(1),
     +          SEQG3,SEQG2(1,1),SEQC3,SEQC2(1,1),
     +          PERMS,PERMIS(1))
            ELSE IF(PERMS.LT.PERMIS(2)) THEN
C    Save in element 2
                CALL COPYM(JLEFTS(I),ILEFTS(2),JLC(I),ILC(2),
     +          JPOSC(I),IPOSC(2),JSENSE(I),ISENSE(2),
     +          JLLINO(I),LLINO(2),JJOINT,JOINT(2),JTOTPC,
     +          ITOTPC(2),JTOTPG,ITOTPG(2),JTYPE,ITYPE(2),
     +          JDOUT,IDOUT(2),JDIM22,IDIM22(2),
     +          SEQG3,SEQG2(1,2),SEQC3,SEQC2(1,2),
     +          PERMS,PERMIS(2))
            END IF
          END IF
        ELSE
          KFAIL = 1
        END IF
100   CONTINUE
      IMATC = MIN(2,JMATC)
      END
      SUBROUTINE BREAKC(RELPG,LNGTHG,LNBR,RNBR,NGELS,NCONTS,
     +IDBSIZ,KBIN,KBOUT,IDEVR,IDEVW,IDEVN,
     +IHELPS,IHELPE,IHELP1,IHELP2,FILEH,IDEVH,IOK)
      CHARACTER FILEH*(*)
C   AUTHOR: RODGER STADEN
      INTEGER RELPG(IDBSIZ)
      INTEGER LNGTHG(IDBSIZ),LNBR(IDBSIZ),RNBR(IDBSIZ)
      INTEGER CHAINL,GCLIN
      EXTERNAL CHAINL,GCLIN
C ROUTINE TO BREAK A CONTIG INTO 2
C LEFT GEL OF NEW RIGHT CONTIG IS IR
C RIGHT GEL OF NEW LEFT CONTIG IS IL
C LEFT GEL OF OLD LEFT CONTIG IS ILO
C CONTIG LINE OF OLD CONTIG IS NCONTO
C CONTIG LINE OF NEW RIGHT CONTIG IS NCONTR
C CONTIG LINE OF NEW LEFT CONTIG IS NCONTO
C LENGTH OF OLD CONTIG IS LCONTO
      IOK = 1
      NCONTR = IDBSIZ - NCONTS - 1
      IF(NCONTR.LE.NGELS) THEN
        WRITE(KBOUT,*)'Insufficient space for new contig line.'
        WRITE(KBOUT,*)'Increase database size with copy'
        RETURN
      END IF
10    CONTINUE
        MN = 0
        MX = NGELS
        IR  = 0
        CALL GETINT(MN,MX,IR,
     +  'Number of gel reading that will become a left end',
     +  IVAL,KBIN,KBOUT,
     +  IHELPS,IHELPE,FILEH,IDEVH,IOK)
        IF(IOK.NE.0) RETURN
        IF(IVAL.LT.1) RETURN
        IR = IVAL
      IL = LNBR(IR)
      IF(IL.EQ.0)THEN
        WRITE(KBOUT,*)'Gel number',IR,' is already a left end'
        GO TO 10
      END IF
      ILO = CHAINL(RELPG,LNGTHG,LNBR,RNBR,NGELS,NCONTS,
     +IDBSIZ,IR)
      IF(ILO.EQ.0)THEN
        WRITE(KBOUT,*)
     +'Problem with this contig. Check logical consistency'
        WRITE(KBOUT,*)'of database. Break not made'
        RETURN
      END IF
      NCONTO = GCLIN(RELPG,LNGTHG,LNBR,RNBR,NGELS,NCONTS,
     +IDBSIZ,ILO)
      IF(NCONTO.EQ.0)THEN
        WRITE(KBOUT,*)'No contig line for this contig. Check logical'
        WRITE(KBOUT,*)'consistency of database. Break not made'
        RETURN
      END IF
      LCONTO = RELPG(NCONTO)
      IF(LCONTO.LT.1)THEN
        WRITE(KBOUT,*)'Contig has zero length. Break not made'
        RETURN
      END IF
      CALL CBREAK(RELPG,LNGTHG,LNBR,RNBR,NGELS,NCONTS,
     +IDBSIZ,KBOUT,IDEVR,IDEVW,IDEVN,IR,IL,ILO,NCONTO,NCONTR,IOK)
      END
      SUBROUTINE CBREAK(RELPG,LNGTHG,LNBR,RNBR,NGELS,NCONTS,
     +IDBSIZ,KBOUT,IDEVR,IDEVW,IDEVN,IR,IL,ILO,NCONTO,NCONTR,IOK)
C   AUTHOR: RODGER STADEN
      INTEGER RELPG(IDBSIZ)
      INTEGER LNGTHG(IDBSIZ),LNBR(IDBSIZ),RNBR(IDBSIZ)
      INTEGER CLEN
      EXTERNAL CLEN
C ROUTINE TO BREAK A CONTIG INTO 2
C LEFT GEL OF NEW RIGHT CONTIG IS IR
C RIGHT GEL OF NEW LEFT CONTIG IS IL
C LEFT GEL OF OLD LEFT CONTIG IS ILO
C CONTIG LINE OF OLD CONTIG IS NCONTO
C CONTIG LINE OF NEW RIGHT CONTIG IS NCONTR
C CONTIG LINE OF NEW LEFT CONTIG IS NCONTO
C LENGTH OF OLD CONTIG IS LCONTO
      IOK = 1
      NCONTS = NCONTS + 1
C  WRITE LAST LINE OF DB
      WRITE(KBOUT,*)'Increasing number of contigs by 1'
      CALL WRITER(IDEVR,IDBSIZ,NGELS,NCONTS,NGELS,NCONTS)
C  MAKE NEW CONTIG A COPY OF OLD
      RELPG(NCONTR) = RELPG(NCONTO)
      LNGTHG(NCONTR) = LNGTHG(NCONTO)
      LNBR(NCONTR) = IR
      RNBR(NCONTR) = RNBR(NCONTO)
      WRITE(KBOUT,*)'Writing new right contig line'
      CALL WRITER(IDEVR,NCONTR,RELPG(NCONTR),LNGTHG(NCONTR),
     +LNBR(NCONTR),RNBR(NCONTR))
C  NEED LENGTH FOR OLD LEFT CONTIG
      RNBR(IL) = 0
      L = CLEN(RELPG,LNGTHG,LNBR,RNBR,NGELS,NCONTS,
     +IDBSIZ,ILO)
      IF(L.LT.1)THEN
        WRITE(KBOUT,*)
     +  'New left contig has zero length. Break not made'
        RETURN
      END IF
      RELPG(NCONTO) = L
      RNBR(NCONTO) = IL
C  DO CONTIG LINE FOR NEW LEFT CONTIG
      WRITE(KBOUT,*)'Writing new left contig line'
      WRITE(KBOUT,*)'New length=',RELPG(NCONTO)
      WRITE(KBOUT,*)'New right gel=',RNBR(NCONTO)
      CALL WRITER(IDEVR,NCONTO,RELPG(NCONTO),LNGTHG(NCONTO),
     +LNBR(NCONTO),RNBR(NCONTO))
C  DO GEL LINE FOR RIGHT GEL OF NEW LEFT CONTIG
      WRITE(KBOUT,*)'Writing new right gel of left contig'
      WRITE(KBOUT,*)'Gel number=',IL
      CALL WRITER(IDEVR,IL,RELPG(IL),LNGTHG(IL),
     +LNBR(IL),RNBR(IL))
C  DO GEL LINE FOR NEW RIGHT CONTIG
      LNBR(IR) = 0
      WRITE(KBOUT,*)'Writing new left gel of right contig'
      WRITE(KBOUT,*)'Gel number=',IR
      CALL WRITER(IDEVR,IR,RELPG(IR),LNGTHG(IR),
     +LNBR(IR),RNBR(IR))
C  NOW SHIFT
      I = 1 - RELPG(IR)
      WRITE(KBOUT,*)'Shifting gels in right contig by distance=',I
      CALL SHIFTC(RELPG,LNGTHG,LNBR,RNBR,NGELS,NCONTS,IDEVR,
     +IDBSIZ,IR,NCONTR,I)
      WRITE(KBOUT,*)'Right contig length=',RELPG(NCONTR)
      WRITE(KBOUT,*)'Break completed'
      IOK = 0
      END
C     BUBBL3
C   SUBROUTINE TO SORT INTEGER ARRAY (LIST) INTO ASCENDING  ORDER
C
      SUBROUTINE BUBBL3(LIST,LISTEL,LISTAL,IDIM)
C   AUTHOR: RODGER STADEN
      INTEGER LIST(IDIM),LISTEL(IDIM),LISTAL(IDIM)
C
C   SET POINTERS TO ZERO
      I=0
      J=0
C
10    CONTINUE
C
C   SET I=J IF WE HAVE JUST CORRECTLY POSITIONED AN ELEMENT
      IF(J.GT.I)I=J
C
C   INCREMENT POINTER TO NEXT ELEMENT
      I=I+1
C   TEST FOR END OF ARRAY
      IF(I.EQ.IDIM)RETURN
C
20    CONTINUE
C
C   COMPARE ADJACENT ELEMENTS
      IF(LIST(I).GE.LIST(I+1))GO TO 10
C
C   FIRST MOVE THIS ELEMENT? IF SO SET POINTER TO ITS INITIAL POSITION
      IF(J.LT.I)J=I
C
C   EXCHANGE ADJACENT ELEMENTS
      ITEMP=LIST(I)
      LIST(I)=LIST(I+1)
      LIST(I+1)=ITEMP
C
      ITEMP=LISTEL(I)
      LISTEL(I)=LISTEL(I+1)
      LISTEL(I+1)=ITEMP
      ITEMP=LISTAL(I)
      LISTAL(I)=LISTAL(I+1)
      LISTAL(I+1)=ITEMP
C
C
C   DECREMENT BACK THRU LIST WITH THIS ELEMENT
      IF(I.GT.1)I=I-1
C
      GO TO 20
      END
      SUBROUTINE CCTA(SEQ,ID)
      CHARACTER SEQ(ID),COM,AS
      SAVE COM,AS
      DATA COM/','/,AS/'*'/
      DO 10 I = 1,ID
        IF(SEQ(I).EQ.COM) SEQ(I) = AS
10    CONTINUE
      END
C
C     CFGEL  new version 15-4-92
C
C   ROUTINE TO COMPARE A STRING OF WORD NUMBERS FOR A GEL WITH A SERIES
C   OF ARRAYS REPRESENTING A CONSENSUS SEQUENCE. WE LOOK FOR OCCURENCES
C   OF PAIRS OF WORDS (EACH WORD IS LENGTH CHARS LONG AND SO TOTAL MATCH IS
C   2*LENGTH CHARS LONG). THE ARRAYS SENT ARE OF SIZE 4**LENGTH (LE4)
      SUBROUTINE CFGEL(GELN,IDIMG,POSNS,IDIM,WORDP,WORDN,LENGTH,LE4,
     +SAVPG,
     +SAVPS,SAVL,IDSAV,SEQ,GEL,MINMAT,IFAIL,KBOUT)
C   AUTHOR: RODGER STADEN
      CHARACTER SEQ(IDIM),GEL(IDIMG)
      INTEGER POSNS(IDIM),WORDP(LE4),SAVPS(IDSAV),SAVPG(IDSAV)
      INTEGER GELN(IDIMG),SAVL(IDSAV)
      INTEGER WORDN(LE4)
      INTEGER W1,W2,PS1W1,PS1W2
      INTEGER CTONUM
      EXTERNAL CTONUM
C go thru the gel reading looking at words to see if they exist in the consensus
      IDSAVM=IDSAV
      IDSAV=0
C length of a pair of words is LX2
      LX2=2*LENGTH
C number of pairs of words in gel reading is NW
      NW=IDIMG-(LX2-1)
C loop for each words start point
      DO 200 I=1,NW
C       IS THIS WORD A ZERO?
        W1=GELN(I)
        IF(W1.EQ.0)GO TO 200
C       POINT TO NEXT WORD OF PAIR
        W2=GELN(I+LENGTH)
        IF(W2.EQ.0)GO TO 200
C       DOES W1 EXIST IN SEQ?
        N1S1=WORDN(W1)
        IF(N1S1.EQ.0)GO TO 200
        N2S1=WORDN(W2)
        IF(N2S1.EQ.0)GO TO 200
C       BOTH EXIST, SO POINT TO THE FIRST + LENGTH
        PS1W1=WORDP(W1)+LENGTH
C       LOOP FOR ALL PAIRS
C there are N1S1 occurrences of word 1 and N2S1 of word 2 in consensus
C compare the positions of all pairs to see if they are LENGTH apart
        DO 50 J=1,N1S1
C         POINT TO FIRST W2 BECAUSE IT IS IN WORDP NOT POSNS
          PS1W2=WORDP(W2)
C         LOOP FOR THESE
          DO 40 K=1,N2S1
C           ARE THIS PAIR LENGTH APART?
            N=PS1W1-PS1W2
            IF(N.NE.0)GO TO 20
C           THEY ARE SO, IF REQUIRED LOOK FOR REST OF MATCH
            LMAT=LX2
C
C new code
C
            IPC = PS1W2 + LENGTH - 1
            IPG = I + LX2 - 1
 16         CONTINUE
            IF (LMAT.LT.MINMAT) THEN
              IPC = IPC + 1
              IPG = IPG + 1
              IF(IPG.GT.IDIMG)GO TO 20
              IF(IPC.GT.IDIM)GO TO 20
              IF(CTONUM(SEQ(IPC)).NE.CTONUM(GEL(IPG)))GO TO 20
              LMAT=LMAT+1
              GO TO 16
            END IF
C
C match found, is it an extension of a previous one ?
C
C            WRITE(*,*)I,PS1W1-LENGTH
            IF (IDSAV.GT.0) THEN
              IF (I-SAVPG(IDSAV).EQ.PS1W1-LENGTH-SAVPS(IDSAV)) GO TO 20
            END IF
            IDSAV = IDSAV + 1
            IF (IDSAV.GT.IDSAVM) THEN
              WRITE(KBOUT,1000)IDSAVM
1000          FORMAT(' More than ',I6,' matches. Search aborted')
              IFAIL = 1
              RETURN
            END IF
C            WRITE(*,*)IDSAV
            SAVPG(IDSAV) = I
            SAVPS(IDSAV) = PS1W1 - LENGTH
 20         CONTINUE
C           POINT TO NEXT W2
            PS1W2=POSNS(PS1W2)
40        CONTINUE
C         ALL TRIED THIS PS1W1, TRY NEXT
          PS1W1=POSNS(PS1W1-LENGTH)+LENGTH
50      CONTINUE
200   CONTINUE
      IFAIL=0
      RETURN
      END
C
C     CFGEL old version (before 15-4-92)
C
C   ROUTINE TO COMPARE A STRING OF WORD NUMBERS FOR A GEL WITH A SERIES
C   OF ARRAYS REPRESENTING A CONSENSUS SEQUENCE. WE LOOK FOR OCCURENCES
C   OF PAIRS OF WORDS (EACH WORD IS LENGTH CHARS LONG AND SO TOTAL MATCH IS
C   2*LENGTH CHARS LONG). THE ARRAYS SENT ARE OF SIZE 4**LENGTH (LE4)
      SUBROUTINE CFGELO(GELN,IDIMG,POSNS,IDIM,WORDP,WORDN,LENGTH,LE4,
     +SAVPG,
     +SAVPS,SAVL,IDSAV,SEQ,GEL,MINMAT,IFAIL,KBOUT)
C   AUTHOR: RODGER STADEN
      CHARACTER SEQ(IDIM),GEL(IDIMG)
      INTEGER POSNS(IDIM),WORDP(LE4),SAVPS(IDSAV),SAVPG(IDSAV)
      INTEGER GELN(IDIMG),SAVL(IDSAV)
      INTEGER WORDN(LE4)
      INTEGER W1,W2,PS1W1,PS1W2
      INTEGER CTONUM
      EXTERNAL CTONUM
      IDSAVM=IDSAV
      IDSAV=0
C   LENGTH OF PAIR OF WORDS
      LX2=2*LENGTH
C   NUMBER OF PAIRS OF WORDS OF LENGTH LENGTH IN GEL
      NW=IDIMG-(LX2-1)
C   LOOP FOR EACH START POINT
      DO 200 I=1,NW
C   IS THIS WORD A ZERO?
      W1=GELN(I)
      IF(W1.EQ.0)GO TO 200
C   POINT TO NEXT WORD OF PAIR
      W2=GELN(I+LENGTH)
      IF(W2.EQ.0)GO TO 200
C   DOES W1 EXIST IN SEQ?
      N1S1=WORDN(W1)
      IF(N1S1.EQ.0)GO TO 200
      N2S1=WORDN(W2)
      IF(N2S1.EQ.0)GO TO 200
C   BOTH EXIST, SO POINT TO THE FIRST + LENGTH
      PS1W1=WORDP(W1)+LENGTH
C   LOOP FOR ALL PAIRS
      DO 50 J=1,N1S1
C   POINT TO FIRST W2 BECAUSE IT IS IN WORDP NOT POSNS
      PS1W2=WORDP(W2)
C   LOOP FOR THESE
      DO 40 K=1,N2S1
C   ARE THIS PAIR LENGTH APART?
      N=PS1W1-PS1W2
      IF(N.NE.0)GO TO 20
C   THEY ARE SO, IF REQUIRED LOOK FOR REST OF MATCH
      LMAT=LX2
      IF(MINMAT.EQ.LX2)GO TO 15
      IPC=PS1W2+LENGTH
      IPG=I+LX2
16    CONTINUE
      IF(IPG.GT.IDIMG)GO TO 15
      IF(IPC.GT.IDIM)GO TO 15
C
      IF(CTONUM(SEQ(IPC)).NE.CTONUM(GEL(IPG)))GO TO 15
      LMAT=LMAT+1
      IPC=IPC+1
      IPG=IPG+1
      GO TO 16
15    CONTINUE
C  IS MATCH LONG ENOUGH?
      IF(LMAT.LT.MINMAT)GO TO 20
      IDSAV=IDSAV+1
      IF(IDSAV.LE.IDSAVM)GO TO 18
      WRITE(KBOUT,1000)IDSAVM
1000  FORMAT(' More than ',I6,' matches. Search aborted')
      IFAIL=1
      RETURN
18    CONTINUE
      SAVL(IDSAV)=LMAT
      SAVPG(IDSAV)=I
      SAVPS(IDSAV)=PS1W1-LENGTH
20    CONTINUE
C   POINT TO NEXT W2
      PS1W2=POSNS(PS1W2)
40    CONTINUE
C   ALL TRIED THIS PS1W1, TRY NEXT
      PS1W1=POSNS(PS1W1-LENGTH)+LENGTH
50    CONTINUE
200   CONTINUE
      IFAIL=0
      RETURN
      END
      INTEGER FUNCTION CHAINL(RELPG,LNGTHG,LNBR,RNBR,NGELS,NCONTS,
     +IDBSIZ,IIN)
C  AUTHOR: RODGER STADEN
C  RETURNS CONTIG LEFT GEL NUMBER OR ZERO FOR ERROR
      INTEGER RELPG(IDBSIZ)
      INTEGER LNGTHG(IDBSIZ),LNBR(IDBSIZ),RNBR(IDBSIZ)
      I = IIN
      J = I
      CHAINL = 0
10    CONTINUE
      IF(I.NE.0)THEN
        J = I
        I = LNBR(I)
        IF(I.EQ.IIN)RETURN
        GO TO 10
      END IF
      CHAINL = J
      END
C
C      CHANGE
C
C   ROUTINE TO EXCHANGE ALL THE CHARS IN A CHARACTER ARRAY USING
C   A PAIR OF LOOKUP ARRAYS SENT BY CALLING PROG
C
C
      SUBROUTINE CHANGE(SEQ,IDIM1,CHAR1,CHAR2,IDIM2,ELSE)
C   AUTHOR: RODGER STADEN
      CHARACTER SEQ(IDIM1)
      CHARACTER CHAR1(IDIM2),CHAR2(IDIM2)
      CHARACTER ELSE
      DO 100 I=1,IDIM1
C
      DO 50 J=1,IDIM2
C
      IF(SEQ(I).NE.CHAR1(J))GO TO 50
C   MATCH SO EXCHANGE CHARS
      SEQ(I)=CHAR2(J)
      GO TO 100
50    CONTINUE
      SEQ(I)=ELSE
100   CONTINUE
C
      RETURN
      END
      CHARACTER*1 FUNCTION CHARSL(I)
      CHARACTER C*6
      SAVE C
      DATA C/'ctag*-'/
      CHARSL = C(I:I)
      END
      CHARACTER*1 FUNCTION CHARSU(I)
      CHARACTER C*6
      SAVE C
      DATA C/'CTAG*-'/
      CHARSU = C(I:I)
      END
      INTEGER FUNCTION CLEN(RELPG,LNGTHG,LNBR,RNBR,NGELS,NCONTS,
     +IDBSIZ,IIN)
C  AUTHOR: RODGER STADEN
C  RETURNS CONTIG LEFT GEL NUMBER OR ZERO FOR ERROR
      INTEGER RELPG(IDBSIZ)
      INTEGER LNGTHG(IDBSIZ),LNBR(IDBSIZ),RNBR(IDBSIZ)
      I = IIN
      CLEN= 0
      LEN = 0
10    CONTINUE
      IF(I.NE.0)THEN
        LEN = MAX(LEN,(RELPG(I) + ABS(LNGTHG(I)) - 1))
        I = RNBR(I)
        IF(I.EQ.IIN)RETURN
        GO TO 10
      END IF
      CLEN = LEN
      END
      SUBROUTINE CLIST(GELNO1,LINNO1,IGEL1,GELNO2,LINNO2,
     +IGEL2,GELNOS,GELSTR,GELEND,IUNIQ1,IUNIQ,KBOUT,IOK)
C   AUTHOR: RODGER STADEN
      INTEGER GELNO1(IGEL1),GELNO2(IGEL2),GELNOS(IUNIQ1)
      INTEGER GELSTR(IUNIQ1),GELEND(IUNIQ1)
      INTEGER LINNO1(IGEL1),LINNO2(IGEL2)
      EXTERNAL INLIST
C   GELNOS === GEL NUMBERS (GELNOS)
C   GELSTR === GEL START LINES
C   GELEND === GEL END LINES
C   GELNO  === GEL NUMBERS PER STRIP
C   LINNO  === GEL LINE NUMBERS PER STRIP
C   IGEL   === NUMBER OF GELS PER STRIP
C   LINENO === CURRENT LINE NUMBER
C
C     WHICH GELS IN GELNO2 DO NOT APPEAR IN GELNO1
C     IE HAVE STARTED IN GELNO2
        DO 20 I=1,IGEL2
        MATCH=INLIST(GELNO1,IGEL1,GELNO2(I))
        IF(MATCH.EQ.0)THEN
C         NO MATCH SO NEW
C         PUT IN GELSTR
          IUNIQ=IUNIQ+1
          GELNOS(IUNIQ)=GELNO2(I)
          GELSTR(IUNIQ)=LINNO2(I)
        END IF
20    CONTINUE
C     WHICH GELS IN GELNO1 DO NOT APPEAR IN GELNO2
C     IE WHICH HAVE ENDED IN GELNO1
      DO 10 I=1,IGEL1
        MATCH=INLIST(GELNO2,IGEL2,GELNO1(I))
        IF(MATCH.EQ.0)THEN
C         NO MATCH  SO MUST HAVE ENDED
C         WHERE IS IT STORED IN GELNOS?
          MATCH=INLIST(GELNOS,IUNIQ,GELNO1(I))
          IF(MATCH.NE.0)THEN
            GELEND(MATCH)=LINNO1(I)
            GO TO 10
          END IF
C         ERROR
          WRITE(KBOUT,1000)GELNO1(I)
1000      FORMAT( ' Error: gel number ',I5,
     +    ' expected but not found in list')
          IOK = 1
          RETURN
        END IF
10    CONTINUE
      IOK = 0
      RETURN
      END
C
C      CMPLMT
C
C   SUBROUTINE TO REVERSE AND COMPLEMENT GELS AND DATA BASE
C   THE POSITIONS OF THE RIGHT ENDS OF GELS ARE FIRST STORED
C   IN RELPG THEN WE DO A BUBBLE SORT ON THESE POSITIONS
C   UPDATING RELATIONSHIPS AS WE GO
C   ALSO SEQUENCES ARE COMPLEMENTED, SIGNS OF LENGTH ARE
C   MULTIPLIED BY -1 AND THE CONTIG LINE IS ALTERED
      SUBROUTINE CMPLMT(RELPG,LNGTHG,LNBR,RNBR,NGELS,NCONTS,
     +LINCON,LLINO,GEL,IDBSIZ,KBOUT,IDEVR,IDEVW,MAXGEL)
C   AUTHOR: RODGER STADEN
      INTEGER RELPG(IDBSIZ)
      INTEGER LNGTHG(IDBSIZ),LNBR(IDBSIZ),RNBR(IDBSIZ)
      CHARACTER GEL(MAXGEL)
      INTEGER X
C
      WRITE(KBOUT,1000)LLINO
1000  FORMAT( ' Complementing contig',I6)
C   CHAIN THRU AND PUT RIGHT ENDS IN RELPG
      N=LLINO
10    CONTINUE
      RELPG(N)=RELPG(N)+(ABS(LNGTHG(N)))-1
      IF(RNBR(N).EQ.0)GO TO 20
      N=RNBR(N)
      GO TO 10
20    CONTINUE
C
C   NOW EFFECTIVELY BUBBLE SORT ON RELPG
      N=RNBR(LINCON)
      GO TO 22
21    CONTINUE
      N=NL
      IF(I1.GT.0)N=I2
22    CONTINUE
      NL=LNBR(N)
      IF(NL.EQ.0)GO TO 30
      I1=0
23    CONTINUE
      IF(RELPG(N).GE.RELPG(NL))GO TO 21
C   NOT IN CORRECT ORDER SO CHAIN ALONG UNTIL CORRECT,THEN COME
C   BACK TO THIS POINT AND CONTINUE
C   IF FIRST MOVE THIS LINE SET POINTER TO CURRENT POSITION
      IF(I1.EQ.0)I2=N
      I1=1
C
C   EXCHANGE NEIGHBOURS. CURRENTLY LOOKING AT N AND ITS LEFT
C   NBR, AND THE LEFT NBR IS FURTHER RIGHT THAN N
C   FIX UP POINTERS TO LEFT AND RIGHT OF THESE TWO
      M=LNBR(NL)
      IF(M.NE.0)RNBR(M)=N
      M=RNBR(N)
      IF(M.NE.0)LNBR(M)=NL
      LNBR(N)=LNBR(NL)
      LNBR(NL)=N
      RNBR(NL)=RNBR(N)
      RNBR(N)=NL
C   CHAIN BACK THRU LIST WITH THIS LINE
      N=RNBR(NL)
      IF(N.EQ.0)GO TO 21
C   IE END MET
      GO TO 23
30    CONTINUE
C   FINISH WITH LEFT END IN N
40    CONTINUE
C   NOW REVERSE NBRS SO CHAIN BACK RIGHT
      NL=RNBR(N)
      IF(NL.EQ.0)GO TO 50
      RNBR(N)=LNBR(N)
      LNBR(N)=NL
      N=NL
      GO TO 40
50    CONTINUE
C   NEED TO FIX UP NEW LEFT END
      RNBR(N)=LNBR(N)
      LNBR(N)=0
C   ALL POINTERS FIXED NOW DO RELATIVE POSITION
C   FINISH WITH LEFT END IN N
C   SO CHAIN BACK RIGHT
C   SAVE RIGHT LINE NUMBER
      NL=N
      X=RELPG(N)
60    CONTINUE
      RELPG(N)=1+(-1*(RELPG(N)-X))
      IF(RNBR(N).EQ.0)GO TO 70
      N=RNBR(N)
      GO TO 60
70    CONTINUE
C   NOW FIX CONTIG LINE
      LNBR(LINCON)=NL
      RNBR(LINCON)=N
C   WRITE NEW CONTIG LINE
      CALL WRITER(IDEVR,LINCON,RELPG(LINCON),LNGTHG(LINCON),
     +LNBR(LINCON),RNBR(LINCON))
C      WRITE(IDEVR,REC=LINCON)RELPG(LINCON),LNGTHG(LINCON),LNBR(LINCON),
C     1RNBR(LINCON)
C   NOW REVERSE AND COMPLEMENT GELS
      N=NL
80    CONTINUE
C      READ(IDEVW,REC=N)GEL
      CALL READW(IDEVW,N,GEL,MAXGEL)
      M=ABS(LNGTHG(N))
      CALL SQREV(GEL,M)
      CALL SQCOM(GEL,M)
      CALL WRITEW(IDEVW,N,GEL,MAXGEL)
C      WRITE(IDEVW,REC=N)GEL
C   CHANGE SIGNS
      LNGTHG(N)=-1*LNGTHG(N)
C   WRITE NEW GEL LINE
      CALL WRITER(IDEVR,N,RELPG(N),LNGTHG(N),
     +LNBR(N),RNBR(N))
C      WRITE(IDEVR,REC=N)RELPG(N),LNGTHG(N),LNBR(N),RNBR(N)
C   ANY MORE?
      N=RNBR(N)
      IF(N.NE.0)GO TO 80
C   NO MORE
      RETURN
      END
C      CONSEN
C   CALCULATES A CONSENSUS USING THE RULES OUTLINED IN THE DOCUMENTATION
C   AND SUBROUTINE SUMMER
C   UNIT IDEV IS USED FOR OUTPUT
      SUBROUTINE CONSEN(RELPG,LNGTHG,LNBR,RNBR,NAMPRO,NGELS,NCONTS,
     +SEQ1,IDIM1,GEL,IDBSIZ,TEMP,CHRSIZ,MAXGL2,
     +KBIN,KBOUT,IDEVW,IDEV,NAMCON,
     +IHELPS,IHELPE,FILEH,IDEVH,MAXGEL,IDM,PERCD,IDEVN,LLINO)
      CHARACTER FILEH*(*)
C   AUTHOR: RODGER STADEN
      INTEGER RELPG(IDBSIZ),ANS,CHRSIZ
      INTEGER LREG,RREG,X,Y,TEMP(CHRSIZ,MAXGL2)
      CHARACTER SEQ1(IDIM1)
      INTEGER LNGTHG(IDBSIZ),LNBR(IDBSIZ),RNBR(IDBSIZ)
      CHARACTER GEL(MAXGEL)
      CHARACTER NAMPRO*(*)
      CHARACTER NAMCON*(*)
100   CONTINUE
      ISTART=1
      NAMCON = ' '
      CALL OPENF1(IDEV,NAMCON,1,IOK,KBIN,KBOUT,
     +'Name for consensus file',
     +IHELPS,IHELPE,FILEH,IDEVH)
      IF(IOK.NE.0)RETURN
      CALL YESNO(ANS,'Make consensus for whole database',
     +IHELPS,IHELPE,FILEH,IDEVH,KBIN,KBOUT)
      IF(ANS.LT.0) RETURN
      IF(ANS.EQ.1)GO TO 150
      N=IDBSIZ-NCONTS
      CALL BUSY(KBOUT)
      DO 110 I=N,IDBSIZ-1
        J=LNBR(I)
        X=1
        Y=RELPG(I)
        IF((ISTART+19+Y).GT.IDIM1)THEN
          WRITE(KBOUT,1009)IDIM1
1009      FORMAT(
     +    ' Maximum consensus length(',I6,') exceeded,',/,
     +    ' calculation aborted')
          RETURN
        END IF
        CALL ADDTIT(SEQ1(ISTART),NAMPRO,J,ISTART)
        CALL SUMMER(RELPG,LNGTHG,LNBR,RNBR,NGELS,NCONTS,
     +  SEQ1(ISTART),Y,GEL,X,Y,J,IDBSIZ,TEMP,CHRSIZ,MAXGL2,
     +  IDEVW,MAXGEL,IDM,PERCD)
        ISTART=ISTART+Y
110   CONTINUE
      ISTART=ISTART-1
      CALL YESNO(ANS,'Staden format',
     +IHELPS,IHELPE,FILEH,IDEVH,KBIN,KBOUT)
      IF (ANS.LT.0) RETURN
      IF(ANS.EQ.0) THEN
        CALL FMTDK(IDEV,SEQ1,ISTART)
      ELSE
        CALL WRITCF(IDEV,SEQ1,ISTART,NAMPRO,KBOUT,IOK)
      END IF
      RETURN
150   CONTINUE
      CALL GETLN3(RELPG,LNGTHG,LNBR,RNBR,NGELS,NCONTS,
     +LINCON,LLINO,NULGEL,IERR,IDBSIZ,KBIN,KBOUT,IDEVN,
     +'Contig identifier',
     +IHELPS,IHELPE,FILEH,IDEVH)
      IF(IERR.NE.0)GO TO 400
      CALL GETREG(RELPG,LNGTHG,LNBR,RNBR,NGELS,NCONTS,
     +1,RELPG(LINCON),LREG,RREG,LINCON,LLINO,IDBSIZ,KBIN,KBOUT,
     +  IHELPS,IHELPE,FILEH,IDEVH,IERR)
      IF(IERR.NE.0)GO TO 400
      IDIM2=RREG-LREG+1
      IF((ISTART+19+IDIM2).GT.IDIM1)THEN
         WRITE(KBOUT,1009)IDIM1
         RETURN
      END IF
      CALL BUSY(KBOUT)
      CALL ADDTIT(SEQ1(ISTART),NAMPRO,LLINO,ISTART)
      CALL SUMMER(RELPG,LNGTHG,LNBR,RNBR,NGELS,NCONTS,
     +SEQ1(ISTART),IDIM2,GEL,LREG,RREG,LLINO,IDBSIZ,TEMP,
     +CHRSIZ,MAXGL2,IDEVW,MAXGEL,IDM,PERCD)
      ISTART=ISTART+IDIM2
300   CONTINUE
      CALL YESNO(ANS,'Select another contig',
     +IHELPS,IHELPE,FILEH,IDEVH,KBIN,KBOUT)
      IF(ANS.EQ.0) GO TO 150
      ISTART=ISTART-1
      CALL YESNO(ANS,'Staden format',
     +IHELPS,IHELPE,FILEH,IDEVH,KBIN,KBOUT)
      IF (ANS.LT.0) RETURN
      IF(ANS.EQ.0) THEN
        CALL FMTDK(IDEV,SEQ1,ISTART)
      ELSE
        CALL WRITCF(IDEV,SEQ1,ISTART,NAMPRO,KBOUT,IOK)
      END IF
400   CONTINUE
      CALL YESNO(ANS,'Make another consensus',
     +IHELPS,IHELPE,FILEH,IDEVH,KBIN,KBOUT)
      IF(ANS.EQ.0)GO TO 100
      END
      SUBROUTINE WRITCF(IDEV,SEQ,IDSEQ,NAMPRO,KBOUT,IOK)
      CHARACTER SEQ(IDSEQ),TITLE*10,ENAME*10,NAMPRO*(*)
      CHARACTER NL
      PARAMETER (MAXDG = 5)
      EXTERNAL INDEXA
C
C write out in fasta format
C also change -,* to N
C currently name is project name for single contig, but left gel number
C for multiple contigs, title is always left gel number
C
      NC = 0
      DO 1 I=1,IDSEQ
        IF (SEQ(I).EQ.'>') NC = NC + 1
 1    CONTINUE
      NL = CHAR(10)
      ENAME = ' '
      I = INDEX(NAMPRO,'.')
      ENAME(1:I-1) = NAMPRO(1:I-1)
      IAT = 1
 10   CONTINUE
      IF (IAT.GT.IDSEQ) THEN
        CLOSE(UNIT=IDEV)
        IOK =0
        RETURN
      END IF
      IF(SEQ(IAT).NE.'<') THEN
        CALL ERROM(KBOUT,'Missing header in consensus')
        IOK = 1
        RETURN
      END IF
      INAMES = INDEXA(SEQ(IAT),20,'.')
      IF (INAMES.EQ.0) THEN
        CALL ERROM(KBOUT,'Missing dot in header')
        IOK = 1
        RETURN
      END IF
      INAMES = IAT + INAMES
      INAMEE = INAMES + MAXDG - 1
      TITLE = ' '
      K = 0
      DO 20 I=INAMES,INAMEE
        K = K + 1
        TITLE(K:K) = SEQ(I)
 20     CONTINUE
      IF (NC.GT.1) ENAME = TITLE
      IAT = IAT + 20
      IDT = IDSEQ-IAT+2
      IDSQ = INDEXA(SEQ(IAT),IDT,'<')
      IF (IDSQ.EQ.0) IDSQ = IDT
      IDSQ = IDSQ - 1
      CALL SETCCS(SEQ(IAT),IDSQ)
      CALL WRITFF(IDEV,SEQ(IAT),IDSQ,ENAME,TITLE)
      IAT = IAT + IDSQ
      GO TO 10
      END
      SUBROUTINE SETCCS(SEQ,IDSEQ)
      CHARACTER SEQ(IDSEQ),TO(5)
      INTEGER CTONUM
      EXTERNAL CTONUM
      SAVE TO
      DATA TO/'t','c','a','g','n'/
C
C change chars in array seq of type found to type to
C
      DO 10 I=1,IDSEQ
        K = CTONUM(SEQ(I))
        SEQ(I) = TO(K)
 10     CONTINUE
      END
      SUBROUTINE COPYM(JLEFTS,ILEFTS,JLC,ILC,
     +JPOSC,IPOSC,JSENSE,ISENSE,JLLINO,LLINO,
     +JJOINT,JOINT,JTOTPC,ITOTPC,JTOTPG,ITOTPG,
     +JTYPE,ITYPE,JDOUT,IDOUT,JDIM22,IDIM22,
     +SEQG3,SEQG2,SEQC3,SEQC2,PERMS,PERMIS)
      CHARACTER SEQG3(JDIM22),SEQG2(JDIM22),SEQC3(JDOUT),SEQC2(JDOUT)
      ILEFTS = JLEFTS
      ILC = JLC
      IPOSC = JPOSC
      ISENSE = JSENSE
      LLINO = JLLINO
      JOINT = JJOINT
      ITOTPC = JTOTPC
      ITOTPG = JTOTPG
      ITYPE = JTYPE
      IDOUT = JDOUT
      IDIM22 = JDIM22
      CALL SQCOPY(SEQG3,SEQG2,JDIM22)
      CALL SQCOPY(SEQC3,SEQC2,JDOUT)
      PERMIS = PERMS
      END
C     SUBROUTINE DALIGN
C
C   COUNTS MISMATCHES AND DISPLAYS OVERLAP.
      SUBROUTINE DALIGN(SEQC2,SEQG2,SEQ3,MAXGEL,IDOUT,IDIM2,
     +JOINT,ITYPE,X,KBOUT,IFAIL,LO,PERMAX,ISHOW)
C   AUTHOR: RODGER STADEN
      CHARACTER SEQC2(MAXGEL),SEQG2(MAXGEL),SEQ3(MAXGEL)
      CHARACTER PAD,DASH
      SAVE PAD,DASH
      DATA PAD,DASH/',','-'/
      IFAIL = 1
      IENDG=1
      IENDC=JOINT
C   ONLY LOOK AT OVERLAP WHICH IS FROM JOINT FOR LEFT TYPE JOIN
      IF(ITYPE.EQ.1)THEN
        IENDG=JOINT
        IENDC=1
      END IF
100   CONTINUE
C   LENGTH OF OVERLAP?
      LG=IDIM2-IENDG+1
      LO=MIN(IDOUT,LG)
C   SAVE RAW DATA
      CALL SQCOPY(SEQG2,SEQ3,IDIM2)
      CALL MSTLKL(SEQ3,IDIM2)
      X=FLOAT(LO)
      Y=X
      K=IENDG+LO-1
C   POINT TO CONSENSUS
      J=0
C   CHECK FOR OVERFLOW
      IF(K.GT.MAXGEL)THEN
        CALL ERROM(KBOUT,'DALIGN: matching region too long')
        RETURN
      END IF
      DO 200 I=IENDG,K
        J=J+1
        IF(SEQC2(J).EQ.SEQ3(I))GO TO 200
C        IF(SEQ3(I).EQ.DASH)GO TO 200
C        IF(SEQC2(J).EQ.DASH)GO TO 200
C        IF(SEQC2(J).EQ.PAD)GO TO 200
        X=X-1.
200   CONTINUE
      X=(Y-X)*100./Y
      IF (X.GT.PERMAX) RETURN
      IF (ISHOW.EQ.1) THEN
        WRITE(KBOUT,1002)
1002    FORMAT(' Best alignment found')
        CALL SQMTCH(SEQC2(1),SEQG2(IENDG),SEQ3,LO)
        CALL FMT4LN(SEQC2(1),SEQG2(IENDG),SEQ3,LO,IENDC,IENDG,KBOUT)
      END IF
      IFAIL=0
      END
      SUBROUTINE DBCHEK(RELPG,LNGTHG,LNBR,RNBR,NGELS,NCONTS,IDBSIZ,
     +TEMP,IERR,KBOUT)
C   AUTHOR: RODGER STADEN
      INTEGER RELPG(IDBSIZ)
      INTEGER LNGTHG(IDBSIZ),LNBR(IDBSIZ),RNBR(IDBSIZ)
      INTEGER TEMP(IDBSIZ)
      EXTERNAL LCCHEK,NCCHEK
C to check the logical consistency of a database
C
C 1. are all nbrs holding hands
C 2. are all gels in exactly 1 contig
C 3. are there loops in contigs
C 4. do the gels designated left or right ends have outward neighbours
C 5. are the relative positions in same order as hand holding
C 6. are there gels of zero length
C 7. are there contigs of length < 1
C 8. does the designated length of the contigs agree with the gel positions
C 9. if i chain left thru a contig do i reach the gel designated as the left end
C10.if i chain right thru a contig do i reach the gel designated as the right end
C
C return error code 2 for all errors except where only error is "gel not used"
C for which we return 1
      IERR=0
C   hand holding OK?
      DO 100 I=1,NGELS
        K=LNBR(I)
        IF(K.EQ.0)GO TO 50
        IF(RNBR(K).EQ.I)GO TO 50
        WRITE(KBOUT,1000)I
1000    FORMAT(' Hand holding problem for gel reading',I6)
        WRITE(KBOUT,1001)I,RELPG(I),LNGTHG(I),LNBR(I),RNBR(I)
        WRITE(KBOUT,1001)K,RELPG(K),LNGTHG(K),LNBR(K),RNBR(K)
1001    FORMAT(' ',5I6)
        IERR=2
50      CONTINUE
        K=RNBR(I)
        IF(K.EQ.0)GO TO 100
        IF(LNBR(K).EQ.I)GO TO 100
        WRITE(KBOUT,1000)I
        WRITE(KBOUT,1001)I,RELPG(I),LNGTHG(I),LNBR(I),RNBR(I)
        WRITE(KBOUT,1001)K,RELPG(K),LNGTHG(K),LNBR(K),RNBR(K)
        IERR=2
100   CONTINUE
C
C   are all gels in exactly 1 contig
C
      CALL FILLI(TEMP,IDBSIZ,0)
      N=IDBSIZ-NCONTS
C
C count the number of times thru loop 320 to 300: if this exceeds the
C database size a loop has been found
C
      ICOUNT=0
      DO 300 I=N,IDBSIZ-1
        II=I
        K=LNBR(I)
        J=RNBR(I)
        IF((K.NE.0).AND.(J.NE.0))GO TO 310
C
C This contig points to zero gel number as a left or right end
C
        WRITE(KBOUT,1002)I
1002    FORMAT(' Contig',I4,' has gel numbers of zero')
        IERR=2
        GO TO 290
310     CONTINUE
        IF((LNBR(K).EQ.0).AND.(RNBR(J).EQ.0))GO TO 290
C
C These ends reads have outward neighbours
C
        WRITE(KBOUT,1004)I
1004  FORMAT(' The end gels of contig',I4,' have outward neighbours')
        IERR=2
290     CONTINUE
C
C Does the contig have nonzero length?
C
        IF(RELPG(I).GT.0)GO TO 320
        WRITE(KBOUT,1010)I
1010    FORMAT(' The contig on line number',I4,' has zero length')
        IERR=2
320     CONTINUE
        TEMP(K)=TEMP(K)+1
        ICOUNT=ICOUNT+1
        IF(ICOUNT.GT.IDBSIZ)GO TO 601
        K=RNBR(K)
        IF(K.NE.0)GO TO 320
300   CONTINUE
      DO 400 I=1,NGELS
        IF(TEMP(I).EQ.1)GO TO 390
        IF(TEMP(I).EQ.0)GO TO 410
        WRITE(KBOUT,1005)I,TEMP(I)
1005    FORMAT(' Gel number ',I6,' is used ',I6,' times')
        IERR=2
        GO TO 400
390     CONTINUE
C
C does the gel have nonzero length (only check those used once)
C
        IF(LNGTHG(I).NE.0)GO TO 400
        WRITE(KBOUT,1011)I
1011    FORMAT(' Gel number',I6,' has zero length')
        IERR=2
        GO TO 400
410     CONTINUE
        WRITE(KBOUT,1006)I
1006    FORMAT(' Gel number ',I6,' is not used')
C
C need to increase the error count (dont reset to lower value)
C
        IF(IERR.LT.2)IERR=1
400   CONTINUE
C
C all relative positions ok?
C
      N=IDBSIZ-NCONTS
      DO 500 I=N,IDBSIZ-1
        K=LNBR(I)
        IF(K.EQ.0)GO TO 500
510     CONTINUE
        J=RNBR(K)
        IF(J.EQ.0)GO TO 500
        IF(RELPG(K).GT.RELPG(J))GO TO 520
        K=J
        GO TO 510
520     CONTINUE
        WRITE(KBOUT,1007)K,RELPG(K),J,RELPG(J)
1007    FORMAT(' Gel number',I6,' with position',I6,
     +  ' is the left neighbour of',
     +  /,' gel number',I6,' with position',I6)
        K=J
        IERR=2
        GO TO 510
500   CONTINUE
      IOK = LCCHEK(RELPG,LNGTHG,LNBR,RNBR,NCONTS,IDBSIZ,KBOUT)
      IF (IOK.NE.0) IERR = 2
      IOK = NCCHEK(RELPG,LNGTHG,LNBR,RNBR,NCONTS,IDBSIZ,KBOUT)
      IF (IOK.NE.0) IERR = 2
      IF(IERR.EQ.0) WRITE(KBOUT,1013)
1013  FORMAT(' Database is logically consistent')
      RETURN
601   CONTINUE
      IERR=2
      WRITE(KBOUT,1008)II
1008  FORMAT(' Loop in contig',I6,/,
     +' No further checking done but gel numbers follow')
      CALL FILLI(TEMP,IDBSIZ,0)
      K=LNBR(II)
710   CONTINUE
      TEMP(K)=TEMP(K)+1
      WRITE(KBOUT,1009)K
1009  FORMAT(' ',I6)
      IF(TEMP(K).GT.1)RETURN
      K=RNBR(K)
      GO TO 710
      END
      INTEGER FUNCTION LCCHEK(RELPG,LNGTHG,LNBR,RNBR,NCONTS,IDBSIZ,
     +KBOUT)
C   AUTHOR: RODGER STADEN
      INTEGER RELPG(IDBSIZ),CLEN
      INTEGER LNGTHG(IDBSIZ),LNBR(IDBSIZ),RNBR(IDBSIZ)
      EXTERNAL CLEN
      LCCHEK = 0
      DO 10 I=IDBSIZ-NCONTS,IDBSIZ-1
        IL = LNBR(I)
        L1 = CLEN(RELPG,LNGTHG,LNBR,RNBR,NGELS,NCONTS,
     +  IDBSIZ,IL)
        L2 = RELPG(I)
        IF (L1.NE.L2) THEN
          WRITE(KBOUT,1000)I,L2,L1
 1000     FORMAT(
     +' Contig line',I6,' records length',I6,' but actual length is',I6)
        LCCHEK = LCCHEK + 1
        END IF
 10   CONTINUE
      END
      INTEGER FUNCTION NCCHEK(RELPG,LNGTHG,LNBR,RNBR,NCONTS,IDBSIZ,
     +KBOUT)
C   AUTHOR: RODGER STADEN
      INTEGER RELPG(IDBSIZ),CHAINL,CHAINR
      INTEGER LNGTHG(IDBSIZ),LNBR(IDBSIZ),RNBR(IDBSIZ)
      EXTERNAL CHAINL,CHAINR
      NCCHEK = 0
      DO 10 I=IDBSIZ-NCONTS,IDBSIZ-1
        IL = LNBR(I)
        L1 = CHAINR(RELPG,LNGTHG,LNBR,RNBR,NGELS,NCONTS,
     +  IDBSIZ,IL)
        L2 = RNBR(I)
        IF (L1.NE.L2) THEN
          WRITE(KBOUT,1000)I,L2,L1
 1000     FORMAT(
     +' Contig line',I6,' records right neighbour as',I6,
     +' but left to right chaining gives',I6)
        NCCHEK = NCCHEK + 1
        END IF
        IL = RNBR(I)
        L1 = CHAINL(RELPG,LNGTHG,LNBR,RNBR,NGELS,NCONTS,
     +  IDBSIZ,IL)
        L2 = LNBR(I)
        IF (L1.NE.L2) THEN
          WRITE(KBOUT,1001)I,L2,L1
 1001     FORMAT(
     +' Contig line',I6,' records left neighbour as',I6,
     +' but right to left chaining gives',I6)
        NCCHEK = NCCHEK + 1
        END IF
 10   CONTINUE
      END
      INTEGER FUNCTION CHAINR(RELPG,LNGTHG,LNBR,RNBR,NGELS,NCONTS,
     +IDBSIZ,IIN)
C  AUTHOR: RODGER STADEN
C  RETURNS CONTIG RIGHT GEL NUMBER OR ZERO FOR ERROR
      INTEGER RELPG(IDBSIZ)
      INTEGER LNGTHG(IDBSIZ),LNBR(IDBSIZ),RNBR(IDBSIZ)
      I = IIN
      J = I
      CHAINR = 0
10    CONTINUE
      IF(I.NE.0)THEN
        J = I
        I = RNBR(I)
        IF(I.EQ.IIN)RETURN
        GO TO 10
      END IF
      CHAINR = J
      END
C   DBPRNT
C   PRINTS A DATABASE. IE ITS RELATIONSHIPS
      SUBROUTINE DBPRNT(RELPG,LNGTHG,LNBR,RNBR,NGELS,NCONTS,IDBSIZ,
     +IDEV,KBIN,KBOUT,IDEVN,LLINO,
     +IHELPS,IHELPE,FILEH,IDEVH)
      CHARACTER FILEH*(*)
C   AUTHOR: RODGER STADEN
      INTEGER RELPG(IDBSIZ),LREG,RREG,ANS
      INTEGER LNGTHG(IDBSIZ),LNBR(IDBSIZ),RNBR(IDBSIZ)
      CHARACTER NAMARC*16
      IF(NGELS.GT.0)CALL DBSTAT(RELPG,LNGTHG,LNBR,RNBR,NGELS,
     +NCONTS,IDBSIZ,IDEV)
      WRITE(IDEV,10011)NGELS,NCONTS
10011 FORMAT(' Number of gel readings ',I6,' Number of contigs ',I6)
20    CONTINUE
      CALL YESNO(ANS,'Select contigs',
     +IHELPS,IHELPE,FILEH,IDEVH,KBIN,KBOUT)
      IF(ANS.LT.0) RETURN
      IF(ANS.EQ.0) GO TO 45
      N=IDBSIZ-NCONTS
25    CONTINUE
      CALL YESNO(ANS,'Show gel readings in positional order',
     +IHELPS,IHELPE,FILEH,IDEVH,KBIN,KBOUT)
      IF(ANS.LT.0) RETURN
      IF(ANS.EQ.0)GO TO 41
      WRITE(IDEV,1009)
1009  FORMAT(' CONTIG LINES')
      WRITE(IDEV,1000)
1000  FORMAT(
     +' CONTIG              LINE  LENGTH               ENDS'/
     +'                                             LEFT   RIGHT')
      DO 30 I=N,IDBSIZ-1
        WRITE(IDEV,1007)I,RELPG(I),LNBR(I),RNBR(I)
30    CONTINUE
1007  FORMAT( ' ',18X,I6,2X,I7,9X,I6,2X,I6)
      WRITE(IDEV,1008)
1008  FORMAT(' GEL LINES')
      WRITE(IDEV,1001)
 1001 FORMAT(
     +' NAME              NUMBER POSITION LENGTH     NEIGHBOURS'/
     +'                                             LEFT   RIGHT')
      DO 40 I=1,NGELS
        CALL READN(IDEVN,I,NAMARC)
        WRITE(IDEV,1006)NAMARC,I,RELPG(I),LNGTHG(I),LNBR(I),RNBR(I)
1006    FORMAT( ' ',A,2X,I6,2X,I7,2X,I5,2X,I6,2X,I6)
40    CONTINUE
      RETURN
C
41    CONTINUE
C
C   SORTED DATA
      DO 43 I=N,IDBSIZ-1
        WRITE(IDEV,1021)
1021    FORMAT( )
        WRITE(IDEV,1000)
        WRITE(IDEV,1007)I,RELPG(I),LNBR(I),RNBR(I)
        J=LNBR(I)
        WRITE(IDEV,1001)
42      CONTINUE
        CALL READN(IDEVN,J,NAMARC)
        WRITE(IDEV,1006)NAMARC,J,RELPG(J),LNGTHG(J),LNBR(J),RNBR(J)
        J=RNBR(J)
        IF(J.NE.0)GO TO 42
43    CONTINUE
      RETURN
45    CONTINUE
C   SELECTED CONTIGS ONLY
C
C   GET GEL NUMBER AND CONTIG NUMBER
      CALL GETLN3(RELPG,LNGTHG,LNBR,RNBR,NGELS,NCONTS,LINCON,
     +LLINO,NULGEL,IERR,IDBSIZ,KBIN,KBOUT,IDEVN,
     +'Contig identifier',
     +IHELPS,IHELPE,FILEH,IDEVH)
      IF(IERR.NE.0)RETURN
      WRITE(IDEV,1009)
      WRITE(IDEV,1000)
      WRITE(IDEV,1007)LINCON,RELPG(LINCON),LNBR(LINCON),RNBR(LINCON)
      CALL GETREG(RELPG,LNGTHG,LNBR,RNBR,NGELS,NCONTS,
     +1,RELPG(LINCON),LREG,RREG,LINCON,LLINO,IDBSIZ,KBIN,KBOUT,
     +  IHELPS,IHELPE,FILEH,IDEVH,IERR)
      IF(IERR.NE.0)RETURN
      WRITE(IDEV,1008)
      N=LLINO
      WRITE(IDEV,1001)
46    CONTINUE
      CALL READN(IDEVN,N,NAMARC)
      WRITE(IDEV,1006)NAMARC,N,RELPG(N),LNGTHG(N),LNBR(N),RNBR(N)
      IF(RNBR(N).EQ.0)GO TO 48
      N=RNBR(N)
      IF(RELPG(N).GT.RREG)GO TO 48
      GO TO 46
48    CONTINUE
      GO TO 45
      END
      SUBROUTINE DBSCAN(RELPG,LNGTHG,LNBR,RNBR,NGELS,NCONTS,SEQ1,
     +IDIM1,GEL,IDBSIZ,TEMP3,ID1,CHRSIZ,MAXGL2,KBIN,KBOUT,IDEVW,
     +IDEV,LINLEN,PERCD,
     +IHELPS,IHELPE,FILEH,IDEVH,MAXGEL,LINOU1,LINOU2,
     +MARGL,MARGR,MARGB,MARGT,ISXMAX,ISYMAX,IDEVN,
     +  LLINO,LINCON,LREG,RREG,MXGOOD)
C 28-7-91 added extra parameter mxgood: the max length of read
C we have confidence in
      CHARACTER FILEH*(*)
      PARAMETER (MAXPRM = 10)
      CHARACTER PROMPT(2)*(MAXPRM)
C   AUTHOR: RODGER STADEN
      INTEGER RREG, RELPG(IDBSIZ),CHRSIZ
      INTEGER LREG,TEMP3(ID1,CHRSIZ,MAXGL2),ANS
      INTEGER LNGTHG(IDBSIZ),LNBR(IDBSIZ),RNBR(IDBSIZ)
      CHARACTER GEL(MAXGEL),LINOU1(MAXGEL),LINOU2(MAXGEL)
      CHARACTER SEQ1(IDIM1)
      CALL GETLN3(RELPG,LNGTHG,LNBR,RNBR,NGELS,NCONTS,
     +LINCON,LLINO,NULGEL,IERR,IDBSIZ,KBIN,KBOUT,IDEVN,
     +'Contig identifier',
     +IHELPS,IHELPE,FILEH,IDEVH)
      IF(IERR.NE.0) RETURN
      CALL GETREG(RELPG,LNGTHG,LNBR,RNBR,NGELS,NCONTS,
     +1,RELPG(LINCON),LREG,RREG,LINCON,LLINO,IDBSIZ,KBIN,KBOUT,
     +  IHELPS,IHELPE,FILEH,IDEVH,IERR)
      IF(IERR.NE.0) RETURN
      IDIM2=RREG-LREG+1
      CALL SUMMAR(RELPG,LNGTHG,LNBR,RNBR,NGELS,NCONTS,
     +SEQ1,IDIM1,GEL,LREG,RREG,LLINO,PERCD,IDBSIZ,
     +TEMP3,ID1,CHRSIZ,MAXGL2,IDEVW,
     +MAXGEL,LINOU1,LINOU2,MXGOOD)
      CALL DBSCSM(SEQ1(LREG),IDIM2,KBOUT)
160   CONTINUE
      ANS = 1
      PROMPT(1) = 'List codes'
      PROMPT(2) = 'Plot codes'
      CALL RADION('Select results display mode',PROMPT,2,ANS,
     +  IHELPS,IHELPE,HELPF,IDEVH,KBIN,KBOUT)
      IF(ANS.LT.1) RETURN
      IF(ANS.EQ.1) THEN
        CALL FMTDB(SEQ1,IDIM1,LREG,RREG,LINLEN,IDEV)
        RETURN
      ELSE
        CALL PLTQ(SEQ1(LREG),IDIM2,
     +  MARGL,MARGR,MARGB,MARGT,ISXMAX,ISYMAX)
      END IF
      END
      SUBROUTINE DBSCNP(RELPG,LNGTHG,LNBR,RNBR,NGELS,NCONTS,SEQ1,
     +IDIM1,GEL,IDBSIZ,TEMP3,ID1,CHRSIZ,MAXGL2,IDEVW,LLINO,
     +PERCD,MAXGEL,LINOU1,LINOU2,LREG,RREG,
     +MARGL,MARGR,MARGB,MARGT,ISXMAX,ISYMAX,MXGOOD)
C   AUTHOR: RODGER STADEN
      INTEGER RREG, RELPG(IDBSIZ),CHRSIZ
      INTEGER LREG,TEMP3(ID1,CHRSIZ,MAXGL2)
      INTEGER LNGTHG(IDBSIZ),LNBR(IDBSIZ),RNBR(IDBSIZ)
      CHARACTER GEL(MAXGEL),LINOU1(MAXGEL),LINOU2(MAXGEL)
      CHARACTER SEQ1(IDIM1)
      IDIM2=RREG-LREG+1
C 28-7-91 added extra parameter mxgood: the max length of read
C we have confidence in
      CALL SUMMAR(RELPG,LNGTHG,LNBR,RNBR,NGELS,NCONTS,
     +SEQ1,IDIM1,GEL,LREG,RREG,LLINO,PERCD,IDBSIZ,
     +TEMP3,ID1,CHRSIZ,MAXGL2,IDEVW,
     +MAXGEL,LINOU1,LINOU2,MXGOOD)
      CALL PLTQ(SEQ1(LREG),IDIM2,
     +MARGL,MARGR,MARGB,MARGT,ISXMAX,ISYMAX)
      END
      SUBROUTINE DBSCSM(SEQ1,IDIM1,KBOUT)
C   AUTHOR: RODGER STADEN
      CHARACTER SEQ1(IDIM1)
      CHARACTER CODES(5)
      REAL X(5)
      SAVE CODES
      DATA CODES/'0','1','2','3','4'/
      DO 50 J=1,5
        X(J)=0.
50    CONTINUE
      DO 100 I=1,IDIM1
        DO 60 J=1,5
          IF(SEQ1(I).NE.CODES(J))GO TO 60
          X(J)=X(J)+1.
          GO TO 61
60      CONTINUE
61      CONTINUE
100   CONTINUE
      SUM=0.
      DO 130 J=1,5
        SUM=SUM+X(J)
130   CONTINUE
      DO 140 J=1,5
        IF(SUM.NE.0)X(J)=X(J)*100./SUM
140   CONTINUE
      WRITE(KBOUT,1001)X(1)
1001  FORMAT(' ',F6.2,'% OK on both strands and they agree(0)')
      WRITE(KBOUT,1002)X(2)
1002  FORMAT(' ',F6.2,'% OK on plus strand only(1)')
      WRITE(KBOUT,1003)X(3)
1003  FORMAT(' ',F6.2,'% OK on minus strand only(2)')
      WRITE(KBOUT,1004)X(4)
1004  FORMAT(' ',F6.2,'% Bad on both strands(3)')
      WRITE(KBOUT,1005)X(5)
1005  FORMAT(' ',F6.2,'% OK on both strands but they disagree(4)')
      RETURN
      END
C     DBSTAT
      SUBROUTINE DBSTAT(RELPG,LNGTHG,LNBR,RNBR,NGELS,NCONTS,IDBSIZ,
     +KBOUT)
C   AUTHOR: RODGER STADEN
      INTEGER RELPG(IDBSIZ)
      INTEGER LNGTHG(IDBSIZ),LNBR(IDBSIZ),RNBR(IDBSIZ)
      N=IDBSIZ-NCONTS
      SUM=0.
      DO 20 I=N,IDBSIZ-1
        SUM=SUM+RELPG(I)
20    CONTINUE
      AV=SUM/NCONTS
      WRITE(KBOUT,1020)SUM,AV
1020  FORMAT( ' Total contig length ',F10.0,'  Average',
     +' length ',F10.1)
      SUMG=0.
      DO 30 I=1,NGELS
        SUMG=SUMG+FLOAT(ABS(LNGTHG(I)))
30    CONTINUE
      AV=SUMG/SUM
      WRITE(KBOUT,1021)SUMG
1021  FORMAT( ' Total characters in gel readings ',F10.0)
      WRITE(KBOUT,1022)AV
1022  FORMAT
     +( ' Average gel characters per consensus character ',F10.2)
99    CONTINUE
      RETURN
      END
C     DELCON
C
C   DELETES CONTIG FROM CONSENSUS SEQUENCE
      SUBROUTINE DELCON(SEQ1,ILEFT,ILC,IDIM1)
C   AUTHOR: RODGER STADEN
      CHARACTER SEQ1(IDIM1)
C   FIRST CHAR TO REPLACE
      I1=ILEFT-20
C   FIRST CHAR TO MOVE
      I2=ILEFT+ILC
C   IS THIS RIGHTMOST CONTIG ANYWAY?
      IF(I2.GT.IDIM1)GO TO 10
C   NUMBER TO MOVE
      ID=IDIM1-I2+1
C   MOVE
      CALL SQCOPY(SEQ1(I2),SEQ1(I1),ID)
C   RESET LENGTH
      IDIM1=I1+ID-1
      RETURN
10    CONTINUE
C   RIGHTMOST CONTIG SO DONT MOVE
      IDIM1=I1-1
C
      RETURN
      END
      SUBROUTINE DISMAT(SEQ,IDIM,GEL,IDIMG,SAVPS,SAVPG,IDSAV,
     +CENDS,NENDS,IDCEND,MAXCON,KBOUT,MATCH)
C   AUTHOR: RODGER STADEN
      INTEGER CENDS(MAXCON)
      INTEGER NENDS(MAXCON)
      INTEGER SAVPS(IDSAV),SAVPG(IDSAV)
      CHARACTER SEQ(IDIM),GEL(IDIMG),MATCH(IDIMG)
C   EDIT 07-02-83 TO CHECK FOR CASE WHEN GEL OVERLAPS ADJACENT
C   CONTIGS WITHIN THE LENGTH OF THE GEL!  DONE BY HAVING A
C   PARAMETER THAT STORES THE POSITION OF THE LEFT END OF THE
C   NEXT CONTIG (IE THE ONE AFTER THE ONE THE CURRENT GEL OVERLAPS)
C   SET IT TO A VERY LARGE VALUE INITIALLY
      NEXTC=99999
C   SORT THE MATCHING WORDS INTO ASCENDING ORDER ON POSITION IN SEQ
      CALL BUB2AS(SAVPS,SAVPG,IDSAV)
C   LOOK FOR SEPARATE MATCHES
      LEND=IDIMG-SAVPG(1)+SAVPS(1)
C      WRITE(KBOUT,1000)SAVPG(1),SAVPS(1)
      CALL DISMAU(SEQ,IDIM,GEL,IDIMG,SAVPS(1),
     +SAVPG(1),CENDS,NENDS,IDCEND,MAXCON,
     +NEXTC,KBOUT,MATCH)
      DO 10 I=2,IDSAV
      IF((SAVPS(I).LT.LEND).AND.(SAVPS(I).LT.NEXTC))GO TO 10
C   NEW MATCH, DISPLAY IT
C      WRITE(KBOUT,1000)SAVPG(I),SAVPS(I)
C1000  FORMAT(' ',2I6)
      CALL DISMAU(SEQ,IDIM,GEL,IDIMG,SAVPS(I),
     +SAVPG(I),CENDS,NENDS,IDCEND,MAXCON,
     +NEXTC,KBOUT,MATCH)
C   RESET LEND
      LEND=IDIMG-SAVPG(I)+SAVPS(I)
10    CONTINUE
      RETURN
      END
C
C       DISMAU
C   ROUTINE TO DISPLAY MATCHES
C   EDITED 17-12-81 TO NOT SUBTRACT 1 FROM LCL AND LGR
      SUBROUTINE DISMAU(SEQ,IDIM1,GEL,IDIMG,ISAVPS,SAVPG,CENDS,NENDS,
     +IDCEND,MAXCON,NEXTC,KBOUT,MATCH)
C   AUTHOR: RODGER STADEN
      CHARACTER SEQ(IDIM1),GEL(IDIMG),MATCH(IDIMG)
      INTEGER SAVPS,SAVPG,CENDS(MAXCON)
      INTEGER NENDS(MAXCON)
C   EDITED 07-02-83 FOR NEXTC (SEE DISMAT)
C   DELETE 20 FROM END OF CONSENSUS MATCH
      SAVPS=ISAVPS-19
C   FIND CONTIG CONSENSUS ENDS
      JJ=1
      DO 5 J=2,IDCEND
      IF(SAVPS.GT.CENDS(J))GO TO 5
C   GONE PAST SO LAST IS THE ONE
      JJ=J-1
      GO TO 6
5     CONTINUE
      JJ=IDCEND
6     CONTINUE
C   SUBTRACT 1 FROM END
      SAVPS=SAVPS-1
C   LENGTH FROM MATCH TO LEFT OF CONTIG
      LCL=SAVPS-CENDS(JJ)
C   RIGHT
      LCR=CENDS(JJ+1)-ISAVPS-1
C   LEFT GEL
      LGL=SAVPG-1
      LGR=IDIMG-SAVPG
C   NEED MIN OF EACH PAIR
      LL=MIN(LCL,LGL)
      LR=MIN(LCR,LGR)
C   LENGTH OF OVERLAP
      LM=LR+LL+1
C  DISPLAY STARTS
      ICL=ISAVPS-LL
      IGL=SAVPG-LL
      WRITE(KBOUT,1000)NENDS(JJ)
1000  FORMAT(' Match found with vector number =',I6)
      CALL SQMTCH(SEQ(ICL),GEL(IGL),MATCH,LM)
      L=ICL-CENDS(JJ)-19
      CALL FMT4LN(SEQ(ICL),GEL(IGL),MATCH,LM,L,IGL,KBOUT)
C   SAVE POSN OF END OF NEXT CONTIG
      NEXTC=CENDS(JJ+1)+20
      RETURN
      END
      SUBROUTINE DSPLAY(RELPG,LNGTHG,LNBR,RNBR,
     +GEL,LLINOO,LINCON,LREG,RREG,GEL2,I1,IDIM,NOPT,
     +LLINOR,IDBSIZ,IDEV,KBOUT,IDEVW,IDEVN,LINLEN,PERCD,
     +MAXGEL,IDM)
C   AUTHOR: RODGER STADEN
      INTEGER CHRSIZ
      PARAMETER (CHRSIZ = 6)
      PARAMETER (IDC1 = CHRSIZ*100)
      INTEGER RELPG(IDBSIZ)
      INTEGER LNGTHG(IDBSIZ),LNBR(IDBSIZ),RNBR(IDBSIZ)
      INTEGER LREG,RREG,X,XLS2,XLS1,XRS2,XRS1,RREG2
      CHARACTER MATCH(100)
      INTEGER CHARS(CHRSIZ,100),CHARS1(IDC1)
      CHARACTER NAMARC*16
      CHARACTER GEL(MAXGEL)
      CHARACTER GEL2(MAXGEL)
      INTEGER RELPOS(10),RELPO2(10)
      INTEGER GELC
      INTEGER RP
      INTEGER LSEQNO,RSEQNO
      CHARACTER LINOUT(100)
      CHARACTER MUNOTP
      CHARACTER GTCONC
      EXTERNAL GTCONC
      EQUIVALENCE (CHARS1,CHARS)
      CALL FILLI(CHARS1,IDC1,0)
C   SET CONTIG NUMBER
      ICON=1
      LLINO=LLINOO
      NLEN=LINLEN/10
      LSEQNO=LREG
      X=LINLEN+LSEQNO-1
      RSEQNO=MIN(RREG,X)
C   SET LEFT GEL NUMBER FOR RIGHT CONTIG
      LN2=LLINOR
C   FIRST GEL NO IS LLINOO
C   SET RREG FOR RIGHT CONTIG
      RREG2=IDIM
C   SET UP LSEQNO,RSEQNO FOR FOR NOPT=3
      XLS2=I1
      XRS2=RSEQNO-LSEQNO+XLS2
9     CONTINUE
C   IF RIGHT CONTIG SKIP NUMBER PRINTING
      IF(ICON.EQ.2)GO TO 8
C   NEED TO KEEP LONGEST LINE LENGTH FOR OUTPUT OF CONSENSUS
      IE=0
C   SETUP AND WRITE NUMBERS
      RELPOS(1)=LSEQNO+9
      DO 5 I=2,NLEN
        RELPOS(I)=RELPOS(I-1)+10
5     CONTINUE
      WRITE(IDEV,1023)
     +(RELPOS(K),K=1,MIN(NLEN,MAX(1,(RSEQNO-LSEQNO+1)/10)))
1023  FORMAT( ' ',25X,10(I9,1X))
C   SET CURRENT LINE NUMBER
8     CONTINUE
      GELC=LLINO
10    CONTINUE
C   IS LEFT END OF CURRENT GEL >RREG
      IF(RELPG(GELC).GT.RSEQNO)GO TO 200
C   ALSO NEED TO KNOW IF RIGHT END  ON THIS LINE (IF .LT. NO DATA
C   TO DISPLAY)
      X=RELPG(GELC)+ABS(LNGTHG(GELC))-1
      IF(X.LT.LSEQNO)GO TO 190
      CALL READW(IDEVW,GELC,GEL,MAXGEL)
      CALL FILLC(LINOUT,LINLEN,' ')
      CALL READN(IDEVN,GELC,NAMARC)
C
C   NEED TO KNOW HOW MANY CHARS TO COPY OVER TO OUTPUT LINE
C   AND WHERE IN LINE TO PUT THEM
C   CURRENT LINE LEFT END IS LSEQNO,RIGHT END RSEQNO
C   SO LEFT START CHAR IS
      X=MAX(LSEQNO,RELPG(GELC))
C   POSITION IN ARRAY LINE
      LP=X-LSEQNO+1
C   RIGHT END CHAR IS
      X=RELPG(GELC)+ABS(LNGTHG(GELC))-1
      X=MIN(RSEQNO,X)
C   POSITION IN ARRAY LINE
      RP=X-LSEQNO+1
C   LOOK FOR LONGEST LINE
      IF(RP.GT.IE)IE=RP
C   NEED LEFT START IN GEL
      K=LSEQNO-RELPG(GELC)+1
      IF(K.LT.1)K=1
      NCOP=RP-LP+1
      IF(NCOP.GT.0)CALL SQCOPY(GEL(K),LINOUT(LP),NCOP)
      N=LP+NCOP-1
      II=K-1
      IF(IDM.EQ.26)THEN
        DO 50 I = LP,N
          II = II + 1
          CALL PCON1(GEL(II),CHARS(1,I))
50      CONTINUE
      ELSE
        DO 70 I=LP,N
          II=II+1
          JJ = INDEXS(GEL(II),JSCORE)
          CHARS(JJ,I) = CHARS(JJ,I) + JSCORE
C          CHARS(CHRSIZ,I) = CHARS(CHRSIZ,I) + JSCORE
70      CONTINUE
      END IF
      I=SIGN(GELC,LNGTHG(GELC))
      WRITE(IDEV,1020)I,NAMARC,(LINOUT(K),K=1,RP)
C1020  FORMAT( ' ',I4,2X,A,2X,100A1)
1020  FORMAT( ' ',I6,1X,A,1X,100A1)
C
190   CONTINUE
C   NOW GET NEXT GEL TO RIGHT
      GELC=RNBR(GELC)
      IF(GELC.NE.0)GO TO 10
200   CONTINUE
C   CALC CONSENSUS AND WRITE IT
      IF(IDM.EQ.26)THEN
        DO 49 I = 1,LINLEN
          LINOUT(I) = MUNOTP(CHARS(1,I))
          CHARS(1,I) = 0
49      CONTINUE
      ELSE
        DO 230 I=1,LINLEN
          LINOUT(I) = GTCONC(CHARS(1,I),CHRSIZ,PERCD)
          CALL FILLI(CHARS(1,I),CHRSIZ,0)
230     CONTINUE
      END IF
      WRITE(IDEV,1019)(LINOUT(K),K=1,IE)
C   IF REQUIRED WRITE COMPARISON GEL
C   WHICH OPTION IN OPERATION?
      IF(NOPT.EQ.2)GO TO 52
      IF(NOPT.NE.3)GO TO 250
53    CONTINUE
C   ALREADY DONE THIS LINE CONTIG2?
      IF(ICON.EQ.2)GO TO 54
      ICON=2
C   NEED TO SAVE CONSENSUS FROM LEFT CONTIG
      CALL SQCOPY(LINOUT,GEL2,IE)
C   SAVE VALUES FROM LEFT CONTIG
      XLS1=LSEQNO
      XRS1=RSEQNO
C   SAVE CURRENT LEFT GEL NUMBER
      LN1=LLINO
C   SET UP VALUES FOR RIGHT CONTIG
      LSEQNO=XLS2
      RSEQNO=XRS2
C   SET LEFT GEL NUMBER
      LLINO=LN2
C   GET NEXT GEL
      GO TO 150
54    CONTINUE
C   SAVE CURRENT LEFT GEL NUMBER
      LN2=LLINO
C   SET VALUES FOR RIGHT CONTIG NEXT PASS
      XLS2=XRS2+1
      XRS2=XLS2+LINLEN-1
      IF(XRS2.GT.RREG2)XRS2=RREG2
C   SET UP VALUES FOR LEFT CONTIG
      LLINO=LN1
      ICON=1
      LSEQNO=XLS1
      RSEQNO=XRS1
C   SET DECREMENT FOR POINTER TO GEL2
      MMM=I1-1
52    CONTINUE
C1020  FORMAT( ' ',I4,2X,A,2X,100A1)
1017  FORMAT('        NEWGEL           ',100A1)
1018  FORMAT('        MISMATCH         ',100A1)
1019  FORMAT('        CONSENSUS        ',100A1)
1022  FORMAT( ' ',26X,100A1)
      I2=I1+LINLEN-1
      IF(I2.GT.IDIM)I2=IDIM
      IF(NOPT.EQ.2)WRITE(IDEV,1017)(GEL2(K),K=I1,I2)
C   SET DECREMENT
      IF(NOPT.EQ.2)MMM=0
55    CONTINUE
      CALL FILLC(MATCH,LINLEN,'*')
      K=0
      DO 667 J=I1,I2
        K=K+1
        IF(GEL2(J-MMM).EQ.LINOUT(K))MATCH(K) = ' '
667   CONTINUE
      WRITE(IDEV,1018)(MATCH(K),K=1,IE)
      RELPO2(1)=(I1)+9
      DO 240 I=2,NLEN
        RELPO2(I)=RELPO2(I-1)+10
240   CONTINUE
      WRITE(IDEV,1023)(RELPO2(K),K=1,NLEN)
      I1=I2+1
      I2=I2+LINLEN
      IF(I2.GT.IDIM)I2=IDIM
      IF(I1.GT.I2)RETURN
250   CONTINUE
C
      WRITE(IDEV,1021)
1021  FORMAT( )
C   NEXT LINE LENGTH
C   NEXT LENGTH IS OLD RIGHT +1
      LSEQNO=RSEQNO+1
C   NEW RIGHT IS LEFT +LENGTH
      RSEQNO=LSEQNO+(LINLEN)-1
C   ARE WE OVER END OF REGION
      IF(RSEQNO.GT.RREG)RSEQNO=RREG
C   HAVE WE FINISHED REGION COMPLETELY
      IF(RSEQNO.LT.LSEQNO) RETURN
C   NOT FINISHED SO NEED TO FIND CURRENT LEFT GEL NO
C   CURRENT LEFT GEL IS LLINO
C
150   CONTINUE
C   NEED TO KNOW IF CURRENT LEFT GELS RIGHT END IS INSIDE REGION
      X=RELPG(LLINO)+ABS(LNGTHG(LLINO))-1
      IF(X.GE.LSEQNO)GO TO 9
C   LOOK AT NEXT GEL TO RIGHT
      LLINO=RNBR(LLINO)
C   MAY HAVE GONE OVER END OF CONTIG?????
      IF(LLINO.GT.0)GO TO 150
300   CONTINUE
      RETURN
      END
      SUBROUTINE EC(GEL,IDG,CON,IDC,K)
      CHARACTER GEL(IDG),CON(IDC),CHARSL
      EXTERNAL CHARSL,INDEXS
      PARAMETER (IDASH = 6)
      K = 0
      DO 10 I = 1,MIN(IDC,IDG)
        JC = INDEXS(CON(I),J)
        IF(JC.NE.IDASH) THEN
          JG = INDEXS(GEL(I),J)
          IF(JG.NE.JC) THEN
            GEL(I) = CHARSL(JC)
            K = K + 1
          END IF
        END IF
10    CONTINUE
C      WRITE(*,*)'NUMBER OF CHARS CORRECTED=',K
      END
      SUBROUTINE ED(GEL,IDG,CON,IDC,K)
      CHARACTER GEL(IDG),CON(IDC),CHARSL
      EXTERNAL CHARSL,INDEXS
      K = 0
      DO 10 I = MIN(IDC,IDG),1,-1
        JC = INDEXS(CON(I),J)
        IF(JC.EQ.5) THEN
          IF(I.LT.IDG) CALL SQCOPY(GEL(I+1),GEL(I),IDG-I)
          K = K + 1
        END IF
10    CONTINUE
C      WRITE(*,*)'NUMBER OF CHARS DELETED=',K
      END
      SUBROUTINE EDR(RELPG,LNGTHG,LNBR,RNBR,IDBSIZ,LGEL,NCONT,
     +CON,IDC,IDEVW,IDEVR,LREG)
      INTEGER RELPG(IDBSIZ),LNGTHG(IDBSIZ),LNBR(IDBSIZ),RNBR(IDBSIZ)
      CHARACTER CON(IDC)
      INTEGER CHNRP
      EXTERNAL CHNRP
C  CHANGE RELATIVE POSITIONS FOR AE
      ND = 0
      DO 10 I = IDC,1,-1
        IF(CON(I).EQ.'*') THEN
          ND = ND + 1
          K = I + LREG - 1
          J = CHNRP(RELPG,LNGTHG,LNBR,RNBR,IDBSIZ,LGEL,NCONT,K)
          IF(J.NE.0) THEN
            CALL SHIFTC(RELPG,LNGTHG,LNBR,RNBR,IDUM,JDUM,IDEVR,
     +      IDBSIZ,J,NCONT,-1)
          END IF
        END IF
10    CONTINUE
C      WRITE(*,*)' NUMBER OF DELETIONS=',ND
      END
      SUBROUTINE ET(GEL,IDG,CON,IDC,K)
      CHARACTER GEL(IDG),CON(IDC),CHARSL
      EXTERNAL CHARSL,INDEXS
      K = 0
      DO 10 I = 2,MIN(IDC,IDG)
        JC = INDEXS(CON(I),J)
        IF(JC.NE.6) THEN
          JG = INDEXS(GEL(I),J)
          IF(JG.NE.JC) THEN
            JNG = INDEXS(GEL(I-1),J)
            JNC = INDEXS(CON(I-1),J)
            IF(JNC.NE.JNG) THEN
              IF((JNG.EQ.JC).AND.(JNC.EQ.JG)) THEN
                GEL(I) = CHARSL(JNG)
                GEL(I-1) = CHARSL(JG)
                K = K + 1
              END IF
            END IF
          END IF
        END IF
10    CONTINUE
C      WRITE(*,*)' NUMBER OF CHARS TRANSPOSED=',K
      END
      SUBROUTINE FDEPTH(RELPG,LNGTHG,LNBR,RNBR,
     +IDBSIZ,LGEL,LREG,RREG,LENCON,
     +MARGL,MARGR,MARGB,MARGT,ISXMAX,ISYMAX)
C  AUTHOR: RODGER STADEN
      INTEGER RELPG(IDBSIZ)
      INTEGER LNGTHG(IDBSIZ),LNBR(IDBSIZ),RNBR(IDBSIZ)
      INTEGER RREG,DEPTHP,DEPTHM,STRAND
      STRAND = 1
      CALL FDPTH(RELPG,LNGTHG,LNBR,RNBR,
     +IDBSIZ,LGEL,LREG,RREG,LENCON,STRAND,DEPTHP)
      IF(DEPTHP.LT.0) RETURN
      STRAND = -1
      CALL FDPTH(RELPG,LNGTHG,LNBR,RNBR,
     +IDBSIZ,LGEL,LREG,RREG,LENCON,STRAND,DEPTHM)
      IF(DEPTHM.LT.0) RETURN
      CALL PLTCON(RELPG,LNGTHG,LNBR,RNBR,IDBSIZ,
     +MARGL,MARGR,MARGB,
     +MARGT,ISXMAX,ISYMAX,LGEL,LREG,RREG,DEPTHP,DEPTHM)
      END
      SUBROUTINE FDPTH(RELPG,LNGTHG,LNBR,RNBR,
     +IDBSIZ,LGEL,LREG,RREG,LENCON,STRAND,DEPTH)
C  AUTHOR: RODGER STADEN
      INTEGER RELPG(IDBSIZ)
      INTEGER LNGTHG(IDBSIZ),LNBR(IDBSIZ),RNBR(IDBSIZ)
      INTEGER RREG,DEPTH,STRAND
      EXTERNAL NCDEP
C  LREG = left contig position
C  RREG = right ''      ''
C  LENCON = RREG-LREG+1
      I = LGEL
      DEPTH = 0
5     CONTINUE
      IF(I.NE.0) THEN
        IF((RELPG(I)+ABS(LNGTHG(I))-1).LT.LREG) THEN
          I = RNBR(I)
          GO TO 5
        END IF
      ELSE
        DEPTH = -1
        RETURN
      END IF
C      WRITE(*,*)'LGEL',LGEL
10    CONTINUE
      IF(I.NE.0)THEN
        IF(RELPG(I).LE.RREG) THEN
          IF(SIGN(1,LNGTHG(I)).EQ.STRAND) THEN
            K = RELPG(I) + ABS(LNGTHG(I)) -1
            DEPTH = MAX(NCDEP(RELPG,LNGTHG,LNBR,RNBR,IDBSIZ,I,
     +      STRAND,K),DEPTH)
          END IF
          I = RNBR(I)
          GO TO 10
        END IF
      END IF
C      WRITE(*,*)'DEPTH',DEPTH
      END
C      FIND
C
C   SUBROUTINE TO FIND THE FIRST OCCURENCE OF A GIVEN STRING
C   IN A GIVEN ARRAY
C
      SUBROUTINE FIND(SEQ,IDIM1,STRING,IDIM2,IMATCH)
C   AUTHOR: RODGER STADEN
      CHARACTER SEQ(IDIM1),STRING(IDIM2),DASH
      INTEGER PSEQ,PSTR
      SAVE DASH
      DATA DASH/'-'/
      PSEQ=0
      PSTR=1
      IMATCH=0
C
100   CONTINUE
C
C   PUT PSEQ TO WHERE THIS FAILED MATCH STARTED
      PSEQ=PSEQ+1-PSTR
C
400   CONTINUE
C
      PSTR=0
C
500   CONTINUE
C
C   POINT TO NEXT SEQ CHAR
      PSEQ=PSEQ+1
C   TEST FOR END
      IF(PSEQ.GT.IDIM1)GO TO 300
C   POINT TO NEXT STRING CHAR
      PSTR=PSTR+1
C   TEST FOR DASH IN STRING
      IF(STRING(PSTR).EQ.DASH)GO TO 450
C   TEST FOR DASH IN SEQ
      IF(SEQ(PSEQ).EQ.DASH)GO TO 400
C   TEST FOR MATCH
      IF(SEQ(PSEQ).NE.STRING(PSTR))GO TO 100
C
450   CONTINUE
C
C   TEST FOR END OF STRING IE. WHOLE STRING MATCH
      IF(PSTR.LT.IDIM2)GO TO 500
C   HAVE MATCH. GET POINTER TO WHERE IT STARTED
      IMATCH=PSEQ-IDIM2+1
C
300   CONTINUE
      RETURN
      END
      SUBROUTINE FMT4LP(SEQ1,SEQ2,IDIM,ISW,ISX,IDEV,NAME1,NAME2)
C   AUTHOR: RODGER STADEN
      CHARACTER SEQ1(IDIM),SEQ2(IDIM),MATCH(60),NAME1*(*),NAME2*(*)
      INTEGER KL(6)
      ISXX=ISX
      ISWW=ISW
      IE=0
10    CONTINUE
      IS=IE+1
      IE=IE+60
      IF(IE.GT.IDIM)IE=IDIM
      N=IE-IS+1
      N=1+(N-1)/10
C   SET UP DECIMAL COUNTERS
      DO 50 J=1,N
        KL(J)=ISWW
        ISWW=ISWW+10
50    CONTINUE
      WRITE(IDEV,1001)(KL(K),K=1,N)
      WRITE(IDEV,1002)NAME1,(SEQ1(K),K=IS,IE)
      IL = IE - IS + 1
      CALL SQMTCH(SEQ1(IS),SEQ2(IS),MATCH,IL)
      WRITE(IDEV,1003)(MATCH(K),K=1,IL)
      WRITE(IDEV,1002)NAME2,(SEQ2(K),K=IS,IE)
1002  FORMAT(2X,A,2X,6(10A1,1X))
 1003 FORMAT(10X,6(10A1,1X))
C   SET UP DECIMAL COUNTERS
      DO 60 J=1,N
        KL(J)=ISXX
        ISXX=ISXX+10
60    CONTINUE
      WRITE(IDEV,1001)(KL(K),K=1,N)
1001  FORMAT( 5X,6(I6,5X))
      IF(IE.LT.IDIM) GO TO 10
      END
      SUBROUTINE FMTDB(SEQ1,IDIM,ISW,ISE,LINLEN,IDEV)
C   NOTE SAME AS FMTSEP!
C   AUTHOR: RODGER STADEN
      CHARACTER SEQ1(IDIM)
      INTEGER KL(12)
      ISWW=ISW-1
      IE=ISW-1
1     CONTINUE
      WRITE(IDEV,1003)
1003  FORMAT( )
C   SET UP DECIMAL COUNTERS
      DO 50 J=1,LINLEN/10
        ISWW=ISWW+10
        KL(J)=ISWW
50    CONTINUE
      IS=IE+1
      IE=IE+LINLEN
      IF(IE.GT.ISE)IE=ISE
      WRITE(IDEV,1001)(KL(KKK),KKK=1,MIN(IE-IS+1,LINLEN)/10)
      WRITE(IDEV,1002)(SEQ1(K),K=IS,IE)
1002  FORMAT( '  ',12(10A1,1X))
1001  FORMAT( ' ',12(5X,I6))
      IF(IE.EQ.ISE)RETURN
      GO TO 1
      END
      SUBROUTINE FNDCON(SEQ,IDIM,CENDS,NENDS,IDCEND,MAXCON,KBOUT)
C   AUTHOR: RODGER STADEN
C   STORES THEIR POSITIONS IN CENDS AND THEIR LEFT LINE NUMBERS IN NENDS
      PARAMETER (MAXDG = 5)
      CHARACTER SEQ(IDIM),DC(MAXDG)
      INTEGER CENDS(MAXCON)
      INTEGER NENDS(MAXCON)
      EXTERNAL IFROMC,INDEXA
      IDCEND=0
      DO 10 I=1,IDIM
        IF(SEQ(I).NE.'<')GO TO 10
        IDCEND=IDCEND+1
C       PUT POSITION OF LEFT END OF CONTIG IN CENDS
        CENDS(IDCEND)=I
        K = INDEXA(SEQ(I),20,'.')
        IF(K.EQ.0) THEN
           WRITE(KBOUT,*)'Error in contig title: no dot!'
           IDCEND = 0
           RETURN
         END IF
        K = K + I
C        K=I+11
        DO 5 J=1,MAXDG
          DC(J)=SEQ(K)
          K=K+1
5       CONTINUE
        NENDS(IDCEND)=IFROMC(DC,MAXDG,KBOUT)
10    CONTINUE
C     STORE POSITION OF LAST CHAR +1 TO SIMPLIFY DISPLAY ROUTINES
      CENDS(IDCEND+1)=IDIM+1
      RETURN
      END
      INTEGER FUNCTION GCLIN(RELPG,LNGTHG,LNBR,RNBR,NGELS,NCONTS,
     +IDBSIZ,IIN)
C  AUTHOR: RODGER STADEN
C  RETURNS CONTIG LINE NUMBER OR ZERO FOR ERROR
      INTEGER RELPG(IDBSIZ)
      INTEGER LNGTHG(IDBSIZ),LNBR(IDBSIZ),RNBR(IDBSIZ)
      GCLIN = 0
      N=IDBSIZ-NCONTS
      DO 10 J=N,IDBSIZ-1
        IF(LNBR(J).EQ.IIN) THEN
          GCLIN = J
          RETURN
        END IF
10    CONTINUE
      END
      INTEGER FUNCTION GELID(RELPG,LNGTHG,LNBR,RNBR,NGELS,NCONTS,
     +LLINO,IDBSIZ,KBIN,KBOUT,IDEVN,
     +IHELPS,IHELPE,FILEH,IDEVH,INFLAG)
      CHARACTER FILEH*(*)
C   AUTHOR: RODGER STADEN
C   SEARCHES FOR ARCHIVE NAMES
      INTEGER RELPG(IDBSIZ)
      INTEGER LNGTHG(IDBSIZ),LNBR(IDBSIZ),RNBR(IDBSIZ)
      CHARACTER NAME1*17,NAME2*16,NAME3*17,NFLAG
      PARAMETER (NFLAG='/')
      NAME3 = ' '
      IF(LLINO.NE.0) THEN
        NAME3(1:1) = NFLAG
        CALL READN(IDEVN,LLINO,NAME3(2:))
      END IF
      GELID = 0      
10    CONTINUE
      L = 0
      IF(LLINO.NE.0) L = 17
      CALL GTSTR('Contig identfier',NAME3,
     +NAME1,L,KBOUT,KBIN,INFLAG)
      IF(INFLAG.EQ.2) RETURN
      IF(INFLAG.EQ.4) RETURN
      IF(INFLAG.EQ.3) THEN
        GELID = LLINO
        RETURN
      END IF
      IF(INFLAG.EQ.1) THEN
        CALL HELP2(IHELPS,IHELPE,FILEH,IDEVH,KBIN,KBOUT)
        GO TO 10
      END IF
      IF(NAME1(1:1).EQ.NFLAG) THEN
        CALL CCASE(NAME1,1)
        DO 20 I=1,NGELS
          CALL READN(IDEVN,I,NAME2)
          CALL CCASE(NAME2,1)
          IF(NAME1(2:17).EQ.NAME2) THEN
            GELID = I
            RETURN
          END IF
20     CONTINUE
        WRITE(KBOUT,1004)NAME1(2:)
1004    FORMAT(' ',A,' is not in the database!')
      ELSE
        CALL RJST(NAME1)
        READ(NAME1,1001,ERR=10,END=10)GELID
1001    FORMAT(I17)
        IF((GELID.LT.1).OR.(GELID.GT.NGELS)) THEN
          CALL ERROM(KBOUT,'Illegal gel reading number')
          GO TO 10
        END IF
      END IF
      END
      SUBROUTINE GELOUT(RELPG,LNGTHG,LNBR,RNBR,MAXDB,IDBSIZ,NGELS,
     +NCONTS,GEL,MAXGEL,IDEV3,IDEV4,IDEV5,IDEV1,KBIN,KBOUT,
     +IHELPS,IHELPE,HELPF,IDEVH,FILNAM)
      INTEGER RELPG(MAXDB)
      INTEGER LNGTHG(MAXDB),LNBR(MAXDB),RNBR(MAXDB)
      CHARACTER  GEL(MAXGEL)
      CHARACTER FILNAM*(*),HELPF*(*)
      CHARACTER NAMARC*16
      FILNAM = ' '
      CALL OPENF1(IDEV5,FILNAM,1,IOK,KBIN,KBOUT,
     +'File for names of extracted gel readings',
     +IHELPS,IHELPE,HELPF,IDEVH)
      IF(IOK.NE.0)RETURN
      CALL YESNO(I,'Extract ends of contigs only',
     +IHELPS,IHELPE,HELPF,IDEVH,KBIN,KBOUT)
      IF(I.LT.0) RETURN
      IF(I.EQ.0) GO TO 15
      DO 10 I=1,NGELS
        L=ABS(LNGTHG(I))
        IF(L.GT.0)THEN
          CALL READN(IDEV4,I,NAMARC)
          WRITE(KBOUT,1002)NAMARC
1002      FORMAT(' ',A)
          WRITE(IDEV5,1003)NAMARC
1003      FORMAT(A)
          FILNAM = NAMARC
          CALL OPENRS(IDEV1,FILNAM,IOK,LRECL,1)
          IF(IOK.NE.0) GO TO 100
          CALL READW(IDEV3,I,GEL,MAXGEL)
          IF(LNGTHG(I).LT.0)THEN
            CALL SQREV(GEL,L)
            CALL SQCOM(GEL,L)
          END IF
          CALL FMTDKN(IDEV1,GEL,L)
          CLOSE(UNIT=IDEV1)
      END IF
10    CONTINUE
      RETURN
15    CONTINUE
C   NUMBER OF LINES TO PROCESS
      N=IDBSIZ-NCONTS
      DO 20 I=N,IDBSIZ-1
        JL=LNBR(I)
        JR=RNBR(I)
        CALL READN(IDEV4,JL,NAMARC)
        WRITE(KBOUT,1002)NAMARC
        WRITE(IDEV5,1003)NAMARC
        FILNAM = NAMARC
        CALL OPENRS(IDEV1,NAMARC,IOK,LRECL,1)
        IF(IOK.NE.0) GO TO 100
        CALL READW(IDEV3,JL,GEL,MAXGEL)
        L=ABS(LNGTHG(JL))
        IF(LNGTHG(JL).LT.0)THEN
          CALL SQREV(GEL,L)
          CALL SQCOM(GEL,L)
        END IF
        CALL FMTDKN(IDEV1,GEL,L)
        CLOSE(UNIT=IDEV1)
        IF(JR.EQ.JL)GO TO 20
        CALL READN(IDEV4,JR,NAMARC)
        WRITE(KBOUT,1002)NAMARC
        WRITE(IDEV5,1003)NAMARC
        CALL OPENRS(IDEV1,NAMARC,IOK,LRECL,1)
        IF(IOK.NE.0) GO TO 100
        CALL READW(IDEV3,JR,GEL,MAXGEL)
        L=ABS(LNGTHG(JR))
        IF(LNGTHG(JR).LT.0)THEN
          CALL SQREV(GEL,L)
          CALL SQCOM(GEL,L)
        END IF
        CALL FMTDKN(IDEV1,GEL,L)
        CLOSE(UNIT=IDEV1)
20    CONTINUE
      RETURN
100   CONTINUE
      WRITE(KBOUT,*)'Error opening file for extracted gel reading'
      RETURN
      END
      SUBROUTINE GETLN2(RELPG,LNGTHG,LNBR,RNBR,NGELS,NCONTS,
     +LINCON,LLINO,IGELNO,IERR,IDBSIZ,KBIN,KBOUT,IDEVN,
     +IHELPS,IHELPE,FILEH,IDEVH)
C   AUTHOR: RODGER STADEN
      INTEGER RELPG(IDBSIZ),GELID
      INTEGER LNGTHG(IDBSIZ),LNBR(IDBSIZ),RNBR(IDBSIZ)
      CHARACTER FILEH*(*)
      EXTERNAL GELID
      IERR = 1
      NCONTC = GELID(RELPG,LNGTHG,LNBR,RNBR,NGELS,NCONTS,LLINO,
     +IDBSIZ,KBIN,KBOUT,IDEVN,
     +IHELPS,IHELPE,FILEH,IDEVH,INFLAG)
      IF(INFLAG.EQ.2) RETURN
      IF(INFLAG.EQ.4) RETURN
      IF(NCONTC.EQ.0) RETURN
      IGELNO = NCONTC
      IF(LNBR(NCONTC).NE.0) THEN
        WRITE(KBOUT,1013)RELPG(NCONTC)
1013    FORMAT(' Position of this reading=',I6)
25      CONTINUE
        NCONTC = LNBR(NCONTC)
        IF(LNBR(NCONTC).NE.0) GO TO 25
        WRITE(KBOUT,1014)NCONTC
1014    FORMAT( ' Number of leftmost reading this contig=',I6)
      END IF
30    CONTINUE
      N = IDBSIZ - NCONTS
      DO 20 J=N,IDBSIZ-1
        IF(LNBR(J).EQ.NCONTC) THEN
          LINCON=J
          GO TO 21
        END IF
20    CONTINUE
      WRITE(KBOUT,9999)
9999  FORMAT(' No contig line for this gel! Fix the database')
      RETURN
21    CONTINUE
      LLINO = NCONTC
      IERR = 0
      END
      SUBROUTINE GETLN(RELPG,LNGTHG,LNBR,RNBR,NGELS,NCONTS,
     +LINCON,LLINO,IERR,IDBSIZ,KBIN,KBOUT,IDEVN,
     +IHELPS,IHELPE,FILEH,IDEVH)
      CALL GETLN2(RELPG,LNGTHG,LNBR,RNBR,NGELS,NCONTS,
     +LINCON,LLINO,IGELNO,IERR,IDBSIZ,KBIN,KBOUT,IDEVN,
     +IHELPS,IHELPE,FILEH,IDEVH)
      END
      SUBROUTINE GETREG(RELPG,LNGTHG,LNBR,RNBR,NGELS,NCONTS,
     +LEFTMN,RIGHTM,LREG,RREG,LINCON,LLINO,IDBSIZ,KBIN,KBOUT,
     +  IHELPS,IHELPE,FILEH,IDEVH,IOK)
C   AUTHOR: RODGER STADEN
      INTEGER RELPG(IDBSIZ)
      INTEGER LNGTHG(IDBSIZ),LNBR(IDBSIZ),RNBR(IDBSIZ)
      INTEGER LREG,RREG,RIGHTM
      CHARACTER FILEH*(*)
40    CONTINUE
      MN = LEFTMN
      MX = RIGHTM
      LREG = MN
      CALL GETINT(MN,MX,LREG,
     +'Start position in contig',
     +IVAL,KBIN,KBOUT,
     +IHELPS,IHELPE,FILEH,IDEVH,IOK)
      IF(IOK.NE.0) RETURN
      LREG = IVAL
      MN = LREG
      MX = RIGHTM
      RREG = MX
      CALL GETINT(MN,MX,RREG,
     +'End position in contig',
     +IVAL,KBIN,KBOUT,
     +IHELPS,IHELPE,FILEH,IDEVH,IOK)
      IF(IOK.NE.0) RETURN
      RREG = IVAL
C   NOW FIND FIRST GEL THAT OVER LAPS
50    CONTINUE
      X=RELPG(LLINO)+(ABS(LNGTHG(LLINO)))-1
      IF(X.GE.LREG)GO TO 60
C   NOT IN REGION
      LLINO=RNBR(LLINO)
      GO TO 50
60    CONTINUE
      RETURN
      END
      SUBROUTINE GLEVEL(T,YF,YT,Y0,YP1,YP2,YM1,YM2)
      CHARACTER T
        IF(T.EQ.'0') THEN
          YF = Y0
          YT = Y0
        ELSE IF(T.EQ.'1') THEN
          YF = Y0
          YT = YM1
        ELSE IF(T.EQ.'2') THEN
          YF = Y0
          YT = YP1
        ELSE IF(T.EQ.'3') THEN
          YF = YP1
          YT = YM1
        ELSE IF(T.EQ.'4') THEN
          YF = YP2
          YT = YM2
        END IF
      END
      CHARACTER*1 FUNCTION GTCONC(COUNTS,IDM,CUT)
      INTEGER IDM
      INTEGER COUNTS(IDM)
      CHARACTER CHARSU
      EXTERNAL CHARSU
C 30-3-92 made this routine sum counts
      GTCONC = '-'
      ISUM = 0
      DO 5 I=1,IDM
        ISUM = ISUM + COUNTS(I)
 5    CONTINUE
      IF(ISUM.EQ.0) RETURN
      Y = ISUM
      DO 10 I = 1,IDM - 1
        X = REAL(COUNTS(I))/Y
        IF(X.GE.CUT) THEN
          GTCONC = CHARSU(I)
          RETURN
        END IF
10    CONTINUE
      END
      SUBROUTINE HIGHLT(GELSAV,NAMSAV,NUMSAV,KBIN,KBOUT,
     +IHELPS,IHELPE,HELPF,IDEVH,IDEV1,IDEV2,
     +FILNAM,IOK)
      CHARACTER LINEIN*132,CONSEN*100
      CHARACTER GELNO*6,GEL*100,GELSAV*100,GELNAM*16
      CHARACTER NAMSAV*16,NUMSAV*6
      CHARACTER FILNAM*(*),HELPF*(*)
      DIMENSION GELSAV(50),NAMSAV(50),NUMSAV(50)
      CHARACTER PLUS*4,MINUS*4
      EQUIVALENCE (LINEIN(2:2),GELNO),(LINEIN(9:9),GELNAM)
      EQUIVALENCE (LINEIN(26:26),GEL)
      EXTERNAL NOTIRL
      CALL OPENF1(IDEV1,FILNAM,0,IOK,KBIN,KBOUT,
     +'File containing contig display',
     +IHELPS,IHELPE,HELPF,IDEVH)
      IF(IOK.NE.0) RETURN
      FILNAM = ' '
      CALL OPENF1(IDEV2,FILNAM,1,IOK,KBIN,KBOUT,
     +'File for problem display',
     +IHELPS,IHELPE,HELPF,IDEVH)
      IF(IOK.NE.0) RETURN
C
C   FORMAT:
C
C12345678901234567890       10        20         30    ETC
C   12  GELNAM0000  CAGACGCGCGCGCGCGCGGATATAGTCTCTCCGCTCT
C  100  GELNAM0000       TGATACGCTCGCTCTCTCTCTCTCTCTCTTTC
C                   AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
C
C                           70        80       ETC
C   12  GELNAM0000     AAAAAAAAAAAAAAAAAAAAAAAAAAAA
C
C
      LIN = 1
      CALL GTSTR('plus strand symbol',':',PLUS,LIN,KBOUT,KBIN,INFLAG)
      IF(INFLAG.EQ.1) THEN
        CALL HELP2(IHELPS,IHELPE,HELPF,IDEVH,KBIN,KBOUT)
        GO TO 10
      END IF
      IF(INFLAG.EQ.2) RETURN
      IF(LIN.EQ.0) PLUS = ':'
      LIN = 1
      CALL GTSTR('minus strand symbol','.',MINUS,LIN,KBOUT,KBIN,INFLAG)
      IF(INFLAG.EQ.1) THEN
        CALL HELP2(IHELPS,IHELPE,HELPF,IDEVH,KBIN,KBOUT)
        GO TO 10
      END IF
      IF(INFLAG.EQ.2) RETURN
      IF(LIN.EQ.0) MINUS = '.'
C   COUNT LINE NUMBERS
      LINNO=0
10    CONTINUE
C   READ LINE OF NOS
      READ(IDEV1,1003,END=100)LINEIN
      LINNO=LINNO+1
1003  FORMAT(A)
C   WRITE IT OUT AGAIN
      WRITE(IDEV2,1003)LINEIN
C   ZERO GEL COUNT FOR THIS STRIP
      IGEL=0
20    CONTINUE
C
C   READ A LINE, COULD BE 1 GEL, 2 CONSENSUS OR BLANK
C       LINEIN=' '
      READ(IDEV1,1003,END=100)LINEIN
      LINNO=LINNO+1
C   WHAT SORT OF LINE?  ONLY A GEL WILL HAVE NON BLANK CHARS AT THE LEFT END
      IF(LINEIN(2:7).NE.' ')THEN
C   GEL LINE SO SAVE
      IGEL=IGEL+1
      GELSAV(IGEL)=GEL
      NAMSAV(IGEL)=GELNAM
      NUMSAV(IGEL)=GELNO
      GO TO 20
      END IF
C   MUST BE CONSENSUS
      CONSEN=GEL
C   PROCESS THIS STRIP OF GELS (IGEL OF THEM)
      DO 50 I=1,IGEL
C       WHERE DOES DATA START AND END?
        IFIRST=1
40      CONTINUE
        IF(GELSAV(I)(IFIRST:IFIRST).NE.' ')GO TO 45
        IFIRST=IFIRST+1
        IF(IFIRST.LE.100)GO TO 40
C       ERROR --- NO DATA FOUND
        WRITE(KBOUT,1004)LINNO
1004    FORMAT(' Error on line',I6,' of file')
        RETURN
45      CONTINUE
C       NOW WHERE DOES IT END
        ILAST=NOTIRL(GELSAV(I),100,' ')
C       COMPARE WITH CONSENSUS
        READ(NUMSAV(I),1001,ERR=900)INTEG
 1001   FORMAT(I6)
        IF(INTEG.GE.0)CALL IDTOD(CONSEN,GELSAV(I),IFIRST,ILAST,PLUS)
        IF(INTEG.LT.0)CALL IDTOD(CONSEN,GELSAV(I),IFIRST,ILAST,MINUS)
        WRITE(IDEV2,1008)NUMSAV(I),NAMSAV(I),GELSAV(I)(1:ILAST)
1008    FORMAT(' ',A,1X,A,1X,A)
50    CONTINUE
      WRITE(IDEV2,1009)CONSEN
1009  FORMAT(' ',24X,A)
1006  FORMAT( )
C     READ A BLANK LINE
      READ(IDEV1,1003,END=100)LINEIN
      LINNO=LINNO+1
      WRITE(IDEV2,1003)LINEIN
C     NO GO BACK FOR THE NEXT LINE OF NUMBERS
      GO TO 10
100   CONTINUE
      WRITE(KBOUT,1005)
1005  FORMAT(' Finished')
      RETURN
900   WRITE(KBOUT,*)'Error reading gel number'
      END
      SUBROUTINE IDPLC(RELPG,LNGTHG,LNBR,RNBR,IDBSIZ,
     +NCONTS,IX,IY,MARGL,MARGR,MARGB,MARGT,ISXMAX,ISYMAX,
     +DBTDUX,DBTDUY,NCONT,IGEL,IS)
      INTEGER RELPG(IDBSIZ),LNGTHG(IDBSIZ),LNBR(IDBSIZ),RNBR(IDBSIZ)      
      INTEGER CHNRP1
      EXTERNAL CWORLD,CHNRP1
      YMAX = ISYMAX
      YMIN = 0.
      XMIN = 0.
      LENCON = 0
      DO 10 I = IDBSIZ-NCONTS,IDBSIZ-1
        LENCON = LENCON + RELPG(I)
10    CONTINUE
      XMAX = LENCON
      XX = CWORLD(IX,MARGL,MARGR,XMIN,XMAX)
      YX = CWORLD(IY,MARGB,MARGT,YMIN,YMAX)
      YINC = (YMAX-YMIN)/3.
      Y = 0.
      XF = XMIN
      N = 0
      DO 20 I = IDBSIZ-NCONTS,IDBSIZ-1
        N = N + 1
        XT = XF + RELPG(I)
        Y = Y + YINC
        IF((XX.GT.XF).AND.(XX.LT.XT)) THEN
          IS = NINT(((XX-XF)/(XT-XF)) * RELPG(I))
          JGEL = LNBR(I)
          IGEL = CHNRP1(RELPG,LNGTHG,LNBR,RNBR,IDBSIZ,JGEL,IS)
          NCONT = I
          RETURN
        END IF
        XF = XT
        IF(N.EQ.2) THEN
          N = 0
          Y = 0.
        END IF
20    CONTINUE
      IGEL = 0
      NCONT = 0
      END
      SUBROUTINE IDTOD(TOPLIN,GEL,IFIRST,ILAST,SYMBOL)
      CHARACTER TOPLIN*100,GEL*100,SYMBOL*4
      DO 10 I=IFIRST,ILAST
        IF(GEL(I:I).EQ.TOPLIN(I:I))GEL(I:I)=SYMBOL(1:1)
10    CONTINUE
      END
      INTEGER FUNCTION INDEXS(C,S)
      PARAMETER (IDM = 29)
      CHARACTER C
      INTEGER POINTS(0:255),SCORES(IDM),IND(IDM),S
      COMMON /SHOTC/POINTS
      SAVE /SHOTC/
      SAVE SCORES,IND
      DATA 
     +IND/1,2,3,4,1,2,3,4,1,2,3,4,1,2,3,4,6,6,6,6,6,6,1,2,3,4,5,5,6/
C      DATA DUP/'CTAG1234DVBHKLMNRY5678ctag*,-'/
C  changed 28-7-91 to give 10 to old zeroes and 100 to lowercase
      DATA SCORES/
     +100,100,100,100,
     +75,75,75,75,
     +100,100,100,100,
     +100,100,100,100,
     +10,10,10,10,10,10,
     +100,100,100,100,100,100,10/
      I = ICHAR(C)
      I = POINTS(I)
      S = SCORES(I)
      INDEXS = IND(I)
      END
C  ROUTINES TO CONTROL CHARACTER LOOKUP FOR SHOTGUN SEQUENCING
      SUBROUTINE INITS
C  AUTHOR RODGER STADEN
      INTEGER POINTS(0:255)
      PARAMETER (IDM = 29)
      CHARACTER DUP*29
      COMMON /SHOTC/POINTS
      SAVE /SHOTC/
      DATA DUP/'CTAG1234DVBHKLMNRY5678ctag*,-'/
C  ICHAR RETURNS THE COLLATING SEQUENCE NUMBER
C  I WANT 1-4 FOR ACGT
C                 acgt
C                 1234
C                 BDHV
C                 KLMN
C      5 FOR      *
C      6 FOR      5678- AND ELSE
C  THE ACTUAL VALUE RETURNED BY ICHAR IS NOT PORTABLE 
C  SO I NEED TO INITIALIZE POINTR SO THAT THE CORRECT 
C  ELEMENTS CONTAIN VALUES 1 - 6
C
        DO 30 I = 0,255
          POINTS(I) = IDM
30      CONTINUE
        DO 35 I = 1,IDM
          J = ICHAR(DUP(I:I))
          POINTS(J) = I
35      CONTINUE
      END
      FUNCTION INLIST(LIST,IDLIST,ITEM)
C   AUTHOR: RODGER STADEN
C   SENT LIST LIST, AND ITEM ITEM. IF IN LIST RETURNS ELEMENT NUMBER, ELSE 0
      INTEGER LIST(IDLIST)
      INLIST=0
      DO 1 I=1,IDLIST
        IF(LIST(I).NE.ITEM)GO TO 1
        INLIST=I
      RETURN
1     CONTINUE
      RETURN
      END
      SUBROUTINE IPLTC(RELPG,LNGTHG,LNBR,RNBR,
     +IDBSIZ,LGEL,LREG,RREG,STRAND,
     +MARGL,MARGR,MARGB,MARGT,ISXMAX,ISYMAX,YMID,YINC,DEPTH,X,Y,KBOUT,
     +IGEL,IOK)
C  AUTHOR: RODGER STADEN
      INTEGER RELPG(IDBSIZ)
      INTEGER LNGTHG(IDBSIZ),LNBR(IDBSIZ),RNBR(IDBSIZ)
      INTEGER RREG,STRAND,DEPTH
      IOK = 1
      XMIN = LREG
      XMAX = RREG
      YMAX = ISYMAX
      YMIN = 0.
      YINCO2 = STRAND*YINC/2.
      I = LGEL
      IGEL = 0
5     CONTINUE
      IF(I.NE.0) THEN
        IF((RELPG(I)+ABS(LNGTHG(I))-1).LT.LREG) THEN
          I = RNBR(I)
          GO TO 5
        END IF
      END IF
      N = 0
10    CONTINUE
      IF(I.NE.0)THEN
        IF(RELPG(I).LE.RREG) THEN
          IF(SIGN(1,LNGTHG(I)).EQ.STRAND) THEN
            XF = MAX(RELPG(I),LREG)
            XT = MIN(ABS(LNGTHG(I))+RELPG(I)-1,RREG)
            N = N + 1
            IF(N.GT.DEPTH) N = 1
            YF = YMID + N * YINC
            IF((X.GE.XF).AND.(X.LE.XT)) THEN
              IGEL = I
              IF((Y.GE.YF-YINCO2).AND.(Y.LE.YF+YINCO2)) THEN
                IOK = 0
                RETURN
              END IF
            END IF
          END IF
          I = RNBR(I)
          GO TO 10
        END IF
      END IF
      END
C     LINEUP
C
C   TAKES 2 SEQS SET OF MATCHES AND PRODUCES LINED UP SEQS
C   FINDS IF WE HAVE A LEFT OVERLAP
C   RETURNS POSITION OF JOINT. THIS IS RELATIVE TO THE CONTIG
C   FOR MOST MATCHES BUT I RELATIVE TO THE GEL FOR A LEFT OVERLAP
      SUBROUTINE LINEUP(SEQG,SEQC,SEQG2,SEQC2,IDC,IDG,IDOUT,
     1MATG,MATC,MATL,IP,ITOTPC,ITOTPG,JOINT,ITYPE,KBOUT,MAXGEL,IFAIL)
C   AUTHOR: RODGER STADEN
      CHARACTER SEQG(IDG),SEQC(IDC),SEQG2(IDOUT),SEQC2(IDOUT),PAD
      INTEGER MATG(IP),MATC(IP),MATL(IP)
      SAVE PAD
      DATA PAD/','/
      IFAIL=0
C   ZERO PADDING CHARS IN CONTIG (GEL DONE AT END BY DIFFERENCE
C   IN INPUT AND OUTPUT LENGTHS)
      ITOTPC=0
C   FILL OUTPUT WITH PADDING
      DO 10 I=1,IDOUT
        SEQG2(I)=PAD
        SEQC2(I)=PAD
10    CONTINUE
      NMTCH=0
C   SET INITIAL POINTERS TO OUTPUT
C   CONSENSUS
      IS1=1
C   GEL
      IS2=1
C   FIND DISTANCE FROM LEFT MATCH IN GEL TO LEFT OF GEL
      IG2=MATG(1)-1
      IF(IG2.EQ.0)THEN
C       THE LEFT END OF THE GEL MATCHES SO THIS IS NOT A LEFT OVERLAP
C       SET TYPE
        ITYPE=-1
C       SET JOINT
        JOINT=MATC(1)
C       SKIP NEXT SECTION
        GO TO 50
      END IF
C   FIND DISTANCE FROM LEFT MATCH IN CONTIG TO LEFT OF CONTIG
      IC2=MATC(1)-1
C   GET DISTANCE FROM FIRST MATCH IN CONTIG TO FIRST MATCH IN GEL.
C   IF THIS DISTANCE <0 THEN WE HAVE A LEFT OVERLAP
      IC1=IC2-IG2+1
      IF(IC1.GT.0)THEN
C       THIS IS NOT A LEFT OVERLAP
C       SET TYPE
        ITYPE=-1
C       SET LEFT END
        JOINT=IC1
C       COPY THE GEL UPTO THE FIRST MATCH, INTO THE OUTPUT ARRAY
C       CHECK FOR OVERFLOW
        IF(IG2.GT.MAXGEL)GO TO 700
        CALL SQCOPY(SEQG(1),SEQG2(1),IG2)
C       COPY THE CONTIG FOR THE SAME REGION
        IF(IG2.GT.MAXGEL)GO TO 700
        CALL SQCOPY(SEQC(IC1),SEQC2(1),IG2)
        IS1=IS1+IG2
        IS2=IS2+IG2
        GO TO 50
      END IF
C   MUST BE LEFT END OVERLAP
C   SET TYPE
      ITYPE=1
C   SET POSITION OF JOINT RELATIVE TO GEL
      JOINT=ABS(IC1)+2
C   COPY OVER THE GEL UPTO THE JOINT
C   CHECK FOR OVERFLOW
      IF(IG2.GT.MAXGEL)GO TO 700
      CALL SQCOPY(SEQG(1),SEQG2(1),IG2)
      IS2=IS2+IG2
C   WE MAY ALSO HAVE MISMATCHING
C   DATA AT THE JOIN SO DEAL WITH THAT NOW
C   IF IC2 >0 THE LEFT END OF THE CONTIG MATCHES THE GEL BUT OTHERWISE
C   WE HAVE SOME MISMATCHED DATA TO DEAL WITH - WE NEED TO TRANSFER
C   THE MISMATCHED REGION OF THE CONTIG TO THE OUTPUT ARRAY
      IF(IC2.GT.0)THEN
        IF(IC2.GT.MAXGEL)GO TO 700
        CALL SQCOPY(SEQC(1),SEQC2(1),IC2)
        IS1=IS1+IC2
      END IF
C   WHEN WE GET HERE WE HAVE SORTED OUT THE LEFT ENDS FOR LEFT OVERLAP
C   AND MISMATCHED LEFT ENDS, WE NOW DEAL WITH THE REST OF THE SEQUENCE
C   STARTING WITH THE FIRST BLOCK OF IDENTITY
C
C IG1 POSITION IN INPUT GEL
C IS2 POSITION IN OUTPUT GEL
C IC1 POSITION IN INPUT CONTIG
C IS1 POSITION IN OUTPUT CONTIG
C LG1 POSITION OF END OF CURRENT MATCH IN OUTPUT GEL
C LC1 POSITION OF END OF CURRENT MATCH IN OUTPUT CONTIG
C LG2 DISTANCE FROM CURRENT MATCH IN INPUT GEL TO NEXT MATCH
C LC2 DISTANCE FROM CURRENT MATCH IN INPUT CONTIG TO NEXT MATCH
C
50    CONTINUE
C   POINT TO NEXT MATCH
      NMTCH=NMTCH+1
C   COPY NEXT MATCH
      IG1=MATG(NMTCH)
      IC1=MATC(NMTCH)
      L=MATL(NMTCH)
C   CHECK FOR OVERFLOW
      IF(IS2+L-1.GT.MAXGEL)GO TO 700
      CALL SQCOPY(SEQG(IG1),SEQG2(IS2),L)
C   CHECK FOR OVERFLOW
      IF(IS1+L-1.GT.MAXGEL)GO TO 700
      CALL SQCOPY(SEQC(IC1),SEQC2(IS1),L)
C   POINT TO NEXT OUTPUT POSITIONS
      IS1=IS1+L
      IS2=IS2+L
C   END OF CURRENT MATCH
      LG1=IG1+L
      LC1=IC1+L
C   ANY MORE MATCHES
      IF(NMTCH.EQ.IP)GO TO 500
      K=NMTCH+1
      LG2=MATG(K)-LG1
      LC2=MATC(K)-LC1
C   ANY DIFFERENCE IN LENGTH? IF SO WE HAVE TO PAD SO THEY BECOME THE SAME
      L5=ABS(LG2-LC2)
C   COUNT PADDING CHARS IN CONTIG
      IF(LG2.GT.LC2)ITOTPC=ITOTPC+L5
C   IF DIFFERENCE INCREMENT SHORTER
      IF(LG2.GT.LC2)IS1=IS1+L5
C   IF GEL NEEDS PADDING TRY TO PUT PADS NEXT TO DOUBLE CODES
      IF(LC2.GT.LG2)CALL PADCOP(SEQG,SEQG2,
     +LG1,MATG(K),L5,IS2,LG2,MAXGEL,IFAIL,KBOUT,SEQC,LC1)
C   CHECK FOR OVERFLOW
      IF(IFAIL.EQ.1)GO TO 700
C   NOW COPY MISSMATCHED REGION
C   CHECK FOR OVERFLOW
      IF(IS2+LG2-1.GT.MAXGEL)GO TO 700
      IF(LG2.GT.0)CALL SQCOPY(SEQG(LG1),SEQG2(IS2),LG2)
C   CHECK FOR OVERFLOW
      IF(IS1+LC2-1.GT.MAXGEL)GO TO 700
      IF(LC2.GT.0)CALL SQCOPY(SEQC(LC1),SEQC2(IS1),LC2)
C   POINT TO NEXT OUTPUT POSITIONS
      IS1=IS1+LC2
      IS2=IS2+LG2
C   GET NEXT MATCH
      GO TO 50
500   CONTINUE
C
C   FINISH RIGHT ENDS
C   ONLY COPY TO END OF GEL IN GEL AND TO THE SAME RELATIVE POSITION
C   IN THE CONTIG FOR DISPLAY PURPOSES AND FOR COUNTING MISMATCH
C   CURRENT ENDS AT LG1,LC1
C   HOW FAR TO END OF GEL?
C   SET M
      M=0
      L=IDG-LG1+1
      IF(L.LT.1)GO TO 600
C   CHECK FOR OVERFLOW
      IF(IS2+L-1.GT.MAXGEL)GO TO 700
      CALL SQCOPY(SEQG(LG1),SEQG2(IS2),L)
C   NEED TO COPY TO END OF GEL IN CONTIG FOR DISPLAY
C   POINT TO POSN IN CONTIG LEVEL WITH END OF GEL
      M=LC1+L-1
C   IS THIS OVER END OF CONTIG?
      IF(M.GT.IDC)M=IDC
C   NUMBER TO COPY
      M=M-LC1+1
C   CHECK FOR OVERFLOW
      IF(IS1+M-1.GT.MAXGEL)GO TO 700
      IF(M.GT.0)CALL SQCOPY(SEQC(LC1),SEQC2(IS1),M)
600   CONTINUE
C   COUNT PADDING IN GEL
      ITOTPG=IS2+L-1-IDG
C   SET NEW LENGTHS FOR RETURN TO CALLING ROUTINE
      IDOUT=IS1+M-1
      IDG=IS2+L-1
      IFAIL=0
      RETURN
700   CONTINUE
      WRITE(KBOUT,1000)
1000  FORMAT(' Matching region too long for routine lineup,',
     +' alignment aborted')
      IFAIL=1
      RETURN
      END
      SUBROUTINE LSTCON(RELPG,LNGTHG,LNBR,RNBR,IDBSIZ,LLINO,
     +RREG,IDEV,IDEVN,NAMARC)
      INTEGER RELPG(IDBSIZ),LNGTHG(IDBSIZ),LNBR(IDBSIZ),RNBR(IDBSIZ)      
      INTEGER RREG
      CHARACTER NAMARC*(*)
      N = LLINO
      WRITE(IDEV,1001)
10    CONTINUE
      CALL READN(IDEVN,N,NAMARC)
      WRITE(IDEV,1006)NAMARC,N,RELPG(N),LNGTHG(N),LNBR(N),RNBR(N)
      IF(RNBR(N).NE.0) THEN
        N = RNBR(N)
        IF(RELPG(N).LE.RREG) GO TO 10
      END IF
1001  FORMAT(
     +' NAME              NUMBER POSITION LENGTH     NEIGHBOURS'/
     +'                                             LEFT   RIGHT')
1006    FORMAT( ' ',A,2X,I6,2X,I7,2X,I5,2X,I6,2X,I6)
C1006    FORMAT( ' ',A,2X,I4,2X,I7,2X,I5,2X,I6,2X,I6)
      END
C12345678901234567890
C                          710       720       730       740       750
C   -1  HINW.004    CGTCAGACGCACGCTGGAAAA
      INTEGER FUNCTION LTYPE(LINE,LL,J1,J2,N,MAXDB,KBOUT)
      PARAMETER (MAXDG = 5)
      CHARACTER LINE*(*),NUM*(MAXDG),SPACE
      EXTERNAL NOTRL,NOTLR
      PARAMETER (SPACE= ' ')
      J1 = NOTLR(LINE,LL,SPACE)
      IF(J1.EQ.0) THEN
C       BLANK LINE
        LTYPE = 1
        RETURN
      END IF
      IF(J1.GT.20) THEN
C       LINE OF NUMBERS
        LTYPE = 2
        RETURN
      END IF
      IF(J1.GT.MAXDG+2) THEN
C       CONSENSUS LINE
        LTYPE = 3
        RETURN
      END IF
C       SHOULD BE A SEQUENCE LINE
      J = INDEX(LINE(J1:),SPACE)
      NUM = SPACE
      NUM = LINE(J1:J1+J-2)
      CALL RJST(NUM)
 1001 FORMAT(I6)
      READ(NUM,1001,ERR=10) N
      IF(N.GT.MAXDB-2) GO TO 10
C     NUMBER ENDS AT J1+J-2
      J1 = J1 + J - 1
C     LOOK FOR BEGINNING OF NAME
      J = NOTLR(LINE(J1:),LL-J1+1,SPACE)
      N1 = J1 + J - 1
C     LOOK FOR END OF NAME
      J = INDEX(LINE(N1:),SPACE)
      N2 = N1 + J - 2
C     LOOK FOR BEGINNING OF SEQ
      J = NOTLR(LINE(N2+1:),LL-N2,SPACE)
      J1 = N2 + J
      LTYPE = 4
C     LOOK FOR END OF SEQ
      J2 = NOTRL(LINE,LL,SPACE)
      IF(J2.GT.N2) RETURN
10    CONTINUE
      LTYPE = 0
      END
      INTEGER FUNCTION LWRAPS(I,J)
      K = MOD(I,J)
      IF(K.EQ.0) K = J
      LWRAPS = K
      END
C     MERGE
C
C   ROUTINE SENT CONTIG WHOSE GELS MAY BE OUT OF ORDER
C   REORDERS GELS ON POSITION OF LEFT ENDS AND SETS LEFT
C   GEL NUMBER FOR THE REORDERED CONTIG
C
      SUBROUTINE MERGE(RELPG,LNGTHG,LNBR,RNBR,LINCON,IDBSIZ)
C   AUTHOR: RODGER STADEN
      INTEGER RELPG(IDBSIZ)
      INTEGER LNGTHG(IDBSIZ),LNBR(IDBSIZ),RNBR(IDBSIZ)
C
C   START AT LEFT END
      N=LNBR(LINCON)
      GO TO 22
21    CONTINUE
C   SET POINTER TO NEXT GEL TO RIGHT IN LIST
      N=NR
      IF(I1.GT.0)N=I2
22    CONTINUE
C   SET POINTER TO NEXT GEL TO RIGHT
      NR=RNBR(N)
      IF(NR.EQ.0)GO TO 30
C   HAVENT REACHED END YET
      I1=0
23    CONTINUE
C   ARE THESE 2 IN CORRECT ORDER IE N<=NR ?
      IF(RELPG(N).LE.RELPG(NR))GO TO 21
C   NOT IN ORDER SO CHAIN LEFT UNTIL CORRECTLY POSITIONED
C   THEN COME BACK TO THIS POINT AND CONTINUE
C   IF FIRST MOVE SAVE POSITION
      IF(I1.EQ.0)I2=N
      I1=1
C   EXCHANGE NEIGHBOURS
      M=RNBR(NR)
      IF(M.NE.0)LNBR(M)=N
      M=LNBR(N)
      IF(M.NE.0)RNBR(M)=NR
      RNBR(N)=RNBR(NR)
      RNBR(NR)=N
      LNBR(NR)=LNBR(N)
      LNBR(N)=NR
C   CHAIN BACK THRU LIST
      N=LNBR(NR)
      IF(N.EQ.0)GO TO 21
C   END NOT REACHED
      GO TO 23
30    CONTINUE
C  ALL DONE POINTER AT RIGHT GEL
      RNBR(LINCON)=N
      RETURN
      END
      SUBROUTINE MINCOM(SEQ1,IDIM1,SEQ2,IDIM2,SAV1,SAV2,SAV3,
     +IP,MINM,KBOUT)
C   AUTHOR: RODGER STADEN
C
      CHARACTER SEQ1(IDIM1),SEQ2(IDIM2)
      INTEGER SAV1(IP),SAV2(IP),SAV3(IP)
C
      IP1=IP
      IP=0
C
C   SITUATION 1
      NT1=IDIM2-MINM
      IES1=MINM-1
      ISS2=NT1+1
C
      DO 100 I=1,NT1
C
C   POINT TO FIRST CHAR-1 OF SEQ2
      ISS2=ISS2-1
C   POINT TO LAST CHAR SEQ1
      IES1=IES1+1
C
      N=0
C
      DO 200 J=1,IES1
C   STORE POINTER
      JJ=J
C
C   POINT TO SEQ2
      K=ISS2+J
C   TEST FOR EQUALITY
      IF(SEQ1(J).NE.SEQ2(K))GO TO 220
C   INCREMENT N
      N=N+1
      GO TO 200
220   CONTINUE
C   TEST FOR SUFFICENTLY LARGE N
      IF(N.GE.MINM)CALL SAVIT(N,J,K,IP,SAV1,SAV2,SAV3,IP1)
C   TEST FOR OVERFLOW
      IF(IP.GT.IP1)GO TO 5000
C   RESET N TO ZERO
      N=0
200   CONTINUE
C
C   GOOD SCORE AT END?
C   NEED TO INCREMENT POINTERS AS SAVIT EXPECTS TO BE POINTING AT NEXT
C   MISMATCH
      JJ=JJ+1
      KK=K+1
      IF(N.GE.MINM)CALL SAVIT(N,JJ,KK,IP,SAV1,SAV2,SAV3,IP1)
C   TEST FOR OVERFLOW
      IF(IP.GT.IP1)GO TO 5000
C
100   CONTINUE
C
C
C   SITUATION 2
      NT2=IDIM1-IDIM2+1
C
      DO 300 I=1,NT2
      N=0
C
      DO 400 J=1,IDIM2
C   SAVE POINTER
      JJ=J
C
C   SET POINTER TO SEQ1
      L=I+J-1
      IF(SEQ1(L).NE.SEQ2(J))GO TO 420
      N=N+1
      GO TO 400
420   CONTINUE
      IF(N.GE.MINM)CALL SAVIT(N,L,J,IP,SAV1,SAV2,SAV3,IP1)
C   TEST FOR OVERFLOW
      IF(IP.GT.IP1)GO TO 5000
      N=0
400   CONTINUE
      LL=L+1
      JJ=JJ+1
      IF(N.GE.MINM)CALL SAVIT(N,LL,JJ,IP,SAV1,SAV2,SAV3,IP1)
C   TEST FOR OVERFLOW
      IF(IP.GT.IP1)GO TO 5000
300   CONTINUE
C
C
C   SITUATION 3
      ISS1=IDIM1-IDIM2
C
      DO 500 I=1,NT1
C
C   POINT TO FIRST CHAR SEQ1
      K=ISS1+I
      IES2=IDIM2-I
      N=0
C
      DO 600 J=1,IES2
C   SAVE POINTER
      JJ=J
C
C   POINT TO SEQ1
      L=K+J
      IF(SEQ1(L).NE.SEQ2(J))GO TO  620
      N=N+1
      GO TO 600
620   CONTINUE
      IF(N.GE.MINM)CALL SAVIT(N,L,J,IP,SAV1,SAV2,SAV3,IP1)
C    TEST FOR OVERFLOW
      IF(IP.GT.IP1)GO TO 5000
      N=0
600   CONTINUE
C
      LL=L+1
      JJ=JJ+1
      IF(N.GE.MINM)CALL SAVIT(N,LL,JJ,IP,SAV1,SAV2,SAV3,IP1)
C   TEST FOR OVERFLOW
      IF(IP.GT.IP1)GO TO 5000
500   CONTINUE
C
      RETURN
5000  CONTINUE
C   OVERFLOW
C
      WRITE(KBOUT,1000)IP1
1000  FORMAT(/' TOO MANY MATCHES. LIMIT = ',I6)
      RETURN
      END
      SUBROUTINE ML(PC,PG,L,N,J)
      INTEGER PC(N),PG(N),L(N)
      DO 10 I = J,N-1
        PC(I) = PC(I+1)
        PG(I) = PG(I+1)
        L(I) = L(I+1)
10    CONTINUE
      END
      SUBROUTINE MSTLKL(SEQ,IDIM)
C   AUTHOR: RODGER STADEN
      CHARACTER SEQ(IDIM)
      CHARACTER CHARSU
      EXTERNAL CHARSU,INDEXS
      DO 100 I=1,IDIM
        J = INDEXS(SEQ(I),K)
        SEQ(I) = CHARSU(J)
100   CONTINUE
      END
      CHARACTER FUNCTION MUNOTP(IP)
C     AUTHOR RODGER STADEN
      CHARACTER PUP*26
      SAVE PUP
      DATA PUP/'CSTPAGNDEQBZHRKMILVFYW-X? '/
      MUNOTP = '-'
      IF((IP.GT.0).AND.(IP.LT.23))MUNOTP = PUP(IP:IP)
      END
      INTEGER FUNCTION NCDEP(RELPG,LNGTHG,LNBR,RNBR,IDBSIZ,IGEL,
     +STRAND,RREG)
      INTEGER RELPG(IDBSIZ)
      INTEGER LNGTHG(IDBSIZ),LNBR(IDBSIZ),RNBR(IDBSIZ)
      INTEGER RREG,STRAND
      NCDEP = 0
      N = 0
      I = IGEL
10    CONTINUE
      IF(I.NE.0) THEN
        IF(RELPG(I).LE.RREG) THEN
          IF(SIGN(1,LNGTHG(I)).EQ.STRAND) N = N + 1
          I = RNBR(I)
          GO TO 10
        END IF
      END IF
      NCDEP = N
      END
      SUBROUTINE PADCOP(SEQG,SEQG2,LG1,MG,L5,IS2,LG2,MAXGEL,IFAIL,
     +KBOUT,SEQC,IC1)
C   AUTHOR: RODGER STADEN
      PARAMETER (NDUBL = 4)
      CHARACTER SEQG(MAXGEL),SEQG2(MAXGEL),DUBBL(NDUBL),SEQC(MAXGEL)
      SAVE DUBBL
      DATA DUBBL/'D','B','V','H'/
      JC1 = IC1
C Make seqg2 from seqg placing L5 padding chars before position MG
C which is the start of the next block of identity. Try to put the
C padding either in line with consensus pads, or next to double
C codes. The positions in seqg are LG1 to MG-1. seqg2 needs to be long
C enough to be extended from IS2 to IS2 + L5 -1 + MGM1-LG1 +1
C ie we add L5 pads, plus the chars between and including  LG1 and MGM1
      IDONE=0
C   POINT TO END OF MISMATCH
      MGM1=MG-1
C   MAY BE NO CHARS TO COPY
      IF(MGM1.LT.LG1)GO TO 111
C  Next check added 26-2-91
      MAXREQ = IS2 + L5 - 1 + MGM1 - LG1 + 1
      IF((MGM1.GT.MAXGEL).OR.(MAXREQ.GT.MAXGEL)) THEN
        WRITE(KBOUT,1000)
1000    FORMAT(' Matching region too large for routine padcop,',
     +  ' alignment aborted')
        IFAIL=1
        RETURN
      END IF
      DO 110 J=LG1,MGM1
        IF(IDONE.LT.L5) THEN
          IF((JC1.GT.0).AND.(JC1.LT.MAXGEL)) THEN
          IF(SEQC(JC1).EQ.'*') THEN
            IS2 = IS2 + 1
            JC1 = JC1 + 1
            IDONE = IDONE + 1
            GO TO 109
          END IF
          END IF
          DO 108 M=1,NDUBL
            IF(SEQG(J).EQ.DUBBL(M)) THEN
              IS2 = IS2 + 1
              JC1 = JC1 + 1
              IDONE = IDONE + 1
              GO TO 109
            END IF
108       CONTINUE
109       CONTINUE
        END IF
        SEQG2(IS2) = SEQG(J)
        IS2 = IS2 + 1
        JC1 = JC1 + 1
110   CONTINUE
111   CONTINUE
C   ALL CHARS COPIED. ENOUGH PADDING?
      IF(IDONE.LT.L5)IS2=IS2+L5-IDONE
C   IS2 SHOULD NOW BE POINTING AT NEXT CHAR
C   ZERO LG2 TO SHOW CALLING ROUTINE COPYING DONE
      LG2=0
      IFAIL=0
      END
      SUBROUTINE PCON1(CHAR,CHRSUM)
C  AUTHOR RODGER STADEN
C  PART OF PROTEIN 'CONSENSUS' CALCULATION
      CHARACTER CHAR
      INTEGER CHRSUM
      INTEGER CTONUM
      EXTERNAL CTONUM
      K = CTONUM(CHAR)
      IF(K.NE.26)THEN
        IF(CHRSUM.EQ.0)THEN
          CHRSUM = K
        ELSE
          IF(K.NE.CHRSUM)CHRSUM = -1
        END IF
      END IF
      END
      SUBROUTINE PLC(RELPG,LNGTHG,LNBR,RNBR,IDBSIZ,LINCON,IGEL,
     +NCONTS,MARGL,MARGR,MARGB,MARGT,ISXMAX,ISYMAX)
      INTEGER RELPG(IDBSIZ),LNGTHG(IDBSIZ),LNBR(IDBSIZ),RNBR(IDBSIZ)
      YMIN = 0.
      YMAX = ISYMAX
      XMIN = 0.
      LENCON = 0
      DO 10 I = IDBSIZ-NCONTS,IDBSIZ-1
        LENCON = LENCON + RELPG(I)
10    CONTINUE
      XMAX = LENCON
      YINC = (YMAX-YMIN)/3.
      Y = 0.
      XF = XMIN
      N = 0
      CALL VECTOM
      CALL FRAME(MARGL,MARGR,MARGB,MARGT,ISXMAX,ISYMAX)
      DO 20 I = IDBSIZ-NCONTS,IDBSIZ-1
        N = N + 1
        XT = XF + RELPG(I)
        Y = Y + YINC
        CALL LINE(XF,XT,Y,Y,XMAX,XMIN,YMAX,YMIN,
     +  MARGL,MARGR,MARGB,MARGT,ISXMAX,ISYMAX)
        IF((IGEL.NE.0).AND.(I.EQ.LINCON)) THEN
          XZ = XF + RELPG(IGEL) + ABS(LNGTHG(IGEL))/2
          CALL LINE(XZ,XZ,YMAX,YMIN,XMAX,XMIN,YMAX,YMIN,
     +    MARGL,MARGR,MARGB,MARGT,ISXMAX,ISYMAX)
        END IF
        XF = XT
        IF(N.EQ.2) THEN
          N = 0
          Y = 0.
        END IF
20    CONTINUE
      CALL VT100M
      END
      SUBROUTINE PLTC(RELPG,LNGTHG,LNBR,RNBR,
     +IDBSIZ,LGEL,LREG,RREG,STRAND,
     +MARGL,MARGR,MARGB,MARGT,ISXMAX,ISYMAX,YMID,YINC,DEPTH)
C  AUTHOR: RODGER STADEN
      INTEGER RELPG(IDBSIZ)
      INTEGER LNGTHG(IDBSIZ),LNBR(IDBSIZ),RNBR(IDBSIZ)
      INTEGER RREG,STRAND,DEPTH
      XMIN = LREG
      XMAX = RREG
      YMAX = ISYMAX
      YMIN = 0.
      I = LGEL
5     CONTINUE
      IF(I.NE.0) THEN
        IF((RELPG(I)+ABS(LNGTHG(I))-1).LT.LREG) THEN
          I = RNBR(I)
          GO TO 5
        END IF
      END IF
      N = 0
10    CONTINUE
      IF(I.NE.0)THEN
        IF(RELPG(I).LE.RREG) THEN
          IF(SIGN(1,LNGTHG(I)).EQ.STRAND) THEN
            XF = MAX(RELPG(I),LREG)
            XT = MIN(ABS(LNGTHG(I))+RELPG(I)-1,RREG)
            N = N + 1
            IF(N.GT.DEPTH) N = 1
            YF = YMID + N * YINC
            CALL LINE(XF,XT,YF,YF,XMAX,XMIN,YMAX,YMIN,
     +      MARGL,MARGR,MARGB,MARGT,ISXMAX,ISYMAX)
          END IF
          I = RNBR(I)
          GO TO 10
        END IF
      END IF
      END
      SUBROUTINE PLTCON(RELPG,LNGTHG,LNBR,RNBR,IDBSIZ,
     +MARGL,MARGR,MARGB,
     +MARGT,ISXMAX,ISYMAX,LGEL,LREG,RREG,DEPTHP,DEPTHM)
      INTEGER DEPTHP,DEPTHM
      INTEGER RELPG(IDBSIZ)
      INTEGER LNGTHG(IDBSIZ),LNBR(IDBSIZ),RNBR(IDBSIZ)
      INTEGER RREG,STRAND
C  have window size margt starting at margb
C  depths depthp, depthm
      YMAX = ISYMAX
      YMIN = 0.
      XMIN = LREG
      XMAX = RREG
      RINC = YMAX / (DEPTHP + DEPTHM + 2)
      RMID =(DEPTHM+1) * RINC
      CALL VECTOM
      CALL FRAME(MARGL,MARGR,MARGB,MARGT,ISXMAX,ISYMAX)
      CALL LINE(XMIN,XMAX,RMID,RMID,XMAX,XMIN,YMAX,YMIN,
     +MARGL,MARGR,MARGB,MARGT,ISXMAX,ISYMAX)
      CALL TEXT(XMIN,RMID,'*',1,ISIZE,XMAX,XMIN,YMAX,YMIN,
     +MARGL,MARGR,MARGB,MARGT,ISXMAX,ISYMAX)
      CALL TEXT(XMAX,RMID,'*',1,ISIZE,XMAX,XMIN,YMAX,YMIN,
     +MARGL,MARGR,MARGB,MARGT,ISXMAX,ISYMAX)
      STRAND = 1
      YINC = RINC * STRAND
      CALL PLTC(RELPG,LNGTHG,LNBR,RNBR,
     +IDBSIZ,LGEL,LREG,RREG,STRAND,
     +MARGL,MARGR,MARGB,MARGT,ISXMAX,ISYMAX,RMID,YINC,DEPTHP)
      STRAND = -1
      YINC = RINC * STRAND
      CALL PLTC(RELPG,LNGTHG,LNBR,RNBR,
     +IDBSIZ,LGEL,LREG,RREG,STRAND,
     +MARGL,MARGR,MARGB,MARGT,ISXMAX,ISYMAX,RMID,YINC,DEPTHM)
      CALL VT100M
      END
      SUBROUTINE PLTQ(SEQ,IDIM2,
     +MARGL,MARGR,MARGB,MARGT,ISXMAX,ISYMAX)
      CHARACTER SEQ(IDIM2),T
      PARAMETER (Y0  = 0.,
     +           YP1 = 1.,
     +           YP2 = 2.,
     +           YM1 = -1.,
     +           YM2 = -2.)
      XMIN = 0.
      XMAX = IDIM2
      YMIN = YM2
      YMAX = YP2
      CALL VECTOM
      CALL FRAME(MARGL,MARGR,MARGB,MARGT,ISXMAX,ISYMAX)
      CALL LINE(XIN,XMAX,Y0,Y0,XMAX,XMIN,YMAX,YMIN,
     +MARGL,MARGR,MARGB,MARGT,ISXMAX,ISYMAX)
      I = 1
10    CONTINUE
      XF = I
      T = SEQ(I)
20    CONTINUE
      IF(SEQ(I).NE.T) THEN
        CALL GLEVEL(T,YF,YT,Y0,YP1,YP2,YM1,YM2)
        XT = I - 1
        CALL LINE(XF,XF,YF,YT,XMAX,XMIN,YMAX,YMIN,
     +  MARGL,MARGR,MARGB,MARGT,ISXMAX,ISYMAX)
        CALL LINE(XF,XT,YT,YT,XMAX,XMIN,YMAX,YMIN,
     +  MARGL,MARGR,MARGB,MARGT,ISXMAX,ISYMAX)
        CALL LINE(XT,XT,YF,YT,XMAX,XMIN,YMAX,YMIN,
     +  MARGL,MARGR,MARGB,MARGT,ISXMAX,ISYMAX)
        GO TO 10
      END IF
      I = I + 1
      IF(I.LT.IDIM2) GO TO 20
        CALL GLEVEL(T,YF,YT,Y0,YP1,YP2,YM1,YM2)
        XT = I
        CALL LINE(XF,XF,YF,YT,XMAX,XMIN,YMAX,YMIN,
     +  MARGL,MARGR,MARGB,MARGT,ISXMAX,ISYMAX)
        CALL LINE(XF,XT,YT,YT,XMAX,XMIN,YMAX,YMIN,
     +  MARGL,MARGR,MARGB,MARGT,ISXMAX,ISYMAX)
        CALL LINE(XT,XT,YF,YT,XMAX,XMIN,YMAX,YMIN,
     +  MARGL,MARGR,MARGB,MARGT,ISXMAX,ISYMAX)
      CALL VT100M
      END
      SUBROUTINE READN(IDEVN,N,NAME)
      CHARACTER NAME*(*)
      READ(IDEVN,REC=N)NAME
      RETURN
      END
      SUBROUTINE READR(IDEVR,N,RELPG,LNGTHG,LNBR,RNBR)
      INTEGER RELPG,RNBR
      INTEGER SWAPBO
      EXTERNAL SWAPBO
      READ(IDEVR,REC=N+1)RELPG,LNGTHG,LNBR,RNBR
      RELPG = SWAPBO(RELPG)
      LNGTHG = SWAPBO(LNGTHG)
      LNBR = SWAPBO(LNBR)
      RNBR = SWAPBO(RNBR)
      RETURN
      END
      SUBROUTINE READW(IDEVW,N,GEL,MAXGEL)
      CHARACTER GEL(MAXGEL)
      READ(IDEVW,REC=N)GEL
      RETURN
      END
      SUBROUTINE REMOVL(MATC,MATG,MATL,IP)
C   AUTHOR: RODGER STADEN
      INTEGER MATC(IP),MATG(IP),MATL(IP)
C
C   SET POINTER TO FIRST MATCH
      NMTCH=0
10    CONTINUE
C   POINT TO NEXT MATCH
      NMTCH=NMTCH+1
C   SORT MATCHES ON LENGTH
      IPP=IP-NMTCH+1
      CALL BUBBL3(MATL(NMTCH),MATG(NMTCH),MATC(NMTCH),IPP)
C   LOOK FOR END OF POSITIVES
      DO 20 I=NMTCH,IP
      J=I
20    IF(MATL(I).LT.1)GO TO 30
      J=J+1
30    CONTINUE
      IP=J-1
C   END OF POSITIVES AT IP
      IF(NMTCH.GE.IP)RETURN
      K1=MATC(NMTCH)
      K2=K1+MATL(NMTCH)-1
      K3=MATG(NMTCH)
      K4=K3+MATL(NMTCH)-1
C   POINT TO FIRST MATCH TO TEST
      K6=NMTCH+1
      DO 200 I=K6,IP
C   DO CONSENSUS FIRST
C   OVERLAP?
      IF(MATC(I).GT.K2)GO TO 100
      K5=MATC(I)+MATL(I)-1
      IF(K5.LT.K1)GO TO 100
C   DOES OVERLAP
C   WHICH END
      IF(K5.LE.K2)GO TO 80
C   LENGTH TO REDUCE MATCH BY IS IDELT
      IDELT=K2-MATC(I)+1
C   NEW LENGTH
      MATL(I)=MATL(I)-IDELT
C  MOVE LEFT ENDS
      MATC(I)=MATC(I)+IDELT
      MATG(I)=MATG(I)+IDELT
      GO TO 100
80    CONTINUE
C   LENGTH
      MATL(I)=K1-MATC(I)
100   CONTINUE
C   NOW LOOK FOR OVERLAPS WITH GEL
C   OVERLAP?
      IF(MATG(I).GT.K4)GO TO 200
      K5=MATG(I)+MATL(I)-1
      IF(K5.LT.K3)GO TO 200
C   DOES OVERLAP
C   WHICH END?
      IF(K5.LE.K4)GO TO 180
C   LENGTH TO REDUCE MATCH BY IS IDELT
      IDELT=K4-MATG(I)+1
C   NEW LENGTH
      MATL(I)=MATL(I)-IDELT
C   MOVE LEFT ENDS
      MATC(I)=MATC(I)+IDELT
      MATG(I)=MATG(I)+IDELT
      GO TO 200
180   CONTINUE
C   LENGTH
      MATL(I)=K3-MATG(I)
200   CONTINUE
      GO TO 10
      END
C     SAVIT
C
      SUBROUTINE SAVIT(N,J,K,IP,S1,S2,S3,IP1)
C   AUTHOR: RODGER STADEN
      INTEGER S1(IP1),S2(IP1),S3(IP1)
C
      IP=IP+1
C   TEST FOR OVERFLOW
      IF(IP.GT.IP1)RETURN
      S1(IP)=N
      S2(IP)=J-N
      S3(IP)=K-N
C
      RETURN
      END
      SUBROUTINE SCRENR(GEL,MAXGEL,STRING,NAME,FILNAM,
     +IDEV1,IDEV2,IDEV3,IDEV4,IDEV,KBIN,KBOUT,
     +IHELPS,IHELPE,HELPF,IDEVH)
      CHARACTER NAME*(*),FILNAM*(*),HELPF*(*)
      CHARACTER GEL(MAXGEL),STRING(60)
      INTEGER GNFFOF
      EXTERNAL GNFFOF
      CALL YESNO(INF,'Use file of file names',
     +IHELPS,IHELPE,HELPF,IDEVH,KBIN,KBOUT)
      IF(INF.LT.0) RETURN
      IF(INF.EQ.0) THEN
        FILNAM = ' '
        CALL OPENF1(IDEV1,FILNAM,0,IOK,KBIN,KBOUT,
     +  'File of gel reading names',
     +  IHELPS,IHELPE,HELPF,IDEVH)
        IF(IOK.NE.0) RETURN
        FILNAM = ' '
        CALL OPENF1(IDEV2,FILNAM,1,IOK,KBIN,KBOUT,
     +  'File for names of sequences that pass',
     +  IHELPS,IHELPE,HELPF,IDEVH)
        IF(IOK.NE.0) RETURN
      END IF
      FILNAM = ' '
      CALL OPENF1(IDEV3,FILNAM,0,IOK,KBIN,KBOUT,
     +'File name of recognition sequences',
     +IHELPS,IHELPE,HELPF,IDEVH)
      IF(IOK.NE.0) RETURN
C
      JGEL = 0
      IGEL = 0
1     CONTINUE
      IF(INF.EQ.1) THEN
31      CONTINUE
        MN = 0
        CALL GTSTR('Gel reading name',' ',NAME,MN,KBOUT,KBIN,INFLAG)
        IF(INFLAG.EQ.3) RETURN
        IF(INFLAG.EQ.2) RETURN
        IF(INFLAG.EQ.1) THEN
          CALL HELP2(IHELPS,IHELPE,FILEH,IDEVH,KBIN,KBOUT)
          GO TO 31
        END IF
      ELSE
        IOK = GNFFOF(IDEV1,NAME)
        IF(IOK.EQ.1) GO TO 100
        IF(IOK.NE.0) GO TO 1
      END IF
1002  FORMAT(A)
      JGEL = JGEL + 1
      WRITE(IDEV,*)'Processing', JGEL,' in batch'
      WRITE(IDEV,1003)NAME
1003  FORMAT(' Gel reading name ',A)
      IDIMG=MAXGEL
      CALL OPENRS(IDEV4,NAME,IOK,LRECL,2)
      IF(IOK.NE.0)THEN
        IF(INF.EQ.1) RETURN
        WRITE(KBOUT,*)' Error opening gel reading file'
        GO TO 1
      END IF
      CALL ARRFIM(IDEV4,GEL,IDIMG,KBOUT)
      CLOSE(UNIT=IDEV4)
2     CONTINUE
      IF(IDIMG.LT.1)THEN
        WRITE(KBOUT,*)' Gel reading too short to compare'
        GO TO 1
      END IF
      CALL MSTLKL(GEL,IDIMG)
3     CONTINUE
      READ(IDEV3,1005,END=6)STRING
1005  FORMAT(60A1)
C   FIND LENGTH OF STRING ASSUMING NO SPACES
      DO 4 I=1,60
        II=I
        IF(STRING(I).EQ.' ')GO TO 5
4     CONTINUE
5     CONTINUE
      II=II-1
      IF(II.GT.0)CALL FIND(GEL,IDIMG,STRING,II,JMATCH)
      IF(JMATCH.EQ.0)GO TO 3
C   A MATCH
      WRITE(IDEV,1007)JMATCH,(STRING(K),K=1,II)
1007  FORMAT(' Match at',I6,' with ',60A1)
      REWIND IDEV3
      GO TO 1
C   NO MATCH SO SAVE
6     CONTINUE
      WRITE(IDEV2,1002)NAME
      IGEL = IGEL + 1
      REWIND IDEV3
      GO TO 1
100   CONTINUE
      WRITE(KBOUT,*)'Batch finished'
      WRITE(KBOUT,*)JGEL,' compared and ',IGEL,' passed'
      RETURN
      END
      SUBROUTINE SCRENV(MAXGEL,
     +WORDP,WORDN,LPOWRC,POSNS,GELN,
     +SEQ,MAXSEQ,GEL,GELCOP,MATCH,
     +LENGTH,
     +SAVPS,SAVPG,SAVL,MAXMAT,CENDS,NENDS,MAXCON,CONST,
     +KBIN,KBOUT,IDEV1,IDEV2,IDEV3,IDEV4,IDEV,
     +IHELPS,IHELPE,HELPF,IDEVH,FILNAM,NAME,IOK)
      INTEGER POSNS(MAXSEQ),GELN(MAXGEL),WORDP(LPOWRC),SAVPS(MAXMAT)
      INTEGER SAVPG(MAXMAT),SAVL(MAXMAT)
      INTEGER WORDN(LPOWRC)
      CHARACTER FILNAM*(*),NAME*(*),HELPF*(*)
      CHARACTER GELCOP(MAXGEL)
      INTEGER CENDS(MAXCON)
      INTEGER NENDS(MAXCON)
      INTEGER CONST(LENGTH),CSTART
      CHARACTER SEQ(MAXSEQ),GEL(MAXGEL),MATCH(MAXGEL)
      INTEGER GNFFOF
      EXTERNAL GNFFOF
      JGEL = 0
      IGELS = 0
      CALL YESNO(INF,'Use file of file names',
     +IHELPS,IHELPE,HELPF,IDEVH,KBIN,KBOUT)
      IF(INF.LT.0) RETURN
      IF(INF.EQ.0) THEN
        FILNAM = ' '
        CALL OPENF1(IDEV1,FILNAM,0,IOK,KBIN,KBOUT,
     +  'File of gel reading names',
     +  IHELPS,IHELPE,HELPF,IDEVH)
        IF(IOK.NE.0) RETURN
        FILNAM = ' '
        CALL OPENF1(IDEV2,FILNAM,1,IOK,KBIN,KBOUT,
     +  'File for names of gel readings that pass',
     +  IHELPS,IHELPE,HELPF,IDEVH)
        IF(IOK.NE.0) RETURN
      END IF
      FILNAM = ' '
      CALL OPENF1(IDEV4,FILNAM,0,IOK,KBIN,KBOUT,
     +'File name of vector sequence',
     +IHELPS,IHELPE,HELPF,IDEVH)
      IF(IOK.NE.0) RETURN
      IDIM = MAXSEQ
      CALL ARRFIM(IDEV4,SEQ,IDIM,KBOUT)
      CLOSE(UNIT=IDEV4)
      MN = LENGTH*2
      MX = 50
      MINMAT = MAX(15,MN)
      CALL GETINT(MN,MX,MINMAT,
     +'Minimum initial match',
     +IVAL,KBIN,KBOUT,
     +IHELPS,IHELPE,HELPF,IDEVH,IOK)
      IF(IOK.NE.0) RETURN
      MINMAT = IVAL
      IDCEND=MAXCON
      CALL FNDCON(SEQ,IDIM,CENDS,NENDS,IDCEND,MAXCON,KBOUT)
C   IS THE VECTOR SEQUENCE IN THE CORRECT FORMAT WITH A TITLE AT THE FRONT?
      IF(IDCEND.EQ.0)THEN
        CENDS(1) = -19
        NENDS(1) = 1
        CENDS(2) = IDIM + 1
        IDCEND = 1
      END IF
C      WRITE(KBOUT,9999)
C9999  FORMAT(' VECTOR SEQUENCE REQUIRES A TITLE EG ',
C     1' <---M13MP7.001----->')
C      RETURN
C      END IF
      CALL BUSY(KBOUT)
C
C init hashing routines
C
      CALL INITE(CONST,CSTART,LENGTH)
      CALL ENCOF(SEQ,IDIM,CONST,CSTART,LENGTH,POSNS)
      CALL ENCONN(POSNS,IDIM,WORDP,WORDN,LPOWRC,LENGTH,1)
C
1     CONTINUE
      IF(INF.EQ.1) THEN
3       CONTINUE
        MN = 0
        CALL GTSTR('Gel reading name',' ',NAME,MN,KBOUT,KBIN,INFLAG)
        IF(INFLAG.EQ.3) RETURN
        IF(INFLAG.EQ.2) RETURN
        IF(INFLAG.EQ.1) THEN
          CALL HELP2(IHELPS,IHELPE,FILEH,IDEVH,KBIN,KBOUT)
          GO TO 3
        END IF
      ELSE
        IOK = GNFFOF(IDEV1,NAME)
        IF(IOK.EQ.1) GO TO 100
        IF(IOK.NE.0) GO TO 1
      END IF
      JGEL = JGEL + 1
      WRITE(IDEV,*)'Processing',JGEL,' in batch'
      WRITE(IDEV,1003)NAME
1003  FORMAT(' Gel reading name ',A)
      IDIMG=MAXGEL
      CALL OPENRS(IDEV3,NAME,IOK,LRECL,2)
      IF(IOK.NE.0)THEN
        IF(INF.EQ.1) RETURN
        WRITE(IDEV,*)' Gel reading file not found'
        GO TO 1
      END IF
      CALL ARRFIM(IDEV3,GEL,IDIMG,KBOUT)
      CLOSE(UNIT=IDEV3)
C  LONG ENOUGH ?
      IF(IDIMG.LT.MINMAT)THEN
        WRITE(IDEV,*)' Gel reading too short to compare'
        GO TO 1
      END IF
      CALL SQCOPY(GEL,GELCOP,IDIMG)
      ISTRAN=1
      IMATCH=0
2     CONTINUE
      CALL BUSY(KBOUT)
      CALL MSTLKL(GEL,IDIMG)
      CALL ENCO(GEL,IDIMG,GELN,CONST,LENGTH)
      WRITE(IDEV,1009)ISTRAN
1009  FORMAT(' Searching strand',I6)
      IDSAV=MAXMAT
      CALL CFGEL(GELN,IDIMG,POSNS,IDIM,WORDP,WORDN,LENGTH,LPOWRC,
     +SAVPG,SAVPS,SAVL,
     +IDSAV,SEQ,GELCOP,MINMAT,IFAIL,KBOUT)
      IF(IDSAV.GT.0) THEN
        IMATCH=1
        CALL DISMAT(SEQ,IDIM,GELCOP,IDIMG,SAVPS,SAVPG,IDSAV,
     +  CENDS,NENDS,IDCEND,MAXCON,IDEV,MATCH)
      END IF
      IF(ISTRAN.EQ.1) THEN
        CALL SQREV(GELCOP,IDIMG)
        CALL SQCOM(GELCOP,IDIMG)
        CALL SQCOPY(GELCOP,GEL,IDIMG)
        ISTRAN = 2
        GO TO 2
      END IF
      IF(IMATCH.EQ.0) THEN
        WRITE(IDEV2,1010)NAME
        IGELS = IGELS + 1
      END IF
      GO TO 1
1010  FORMAT(A)
100   CONTINUE
      WRITE(KBOUT,*)'Batch finished'
      WRITE(KBOUT,*)JGEL,' compared and ',IGELS,' passed'
      RETURN
      END
      SUBROUTINE SHIFTC(RELPG,LNGTHG,LNBR,RNBR,NGELS,NCONTS,IDEVR,
     +IDBSIZ,IGN,NCONT,DIST)
C  AUTHOR: RODGER STADEN
C  SHIFTS PART OF A CONTIG FORM GEL IGN TO RIGHT END
C  CONTIG LINE NUMBER IF NCONT
      INTEGER RELPG(IDBSIZ)
      INTEGER LNGTHG(IDBSIZ),LNBR(IDBSIZ),RNBR(IDBSIZ)
      INTEGER DIST,CLEN
      EXTERNAL CLEN
      I = IGN
10    CONTINUE
      IF(I.NE.0)THEN
        RELPG(I) = RELPG(I) + DIST
        CALL WRITER(IDEVR,I,RELPG(I),LNGTHG(I),LNBR(I),RNBR(I))
        I = RNBR(I)
        GO TO 10
      END IF
C  UPDATE CONTIG LENGTH
      L = CLEN(RELPG,LNGTHG,LNBR,RNBR,NGELS,NCONTS,
     +IDBSIZ,IGN)
      RELPG(NCONT) = L
      CALL WRITER(IDEVR,NCONT,RELPG(NCONT),LNGTHG(NCONT),
     +LNBR(NCONT),RNBR(NCONT))
      END
      SUBROUTINE SLIDER(SEQ1,IDC,SEQ2,IDIM2,MS1,MS2,MAXPG,MAXPC,MINSLI,
     +MATL,MATC,MATG,IP)
C   AUTHOR: RODGER STADEN
      CHARACTER SEQ1(IDC),SEQ2(IDIM2)
      INTEGER MATL(IP),MATC(IP),MATG(IP),P1S,P1,P2
      IP1 = IP
      IP = 0
C   LEFT END S2 RELATIVE S1 - MAX PADS -2 READY FOR LOOP
      P1S = MS1 - MS2 - MAXPC - 1
C   TRY NSLIDE START POSNS FOR SEQ2
      DO 100 I=1,MAXPG+MAXPC+1
C       POINT TO SEQ1 START
        P1S = P1S + 1
C       POINT TO CURRENT SEQ1 POSN
        P1 = P1S
        N = 0
C       COMPARE WHOLE LENGTH OF SEQ2 (IF P1 WITHIN RANGE)
        DO 50 J=1,IDIM2
          P2 = J
          P1 = P1 + 1
          IF(P1.LT.1)GO TO 50
C         OFF RIGHT END? IF SO MAY HAVE BEEN A MATCH
          IF(P1.GT.IDC)GO TO 40
          IF(SEQ1(P1).EQ.SEQ2(P2))GO TO 45
40        CONTINUE
          IF(N.GE.MINSLI)CALL SAVIT(N,P1,P2,IP,MATL,MATC,MATG,IP1)
          N = 0
          GO TO 50
45        CONTINUE
          N = N + 1
50      CONTINUE
C       GOOD SCORE AT END? NEED TO INCREMENT POINTERS FOR SAVIT
        P1 = P1 + 1
        P2 = P2 + 1
        IF(N.GE.MINSLI)CALL SAVIT(N,P1,P2,IP,MATL,MATC,MATG,IP1)
100   CONTINUE
      END
      SUBROUTINE SUBS(SEQ,IDIMS,FROM,TO)
      CHARACTER SEQ(IDIMS),FROM,TO
C   AUTHOR RODGER STADEN
      DO 10 I = 1,IDIMS
        IF(SEQ(I).EQ.FROM) SEQ(I) = TO
10    CONTINUE
      END
      SUBROUTINE SUMMAR(RELPG,LNGTHG,LNBR,RNBR,NGELS,NCONTS,
     +SEQ1,IDIM1,GEL,LREG,RREG,IGELC,PERCD,IDBSIZ,CHARS,
     +ID1,CHRSIZ,MAXGL2,IDEVW,MAXGEL,LINOU1,LINOU2,MXGOOD)
C   AUTHOR: RODGER STADEN
      INTEGER RELPG(IDBSIZ),CHRSIZ
      INTEGER LNGTHG(IDBSIZ),LNBR(IDBSIZ),RNBR(IDBSIZ)
      CHARACTER SEQ1(IDIM1)
      CHARACTER GEL(MAXGEL)
      INTEGER LREG,RREG,LSEQNO,POSN,Y,POSN1
      INTEGER GELC
      CHARACTER LINOU1(MAXGEL),LINOU2(MAXGEL),GTCONC
      INTEGER CHARS(CHRSIZ,ID1,MAXGL2)
      EXTERNAL INDEXS,LWRAPS,GTCONC
C 28-7-91 added extra parameter: mxgood is the maximum reading
C length for which we have confidence, so only the parts of
C reads 1 to mxgood will be included in the calculation
C   SET INITIAL VALUES
C hard to understand this very old code! rewrite it.
C we have a summing array of twice the length of the longest sequence
C posn is posn in contig of next to write, lseqno is current posn in contig
C we write when lseqno-posn ge the length of the longest seq
      POSN=LREG
      GELC=IGELC
      LINLEN=MAXGEL
      LSEQNO=RELPG(GELC)
      IEND=0
      DO 40 I=1,MAXGL2
      DO 40 J=1,ID1
      DO 40 K=1,CHRSIZ
        CHARS(K,J,I)=0
40    CONTINUE
50    CONTINUE
      ISS=1
      IF(LNGTHG(GELC).LT.0)ISS=2
      CALL READW(IDEVW,GELC,GEL,MAXGEL)
C     LOOP FOR RELEVANT ELEMENTS THIS GEL
C only use mxgood characters from start of read
C
      IF(ISS.EQ.1) THEN
        N = MIN(MXGOOD,ABS(LNGTHG(GELC)))
        IF(LSEQNO.LT.LREG)LSEQNO=LREG
        IS = LSEQNO-RELPG(GELC)+1
      ELSE
C     LOOP FOR RELEVANT ELEMENTS THIS GEL
C only use mxgood characters from start of read (right end for these)
C
        IREND = RELPG(GELC) - LNGTHG(GELC) + 1
        IF (MXGOOD.LT.MAXGEL) THEN
          LSEQNO = IREND - MXGOOD + 1
        ELSE
          LSEQNO = RELPG(GELC)
        END IF
        LSEQNO = MAX(LSEQNO,LREG)
        IS = LSEQNO - RELPG(GELC) + 1
        N  = ABS(LNGTHG(GELC))
      END IF
      DO 70 I=IS,N
        JJ = INDEXS(GEL(I),JSCORE)
        JJJ = LWRAPS(LSEQNO,MAXGL2)
        CHARS(JJ,ISS,JJJ) = CHARS(JJ,ISS,JJJ) + JSCORE
        LSEQNO = LSEQNO + 1
70    CONTINUE
      IF(RNBR(GELC).EQ.0)GO TO 200
      GELC=RNBR(GELC)
      LSEQNO=RELPG(GELC)
      IF(LSEQNO.GT.RREG)GO TO 200
C     ENOUGH TO OUTPUT?
      Y=LSEQNO-POSN
      IF(Y.GE.MAXGEL)GO TO 210
      GO TO 50
200   CONTINUE
C     SET FLAG TO SHOW END REACHED
      IEND=1
      LINLEN=MAXGEL
      Y=RREG-POSN
      IF(Y.LT.MAXGEL)LINLEN=Y+1
210   CONTINUE
C   SET POINTER TO SEQ1
      POSN1=POSN-1
C   PREPARE NEXT SECTION OF CHARS FOR OUTPUT
      DO 230 I=1,LINLEN
        JJJ = LWRAPS(POSN,MAXGL2)
        LINOU1(I) = GTCONC(CHARS(1,1,JJJ),CHRSIZ,PERCD)
        LINOU2(I) = GTCONC(CHARS(1,2,JJJ),CHRSIZ,PERCD)
        DO 250 J=1,CHRSIZ
          CHARS(J,1,JJJ)=0
          CHARS(J,2,JJJ)=0
250     CONTINUE
        POSN=POSN+1
230   CONTINUE
C
C   COMPARE STRANDS
C
      DO 500 I=1,LINLEN
C        WRITE(*,*)I,LINOU1(I),LINOU2(I)
        POSN1=POSN1+1
        IF(LINOU1(I).EQ.LINOU2(I)) THEN
          IF(LINOU1(I).EQ.'-') THEN
            SEQ1(POSN1) = '3'
            GO TO 500
          END IF
          IF(LINOU1(I).EQ.'*') THEN
            SEQ1(POSN1) = '3'
            GO TO 500
          END IF
          SEQ1(POSN1) = '0'
        ELSE
          IF((LINOU1(I).EQ.'*').AND.(LINOU2(I).EQ.'-')) THEN
            SEQ1(POSN1) = '3'
            GO TO 500
          END IF
          IF((LINOU2(I).EQ.'*').AND.(LINOU1(I).EQ.'-')) THEN
            SEQ1(POSN1) = '3'
            GO TO 500
          END IF
          IF((LINOU1(I).NE.'-').AND.(LINOU1(I).NE.'*')) THEN
            SEQ1(POSN1) = '1'
            IF((LINOU2(I).NE.'-').AND.(LINOU2(I).NE.'*')) 
     +      SEQ1(POSN1) = '4'
              GO TO 500
          END IF
          IF((LINOU2(I).NE.'-').AND.(LINOU2(I).NE.'*')) THEN
            SEQ1(POSN1) = '2'
            IF((LINOU1(I).NE.'-').AND.(LINOU1(I).NE.'*'))
     +      SEQ1(POSN1) = '4'
            GO TO 500
          END IF
        END IF
500   CONTINUE
      IF(POSN.GT.RREG)RETURN
      IF((IEND.EQ.1).AND.(POSN.LE.RREG))GO TO 200
C   ANY MORE MAXGEL CHAR  LENGTHS TO OUTPUT
      Y=LSEQNO-POSN
      IF(Y.LT.MAXGEL)GO TO 50
C   FINISHED COMPLETELY?
      GO TO 210
      END
C      SUMMER
C
C   SUBROUTINE TO PRODUCE A CONSENSUS FROM LINED UP GEL READINGS
      SUBROUTINE SUMMER(RELPG,LNGTHG,LNBR,RNBR,NGELS,NCONTS,
     1SEQ1,IDIM1,GEL,LREG,RREG,IGELC,IDBSIZ,CHARS,CHRSIZ,MAXGL2,
     +IDEVW,MAXGEL,IDM,PERCD)
C   AUTHOR: RODGER STADEN
      INTEGER RELPG(IDBSIZ),CHRSIZ
      INTEGER LREG,RREG,LSEQNO,POSN,Y
      INTEGER LNGTHG(IDBSIZ),LNBR(IDBSIZ),RNBR(IDBSIZ)
      CHARACTER GEL(MAXGEL)
      INTEGER GELC
      CHARACTER SEQ1(IDIM1)
      INTEGER CHARS(CHRSIZ,MAXGL2)
      CHARACTER MUNOTP,GTCONC
      EXTERNAL MUNOTP,INDEXS,GTCONC,LWRAPS
C
C   SET INITIAL VALUES
      POSN=LREG
      GELC=IGELC
      LINLEN=MAXGEL
      LSEQNO=RELPG(GELC)
      IEND=0
      IPSEQ1=0
C
C   ZERO ARRAY
      DO 40 I=1,MAXGL2
      DO 40 J=1,CHRSIZ
      CHARS(J,I)=0
40    CONTINUE
50    CONTINUE
      CALL READW(IDEVW,GELC,GEL,MAXGEL)
C   LOOP FOR RELEVANT ELEMENTS THIS GEL
      N=ABS(LNGTHG(GELC))
      IF(LSEQNO.LT.LREG)LSEQNO=LREG
      IS=(LSEQNO-RELPG(GELC))+1
****************************
      IF(IDM.EQ.26)THEN
        DO 51 I = IS,N
          JJJ=(MOD(LSEQNO,MAXGL2))
          IF(JJJ.EQ.0)JJJ=MAXGL2
          CALL PCON1(GEL(I),CHARS(1,JJJ))
          LSEQNO = LSEQNO + 1
51      CONTINUE
      ELSE
****************************
      DO 70 I=IS,N
        JJ = INDEXS(GEL(I),JSCORE)
        JJJ = LWRAPS(LSEQNO,MAXGL2)
        CHARS(JJ,JJJ) = CHARS(JJ,JJJ) + JSCORE
        LSEQNO = LSEQNO + 1
70    CONTINUE
      END IF
C
C   LOOK AT NEXT GEL TO RIGHT
      IF(RNBR(GELC).EQ.0)GO TO 200
      GELC=RNBR(GELC)
C   RESET LSEQNO
      LSEQNO=RELPG(GELC)
C   IS THIS OVER END?
      IF(LSEQNO.GT.RREG)GO TO 200
C   ENOUGH TO OUTPUT?
      Y=LSEQNO-POSN
      IF(Y.GE.MAXGEL)GO TO 210
      GO TO 50
200   CONTINUE
C   SET FLAG TO SHOW END REACHED
      IEND=1
C   NEED TO SUM AND OUTPUT
      LINLEN=MAXGEL
      Y=RREG-POSN
      IF(Y.LT.MAXGEL)LINLEN=Y+1
210   CONTINUE
C   SUM NEXT SECTION OF CHARS
      IF(IDM.EQ.26)THEN
        DO 211 I = 1,LINLEN
          IPSEQ1 = IPSEQ1 + 1
          SEQ1(IPSEQ1) = '-'
          JJJ = MOD(POSN,MAXGL2)
          IF(JJJ.EQ.0)JJJ = MAXGL2
          SEQ1(IPSEQ1) = MUNOTP(CHARS(1,JJJ))
          CHARS(1,JJJ) = 0
          POSN = POSN + 1
211     CONTINUE
      ELSE
      DO 230 I=1,LINLEN
        IPSEQ1=IPSEQ1+1
        ISUM=0
        JJJ = LWRAPS(POSN,MAXGL2)
        SEQ1(IPSEQ1) = GTCONC(CHARS(1,JJJ),CHRSIZ,PERCD)
        CALL FILLI(CHARS(1,JJJ),CHRSIZ,0)
        POSN = POSN + 1
230   CONTINUE
      END IF
C
C
C   ANY MORE TO OUTPUT?
      IF(POSN.GT.RREG)RETURN
      IF((IEND.EQ.1).AND.(POSN.LE.RREG))GO TO 200
C   ANY MORE MAXGLEL CHAR  LENGTHS TO OUTPUT
      Y=LSEQNO-POSN
      IF(Y.LT.MAXGEL)GO TO 50
C   FINISHED COMPLETELY?
      GO TO 210
      END
      SUBROUTINE SUMSS(RELPG,LNGTHG,LNBR,RNBR,NGELS,NCONTS,
     +SEQ1,IDIM1,GEL,LREG,RREG,IGELC,PERCD,IDBSIZ,CHARS,
     +ID1,CHRSIZ,MAXGL2,IDEVW,MAXGEL,LINOU1,LINOU2)
C   AUTHOR: RODGER STADEN
      INTEGER RELPG(IDBSIZ),CHRSIZ
      INTEGER LNGTHG(IDBSIZ),LNBR(IDBSIZ),RNBR(IDBSIZ)
      CHARACTER SEQ1(IDIM1)
      CHARACTER GEL(MAXGEL)
      INTEGER LREG,RREG,LSEQNO,POSN,Y,POSN1
      INTEGER GELC
      CHARACTER LINOU1(MAXGEL),LINOU2(MAXGEL),GTCONC
      INTEGER CHARS(CHRSIZ,ID1,MAXGL2)
      EXTERNAL INDEXS,LWRAPS,GTCONC
C
C Routine to calculate a consensus. Only if the two strands agree
C is a non dash character assigned.
C
      POSN=LREG
      GELC=IGELC
      LINLEN=MAXGEL
      LSEQNO=RELPG(GELC)
      IEND=0
      DO 40 I=1,MAXGL2
      DO 40 J=1,ID1
      DO 40 K=1,CHRSIZ
        CHARS(K,J,I)=0
40    CONTINUE
50    CONTINUE
      ISS=1
      IF(LNGTHG(GELC).LT.0)ISS=2
      CALL READW(IDEVW,GELC,GEL,MAXGEL)
C     LOOP FOR RELEVANT ELEMENTS THIS GEL
      N=ABS(LNGTHG(GELC))
      IF(LSEQNO.LT.LREG)LSEQNO=LREG
      IS=LSEQNO-RELPG(GELC)+1
      DO 70 I=IS,N
        JJ = INDEXS(GEL(I),JSCORE)
        JJJ = LWRAPS(LSEQNO,MAXGL2)
        CHARS(JJ,ISS,JJJ) = CHARS(JJ,ISS,JJJ) + JSCORE
        LSEQNO = LSEQNO + 1
70    CONTINUE
      IF(RNBR(GELC).EQ.0)GO TO 200
      GELC=RNBR(GELC)
      LSEQNO=RELPG(GELC)
      IF(LSEQNO.GT.RREG)GO TO 200
C     ENOUGH TO OUTPUT?
      Y=LSEQNO-POSN
      IF(Y.GE.MAXGEL)GO TO 210
      GO TO 50
200   CONTINUE
C     SET FLAG TO SHOW END REACHED
      IEND=1
      LINLEN=MAXGEL
      Y=RREG-POSN
      IF(Y.LT.MAXGEL)LINLEN=Y+1
210   CONTINUE
C   SET POINTER TO SEQ1
      POSN1=POSN-1
C   PREPARE NEXT SECTION OF CHARS FOR OUTPUT
      DO 230 I=1,LINLEN
        JJJ = LWRAPS(POSN,MAXGL2)
        LINOU1(I) = GTCONC(CHARS(1,1,JJJ),CHRSIZ,PERCD)
        LINOU2(I) = GTCONC(CHARS(1,2,JJJ),CHRSIZ,PERCD)
        DO 250 J=1,CHRSIZ
          CHARS(J,1,JJJ)=0
          CHARS(J,2,JJJ)=0
250     CONTINUE
        POSN=POSN+1
230   CONTINUE
C
C   Compare the strands. If they the same then set the consensus
C   accordingly, otherwise set it to - so no edits are made.
C
      DO 500 I=1,LINLEN
        POSN1=POSN1+1
        IF(LINOU1(I).EQ.LINOU2(I)) THEN
          SEQ1(POSN1) = LINOU1(I)
        ELSE
          SEQ1(POSN1) = '-'
        END IF
500   CONTINUE
      IF(POSN.GT.RREG)RETURN
      IF((IEND.EQ.1).AND.(POSN.LE.RREG))GO TO 200
C   ANY MORE MAXGEL CHAR  LENGTHS TO OUTPUT
      Y=LSEQNO-POSN
      IF(Y.LT.MAXGEL)GO TO 50
C   FINISHED COMPLETELY?
      GO TO 210
      END
      SUBROUTINE TPCHEK(PC,PG,L,N)
      INTEGER PC(N),PG(N),L(N)
C     AUTHOR RODGER STADEN
C     IF OVERLAPPING BLOCKS ARE FOUND REMOVE THE SHORTER ONE
C     THEN REMOVE LARGE GAPS AT ENDS (THOSE AS LARGE AS THE END BLOCK)
      K1 = 2
1     CONTINUE
      DO 10 I = K1,N
        J1 = I
        IF(PC(I).LE.PC(I-1)) GO TO 20
        IF(PG(I).LE.PG(I-1)) GO TO 20
10    CONTINUE
C     REMOVE LARGE GAPS FROM ENDS
C     THIS RULE OF THUMB COULD BE CHANGED TO USE A DIFFERENCE
C     BETWEEN THE NUMBERS OF MISMATCHING CHARACTERS
      IF(N.GT.1) THEN
        K1 = PC(2) - PC(1) - L(1) 
        J1 = PG(2) - PG(1) - L(1)
        IF(MAX(K1,J1).GT.L(1)) THEN
          CALL ML(PC,PG,L,N,1)
          N = N - 1
        END IF
        IF(N.GT.1) THEN
          K1 = PC(N) - PC(N-1) - L(N-1)
          J1 = PG(N) - PG(N-1) - L(N-1)
          IF(MAX(K1,J1).GT.L(N)) THEN
            CALL ML(PC,PG,L,N,N)
            N = N - 1
          END IF
        END IF
      END IF
      RETURN
20    CONTINUE
      IF(L(J1-1).GT.L(J1)) THEN
        CALL ML(PC,PG,L,N,J1)
      ELSE
        CALL ML(PC,PG,L,N,J1-1)
      END IF
C  Until 25-11-90 next line was k1=j1 but this does not deal with all 
C  cases: when a line is deleted we must compare it with the previous
C  one before dealing with the rest, because it could be left of that
C   one as well!
      K1 = MAX(2,J1-1)
      N = N - 1
      GO TO 1
      END
      SUBROUTINE WRITEN(IDEVN,N,NAME)
      CHARACTER NAME*(*)
      WRITE(IDEVN,REC=N)NAME
      RETURN
      END
      SUBROUTINE WRITER(IDEVR,N,RELPG,LNGTHG,LNBR,RNBR)
      INTEGER RELPG,RNBR
      INTEGER SWAPBO
      EXTERNAL SWAPBO
      WRITE(IDEVR,REC=N+1)SWAPBO(RELPG),SWAPBO(LNGTHG),
     +SWAPBO(LNBR),SWAPBO(RNBR)
      RETURN
      END
      SUBROUTINE WRITEW(IDEVW,N,GEL,MAXGEL)
      CHARACTER GEL(MAXGEL)
      WRITE(IDEVW,REC=N)GEL
      RETURN
      END
      SUBROUTINE XHSAP(RELPG,LNGTHG,LNBR,RNBR,
     +IDBSIZ,NCONTS,LLINOI,LINCNI,LREG,RREG,
     +WINDOW,GWIND,LENCON,DEPTHP,DEPTHM,
     +MARGL,MARGR,MARGB,MARGT,MAXOPT,ISXMAX,ISYMAX,KBIN,IDEV,
     +KBOUT,GEL,GEL2,IDEV2,IDEV3,LINLEN,PERCD,MAXGEL,IDM,
     +SEQ1,IDIM1,NGELS,TEMP3,CHRSIZ,MAXGL2,LINOU1,LINOU2,
     +NOPT1,NOPT2,NOPT3,
     +IHELPS,IHELPE,HELPF,IDEVH,MXGOOD)
C  AUTHOR: RODGER STADEN
      INTEGER RELPG(IDBSIZ),WINDOW,CHRSIZ,GWIND
      INTEGER LNGTHG(IDBSIZ),LNBR(IDBSIZ),RNBR(IDBSIZ)
      INTEGER MARGB(MAXOPT),MARGT(MAXOPT)
      INTEGER RREG,DEPTHP,DEPTHM,STRAND,CHNRP1,HQN
      INTEGER TEMP3(2,CHRSIZ,MAXGL2)
      CHARACTER GEL(MAXGEL),GEL2(MAXGEL)
      CHARACTER TERM,TUPPER,NAMARC*16,HELPF*(*)
      CHARACTER SEQ1(IDIM1),LINOU1(MAXGEL),LINOU2(MAXGEL)
      EXTERNAL NOPWIN,CWORLD,TUPPER,CHNRP1,HQN
C  nopt1 = single contig
C  nopt2 = all contigs
C  nopt3 = scan
10    CONTINUE
      LLINO = LLINOI
      LINCON = LINCNI
      LOCLR = 0
      LOCRR = 0
      CALL BPAUSE(KBIN,KBOUT,IOK)
      CALL CLEARV
      CALL XHAIRR(ISXMAX,ISYMAX,IX,IY,TERM,DBTDUX,DBTDUY)
      CALL VT100M
      INFLAG = HQN(TERM)
      IF(INFLAG.EQ.1) THEN
        CALL HELP2(IHELPS,IHELPE,HELPF,IDEVH,KBIN,KBOUT)
        GO TO 10
      END IF
      IF(INFLAG.EQ.2) RETURN
      IF(INFLAG.EQ.3) RETURN
      NOPT = NOPWIN(IY,MARGB,MARGT,MAXOPT)
      TERM = TUPPER(TERM)
      IF(NOPT.EQ.0) RETURN
      IF(NOPT.EQ.NOPT3) THEN
        IF(TERM.EQ.'S') THEN
          XMIN = LREG
          XMAX = RREG
          X = CWORLD(IX,MARGL,MARGR,XMIN,XMAX)
          LOCLR = MAX(LREG,NINT(X)-WINDOW)
          LOCRR = MIN(RREG,NINT(X)+WINDOW-1)
          IF(LOCLR.NE.0) THEN
            CALL DSPLAY(RELPG,LNGTHG,LNBR,RNBR,
     +      GEL,LLINO,LINCON,LOCLR,LOCRR,GEL2,I1,I2,0,I,
     +      IDBSIZ,IDEV,KBOUT,
     +      IDEV2,IDEV3,LINLEN,PERCD,MAXGEL,IDM)
            GO TO 10
          END IF
        END IF
        IF((TERM.EQ.'N').OR.(TERM.EQ.'Z').OR.(TERM.EQ.'I')) GO TO 10
      END IF
      IF(NOPT.EQ.NOPT1) THEN
        STRAND = 1
        CALL FDPTH(RELPG,LNGTHG,LNBR,RNBR,
     +  IDBSIZ,LLINO,LREG,RREG,LENCON,STRAND,DEPTHP)
        IF(DEPTHP.LT.0) RETURN
        STRAND = -1
        CALL FDPTH(RELPG,LNGTHG,LNBR,RNBR,
     +  IDBSIZ,LLINO,LREG,RREG,LENCON,STRAND,DEPTHM)
        IF(DEPTHM.LT.0) RETURN
        YMAX = ISYMAX
        YMIN = 0.
        XMIN = LREG
        XMAX = RREG
        RINC = ISYMAX / (DEPTHP + DEPTHM + 2)
        RMID =(DEPTHM+1) * RINC
        X = CWORLD(IX,MARGL,MARGR,XMIN,XMAX)
        Y = CWORLD(IY,MARGB(NOPT),MARGT(NOPT),YMIN,YMAX)
        IF(TERM.EQ.'I') THEN
          STRAND = 1
          YINC = RINC * STRAND
          CALL IPLTC(RELPG,LNGTHG,LNBR,RNBR,
     +    IDBSIZ,LLINO,LREG,RREG,STRAND,
     +    MARGL,MARGR,MARGB(NOPT),MARGT(NOPT),
     +    ISXMAX,ISYMAX,RMID,YINC,DEPTHP,X,Y,
     +    KBOUT,IGEL,ICLOSE)
          IF(ICLOSE.EQ.1) THEN
            STRAND = -1
            YINC = RINC * STRAND
            CALL IPLTC(RELPG,LNGTHG,LNBR,RNBR,
     +      IDBSIZ,LLINO,LREG,RREG,STRAND,
     +      MARGL,MARGR,MARGB(NOPT),MARGT(NOPT),
     +      ISXMAX,ISYMAX,RMID,YINC,DEPTHM,X,Y,
     +      KBOUT,IGEL,ICLOSE)
          END IF
          IF(ICLOSE.EQ.1) GO TO 10
          CALL READN(IDEV3,IGEL,NAMARC)
          WRITE(IDEV,1006)NAMARC,IGEL,RELPG(IGEL),LNGTHG(IGEL)
1006      FORMAT
     +    ( ' Name ',A,' Number ',I6,' Rel. Posn. ',I7,' Length ',I5)
          GO TO 10
        END IF
        IF(TERM.EQ.'Z') THEN
          STRAND = 1
          YINC = RINC * STRAND
          CALL IPLTC(RELPG,LNGTHG,LNBR,RNBR,
     +    IDBSIZ,LLINO,LREG,RREG,STRAND,
     +    MARGL,MARGR,MARGB(NOPT),MARGT(NOPT),
     +    ISXMAX,ISYMAX,RMID,YINC,DEPTHP,X,Y,
     +    KBOUT,IGEL,ICLOSE)
          IF(ICLOSE.EQ.1) THEN
            STRAND = -1
            YINC = RINC * STRAND
            CALL IPLTC(RELPG,LNGTHG,LNBR,RNBR,
     +      IDBSIZ,LLINO,LREG,RREG,STRAND,
     +      MARGL,MARGR,MARGB(NOPT),MARGT(NOPT),
     +      ISXMAX,ISYMAX,RMID,YINC,DEPTHM,X,Y,
     +      KBOUT,IGEL,ICLOSE)
          END IF
          IF(IGEL.EQ.0) GO TO 10
          CALL CLEARG
          CALL PLC(RELPG,LNGTHG,LNBR,RNBR,IDBSIZ,LINCON,IGEL,
     +    NCONTS,MARGL,MARGR,MARGB(NOPT2),MARGT(NOPT2),ISXMAX,ISYMAX)
          LREG = MAX(1,RELPG(IGEL)-GWIND)
          RREG = MIN(RELPG(LINCON),RELPG(IGEL)+GWIND)
          LLINO = LNBR(LINCON)
          LLINOI = LLINO
          LINCNI = LINCON
          LENCON = RREG - LREG + 1
          CALL FDEPTH(RELPG,LNGTHG,LNBR,RNBR,
     +    IDBSIZ,LLINO,LREG,RREG,LENCON,
     +    MARGL,MARGR,MARGB(NOPT1),MARGT(NOPT1),ISXMAX,ISYMAX)
          GO TO 10
        END IF
        IF(TERM.EQ.'S') THEN
          LOCLR = MAX(LREG,NINT(X)-WINDOW)
          LOCRR = MIN(RREG,NINT(X)+WINDOW-1)
          IF(LOCLR.NE.0) THEN
            CALL DSPLAY(RELPG,LNGTHG,LNBR,RNBR,
     +      GEL,LLINO,LINCON,LOCLR,LOCRR,GEL2,I1,I2,0,I,
     +      IDBSIZ,IDEV,KBOUT,
     +      IDEV2,IDEV3,LINLEN,PERCD,MAXGEL,IDM)
            GO TO 10
          END IF
        END IF
        IF(TERM.EQ.'N') THEN
          LOCLR = MAX(LREG,NINT(X)-WINDOW)
          LOCRR = MIN(RREG,NINT(X)+WINDOW-1)
          IGEL = CHNRP1(RELPG,LNGTHG,LNBR,RNBR,IDBSIZ,
     +    LLINO,LREG)
          IF(LOCLR.NE.0) THEN
            CALL LSTCON(RELPG,LNGTHG,LNBR,RNBR,IDBSIZ,IGEL,
     +      LOCRR,IDEV,IDEV3,NAMARC)
          END IF
          GO TO 10
        END IF
        IF(TERM.EQ.'Q') THEN
          CALL DBSCNP(RELPG,LNGTHG,LNBR,RNBR,NGELS,NCONTS,SEQ1,
     +    IDIM1,GEL,IDBSIZ,TEMP3,2,CHRSIZ,MAXGL2,IDEV2,LLINO,
     +    PERCD,MAXGEL,LINOU1,LINOU2,LREG,RREG,
     +    MARGL,MARGR,MARGB(NOPT3),MARGT(NOPT3),ISXMAX,ISYMAX,
     +    MXGOOD)
          GO TO 10
        END IF
      END IF
      IF(NOPT.EQ.NOPT2) THEN
        CALL IDPLC(RELPG,LNGTHG,LNBR,RNBR,IDBSIZ,
     +  NCONTS,IX,IY,MARGL,MARGR,MARGB(NOPT),MARGT(NOPT),
     +  ISXMAX,ISYMAX,DBTDUX,DBTDUY,
     +  LINCON,IGEL,IS)
        IF(IGEL.EQ.0) RETURN
        IF(TERM.EQ.'Z') THEN
          CALL CLEARG
          CALL PLC(RELPG,LNGTHG,LNBR,RNBR,IDBSIZ,LINCON,IGEL,
     +    NCONTS,MARGL,MARGR,MARGB(NOPT),MARGT(NOPT),ISXMAX,ISYMAX)
          LREG = 1
          RREG = RELPG(LINCON)
          LLINO = LNBR(LINCON)
          LLINOI = LLINO
          LINCNI = LINCON
          LENCON = RREG - LREG + 1
          CALL FDEPTH(RELPG,LNGTHG,LNBR,RNBR,
     +    IDBSIZ,LLINO,LREG,RREG,LENCON,
     +    MARGL,MARGR,MARGB(NOPT1),MARGT(NOPT1),ISXMAX,ISYMAX)
          GO TO 10
        END IF
        IF(TERM.EQ.'Q') THEN
          CALL CLEARG
          CALL PLC(RELPG,LNGTHG,LNBR,RNBR,IDBSIZ,LINCON,IGEL,
     +    NCONTS,MARGL,MARGR,MARGB(NOPT),MARGT(NOPT),ISXMAX,ISYMAX)
          LREG = 1
          RREG = RELPG(LINCON)
          LLINO = LNBR(LINCON)
          LLINOI = LLINO
          LINCNI = LINCON
          LENCON = RREG - LREG + 1
          CALL FDEPTH(RELPG,LNGTHG,LNBR,RNBR,
     +    IDBSIZ,LLINO,LREG,RREG,LENCON,
     +    MARGL,MARGR,MARGB(NOPT1),MARGT(NOPT1),ISXMAX,ISYMAX)
          CALL DBSCNP(RELPG,LNGTHG,LNBR,RNBR,NGELS,NCONTS,SEQ1,
     +    IDIM1,GEL,IDBSIZ,TEMP3,2,CHRSIZ,MAXGL2,IDEV2,LLINO,
     +    PERCD,MAXGEL,LINOU1,LINOU2,LREG,RREG,
     +    MARGL,MARGR,MARGB(NOPT3),MARGT(NOPT3),ISXMAX,ISYMAX,
     +    MXGOOD)
          GO TO 10
        END IF
        IF(TERM.EQ.'I') THEN
          CALL READN(IDEV3,IGEL,NAMARC)
          WRITE(IDEV,1006)NAMARC,IGEL,RELPG(IGEL),LNGTHG(IGEL)
          GO TO 10
        END IF
        IF(TERM.EQ.'S') THEN
          LOCLR = MAX(1,IS-WINDOW)
          LOCRR = MIN(RELPG(LINCON),IS+WINDOW-1)
          LLINO = LNBR(LINCON)
          IF(LOCLR.NE.0) THEN
            CALL DSPLAY(RELPG,LNGTHG,LNBR,RNBR,
     +      GEL,LLINO,LINCON,LOCLR,LOCRR,GEL2,I1,I2,0,I,
     +      IDBSIZ,IDEV,KBOUT,
     +      IDEV2,IDEV3,LINLEN,PERCD,MAXGEL,IDM)
          END IF
          GO TO 10
        END IF
        IF(TERM.EQ.'N') THEN
          LOCLR = MAX(1,IS-WINDOW)
          LOCRR = MIN(RELPG(LINCON),IS+WINDOW-1)
          LLINO = LNBR(LINCON)
          IF(LOCLR.NE.0) THEN
            CALL LSTCON(RELPG,LNGTHG,LNBR,RNBR,IDBSIZ,IGEL,
     +      LOCRR,IDEV,IDEV3,NAMARC)
          END IF
          GO TO 10
        END IF
      END IF
      END
      INTEGER FUNCTION CLINNO(LNBR,IDBSIZ,NCONTS,IIN)
C  AUTHOR: RODGER STADEN
C  RETURNS CONTIG LINE NUMBER OR ZERO FOR ERROR
      INTEGER LNBR(IDBSIZ)
      CLINNO = 0
      N=IDBSIZ-NCONTS
      DO 10 J=N,IDBSIZ-1
        IF(LNBR(J).EQ.IIN) THEN
          CLINNO = J
          RETURN
        END IF
10    CONTINUE
      END
      SUBROUTINE UPDCON(RELPG,LNGTHG,LNBR,RNBR,IDBSIZ,NGELS,NCONTS,
     +SEQ,MAXSEQ,IDIM1,CSTART,CLENO,LINCON,NAMPRO,SEQ2,TEMP3,
     +ECHRSZ,MAXGL2,KBOUT,IDEV,IDEV2,IFAIL,MAXGEL,IDM,PERCD)
      INTEGER RELPG(IDBSIZ),LNGTHG(IDBSIZ),LNBR(IDBSIZ),RNBR(IDBSIZ)
      INTEGER CSTART,CLENO,S1,B1,ECHRSZ,RREG
      INTEGER TEMP3(ECHRSZ,MAXGL2)
      CHARACTER SEQ(MAXSEQ),SEQ2(MAXGEL)
      CHARACTER NAMPRO*(*)
      INTEGER CHNRP
      EXTERNAL CHNRP
C cstart consensus start point (before new reading)
C cleno consensus length (before new reading)
C lincon element number of contig
C s1 number of first reading to shift
C b1 number of first base to shift (in overall consensus positioning)
C
C there are 2 tasks: 1. make space for the new and altered region
C                    2. calculate the new consensus and put it in the space
C we do not have to make space if:
C a. we are dealing with the last contig in the consensus and there are no
C    readings starting to the right of the new data
C b. the contig has not been padded
C
C New code to update the consensus only for the region affected by the
C new reading. Find the next reading to the right of the new one, which
C the new one does not overlap (might not be one!). Make a consensus from
C start of new reading to here. Prior to this make space for it by moving
C the consensus right (only if the contig is longer (padding or extra data
C at its ends). Let s1 be the first reading to shift. We shift from its
C left end to the end of the contig - where is this in the overall consensus?
C The distance of the left end of s1 to the right end of the contig is
C unchanged. This means that the new relpg(s1) is the same distance from
C the right end of the old consensus as the old relpg(s1) was from the right
C end of the old consensus. So from this we can calculate the position of the
C the first base to move. 
C Let L be the position in the overall consensus of  the last base in this contig
C            L = cstart - cleno - 1
C Let D = distance to end of contig 
C            D = RELPG(LINCON) - relpg(s1) + 1.
C First base to shift B1 = L - D + 1
C Last base to shift is idim1
C Distance to move to right is relpg(lincon) - cleno ie the number of extra bases
C make consensus from relpg(ngels) to relpg(s1) - 1
C put it at cstart + relpg(ngels) - 1
C
C Potential problems:
C 1) reading at right end of contig
C the search for the first nonoverlapping read to the right will return 0
C shift al the next contig: ie cstart + cleno onwards
C make consensus from relpg(ngels) to end of contig
C put it at cstart + relpg(ngels) -1
C
C 2) reading at left end of contig
C shift whole contig ie cstart - 20
C add new title
C shift consensus relpg(lincon) - cleno to the right
C
C 3) new reading contains contig - cases 1 and 2 combined
C the search for the first nonoverlapping read to the right will return 0
C shift whole of next contig and make consensus from relpg(ngels) to end of
C contig.
C
C 4) Might not be a next contig to shift
C
C get number of first reading to shift
C
      S1 = CHNRP(RELPG,LNGTHG,LNBR,RNBR,IDBSIZ,NGELS,NCONTS,
     +RELPG(NGELS)+ABS(LNGTHG(NGELS))-1)
C      WRITE(*,*)'S1',S1
C
C is the altered region longer than the original: only then do we need to shift
C
C           WRITE(*,*)'IDIM1',IDIM1
C           WRITE(*,*)'RELPG(LINCON)',RELPG(LINCON)
C           WRITE(*,*)'CSTART,CLENO',CSTART,CLENO
      IF (RELPG(LINCON) - CLENO.GT.0) THEN
C
C it is longer so we probably need to shift
C
        IF (S1.EQ.0) THEN
C
C no readings start to the right of the new data
C
          IF (CSTART+CLENO-1.LT.IDIM1) THEN
C
C there are other contigs to the right
C
C           WRITE(*,*)'CSTART,CLENO',CSTART,CLENO
            B1 = CSTART + CLENO
C            WRITE(*,*)'B1',B1
            CALL MAKHCA(SEQ,MAXSEQ,B1,RELPG(LINCON)-CLENO,IDIM1)
          ELSE
C
C there are no contigs to the right and no readings start to the right of
C the new one so nothing to shift
C
          END IF
        ELSE
C
C there are readings starting to the right of the new one
C
C shift from start of next reading to right
C
           L = CSTART + CLENO - 1
C           WRITE(*,*)'CSTART,CLENO,L',CSTART,CLENO,L
           LD = RELPG(LINCON) - RELPG(S1) + 1
C           WRITE(*,*)'LD',LD
           B1 = L - LD + 1
C            WRITE(*,*)'B1',B1
           CALL MAKHCA(SEQ,MAXSEQ,B1,RELPG(LINCON)-CLENO,IDIM1)
        END IF
      END IF
C
C now make new consensus (where do we put it,  do we need
C to give it a header, and what region do we make it for ?
C in the simplest case make it for relpg(ngels) to relpg(s1) -1
C if s1=0 make it for relpg(ngels) to end of contig (relpg(lincon))
C we give it a header if it is at the left end of the contig ie lnbr(ngels)=0
C
C we always start at the left end of the new reading
C
      LREG = RELPG(NGELS)
C
C we end at the next reading to the right or the end of the contig
C
      IF (S1.NE.0) THEN
        RREG = RELPG(S1) - 1
      ELSE
        RREG = RELPG(LINCON)
      END IF
C
C where do we put the new consensus ?
C
      B1 = CSTART + RELPG(NGELS) - 1
C      WRITE(*,*)'LREG,RREG',LREG,RREG
C            WRITE(*,*)'B1',B1
C
C do we need to add a title
C
      IF (LNBR(NGELS).EQ.0) THEN
        B1 = CSTART - 20
C        WRITE(*,*)'ADD NEW TIT AT',B1
        CALL ADDTIT(SEQ(B1),NAMPRO,NGELS,B1)
      END IF
      IGELC = LNBR(LINCON)
C
C note aconsn will chain along until it find the first useful reading
C
      JOB = 2
      CALL ACONSN(RELPG,LNGTHG,LNBR,RNBR,NAMPRO,NGELS,NCONTS,
     +SEQ,MAXSEQ,SEQ2,IDBSIZ,B1,JOB,IGELC,LREG,RREG,TEMP3,
     +ECHRSZ,MAXGL2,IDEV,IDEV2,IFAIL,MAXGEL,IDM,PERCD)
      IF(IFAIL.NE.0) THEN
        CALL ERROM(KBOUT,'Error calculating consensus')
        RETURN
      END IF
C
C before we leave we must make the overall consensus length correct
C  so add on the extra length (if any) which is the new length - old length
C
C      WRITE(*,*)'OLD IDIM1',IDIM1
      IDIM1 = IDIM1 + RELPG(LINCON) - CLENO
      IDIM2 = IDIM1 + RELPG(LINCON) - CLENO
C      WRITE(*,*)'NEW IDIM1/2',IDIM1
      END
      SUBROUTINE MAKHCA(STRING,MAXAR,FROM,HSIZE,ASIZE)
      CHARACTER STRING(MAXAR)
      INTEGER FROM,HSIZE,ASIZE
C
C make a hole of size hsize in character array size asize
C
      J = ASIZE + HSIZE
      DO 10 I=ASIZE,FROM,-1
        STRING(J) = STRING(I)
        J = J - 1
 10     CONTINUE
      END
      INTEGER FUNCTION CHNRP(RELPG,LNGTHG,LNBR,RNBR,IDBSIZ,LGEL,NCONT,
     +LREG)
      INTEGER RELPG(IDBSIZ),LNGTHG(IDBSIZ),LNBR(IDBSIZ),RNBR(IDBSIZ)
C
C find first reading starting past lreg (0=none found)
C
      I = LGEL
      CHNRP = 0
10    CONTINUE
      IF(I.NE.0) THEN
        IF(RELPG(I).LE.LREG) THEN
          I = RNBR(I)
          GO TO 10
        END IF
        CHNRP = I
        RETURN
      END IF
      END
      INTEGER FUNCTION CHNRP1(RELPG,LNGTHG,LNBR,RNBR,IDBSIZ,
     +LGEL,LREG)
      INTEGER RELPG(IDBSIZ),LNGTHG(IDBSIZ),LNBR(IDBSIZ),RNBR(IDBSIZ)
C
C find first reading with data covering or past lreg (0=none found)
C
      I = LGEL
      CHNRP1 = 0
10    CONTINUE
      IF(I.NE.0) THEN
        IF(RELPG(I)+ABS(LNGTHG(I))-1.LT.LREG) THEN
          I = RNBR(I)
          GO TO 10
        END IF
        CHNRP1 = I
        RETURN
      END IF
      END
C      ACONSN
      SUBROUTINE ACONSN(RELPG,LNGTHG,LNBR,RNBR,NAMPRO,NGELS,NCONTS,
     +SEQ1,IDIM1,GEL,IDBSIZ,ISTART,JOB,LLINO,LREG,RREG,TEMP,
     +CHRSIZ,MAXGL2,KBOUT,
     +IDEVW,IFAIL,MAXGEL,IDM,PERCD)
C   AUTHOR: RODGER STADEN
      INTEGER RELPG(IDBSIZ),CHRSIZ
      INTEGER LREG,RREG,X,Y,TEMP(CHRSIZ,MAXGL2)
      CHARACTER SEQ1(IDIM1)
      INTEGER LNGTHG(IDBSIZ),LNBR(IDBSIZ),RNBR(IDBSIZ)
      CHARACTER GEL(MAXGEL)
      CHARACTER NAMPRO*(*)
      INTEGER CHNRP1
      EXTERNAL CHNRP1
C
C new consensus calculating routine (could replace acons if we check values of job)
C
C job = 0 do it for whole db
C     = 2 for selected contig only
C     = 1 for selected contig, adding a header
C     note jobs 0 and 1 update istart (it always points to end of overall 
C     consensus), but job=2 does not
C
      CALL BUSY(KBOUT)
      IFAIL=0
      IF(JOB.EQ.1) THEN
C
C do it for a selected contig, adding title
C
        ISTART=ISTART+1
        IDIM11=RREG-LREG+1
        IF((ISTART+19+IDIM11).GT.IDIM1)THEN
           WRITE(KBOUT,1009)IDIM1
           IFAIL=1
           RETURN
        END IF
C
C allow summer to be dumb, and find first relevant reading number
C
        LLINO1 =  CHNRP1(RELPG,LNGTHG,LNBR,RNBR,IDBSIZ,
     +  LLINO,LREG)
        IF(LLINO1.EQ.0) THEN
          CALL ERROM(KBOUT,
     +    'Error in ACONSN: no data found for consensus')
          IFAIL = 1
          RETURN
        END IF
        CALL ADDTIT(SEQ1(ISTART),NAMPRO,LLINO,ISTART)
        CALL SUMMER(RELPG,LNGTHG,LNBR,RNBR,NGELS,NCONTS,
     +  SEQ1(ISTART),IDIM11,GEL,LREG,RREG,LLINO1,IDBSIZ,TEMP,
     +  CHRSIZ,MAXGL2,
     +  IDEVW,MAXGEL,IDM,PERCD)
        ISTART=ISTART+IDIM11-1
        RETURN
      END IF
      IF(JOB.EQ.2) THEN
C
C do it for a selected contig
C
        IDIM11=RREG-LREG+1
        IF((ISTART+IDIM11-1).GT.IDIM1)THEN
           WRITE(KBOUT,1009)IDIM1
           IFAIL=1
           RETURN
        END IF
C
C allow summer to be dumb, and find first relevant reading number
C
        LLINO1 =  CHNRP1(RELPG,LNGTHG,LNBR,RNBR,IDBSIZ,
     +  LLINO,LREG)
        IF(LLINO1.EQ.0) THEN
          CALL ERROM(KBOUT,
     +    'Error in ACONSN: no data found for consensus')
          IFAIL = 1
          RETURN
        END IF
        CALL SUMMER(RELPG,LNGTHG,LNBR,RNBR,NGELS,NCONTS,
     +  SEQ1(ISTART),IDIM11,GEL,LREG,RREG,LLINO1,IDBSIZ,TEMP,
     +  CHRSIZ,MAXGL2,
     +  IDEVW,MAXGEL,IDM,PERCD)
        RETURN
      END IF
C
C  do it for all contigs
C
      N=IDBSIZ-NCONTS
      DO 110 I=N,IDBSIZ-1
        J=LNBR(I)
        X=1
        Y=RELPG(I)
        ISTART=ISTART+1
        IF((ISTART+19+Y).GT.IDIM1)THEN
           WRITE(KBOUT,1009)IDIM1
1009       FORMAT(
     +  ' Database maximum consensus length(',I6,') exceeded',/,
     +  ' calculation aborted')
           IFAIL=1
           RETURN
        END IF
        CALL ADDTIT(SEQ1(ISTART),NAMPRO,J,ISTART)
        CALL SUMMER(RELPG,LNGTHG,LNBR,RNBR,NGELS,NCONTS,
     +  SEQ1(ISTART),Y,GEL,X,Y,J,IDBSIZ,TEMP,CHRSIZ,MAXGL2,IDEVW,MAXGEL,
     +  IDM,PERCD)
        ISTART=ISTART+Y-1
110   CONTINUE
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
      SUBROUTINE SHFTLA(STRING,MAXAR,FROMS,TO,FROME)
      CHARACTER STRING(MAXAR)
      INTEGER FROMS,TO,FROME
C
C shift an array left from froms to to
C
      J = TO
      DO 10 I=FROMS,FROME
        STRING(J) = STRING(I)
        J = J + 1
 10   CONTINUE
      END
      SUBROUTINE GETLN3(RELPG,LNGTHG,LNBR,RNBR,NGELS,NCONTS,
     +LINCON,LLINO,IGELNO,IERR,IDBSIZ,KBIN,KBOUT,IDEVN,PROMPT,
     +IHELPS,IHELPE,FILEH,IDEVH)
C   AUTHOR: RODGER STADEN
      INTEGER RELPG(IDBSIZ),GELIDN
      INTEGER LNGTHG(IDBSIZ),LNBR(IDBSIZ),RNBR(IDBSIZ)
      CHARACTER FILEH*(*),PROMPT*(*)
      EXTERNAL GELIDN
      IERR = 1
      NCONTC = GELIDN(RELPG,LNGTHG,LNBR,RNBR,NGELS,NCONTS,LLINO,
     +IDBSIZ,KBIN,KBOUT,IDEVN,PROMPT,
     +IHELPS,IHELPE,FILEH,IDEVH,INFLAG)
      IF(INFLAG.EQ.2) RETURN
      IF(INFLAG.EQ.4) RETURN
      IF(NCONTC.EQ.0) RETURN
      IGELNO = NCONTC
      IF(LNBR(NCONTC).NE.0) THEN
        WRITE(KBOUT,1013)RELPG(NCONTC)
1013    FORMAT(' Position of this reading=',I6)
25      CONTINUE
        NCONTC = LNBR(NCONTC)
        IF(LNBR(NCONTC).NE.0) GO TO 25
        WRITE(KBOUT,1014)NCONTC
1014    FORMAT( ' Number of leftmost reading this contig=',I6)
      END IF
30    CONTINUE
      N = IDBSIZ - NCONTS
      DO 20 J=N,IDBSIZ-1
        IF(LNBR(J).EQ.NCONTC) THEN
          LINCON=J
          GO TO 21
        END IF
20    CONTINUE
      CALL ERROM(KBOUT,'No contig line for this reading!')
      RETURN
21    CONTINUE
      LLINO = NCONTC
      IERR = 0
      END
      INTEGER FUNCTION GELIDN(RELPG,LNGTHG,LNBR,RNBR,NGELS,NCONTS,
     +LLINO,IDBSIZ,KBIN,KBOUT,IDEVN,PROMPT,
     +IHELPS,IHELPE,FILEH,IDEVH,INFLAG)
      CHARACTER FILEH*(*),PROMPT*(*)
C   AUTHOR: RODGER STADEN
C   SEARCHES FOR ARCHIVE NAMES
      INTEGER RELPG(IDBSIZ)
      INTEGER LNGTHG(IDBSIZ),LNBR(IDBSIZ),RNBR(IDBSIZ)
      CHARACTER NAME1*17,NAME3*17,NFLAG
      PARAMETER (NFLAG='/')
      EXTERNAL NAMENO
      NAME3 = ' '
      IF(LLINO.NE.0) THEN
        NAME3(1:1) = NFLAG
        CALL READN(IDEVN,LLINO,NAME3(2:))
      END IF
      GELIDN = 0
10    CONTINUE
      L = 0
      IF(LLINO.NE.0) L = 17
      CALL GTSTR(PROMPT,NAME3,
     +NAME1,L,KBOUT,KBIN,INFLAG)
      IF(INFLAG.EQ.2) RETURN
      IF(INFLAG.EQ.4) RETURN
      IF(INFLAG.EQ.3) THEN
        GELIDN = LLINO
        RETURN
      END IF
      IF(INFLAG.EQ.1) THEN
        CALL HELP2(IHELPS,IHELPE,FILEH,IDEVH,KBIN,KBOUT)
        GO TO 10
      END IF
      IF(NAME1(1:1).EQ.NFLAG) THEN
        GELIDN = NAMENO(NAME1(2:),NGELS,IDEVN)
        IF (GELIDN.EQ.0) CALL ERROM(KBOUT,'Reading name not found')
      ELSE
        CALL RJST(NAME1)
        READ(NAME1,1001,ERR=10,END=10)GELIDN
1001    FORMAT(I17)
        IF((GELIDN.LT.1).OR.(GELIDN.GT.NGELS)) THEN
          CALL ERROM(KBOUT,'Illegal gel reading number')
          GO TO 10
        END IF
      END IF
      END
      INTEGER FUNCTION NAMENO(NAME,NGELS,IDEVN)
      CHARACTER NAME*(*)
      CHARACTER*16 NAME1,NAME2
      NAME1 = NAME
      CALL CCASE(NAME1,1)
      DO 10 I=1,NGELS
        CALL READN(IDEVN,I,NAME2)
        CALL CCASE(NAME2,1)
        IF (NAME1.EQ.NAME2) THEN
          NAMENO = I
          RETURN
        END IF
 10   CONTINUE
      NAMENO = 0
      END
      SUBROUTINE REMGBD(RELPG,LNGTHG,LNBR,RNBR,NGELS,NCONTS,IDBSIZ,
     +KBIN,KBOUT,GEL,MAXGEL,IDEVR,IDEVW,IDEVN,IDEV2,FILNAM,
     +IHELPS,IHELPE,HELPF,IDEVH,IOK)
      INTEGER RELPG(IDBSIZ),LNGTHG(IDBSIZ),LNBR(IDBSIZ),RNBR(IDBSIZ)
      CHARACTER HELPF*(*),GEL(MAXGEL),NAMARC*16,FILNAM*(*)
      INTEGER REMME,GCLIN,CHAINL,GNFFOF
      PARAMETER (MAXPRM = 35)
      CHARACTER PROMPT(4)*(MAXPRM)
      EXTERNAL GCLIN,CHAINL,NAMENO,GNFFOF
C assumes db is logical consistent
      FILNAM = ' '
      PROMPT(1) = 'Define a region by reading names'
      PROMPT(2) = 'Use a file of reading names'
      PROMPT(3) = 'Move a reading to a separate contig'
      PROMPT(4) = 'Make a list of unattached readings'
      IOPT = 1
      CALL RADION('Select list definition mode',PROMPT,4,IOPT,
     +IHELPS,IHELPE,HELPF,IDEVH,KBIN,KBOUT)
      IF(IOPT.LT.1) RETURN
      IF(IOPT.EQ.4) THEN
C
C here we find all contigs with single readings and write their
C names to a file
C
        CALL OPENF1(IDEV2,FILNAM,1,IOK,KBIN,KBOUT,
     +  'Name for file of reading names',
     +  IHELPS,IHELPE,HELPF,IDEVH)
        IF(IOK.NE.0) RETURN
        DO 5 I=IDBSIZ-NCONTS,IDBSIZ-1
          IF (LNBR(I).EQ.RNBR(I)) THEN
            IF (LNBR(I).NE.0) THEN
              CALL READN(IDEVN,LNBR(I),NAMARC)
              WRITE(IDEV2,1000)NAMARC
            END IF
          END IF
 5      CONTINUE
        CLOSE(UNIT=IDEV2)
        RETURN
      END IF
      IF(IOPT.EQ.3) THEN
C
C here we start a new contig with the selected reading
C
C we get the reading number igelno and move a copy of it
C to ngels+1. Then we use the remove reading routine to delete
C the original copy and move the new one to fill the hole. The 
C reason for this convoluted route is that remgel cleans up
C all the mess. We must write a new contig line and check the
C orientation.
C
C
        IF(NGELS+3.GE.IDBSIZ-NCONTS) THEN
          CALL ERROM(KBOUT,'Insufficient space for new contig')
          RETURN
        END IF
        NGELST = NGELS + 1
        NCONTT = NCONTS + 1
        CALL GETLN3(RELPG,LNGTHG,LNBR,RNBR,NGELS,NCONTS,
     +  LINCOL,LLINOL,IGELNO,IERR,IDBSIZ,KBIN,KBOUT,IDEVN,
     +  'Reading to disconnect',
     +  IHELPS,IHELPE,HELPF,IDEVH)
        IF (IERR.NE.0) RETURN
        CALL READW(IDEVW,IGELNO,GEL,MAXGEL)
C
C move reading info over end of gel list
C        
        CALL READN(IDEVN,IGELNO,NAMARC)
        CALL WRITEN(IDEVN,NGELST,NAMARC)
        LNBR(NGELST) = 0
        RNBR(NGELST) = 0
C
C leave orientation the same
C
        LNGTHG(NGELST) = LNGTHG(IGELNO)
        RELPG(NGELST) = 1
        CALL WRITER(IDEVR,NGELST,RELPG(NGELST),LNGTHG(NGELST),
     +  LNBR(NGELST),RNBR(NGELST))
        CALL WRITEW(IDEVW,NGELST,GEL,MAXGEL)
        CALL MOVTAG(IGELNO,NGELST)
C
C start a new contig
C
        I = IDBSIZ - NCONTT
        LNBR(I) = NGELST
        RNBR(I) = NGELST
        LNGTHG(I) = 0
        RELPG(I) = ABS(LNGTHG(NGELST))
        CALL WRITER(IDEVR,I,RELPG(I),LNGTHG(I),
     +    LNBR(I),RNBR(I))
        NGELS = NGELST
        NCONTS = NCONTT
        CALL WRITER(IDEVR,IDBSIZ,NGELS,NCONTS,0,0)
        IGLNO = IGELNO
        CALL REMGEL(RELPG,LNGTHG,LNBR,RNBR,NGELS,NCONTS,IDBSIZ,
     +  IGLNO,LINCOL,KBOUT,GEL,MAXGEL,IDEVR,IDEVW,IDEVN)
        RETURN
      END IF
      IF(IOPT.EQ.1) THEN
 10     CONTINUE
        LLINOL = 0
        LLINOR = 0
        CALL GETLN3(RELPG,LNGTHG,LNBR,RNBR,NGELS,NCONTS,
     +  LINCOL,LLINOL,IGELNO,IERR,IDBSIZ,KBIN,KBOUT,IDEVN,
     +  'Leftmost reading',
     +  IHELPS,IHELPE,HELPF,IDEVH)
        IF (IERR.NE.0) RETURN
        CALL GETLN3(RELPG,LNGTHG,LNBR,RNBR,NGELS,NCONTS,
     +  LINCOR,LLINOR,JGELNO,IERR,IDBSIZ,KBIN,KBOUT,IDEVN,
     +  'Rightmost reading',
     +  IHELPS,IHELPE,HELPF,IDEVH)
        IF (IERR.NE.0) RETURN
        IF (LLINOL.NE.LLINOR) THEN
          CALL ERROM(KBOUT,
     +    'For this mode readings must be in the same contig')
          GO TO 10
        END IF
        IF (RELPG(IGELNO).GT.RELPG(JGELNO)) THEN
          CALL ERROM(KBOUT,
     +    'For this mode readings must be in left to right order')
          GO TO 10
        END IF
C
C IGELNO is first read to remove, JGELNO the last
C
C we must make a list of reads because removal changes numbers
C
        CALL OPENF1(IDEV2,FILNAM,1,IOK,KBIN,KBOUT,
     +  'Name for temporary file of reading names',
     +  IHELPS,IHELPE,HELPF,IDEVH)
        IF(IOK.NE.0) RETURN
C
C write out their names
C
        J = IGELNO
 20     CONTINUE
        CALL READN(IDEVN,J,NAMARC)
        WRITE(IDEV2,1000)NAMARC
        WRITE(KBOUT,1001)NAMARC
 1000   FORMAT(A)
 1001   FORMAT(' ',A)
        IF (J.NE.JGELNO) THEN
          IF (J.NE.0) THEN
            J=RNBR(J)
            GO TO 20
          END IF
        END IF
        CALL BPAUSE(KBIN,KBOUT,IOK)
        IF (IOK.NE.0) THEN
          CLOSE(UNIT=IDEV2)
          RETURN
        END IF
        REWIND(UNIT=IDEV2)
      ELSE IF(IOPT.EQ.2) THEN
C
C here we start from a file of file names
C 
        CALL OPENF1(IDEV2,FILNAM,0,IOK,KBIN,KBOUT,
     +  'Name of file of reading names',
     +  IHELPS,IHELPE,HELPF,IDEVH)
        IF(IOK.NE.0) RETURN

      ELSE
        CALL ERROM(KBOUT,'How the hell did we get here?')
        RETURN
      END IF
 30   CONTINUE
      IOK = GNFFOF(IDEV2,NAMARC)
      IF(IOK.EQ.1) GO TO 100
      IF(IOK.NE.0) GO TO 30
      REMME = NAMENO(NAMARC,NGELS,IDEVN)
      I = CHAINL(RELPG,LNGTHG,LNBR,RNBR,NGELS,NCONTS,
     +    IDBSIZ,REMME)
      ICONT = GCLIN(RELPG,LNGTHG,LNBR,RNBR,NGELS,NCONTS,IDBSIZ,I)
      IF(ICONT.EQ.0) THEN
        CALL ERROM(KBOUT,'No contig line for this reading')
        IOK = 1
        GO TO 100
      END IF
      CALL REMGEL(RELPG,LNGTHG,LNBR,RNBR,NGELS,NCONTS,IDBSIZ,
     +REMME,ICONT,KBOUT,GEL,MAXGEL,IDEVR,IDEVW,IDEVN)
      GO TO 30
 100  CONTINUE
      CLOSE(UNIT=IDEV2)
      END
