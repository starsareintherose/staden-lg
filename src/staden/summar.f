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
        N  = ABS(LNGTHG(GELC))
        IS = MAX(1,(ABS(LNGTHG(GELC)) - MXGOOD + 1))
        LSEQNO = RELPG(GELC) + MAX(0,ABS(LNGTHG(GELC)) - MXGOOD)
        IF(LSEQNO.LT.LREG)LSEQNO=LREG
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
