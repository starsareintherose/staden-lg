      SUBROUTINE MOVGEL(RELPG,LNGTHG,LNBR,RNBR,NGELS,NCONTS,IDBSIZ,
     +GEL,FROM,TO,IDEVR,IDEVW,IDEVN,MAXGEL,KBOUT)
C   Subroutine to move a gel from line from to line to
C   Extended 22-5-91
C   AUTHOR: RODGER STADEN
      INTEGER RELPG(IDBSIZ),FROM,TO
      INTEGER LNGTHG(IDBSIZ),LNBR(IDBSIZ),RNBR(IDBSIZ)
      CHARACTER NAMGEL*16,GEL(MAXGEL)
      INTEGER GCLIN,CHAINL
      LOGICAL LEFTE,RIGHTE
      EXTERNAL GCLIN,CHAINL
      LEFTE = .FALSE.
      RIGHTE = .FALSE.
C
C left end ?
C
      IF(LNBR(FROM).EQ.0) LEFTE = .TRUE.
C
C right end ?
C
      IF(RNBR(FROM).EQ.0) RIGHTE = .TRUE.
C
C if both true remove the contig line, then overwrite the gel
C     
      IF(LEFTE.AND.RIGHTE) THEN
        NCONTO = GCLIN(RELPG,LNGTHG,LNBR,RNBR,NGELS,NCONTS,
     +    IDBSIZ,FROM)
        IF(NCONTO.EQ.0)THEN
          WRITE(KBOUT,*)
     +      'This gel has no left neighbour but does not'
            WRITE(KBOUT,*)'appear in a contig line!'
        ELSE
          LNBR(NCONTO) = TO
          RNBR(NCONTO) = TO
          CALL WRITER(IDEVR,NCONTO,RELPG(NCONTO),LNGTHG(NCONTO),
     +      LNBR(NCONTO),RNBR(NCONTO))
        END IF
      ELSE IF(LEFTE) THEN
        NCONTO = GCLIN(RELPG,LNGTHG,LNBR,RNBR,NGELS,NCONTS,
     +    IDBSIZ,FROM)
        IF(NCONTO.EQ.0)THEN
          WRITE(KBOUT,*)
     +      'This gel has no left neighbour but does not'
            WRITE(KBOUT,*)'appear in a contig line!'
        ELSE
          LNBR(NCONTO) = TO
          CALL WRITER(IDEVR,NCONTO,RELPG(NCONTO),LNGTHG(NCONTO),
     +      LNBR(NCONTO),RNBR(NCONTO))
        END IF
      ELSE IF(RIGHTE) THEN
        I = CHAINL(RELPG,LNGTHG,LNBR,RNBR,NGELS,NCONTS,
     +    IDBSIZ,FROM)
        NCONTO = GCLIN(RELPG,LNGTHG,LNBR,RNBR,NGELS,NCONTS,
     +    IDBSIZ,I)
        IF(NCONTO.EQ.0)THEN
          WRITE(KBOUT,*)
     +      'This gel has no right neighbour and does not'
          WRITE(KBOUT,*)'appear in a contig!'
        ELSE
          IF(RNBR(NCONTO).NE.FROM)THEN
            WRITE(KBOUT,*)
     +        'This gel has no right neighbour but does not'
            WRITE(KBOUT,*)'appear in a contig line!'
          ELSE
            RNBR(NCONTO) = TO
            CALL WRITER(IDEVR,NCONTO,RELPG(NCONTO),LNGTHG(NCONTO),
     +        LNBR(NCONTO),RNBR(NCONTO))
          END IF
        END IF
      END IF
      RELPG(TO)=RELPG(FROM)
      LNGTHG(TO)=LNGTHG(FROM)
      LNBR(TO)=LNBR(FROM)
      RNBR(TO)=RNBR(FROM)
      CALL READW(IDEVW,FROM,GEL,MAXGEL)
      CALL WRITEW(IDEVW,TO,GEL,MAXGEL)
      CALL READN(IDEVN,FROM,NAMGEL)
      CALL WRITEN(IDEVN,TO,NAMGEL)
      CALL WRITER(IDEVR,TO,RELPG(TO),LNGTHG(TO),
     +LNBR(TO),RNBR(TO))
C   Do neighbours
      IF(LNBR(FROM).NE.0) THEN
        I=LNBR(FROM)
        RNBR(I)=TO
        CALL WRITER(IDEVR,I,RELPG(I),LNGTHG(I),
     +  LNBR(I),RNBR(I))
      END IF
      IF(RNBR(FROM).NE.0) THEN
        I=RNBR(FROM)
        LNBR(I)=TO
        CALL WRITER(IDEVR,I,RELPG(I),LNGTHG(I),
     +  LNBR(I),RNBR(I))
      END IF
      CALL MOVTAG(FROM,TO)
      END
