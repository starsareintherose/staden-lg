      SUBROUTINE REMGEL(RELPG,LNGTHG,LNBR,RNBR,NGELS,NCONTS,IDBSIZ,
     +REMME,ICONT,KBOUT,GEL,MAXGEL,IDEVR,IDEVW,IDEVN)
C Routine to remove a reading from a database
C Cases: 1 left end
C        2 right end
C        3 internal and dispensible
C        4 internal and indispensible
C if 1 change contig lnbr, contig length, lnbr of rnbr of remme, relpgs
C if 2 change contig rnbr, contig length, rnbr of lnbr of remme
C if 3 change contig length, lnbr of rnbr of remme rnbr of lnbr of remme
C if 4 need to break contig, then as for 1
C if 1 and 2 then also remove contig line
C for all cases move gel ngels to remme (if remme/=ngels)
C and update line idbsiz
C
      INTEGER RELPG(IDBSIZ),LNGTHG(IDBSIZ),LNBR(IDBSIZ),RNBR(IDBSIZ)
      INTEGER REMME,CLEN
      LOGICAL LEFTE,RIGHTE,DISPEN
      CHARACTER GEL(MAXGEL)
      EXTERNAL CLEN
      LEFTE = .FALSE.
      RIGHTE = .FALSE.
      DISPEN = .FALSE.
C
C Left end ?
C
      IF(LNBR(REMME).EQ.0) LEFTE = .TRUE.
C
C Right end ?
C
      IF(RNBR(REMME).EQ.0) RIGHTE = .TRUE.
C
C If both true remove the contig line, then overwrite the gel
C     
      IF(LEFTE.AND.RIGHTE) THEN
        WRITE(KBOUT,*)'Removing reading and contig'
        IFROM = NGELS
        NGELS = NGELS - 1
        CALL REMCNL(RELPG,LNGTHG,LNBR,RNBR,NGELS,NCONTS,IDBSIZ,
     +  ICONT,IDEVR)
        IF(REMME.NE.IFROM) THEN
          WRITE(KBOUT,*)'Renumbering reading',IFROM,' to',REMME
          CALL MOVGEL(RELPG,LNGTHG,LNBR,RNBR,NGELS,
     +    NCONTS,IDBSIZ,GEL,IFROM,REMME,IDEVR,IDEVW,IDEVN,MAXGEL,KBOUT)
        END IF
      ELSE IF(LEFTE) THEN
        WRITE(KBOUT,*)'Removing reading from left end of contig'
        LNBR(ICONT) = RNBR(REMME)
        I = 1 - RELPG(RNBR(REMME))
        WRITE(KBOUT,*)'Shifting readings in contig by distance=',I
        CALL SHIFTC(RELPG,LNGTHG,LNBR,RNBR,NGELS,NCONTS,IDEVR,
     +  IDBSIZ,RNBR(REMME),ICONT,I)
        I = LNBR(ICONT)
        LNBR(I) = 0
        CALL WRITER(IDEVR,I,RELPG(I),LNGTHG(I),
     +  LNBR(I),RNBR(I))
        IFROM = NGELS
        IF(REMME.NE.IFROM) THEN
          WRITE(KBOUT,*)'Renumbering reading',IFROM,' to',REMME
          CALL MOVGEL(RELPG,LNGTHG,LNBR,RNBR,NGELS,
     +    NCONTS,IDBSIZ,GEL,IFROM,REMME,IDEVR,IDEVW,IDEVN,MAXGEL,KBOUT)
        END IF
        NGELS = NGELS - 1
        CALL WRITER(IDEVR,IDBSIZ,NGELS,NCONTS,NGELS,NCONTS)
      ELSE IF(RIGHTE) THEN
        WRITE(KBOUT,*)'Removing reading from right end of contig'
        RNBR(ICONT) = LNBR(REMME)
        I = RNBR(ICONT)
        RNBR(I) = 0
        CALL WRITER(IDEVR,I,RELPG(I),LNGTHG(I),
     +  LNBR(I),RNBR(I))
        RELPG(ICONT) = CLEN(RELPG,LNGTHG,LNBR,RNBR,NGELS,NCONTS,
     +  IDBSIZ,LNBR(ICONT))
        CALL WRITER(IDEVR,ICONT,RELPG(ICONT),LNGTHG(ICONT),
     +  LNBR(ICONT),RNBR(ICONT))
        IFROM = NGELS
        IF(REMME.NE.IFROM) THEN
          WRITE(KBOUT,*)'Renumbering reading',IFROM,' to',REMME
          CALL MOVGEL(RELPG,LNGTHG,LNBR,RNBR,NGELS,
     +    NCONTS,IDBSIZ,GEL,IFROM,REMME,IDEVR,IDEVW,IDEVN,MAXGEL,KBOUT)
        END IF
        NGELS = NGELS - 1
        CALL WRITER(IDEVR,IDBSIZ,NGELS,NCONTS,NGELS,NCONTS)
      ELSE
C Is remme indispensible ?
        NSTART = RELPG(RNBR(REMME))
        I = REMME
10      CONTINUE
        I = LNBR(I)
        IF(I.NE.0) THEN
          IF((RELPG(I)+ABS(LNGTHG(I))-1).LT.NSTART) GO TO 10
          DISPEN = .TRUE.
        END IF
        IF(DISPEN) THEN
          WRITE(KBOUT,*)
     +    'Removing dispensible reading from middle of contig'
          I = LNBR(REMME)
          RNBR(I) = RNBR(REMME)
          CALL WRITER(IDEVR,I,RELPG(I),LNGTHG(I),
     +    LNBR(I),RNBR(I))
          I = RNBR(REMME)
          LNBR(I) = LNBR(REMME)
          CALL WRITER(IDEVR,I,RELPG(I),LNGTHG(I),
     +    LNBR(I),RNBR(I))
          IFROM = NGELS
          IF(REMME.NE.IFROM) THEN
            WRITE(KBOUT,*)'Renumbering reading',IFROM,' to',REMME
            CALL MOVGEL(RELPG,LNGTHG,LNBR,RNBR,NGELS,
     +      NCONTS,IDBSIZ,GEL,IFROM,REMME,IDEVR,IDEVW,IDEVN,
     +      MAXGEL,KBOUT)
          END IF
          NGELS = NGELS - 1
          RELPG(ICONT) = CLEN(RELPG,LNGTHG,LNBR,RNBR,NGELS,NCONTS,
     +    IDBSIZ,LNBR(ICONT))
          CALL WRITER(IDEVR,ICONT,RELPG(ICONT),LNGTHG(ICONT),
     +    LNBR(ICONT),RNBR(ICONT))
          CALL WRITER(IDEVR,IDBSIZ,NGELS,NCONTS,NGELS,NCONTS)
        ELSE
          WRITE(KBOUT,*)
     +    'Removing indispensible reading from middle of contig'
          WRITE(KBOUT,*)'So breaking contig first'
          IR = REMME
          IL = LNBR(REMME)
          ILO = LNBR(ICONT)
          NCONTO = ICONT
          NCONTR = IDBSIZ - NCONTS - 1
          CALL CBREAK(RELPG,LNGTHG,LNBR,RNBR,NGELS,NCONTS,IDBSIZ,
     +    KBOUT,IDEVR,IDEVW,IDEVN,IR,IL,ILO,NCONTO,NCONTR,IOK)
          IF(IOK.NE.0) RETURN
          WRITE(KBOUT,*)'Removing reading from left end of contig'
          ICONT = IDBSIZ - NCONTS
          LNBR(ICONT) = RNBR(REMME)
          I = 1 - RELPG(RNBR(REMME))
          WRITE(KBOUT,*)'Shifting readings in contig by distance=',I
          CALL SHIFTC(RELPG,LNGTHG,LNBR,RNBR,NGELS,NCONTS,IDEVR,
     +    IDBSIZ,RNBR(REMME),ICONT,I)
          I = LNBR(ICONT)
          LNBR(I) = 0
          CALL WRITER(IDEVR,I,RELPG(I),LNGTHG(I),
     +    LNBR(I),RNBR(I))
          IFROM = NGELS
          IF(REMME.NE.IFROM) THEN
            WRITE(KBOUT,*)'Renumbering reading',IFROM,' to',REMME
            CALL MOVGEL(RELPG,LNGTHG,LNBR,RNBR,NGELS,
     +      NCONTS,IDBSIZ,GEL,IFROM,REMME,IDEVR,IDEVW,IDEVN,
     +      MAXGEL,KBOUT)
          END IF
          NGELS = NGELS - 1
          CALL WRITER(IDEVR,IDBSIZ,NGELS,NCONTS,NGELS,NCONTS)
        END IF
      END IF
      END
