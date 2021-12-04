C  NonX specific bits
C   AUTHOR: RODGER STADEN
      SUBROUTINE INITRS()
      END
C
C   BELL
C       SUBROUTINE TO RING BELL N TIMES
      SUBROUTINE BELL(N,KBOUT)
C   AUTHOR: RODGER STADEN
      SAVE NUL
      DATA NUL/0/
      DO 1 I=1,N
1     WRITE(KBOUT,1000)NUL,CHAR(7)
1000  FORMAT(A1,A)
      END
      INTEGER FUNCTION XVERSN()
C
C set xversn to false for xterm version
C
      XVERSN = 0
      END
      SUBROUTINE UPDOUT()
      END
      SUBROUTINE CXEDIT(IDEV1,IDEV2,IDEV3,IDEVT,IDEVC,
     +RELPG,LNGTHG,LNBR,RNBR,MAXGEL,
     +IDBSIZ,LINCON,LNBRL,IRNO,LNGTHI,
     +PERCD,IDM,J,IOK)
C
C contig editor (note most of these parameters are not used by it anyway!!)
C
      END
