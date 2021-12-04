      SUBROUTINE FMAIN()
      CHARACTER SEQ1(10000),SEQ2(10000)
      CHARACTER*40 FILNAM,HELPF*40
      INTEGER DEVNOS(2)
      CALL UNITNO(KBIN,KBOUT,DEVNOS,2)
      WRITE(KBOUT,*)'  bsplit v2.2'
      FILNAM = ' '
      CALL OPENF1(DEVNOS(1),FILNAM,0,IOK,KBIN,KBOUT,
     +'Name of gelinb file',
     + IHELPS,IHELPE,HELPF,IDEVH)
      IF(IOK.NE.0)STOP
      IDIM=10000
      CALL ARRFIL(DEVNOS(1),SEQ1,IDIM,KBOUT)
      CLOSE(UNIT=1)
      FILNAM = ' '
      CALL OPENF1(DEVNOS(2),FILNAM,1,IOK,KBIN,KBOUT,
     +'Name for new file of filenames',
     + IHELPS,IHELPE,HELPF,IDEVH)
      IF(IOK.NE.0)STOP
C   REMOVE UNWANTED CHARS note this is very dangerous and is only for
C                         bbc data that has spurious characters. When 
C                         we move to mac only we should skip this
      I2=0
      DO 100 I=1,IDIM
        IF((LGE(SEQ1(I),'-')).AND.(LLE(SEQ1(I),'Z')) .OR.
     +     (LGE(SEQ1(I),'a')).AND.(LLE(SEQ1(I),'z'))) THEN
          I2=I2+1
          SEQ2(I2)=SEQ1(I)
        END IF
100   CONTINUE
      IDIM2=I2
      IP1=1
      WRITE(KBOUT,1005)IDIM-IDIM2
1005  FORMAT(' number of illegal characters removed',I6)
200   CONTINUE
      CALL CHARCT(SEQ2,IDIM2,IP1,NCHAR)
      IF(NCHAR.GT.0)THEN
        FILNAM(1:40)=' '
        DO 210 I=1,NCHAR
          FILNAM(I:I)=SEQ2(IP1+I-1)
210     CONTINUE
        WRITE(KBOUT,1001)FILNAM
1001    FORMAT(' creating file ',A)
        CALL OPENRS(DEVNOS(1),FILNAM,LRECL,IOK,1)
      IF(IOK.NE.0)STOP
      WRITE(DEVNOS(2),1004)FILNAM
1004  FORMAT(A)
      END IF
      IP1=IP1+NCHAR+1
      IF(IP1.LT.IDIM2)THEN
        CALL CHARCT(SEQ2,IDIM2,IP1,NCHAR)
        IF(NCHAR.GT.0)THEN
          CALL FMTDK(DEVNOS(1),SEQ2(IP1),NCHAR)
        END IF
        WRITE(KBOUT,1002)NCHAR
1002    FORMAT(' length',I6)
        IP1=IP1+NCHAR+1
        IF(IP1.LT.IDIM2)GO TO 200
      END IF
      CLOSE(DEVNOS(2))
      CLOSE(DEVNOS(1))
      END
