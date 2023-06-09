      SUBROUTINE FMAIN()
      PARAMETER (NAMLEN = 60)
      CHARACTER*(NAMLEN) HELPF,FILNAM
      CHARACTER*(NAMLEN) LIBLF
      PARAMETER (
     +           MAXSEQ=330000,
     +           MAXWIR=330000,
     +           MAXDEV=9)

      PARAMETER (
     +             LIBLF='SEQUENCELIBRARIES')
      INTEGER DEVNOS(MAXDEV)
      INTEGER WORKI(MAXWIR)
      CHARACTER SEQ(MAXSEQ)
      PARAMETER (MAXPRM = 16, MAXMEN = 3)
      CHARACTER PROMPT(MAXMEN)*(MAXPRM)
      CALL UNITNO(KBIN,KBOUT,DEVNOS,MAXDEV)
      WRITE(KBOUT,1000)
1000  FORMAT(
     +' LIP (Library interface program) V1.0 April 1993',/,
     +' Author: Rodger Staden'/)
C   READ IN THE POINTERS TO THE HELP FILE
C      CALL SETHLP(HELPS,HELPE,BOTOPT,TOPOPT,POINTF,DEVNOS(4),KBOUT)
C      CALL INITLU(IDM)
      IDEVLL = DEVNOS(5)
      IDEVEN = DEVNOS(6)
      IDEVAN = DEVNOS(7)
      IDEVDL = DEVNOS(8)
      IDEVLF = DEVNOS(9)
      IDEV1  = DEVNOS(1)
      IDEV2  = DEVNOS(2)
      IDEVH  = DEVNOS(4)
      IOPEN = 0
      IDEVOT = KBOUT
      LIBIN = 1
 1    CONTINUE
      IDIMT = MAXSEQ
      PROMPT(1) = 'Search a library'
      PROMPT(2) = 'Extract entries'
      PROMPT(3) = 'Redirect output'
      JOPT = 1
      CALL RADION('Select a task',PROMPT,3,JOPT,
     +IHELPS,IHELPE,HELPF,IDEVH,KBIN,KBOUT)
      IF(JOPT.LT.1) THEN
        IF (IOPEN.EQ.1) THEN
          CLOSE(UNIT=IDEVOT)
          IOPEN = 0
        END IF
        STOP
      END IF
      IF (JOPT.EQ.1) THEN
        CALL RDLIB(SEQ,IDIMT,FILNAM,KBIN,KBOUT,
     +  IHELPS,IHELPE,HELPF,IDEVH,IDEVOT,IDEVLL,IDEVEN,IDEVAN,IDEVDL,
     +  IDEVLF,LIBIN,LIBLF,WORKI,MAXWIR,IOK)
        IF (IOPEN.EQ.1) THEN
          CLOSE(UNIT=IDEVOT)
          IOPEN = 0
          IDEVOT = KBOUT
        END IF
      ELSE IF (JOPT.EQ.2) THEN
        IF (IOPEN.EQ.1) THEN
          CLOSE(UNIT=IDEVOT)
          IOPEN = 0
          IDEVOT = KBOUT
        END IF
        CALL RDLIBB(SEQ,IDIMT,FILNAM,KBIN,KBOUT,
     +  IHELPS,IHELPE,HELPF,IDEVH,IDEV2,IDEVLL,IDEVEN,IDEVAN,IDEVDL,
     +  IDEVLF,LIBIN,LIBLF,WORKI,MAXWIR,IOK)
      ELSE IF (JOPT.EQ.3) THEN
        IF (IOPEN.EQ.1) THEN
          CLOSE(UNIT=IDEVOT)
          IOPEN = 0
        END IF
        IDEVOT = IDEV1
        FILNAM = ' '
        CALL OPENF1(IDEVOT,FILNAM,1,IOK,KBIN,KBOUT,
     +  'Results file',
     +  IHELPS,IHELPE,HELPF,IDEVH)
        IF(IOK.NE.0) THEN
          IDEVOT = KBOUT
          GO TO 1
        END IF
        IOPEN = 1
      END IF
      GO TO 1
      END
