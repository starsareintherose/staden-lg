C  ROUTINE TO TRANSLATE PROSITE DATABASE
C 2-3-92 set filnam = ' ' 
      SUBROUTINE FMAIN()
      PARAMETER (MAXSTR = 80, MAXDEV = 4)
      INTEGER DEVNOS(MAXDEV)
      CHARACTER*(MAXSTR) AMOS,IDLINE,TITLE
      CHARACTER*40 PATNAM,HELPF,FILNAM
      CALL UNITNO(KBIN,KBOUT,DEVNOS,MAXDEV)
      WRITE(KBOUT,*)'SPLITP1 splits prosite.dat into one file per entry'
      WRITE(KBOUT,*)'        and creates and index'
      IDEV1 = DEVNOS(1)
      IDEV2 = DEVNOS(2)
      IDEV4 = DEVNOS(4)
      FILNAM = ' '
      CALL OPENF1(DEVNOS(1),FILNAM,0,IOK,KBIN,KBOUT,
     +'Prosite library file',
     +IHELPS,IHELPE,HELPF,DEVNOS(3))
      IF(IOK.NE.0) STOP
      FILNAM = ' '
      CALL OPENF1(DEVNOS(4),FILNAM,1,IOK,KBIN,KBOUT,
     +'Index file',
     +IHELPS,IHELPE,HELPF,DEVNOS(3))
      IF(IOK.NE.0) STOP
      IPAT = 0
5     CONTINUE
      READ(IDEV1,1000,ERR=200,END=100)AMOS
1000  FORMAT(A)
      IF(AMOS(1:2).EQ.'ID') THEN
        IDLINE = AMOS
        GO TO 5
      END IF
      IF(AMOS(1:2).EQ.'AC') THEN
        IPAT = IPAT + 1
        NAMLEN = INDEX(AMOS(1:),';') - 1
        PATNAM = AMOS(6:NAMLEN)//'.DAT'
1005    FORMAT(' ',A)
      CALL OPENRS(IDEV2,PATNAM,IOK,LRECL,1)
      IF(IOK.NE.0) WRITE(*,*)'SCREAM, FILE OPENING'
1009    FORMAT(' ',A)
        WRITE(IDEV2,1009,ERR=200)IDLINE
        WRITE(IDEV2,1009,ERR=200)AMOS
      END IF
6       CONTINUE
        READ(IDEV1,1000,ERR=200,END=100)AMOS
        WRITE(IDEV2,1009,ERR=200)AMOS
        IF(AMOS(1:2).EQ.'DE') TITLE = AMOS(6:)
        IF(AMOS(1:2).EQ.'DO') THEN
          TITLE(70:80) = PATNAM(3:7)//','//AMOS(10:14)
          WRITE(IDEV4,1009,ERR=200)TITLE
        END IF
        IF(AMOS(1:2).NE.'//') GO TO 6
        GO TO 5
100     CONTINUE
      WRITE(KBOUT,*)IPAT,' files created. Normal termination'
      STOP
200   CONTINUE
      WRITE(KBOUT,*)IPAT,' files created. Abnormal termination'
      END
