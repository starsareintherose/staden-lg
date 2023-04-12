C  SEEME: MACHINE SPECIFIC ROUTINES for dec !!!!!!!!!
C         DEC ULTRIX cahnges: iwordl = 1, seqlibs open 'stream'
C 14-8-91 Added check in openrs for empty file names
C 27-8-91 Started files xspec.f and nxspec.f and split out the nonx
C         and X specific bits from here.
C 27-8-91 Added FLUSHL to flush unit idev
C 15-10-91 Moved IANDRS, IORRS and WRITEB from pl4010.f
C 29-10-91 Replaced lnblnk ( a sun routine) by notrl
C 15-11-91 Added routines to call inquire to see if a file exists
C          and another to delete a file
C 25-2-92 added more bit handling routines for use by sequence library
C         searching routines
C 20-5-92 Added readonly where required for dec!
C         moved getrs, getrsl and seekrs from seqlibsubs.f to here
C   AUTHOR: RODGER STADEN
C   25-JAN-1990 INILU: SAVE DUP,PUP,DLOW,PLOW
C   UNITNO
C   OPENRS
C THE FOLLOWING ARE PROBABLY COMPLETELY PORTABLE
C BUT ARE INCLUDED ANY WAY
C   INITLU
C   IFROMP
C   CTONUM
C   DTONUM
C   IUBM
C   IUBM1
C   IUBM2
C   bsw4
C   bsw2
      SUBROUTINE UNITNO(KBIN,KBOUT,DEVNOS,NDEVS)
C   AUTHOR: RODGER STADEN
      INTEGER DEVNOS(NDEVS)
C   SETS CONSOLE I/O UNITS TO 5,6. OTHERS 10 ONWARDS
      KBIN=5
      KBOUT=6
      ISTART=10
      DO 1 I=1,NDEVS
      DEVNOS(I)=ISTART+I-1
1     CONTINUE
      CALL INITRS()
      END
      SUBROUTINE OPENRS(IDEV,FN,IOK,LRECL,JOB)
C   AUTHOR: RODGER STADEN
      CHARACTER     FN*(*)
      CHARACTER*1024 FILNAM,FNDFIL
      EXTERNAL FNDFIL,NOTRL,INQF
C 14-8-91 Added check for blank file names
C 14-11-91 Added routines to check for file existence
C       ROUTINE TO OPEN FILES
C   JOB=1 SEQUENTIAL FORMATTED NEW, DEFAULT RECORD LENGTH
C      =2 SEQUENTIAL FORMATTED OLD, DEFAULT RECORD LENGTH
C      =3 DIRECT ACCESS, UNFORMATTED NEW
C      =4 DIRECT ACCESS, UNFORMATTED OLD
C      =5 DIRECT ACCESS, UNFORMATTED OLD
C      =6 UNFORMATTED NEW
C      =7 UNFORMATTED OLD
C      =8 STATUS='UNKNOWN'
C      =9 TERMINAL FOR GRAPHICS
C      =10 TERMINAL FOR VT100
C      =11 direct access, single byte record length, old
C   RECORD LENGTHS SENT IN WORDS
C   READONLY IS VAX SPECIFIC*********************
C   NOTE THAT ONLY FOR DIRECT ACCESS FILES ARE RECORD
C   LENGTHS SPECIFIED. ON THE VAX UNFORMATTED DIRECT ACCESS
C   RECORDS ARE DEFINED IN WORDS; ON OTHER MACHINES E.G. SOME UNIX
C   MACHINES THEY ARE SPECIFIED IN BYTES. THE VARIABLE IWORDL IS
C   USED TO MULTIPLY RECORD LENGTHS SPECIFIED IN WORDS TO GIVE
C   THE CORRECT VALUE FOR THE MACHINE. ON A VAX IWORDL=1; ON
C   THE UNIX MACHINE IWORDL=4
       PARAMETER (IWORDL=1)
      IF(NOTRL(FN,LEN(FN),' ').EQ.0) GO TO 100
      IF (JOB.LT.9 .OR. JOB.GT.10) THEN
        FILNAM = FNDFIL(FN)
        INQJ = INQF(FILNAM)
      END IF
      IF(JOB.EQ.1) THEN
        IF(INQJ.NE.0) THEN
C  file exists so tell the user
          IOK = 2
          RETURN
        END IF
                OPEN(UNIT=IDEV,FILE=FILNAM,STATUS='NEW',
     +               ACCESS='SEQUENTIAL',ERR=100)
      ELSE IF(JOB.EQ.2)THEN
        IF(INQJ.NE.1) THEN
C  file inquire shows file does not exist
C  so tell the user
          IOK = 3
          RETURN
        END IF
                OPEN(UNIT=IDEV,FILE=FILNAM,STATUS='OLD',
     +               READONLY,
     +               ACCESS='SEQUENTIAL',ERR=100)
      ELSE IF(JOB.EQ.3)THEN
        IF(INQJ.NE.0) THEN
C  file exists so tell the user
          IOK = 2
          RETURN
        END IF
                JRECL=LRECL*IWORDL
                OPEN(UNIT=IDEV,FILE=FILNAM,STATUS='NEW',
     +               ACCESS='DIRECT',RECL=JRECL,ERR=100)
      ELSE IF(JOB.EQ.4)THEN
        IF(INQJ.NE.1) THEN
C  file inquire shows file does not exist
C  so tell the user
          IOK = 3
          RETURN
        END IF
                JRECL=LRECL*IWORDL
                OPEN(UNIT=IDEV,FILE=FILNAM,STATUS='OLD',
     +               ACCESS='DIRECT',RECL=JRECL,ERR=100)
      ELSE IF(JOB.EQ.5)THEN
                JRECL=LRECL*IWORDL
                OPEN(UNIT=IDEV,FILE=FILNAM,STATUS='OLD',
     +               READONLY,
     +               ACCESS='DIRECT',RECL=JRECL,ERR=100)
      ELSE IF(JOB.EQ.6)THEN
        IF(INQJ.NE.0) THEN
C  file exists so tell the user
          IOK = 2
          RETURN
        END IF
                OPEN(UNIT=IDEV,FILE=FILNAM,STATUS='NEW',
     +               FORM='UNFORMATTED',ERR=100)
      ELSE IF(JOB.EQ.7)THEN
        IF(INQJ.NE.1) THEN
C  file inquire shows file does not exist
C  so tell the user
          IOK = 3
          RETURN
        END IF
                OPEN(UNIT=IDEV,FILE=FILNAM,STATUS='OLD',
     +               READONLY,
     +               FORM='UNFORMATTED',ERR=100)
        ELSE IF(JOB.EQ.8)THEN
                OPEN(UNIT=IDEV,FILE=FILNAM,STATUS='UNKNOWN',
     +               ERR=100)
        ELSE IF(JOB.EQ.9)THEN
C               DEVICE FOR GRAPHICS OUTPUT IS 'TT' IF USING A
C               VT640 ON A VAX. IF USING A SEPARATE TERMINAL FOR
C               GRAPHICS OUTPUT THE TERMINAL PROTECTION MUST BE
C               SET W:RW AND THE DEVICE WILL HAVE A DIFFERENT NAME
C               FOR EXAMPLE 'GRAPHICS' WHERE THE LOGICAL NAME 'GRAPHICS'
C               IS ASSIGNED TO THE APPROPRIATE DEVICE. A VT240 SHOULD
C               BE COMPATIBLE WITH THE TEKTRONIX PLOT COMMANDS.
                OPEN(UNIT=IDEV,FILE='/dev/tty',STATUS='UNKNOWN',
     +               ERR=100)
        ELSE IF(JOB.EQ.10)THEN
C               DEVICE FOR VT100 COMPATIBLE TERMINAL WHEN USED
C               FOR GELIN WHICH REQUIRES COMPLETE CONTROL OF
C               CURSOR POSITIONING. ON A VAX IT IS CALLED 'TT'
                OPEN(UNIT=IDEV,FILE='/dev/tty',STATUS='UNKNOWN',
     +               ERR=100)
        ELSE IF(JOB.EQ.11)THEN
        IF(INQJ.NE.1) THEN
C  file inquire shows file does not exist
C  so tell the user
          IOK = 3
          RETURN
        END IF
                JRECL=1
                OPEN(UNIT=IDEV,FILE=FILNAM,STATUS='OLD',
     +               READONLY,
C     +               RECORDTYPE='STREAM',
     +               ERR=100)
      END IF
      IOK=0
      RETURN
100   CONTINUE
      IOK=1
      RETURN
      END
      CHARACTER*1024 FUNCTION FNDFIL(NAME)
      CHARACTER NAME*(*),TMPNAM*1024,PREFIX*1024
      EXTERNAL NOTRL
C  LOOKS IN ENVIRONMENT FOR FILE PATHS
C  ? CASES DEALT WITH:
C  1) IF NAME STARTS WITH / IT IS ABSOLUTE AND SO UNCHANGED
C  2) IF NAME STARTS WITH ~/ REPLACE WITH HOME DIRECTORY NAME
C  3) IF NAME INCLUDES ANY OTHER / LOOK IN ENVIRONMENT FOR
C     NAME UPTO /, AND ADD PATH TO REST IF FOUND
C     IF NOT FOUND ADD PATH TO CURRENT WORKING DIRECTORY
C  4) ELSE LOOK IN ENVIRONMENT FOR WHOLE NAME
C     IF NOT FOUND ADD PATH TO CURRENT DIRECTORY
C     IF FOUND MAKE IT FILE NAME
C
      IF(NAME(1:1).EQ.'/') THEN
        FNDFIL = NAME
      ELSE IF (NAME(1:2).EQ.'~/') THEN
        CALL GETENV('HOME',TMPNAM)
        FNDFIL = TMPNAM(:NOTRL(TMPNAM,LEN(TMPNAM),' ')) //
     +  NAME(2:NOTRL(NAME,LEN(NAME),' '))
      ELSE IF (INDEX(NAME,'/').NE.0) THEN
        TMPNAM = NAME(1:INDEX(NAME,'/')-1)
        CALL GETENV(TMPNAM,PREFIX)
        I = NOTRL(PREFIX,LEN(PREFIX),' ')
        IF(I.EQ.0) THEN
          CALL GETCWD(PREFIX)
          FNDFIL = PREFIX(:NOTRL(PREFIX,LEN(PREFIX),' ')) //
     +    '/' // NAME(:NOTRL(NAME,LEN(NAME),' '))
        ELSE
          FNDFIL = PREFIX(:NOTRL(PREFIX,LEN(PREFIX),' ')) //
     +    NAME(INDEX(NAME,'/'):)
        END IF
      ELSE
        CALL GETENV(NAME,TMPNAM)
        I = NOTRL(TMPNAM,LEN(TMPNAM),' ')
        IF (I.EQ.0) THEN
          CALL GETCWD(PREFIX)
          FNDFIL = PREFIX(:NOTRL(PREFIX,LEN(PREFIX),' ')) //
     +    '/' // NAME(:NOTRL(NAME,LEN(NAME),' '))
        ELSE
          FNDFIL = TMPNAM
        END IF
      END IF
      END
      INTEGER FUNCTION INQF(NAME)
      CHARACTER NAME*(*)
      LOGICAL EX
      INQUIRE(FILE=NAME,EXIST=EX)
C
C if the file exists return 1, else 0
C
      IF (EX) THEN
        INQF = 1
      ELSE
        INQF = 0
      END IF
      END
      INTEGER FUNCTION DELF(FN,IDEV,JRECL,JOB)
      CHARACTER FN*(*)
      CHARACTER FILNAM*1024,FNDFIL*1024
      EXTERNAL FNDFIL
C
C routine to delete files in the most horrible way i could find
C deal with job 2: old sequential formatted
C deal with job 4: old direct unformatted
C deal with job 7: old sequential unformatted
C if the file exists and is not one of these error =-1
C if the file exists and cannot be deleted error =-2
C if the file cannot be opened error =-3
C
C on the sun this technique will delete any file if the directory
C priviliges are set.
C I dont know if i need to worry about the file types: could
C i just open them all the same way? Although its a mess leave it!
      FILNAM = FNDFIL(FN)
      IF(JOB.EQ.2) THEN
        OPEN(UNIT=IDEV,FILE=FILNAM,STATUS='OLD',
     +  ACCESS='SEQUENTIAL',ERR=100)
        CLOSE(UNIT=IDEV,STATUS='DELETE',ERR=200)
        DELF = 0
      ELSE IF(JOB.EQ.4)THEN
        OPEN(UNIT=IDEV,FILE=FILNAM,STATUS='OLD',
     +  ACCESS='DIRECT',RECL=JRECL,ERR=100)
        CLOSE(UNIT=IDEV,STATUS='DELETE',ERR=200)
        DELF = 0
      ELSE IF(JOB.EQ.7)THEN
        OPEN(UNIT=IDEV,FILE=FILNAM,STATUS='OLD',
     +  FORM='UNFORMATTED',ERR=100)
        CLOSE(UNIT=IDEV,STATUS='DELETE',ERR=200)
        DELF = 0
      ELSE
        DELF = -1
        RETURN
      END IF
      RETURN
 100  CONTINUE
      DELF = -3
      RETURN
 200  CONTINUE
      DELF = -2
      END
C  ROUTINES TO CONTROL CHARACTER LOOKUP
C  FOR BOTH DNA AND PROTEIN SEQUENCES
C  THE INITIALISING ROUTINES ARE SENT THE CHARACTERSET SIZE IDM
C  WHICH DETERMINES WHICH CHARACTERSET IS USED
      SUBROUTINE INITLU(IDM)
C  AUTHOR RODGER STADEN
      INTEGER POINT1(0:255),POINT2(0:255)
      CHARACTER DUP*16,DLOW*16,PUP*26,PLOW*26
      COMMON /IASCI1/POINT1
      COMMON /IASCI2/POINT2
      SAVE /IASCI1/
      SAVE /IASCI2/
      SAVE DUP,PUP,DLOW,PLOW
      DATA DUP/'TCAG-RYWSMKHBVDN'/
      DATA PUP/'CSTPAGNDEQBZHRKMILVFYW-X? '/
      DATA DLOW/'tcag-rywsmkhbvdn'/
      DATA PLOW/'cstpagndeqbzhrkmilvfyw-x? '/
C  ICHAR RETURNS THE COLLATING SEQUENCE NUMBER
C  I WANT 1-5 FOR ACGT OR 1-26 FOR AMINO ACIDS BY USING ICHAR. 
C  THE ACTUAL VALUE RETURNED BY ICHAR IS NOT PORTABLE 
C  SO I NEED TO INITIALIZE POINTR SO THAT THE CORRECT 
C  ELEMENTS CONTAIN VALUES 1 - 5, OR 1 - 26
C  WORKS ON UPPER AND LOWER CASE - REMOVE DLOW,PLOW AND LOOPS 41 AND 51
C  IF LOWERCASE NOT ALLOWED
C
      IF(IDM.EQ.5)THEN
        DO 30 I = 0,255
          POINT1(I) = IDM
          POINT2(I) = 17
30      CONTINUE
        DO 35 I = 1,5
          J = ICHAR(DUP(I:I))
          POINT1(J) = I
35      CONTINUE
        DO 36 I = 1,5
          J = ICHAR(DLOW(I:I))
          POINT1(J) = I
36      CONTINUE
        DO 40 I = 1,16
          J = ICHAR(DUP(I:I))
          POINT2(J) = I
40      CONTINUE
C  DEAL WITH U
          J = ICHAR('U')
          POINT1(J) = 1  
          POINT2(J) = 1  
        DO 41 I = 1,16
          J = ICHAR(DLOW(I:I))
          POINT2(J) = I
41      CONTINUE
C  DEAL WITH U
          J = ICHAR('u')
          POINT1(J) = 1  
          POINT2(J) = 1  
        ELSE IF(IDM.EQ.26)THEN
          DO 45 I = 0,255
            POINT1(I) = IDM
45        CONTINUE
C
        DO 50 I = 1,26
          J = ICHAR(PUP(I:I))
          POINT1(J) = I
50      CONTINUE
        DO 51 I = 1,26
          J = ICHAR(PLOW(I:I))
          POINT1(J) = I
51      CONTINUE
        DO 60 I = 0,255
          POINT2(I) = POINT1(I)
60      CONTINUE
      ELSE
        WRITE(*,*)'ERROR INITIALISING CHARACTER LOOKUP POINTERS'
      END IF
      END
      INTEGER FUNCTION IFROMP(CHAR)
C  AUTHOR RODGER STADEN
      INTEGER POINT1(0:255)
      CHARACTER CHAR
      COMMON /IASCI1/POINT1
      SAVE /IASCI1/
C
C  GET COLLATING SEQUENCE VALUE
      ICOL = ICHAR(CHAR)
C  THIS POINTS TO A VALUE IN POINTR
      IFROMP = POINT1(ICOL)
      END
      INTEGER FUNCTION CTONUM(CHAR)
C  AUTHOR RODGER STADEN
      INTEGER POINT1(0:255)
      CHARACTER CHAR
      COMMON /IASCI1/POINT1
      SAVE /IASCI1/
C
C  GET COLLATING SEQUENCE VALUE
      ICOL = ICHAR(CHAR)
C  THIS POINTS TO A VALUE IN POINTR
      CTONUM = POINT1(ICOL)
      END
      INTEGER FUNCTION DTONUM(CHAR)
C  AUTHOR RODGER STADEN
      INTEGER POINT2(0:255)
      CHARACTER CHAR
      COMMON /IASCI2/POINT2
      SAVE /IASCI2/
C
C  GET COLLATING SEQUENCE VALUE
      ICOL = ICHAR(CHAR)
C  THIS POINTS TO A VALUE IN POINTR
      DTONUM = POINT2(ICOL)
      END
      INTEGER FUNCTION IUBM(SEQ,ENZ)
C  AUTHOR: RODGER STADEN
C  RETURNS 1 FOR A DEFINITE MATCH, 2 FOR POSSIBLE, ELSE 0
      CHARACTER SEQ,ENZ
      INTEGER TABLE(17,17),DTONUM
      EXTERNAL DTONUM
      DATA TABLE/
     +1,0,0,0,2,0,2,2,0,0,2,2,2,0,2,2,0,
     +0,1,0,0,2,0,2,0,2,2,0,2,2,2,0,2,0,
     +0,0,1,0,2,2,0,2,0,2,0,2,0,2,2,2,0,
     +0,0,0,1,2,2,0,0,2,0,2,0,2,2,2,2,0,
     +1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
     +0,0,1,1,2,1,0,2,2,2,2,2,2,2,2,2,0,
     +1,1,0,0,2,0,1,2,2,2,2,2,2,2,2,2,0,
     +1,0,1,0,2,2,2,1,0,2,2,2,2,2,2,2,0,
     +0,1,0,1,2,2,2,0,1,2,2,2,2,2,2,2,0,
     +0,1,1,0,2,2,2,2,2,1,0,2,2,2,2,2,0,
     +1,0,0,1,2,2,2,2,2,2,1,2,2,2,2,2,0,
     +1,1,1,0,2,2,2,1,2,1,2,1,2,2,2,2,0,
     +1,1,0,1,2,2,1,2,1,2,1,2,1,2,2,2,0,
     +0,1,1,1,2,1,2,2,1,1,2,2,2,1,2,2,0,
     +1,0,1,1,2,1,2,1,2,2,1,2,2,2,1,2,0,
     +1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
     +0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0/
C
      IUBM = TABLE(DTONUM(ENZ),DTONUM(SEQ))
      END
      INTEGER FUNCTION IUBM2(SEQ,ENZ)
C  AUTHOR: RODGER STADEN
C  RETURNS 1 FOR A POSSIBLE MATCH, ELSE 0
      CHARACTER SEQ,ENZ
      INTEGER TABLE(17,17),DTONUM
      EXTERNAL DTONUM
      DATA TABLE/
     +1,0,0,0,1,0,1,1,0,0,1,1,1,0,1,1,0,
     +0,1,0,0,1,0,1,0,1,1,0,1,1,1,0,1,0,
     +0,0,1,0,1,1,0,1,0,1,0,1,0,1,1,1,0,
     +0,0,0,1,1,1,0,0,1,0,1,0,1,1,1,1,0,
     +1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
     +0,0,1,1,1,1,0,1,1,1,1,1,1,1,1,1,0,
     +1,1,0,0,1,0,1,1,1,1,1,1,1,1,1,1,0,
     +1,0,1,0,1,1,1,1,0,1,1,1,1,1,1,1,0,
     +0,1,0,1,1,1,1,0,1,1,1,1,1,1,1,1,0,
     +0,1,1,0,1,1,1,1,1,1,0,1,1,1,1,1,0,
     +1,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,0,
     +1,1,1,0,1,1,1,1,1,1,1,1,1,1,1,1,0,
     +1,1,0,1,1,1,1,1,1,1,1,1,1,1,1,1,0,
     +0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,
     +1,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,
     +1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
     +0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0/
C
      IUBM2 = TABLE(DTONUM(ENZ),DTONUM(SEQ))
      END
      INTEGER FUNCTION IUBM1(SEQ,ENZ)
C  AUTHOR: RODGER STADEN
C  RETURNS 1 FOR A DEFINITE MATCH, ELSE 0
      CHARACTER SEQ,ENZ
      INTEGER TABLE(17,17),DTONUM
      EXTERNAL DTONUM
      DATA TABLE/
     +1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
     +0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
     +0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
     +0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,
     +1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
     +0,0,1,1,0,1,0,0,0,0,0,0,0,0,0,0,0,
     +1,1,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,
     +1,0,1,0,0,0,0,1,0,0,0,0,0,0,0,0,0,
     +0,1,0,1,0,0,0,0,1,0,0,0,0,0,0,0,0,
     +0,1,1,0,0,0,0,0,0,1,0,0,0,0,0,0,0,
     +1,0,0,1,0,0,0,0,0,0,1,0,0,0,0,0,0,
     +1,1,1,0,0,0,0,1,0,1,0,1,0,0,0,0,0,
     +1,1,0,1,0,0,1,0,1,0,1,0,1,0,0,0,0,
     +0,1,1,1,0,1,0,0,1,1,0,0,0,1,0,0,0,
     +1,0,1,1,0,1,0,1,0,0,1,0,0,0,1,0,0,
     +1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
     +0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0/
C
      IUBM1 = TABLE(DTONUM(ENZ),DTONUM(SEQ))
      END
C
C
      SUBROUTINE FLUSHL(IDEV)
      CALL FLUSH(IDEV)
C       This implementation uses the Sun Fortran FLUSH statement.
      END
      SUBROUTINE FLUSHO
C       The (single) unit number is currently hard-wired, but must
C       be the same as KBOUT in UNITNO.
      CALL FLUSHL(6)
      END
C
      SUBROUTINE CEDIT(FILNAM)
      CHARACTER COMAND*256,EDITOR*256,FILNAM*(*)
      INTEGER SYSTEM,I
      EXTERNAL NOTRL
      CALL GETENV('SEQEDT',EDITOR)
      I=NOTRL(EDITOR,LEN(EDITOR),' ')
      IF (I.EQ.0) THEN
        EDITOR='vi'
        I=2
      ENDIF
      COMAND = EDITOR(1:I)//' '//FILNAM
      ISTAT = SYSTEM(COMAND)
      END
      SUBROUTINE BSW2(IN,OUT)
C byte order on cd is least significant first
C on some machines this routine should be changed to do nothing
      CHARACTER*2 IN,OUT
C Sun
C        OUT(1:1) = IN(2:2)
C        OUT(2:2) = IN(1:2)
C Alliant
        OUT = IN
      END
      SUBROUTINE BSW4(IN,OUT)
C byte order on cd is least significant first
C on some machines this routine should be changed to do nothing
      CHARACTER IN*4,OUT*4
C Sun
C      DO 10 I = 1,2
C        OUT(I:I) = IN(5-I:5-I)
C        OUT(5-I:5-I) = IN(I:I)
C 10     CONTINUE
C Alliant
      OUT = IN
      END
      INTEGER FUNCTION IANDRS(IIN,JIN)
C   AUTHOR: RODGER STADEN 
C        LOGICAL LIIN,LJIN
C        INTEGER IN,JN
C        EQUIVALENCE (LIIN,IN),(LJIN,JN)
*******    PERFORMS LOGICAL OPERATIONS ON INTEGER VARIABLES
*******    SO COULD BE FLAGGED AS ILLEGAL OR GIVE INCORRECT RESULTS
*******    ON MANY MACHINES CAN BE REPLACED BY LOCAL INTRINSIC IAND
C        IN=IIN
C        JN=JIN
C        LIIN=LIIN.AND.LJIN
C dec
        IANDRS=IAND(IIN,JIN)
C Alliant
C        IANDRS=IAND(IIN,JIN)
        END
      INTEGER FUNCTION IORRS(IIN,JIN)
C   AUTHOR: RODGER STADEN 
C        LOGICAL LIIN,LJIN
C        INTEGER IN,JN
C        EQUIVALENCE (LIIN,IN),(LJIN,JN)
*******    PERFORMS LOGICAL OPERATIONS ON INTEGER VARIABLES
*******    SO COULD BE FLAGGED AS ILLEGAL OR GIVE INCORRECT RESULTS
*******    ON MANY MACHINES CAN BE REPLACED BY LOCAL INTRINSIC IOR
C        IN=IIN
C        JN=JIN
C        LIIN=LIIN.OR.LJIN
C dec
        IORRS=IOR(IIN,JIN)
C Alliant
C        IORRS=IOR(IIN,JIN)
        END
       SUBROUTINE WRITEB(CHARS,NCHAR,IDEVGR)
C   AUTHOR: RODGER STADEN 
       CHARACTER CHARS*(*)
       SAVE NUL
       DATA NUL/0/
C   NOTE NULS STOP CARRIAGE RETURN, LINE FEED
C Sun
       WRITE(IDEVGR,1000)CHARS(1:NCHAR)
1000   FORMAT(A,$)
C Alliant
C      WRITE(IDEVGR,1000)NUL,CHARS(1:NCHAR)
C1000   FORMAT(A1,A)
       END
      SUBROUTINE SABIT(ARRAY,MAXAR,POSN)
      INTEGER ARRAY(0:MAXAR),POSN,ELEMNT,BITNUM
C
C set bit corresponding to posn in array (bits 0-31 per word)
C
      I = POSN - 1
      ELEMNT = I / 32
      BITNUM = MOD(I,32)
C      WRITE(*,*)'ELEMENT, BIT',ELEMNT,BITNUM
      CALL SBITRS(ARRAY(ELEMNT),BITNUM)
      END
      SUBROUTINE AAWORD(ARRAY1,ARRAY2,MAXAR)
      INTEGER ARRAY1(0:MAXAR),ARRAY2(0:MAXAR)
      INTEGER IANDRS
      EXTERNAL IANDRS
C
C and array1 and array2 and put result in array1
C
      DO 10 I=0,MAXAR
        ARRAY1(I) = IANDRS(ARRAY1(I),ARRAY2(I))
 10     CONTINUE
      END
      SUBROUTINE OAWORD(ARRAY1,ARRAY2,MAXAR)
      INTEGER ARRAY1(0:MAXAR),ARRAY2(0:MAXAR)
      INTEGER IORRS
      EXTERNAL IORRS
C
C OR array1 and array2 and put result in array1
C
      DO 10 I=0,MAXAR
        ARRAY1(I) = IORRS(ARRAY1(I),ARRAY2(I))
 10     CONTINUE
      END
      LOGICAL FUNCTION TABIT(ARRAY,MAXAR,POSN)
      INTEGER ARRAY(0:MAXAR),POSN,ELEMNT,BIT
      LOGICAL TBITRS
      EXTERNAL TBITRS
C
C test bit corresponding to posn in array (bits 0-31 per word)
C
      I = POSN - 1
      ELEMNT = I / 32
      BIT = MOD(I,32)
C      WRITE(*,*)'TEST ELEMENT, BIT',ELEMNT,BIT
      TABIT = .FALSE.
      IF (ARRAY(ELEMNT).NE.0) THEN
        TABIT = TBITRS(ARRAY(ELEMNT),BIT)
      END IF 
      END
      SUBROUTINE CABIT(ARRAY,MAXAR)
      INTEGER ARRAY(0:MAXAR),POSN
C
C clear all bits in array (1 word at a time)
C
      DO 10 POSN=0,MAXAR
          ARRAY(POSN) = 0
 10     CONTINUE
      END
      SUBROUTINE SBITRS(WORD,BITNUM)
      INTEGER MASKS(0:31),WORD,BITNUM
      EXTERNAL IORRS
      SAVE MASKS
      DATA MASKS/1,2,4,8,16,32,64,128,256,512,1024,2048,4096,
     +8192,16384,32768,65536,131072,262144,524288,1048576,
     +2097152,4194304,8388608,16777216,33554432,67108864,
     +134217728,268435456,536870912,1073741824,-2147483648/
      WORD = IORRS(WORD,MASKS(BITNUM))
      END
      LOGICAL FUNCTION TBITRS(WORD,BITNUM)
      INTEGER MASKS(0:31),WORD,BITNUM
      EXTERNAL IANDRS
      SAVE MASKS
      DATA MASKS/1,2,4,8,16,32,64,128,256,512,1024,2048,4096,
     +8192,16384,32768,65536,131072,262144,524288,1048576,
     +2097152,4194304,8388608,16777216,33554432,67108864,
     +134217728,268435456,536870912,1073741824,-2147483648/
      I = MASKS(BITNUM)
      TBITRS = (IANDRS(WORD,I).EQ.I)
      END
      INTEGER FUNCTION GETRS(IDEV,STRING,NSTRNG,IBYTE)
      CHARACTER STRING*(*)
      INTEGER SEEKRS
      EXTERNAL SEEKRS
      GETRS = SEEKRS(IDEV,IBYTE-1)
      IF(GETRS.NE.0) RETURN
      READ(IDEV,1000,ERR=20,END=20)STRING(1:NSTRNG)
 1000 FORMAT(A)
      GETRS = 0
      RETURN
 20   CONTINUE
      GETRS = -1
      END
      INTEGER FUNCTION GETRSL(IDEV,STRING,NSTRNG)
      CHARACTER STRING*(*)
      CHARACTER CR
      PARAMETER (CR=CHAR(13))
      READ(IDEV,1000,ERR=100,END=200)STRING(1:NSTRNG)
 1000 FORMAT(A)
      DO 10 I=1,NSTRNG
        IF(STRING(I:I).EQ.CR) THEN
          GETRSL = I-1
          RETURN
        END IF
 10     CONTINUE
      GETRSL = NSTRNG
      RETURN
 100  CONTINUE
      WRITE(*,*)'ERROR READING IN GETRSL'
      GETRSL = -2
      RETURN
 200  CONTINUE
      GETRSL = -1
      WRITE(*,*)'END IN GETRSL'
      END
      INTEGER FUNCTION SEEKRS(IDEV,IREC)
      INTEGER FSEEK
      SEEKRS = FSEEK(IDEV,IREC,0)
      END
