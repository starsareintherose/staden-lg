C  seqlibsubs
C 4-5-93 Added lip routine RDLIBB and lots of diagnostics
C  17-2-93 Stopped keyword and author searches giving "error reading index"
C          by changes to ikwrd and ianum
C  28-9-92 Stopped rdlibl from giving "error reading index"
C          for end of library - sipl,pipl,nipl
C  17-6-92 Added routines to deal with fasta format (Not
C          done very carefully!). Requires a change to library
C          searching programs (sipl, pipl, nipl) to denote the
C          first entry.
C  30-5-91 changed major library format to embl cd
C  1-10-91 removed references to cdseqs
C  21-10-91 added keyword index search for what it is currently worth
C           and changed to new format for brief.idx files
C           In the future I hope the keyword index will be a full
C           text index, then i will activate more of the code ive
C           just added for the keywords. Also I ought to list out the
C           contents of the brief.idx entry for any matches found.
C 16-12-91  Added routines to deal with codata format and modified
C           others accordingly. Externally only calls to cdroml are
C           affected. At the moment am only dealing with an entryname
C           accession number, and brief directory indexes for codata and,
C           untidily offering more to the user
C           which wil reslut in error messages (eg keyword searches).
C           Have called codata format 'ltype b'
C 25-2-92   Added routines to deal with genbank format (as for codata)
C           Added full text index search for embl and swissprot. This
C           needs two integer arrays of size nrecen/32 which 
C           are passed down from main. Also removed filename strings from
C           main.
C           Have replaced the keyword search by the full text search.
C 26-2-92   Fixed bug: ftype is now cleared in rdlb1
C 2-3-92 set filnam = ' ' for calls to openf1
C 3-3-92    correction to keyword access - use (irec-1)*recordsize
C           NOT irec*recordsize
C 11-3-92   Added more error messages, allowed file names of 80 characters
C 12-5-92 added author searches (replaced brief.idx search)
C 14-5-92 minimised the number of array elements used by the bit arrays
C 20-5-92 moved getrs, getrsl seekrs to seeme
      SUBROUTINE RDSEQ(SEQ,MAXSEQ,IDIMT,J1,J2,ISTART,IEND,
     +IDIM1,IDIMB,IDEV,FILNAM,KBIN,KBOUT,
     +IHELPS,IHELPE,HELPF,IDEVH,
     +IDEVOT,IFORNO,IDEVLL,IDEVEN,IDEVAN,IDEVDL,
     +IDEVLF,LIBIN,LIBLF,WORKI,MAXWOR,IOK)
      CHARACTER SEQ(MAXSEQ)
      CHARACTER FILNAM*(*),LIBLF*(*)
      CHARACTER HELPF*(*)
      INTEGER ANSF,STYPE,WORKI(MAXWOR)
      PARAMETER (MAXPRM = 16)
      CHARACTER PROMPT(6)*(MAXPRM)
      IDIMIN=IDIMT
1     CONTINUE
      PROMPT(1) = 'Personal file'
      PROMPT(2) = 'Sequence library'
      STYPE = IFORNO + 1
      CALL RADION('Select sequence source',PROMPT,2,STYPE,
     +IHELPS,IHELPE,HELPF,IDEVH,KBIN,KBOUT)
      IF(STYPE.LT.1) GO TO 10
      IF(STYPE.EQ.1) THEN
        PROMPT(1) = 'Staden'
        PROMPT(2) = 'EMBL'
        PROMPT(3) = 'GenBank'
        PROMPT(4) = 'PIR'
        PROMPT(5) = 'GCG'
        PROMPT(6) = 'FASTA'
        ANSF = 1
        CALL RADION('Select sequence file format',PROMPT,6,ANSF,
     +  IHELPS,IHELPE,HELPF,IDEVH,KBIN,KBOUT)
        IF(ANSF.LT.1)GO TO 10
        FILNAM = ' '
        CALL OPENF1(IDEV,FILNAM,0,IOK,KBIN,KBOUT,
     +  'Sequence file name',
     +  IHELPS,IHELPE,HELPF,IDEVH)
        IF(IOK.NE.0)GO TO 10
        IDIMT=MAXSEQ
        IF(ANSF.EQ.2)THEN
C   READ FROM EMBL FILE
          CALL REMBL(IDEV,SEQ(1),IDIMT,KBOUT)
        ELSE IF(ANSF.EQ.5)THEN
          CALL  RGCG(IDEV,SEQ(1),IDIMT,KBOUT)
        ELSE IF(ANSF.EQ.3)THEN
C   READ FROM GENBANK FILE
          CALL RGEN(IDEV,SEQ(1),IDIMT,KBOUT)
        ELSE IF(ANSF.EQ.1)THEN
C   READ FROM STADEN FILE
          CALL ARRFIL(IDEV,SEQ,IDIMT,KBOUT)
C   REMOVE CONTIG NAME IF PRESENT
          IF(SEQ(20).EQ.'>')THEN
            CALL MOVEC(
     +      SEQ(1),IDIMT,IDIMT,1,-20)
            IDIMT=IDIMT-20
            WRITE(KBOUT,*)' Contig title removed'
          END IF
        ELSE IF(ANSF.EQ.4)THEN
C   READ FROM A PIR FILE
          IDIMT=MAXSEQ
          CALL RDPIRP(SEQ(1),IDIMT,
     +    FILNAM,IDEV,KBIN,KBOUT,
     +    IHELPS,IHELPE,HELPF,IDEVH,IOK)
        ELSE IF(ANSF.EQ.6)THEN
C   READ FROM A FASTA FILE
          IDIMT=MAXSEQ
          CALL RDFASP(SEQ(1),IDIMT,
     +    FILNAM,IDEV,KBIN,KBOUT,
     +    IHELPS,IHELPE,HELPF,IDEVH,IOK)
        END IF
      ELSE IF(STYPE.EQ.2)THEN
C   READ FROM A LIBRARY FILE
        IDIMT=MAXSEQ
        CALL RDLIB(SEQ,IDIMT,FILNAM,KBIN,KBOUT,
     +  IHELPS,IHELPE,HELPF,IDEVH,IDEVOT,IDEVLL,IDEVEN,IDEVAN,IDEVDL,
     +  IDEVLF,LIBIN,LIBLF,WORKI,MAXWOR,IOK)
      END IF
      CLOSE (UNIT=IDEV)
10    CONTINUE
      IF(IDIMT.EQ.0)IDIMT = IDIMIN
      WRITE(KBOUT,1001)IDIMT
1001  FORMAT(' Sequence length ',I6)
      J1=1
      J2=MIN(MAXSEQ,IDIMT)
      ISTART=J1
      IEND=J2
      IDIMB=IEND-ISTART+1
      IDIM1=J2-J1+1
      END
      SUBROUTINE REMBL(IDEV,SEQ,IDIM,KBOUT)
C   AUTHOR: RODGER STADEN
      CHARACTER SEQ(IDIM),CODE*2
10    CONTINUE
      READ(IDEV,1000,END=50)CODE
1000  FORMAT(A)
C   sequence?
      IF(CODE.NE.'SQ')GO TO 10
      IP1=1
      IP2=60
20    CONTINUE
      READ(IDEV,1002,END=30)CODE,(SEQ(K),K=IP1,IP2)
1002  FORMAT(A,3X,6(10A1,1X))
C   end of data?
      IF(CODE(1:1).NE.'/')THEN
C   sequence
        IP1=IP1+60
        IP2=IP2+60
C  check for overflow
        IF(IP2.GT.IDIM)IP2=IDIM
        IF(IP1.LE.IP2)GO TO 20
        WRITE(KBOUT,1005)IDIM
1005    FORMAT(' Maximum sequence length',I6,
     +  ' exceeded, no more read')
        RETURN
      END IF
30    CONTINUE
C   find end of data
      IP3=IP2+1
40    IP3=IP3-1
      IF(IP3.GT.0)THEN
        IF(SEQ(IP3).EQ.' ')GO TO 40
      END IF
C  end found
      IDIM=IP3
      RETURN
50    CONTINUE
C   NO DATA FOUND
      IDIM=0
      END
      SUBROUTINE RGEN(IDEV,SEQ,IDIM,KBOUT)
C   AUTHOR: RODGER STADEN
      CHARACTER LINE*80,LINE2*60,SEQ(IDIM)
C   THIS ROUTINE READS A GENBANK FILE.
C       IT ASSUMES THE WORD ORIGIN APPEARS ON THE LINE
C       IMMEDIATELY PRECEDING THE SEQUENCE, AND THAT
C       THE LAST LINE CONTAINS //
      IDIMIN=IDIM
      IDIM=0
10    READ(IDEV,1000,END=40)LINE
1000  FORMAT(A)
      IF(LINE(1:6).NE.'ORIGIN')GO TO 10
20    READ(IDEV,1000,END=40)LINE
      IF(LINE(1:2).NE.'//')THEN
        LINE2(1:60)=
     +  LINE(11:20)//LINE(22:31)//LINE(33:42)//LINE(44:53)//
     +  LINE(55:64)//LINE(66:75)
        DO 30 I=1,60
          IF(LINE2(I:I).EQ.' ')GO TO 40
          IF(IDIM.LT.IDIMIN)THEN
            IDIM=IDIM+1
            SEQ(IDIM)=LINE2(I:I)
          ELSE
C   OVERFLOW!
            WRITE(KBOUT,1001)IDIMIN
1001  FORMAT(' Maximum sequence length=',I6,'. Only this much read')
            GO TO 40
          END IF
30      CONTINUE
        GO TO 20
      END IF
40    CONTINUE
100   CONTINUE
      CLOSE(UNIT=IDEV)
      END
      SUBROUTINE RDPIRP(SEQ,IDIM,FILNAM,IDEV,KBIN,KBOUT,
     +IHELPS,IHELPE,HELPF,IDEVH,IOK)
C   AUTHOR RODGER STADEN
      CHARACTER FILNAM*(*)
      CHARACTER SEQ(IDIM),TEMP(80),NAME*20,NAMIN*20,NEWNAM*20
      CHARACTER HELPF*(*)
      EXTERNAL NOTRL,NMMTCH
      IDIMIN = IDIM
      IDIM = 0
      IOK = 1
          CALL YESNO(LIST,'Skip listing of entry names',
     +    IHELPS,IHELPE,HELPF,IDEVH,KBIN,KBOUT)
          IF(LIST.LT.0) RETURN
4         CONTINUE 
          LENGTH = NOTRL(NAMIN,20,' ')
          CALL GTSTR('Entry name',NAMIN,NEWNAM,
     +    LENGTH,KBOUT,KBIN,INFLAG)
          IF(INFLAG.EQ.2) RETURN
          IF(INFLAG.EQ.1) THEN
            CALL HELP2(IHELPS,IHELPE,HELPF,IDEVH,KBIN,KBOUT)
            GO TO 4
          END IF
          IF (LENGTH.GT.0) NAMIN = NEWNAM
          CALL CCASE(NAMIN,1)
1005      FORMAT(A)
10        CONTINUE
          READ(IDEV,1005,ERR=200,END=300)NAME
          IF(NAME(1:1).NE.'>')GO TO 10
          IF(LIST.EQ.1)WRITE(KBOUT,1003)NAME
1003      FORMAT(' ',A)
C         IS THIS THE ENTRY WE WANT?
C          IF(NAME(5:).NE.NAMIN(1:12))GO TO 10
          IF(NMMTCH(NAME(5:),NAMIN).NE.0) GO TO 10
          FILNAM(1:16)=NAME(5:20)
          CALL RPIR(SEQ,IDIMIN,IDEV,KBOUT,TEMP,IOK)
          IDIM = IDIMIN
          RETURN
200   CONTINUE
      IOK = 1
      CALL ERROM(KBOUT,'Error reading file')
      RETURN
300   CONTINUE
      IOK = 1
      CALL ERROM(KBOUT,'Unexpected end of file')
      END
      SUBROUTINE RDPIRA(SEQ,IDIM,
     + IDEV,KBOUT,TITLE,FILNAM,LIST,NAMIN,IDEVL)
C   AUTHOR RODGER STADEN
      CHARACTER TITLE*(*),FILNAM*(*)
      PARAMETER (NAMLEN = 10)
      CHARACTER SEQ(IDIM),TEMP(80),NAMIN*(*)
      EXTERNAL NMMTCH
      IF(LIST.EQ.1) THEN
        READ(IDEVL,1005,ERR=999,END=950)NAMIN
      END IF
10    CONTINUE
        READ(IDEV,1005,ERR=200,END=300)TITLE
1005    FORMAT(A)
        IF(TITLE(1:1).NE.'>')GO TO 10
        IF(LIST.EQ.1)THEN
          IF(NMMTCH(TITLE(5:),NAMIN).NE.0) GO TO 10
        END IF
        IF(LIST.EQ.-1)THEN
          IF(NMMTCH(TITLE(5:),NAMIN).EQ.0) THEN
            READ(IDEVL,1005,ERR=999,END=900)NAMIN
            GO TO 10
          END IF
        END IF
      L = INDEX(TITLE(5:),' ')
      IF(L.NE.0) THEN
        L = 4 + L - 1
      ELSE
        L = NAMLEN
      END IF
      FILNAM = TITLE(5:L)
C   GOT WANTED SEQUENCE SO READ IT. FIRST READ 1 LINE TITLE
      CALL RDPIRS(SEQ,IDIM,TEMP,TITLE,IDEV,KBOUT,IOK)
      IF(IOK.EQ.1) GO TO 200
      IF(IOK.EQ.2) GO TO 300
      RETURN
200   CONTINUE
      CALL ERROM(KBOUT,'Error reading library file')
      IOK = 1
      IDIM = -9
      RETURN
300   CONTINUE
      CALL ERROM(KBOUT,'End of library file reached')
      IDIM = -9
      IOK = 2
      RETURN
900   CONTINUE
      NAMIN = ' RODGER'
      GO TO 10
999   CONTINUE
      CALL ERROM(KBOUT,'Error reading entry names file')
      IDIM = -9
      RETURN
 950  CONTINUE
      IOK = 3
      IDIM = -9
      END
      SUBROUTINE RDPIRD(SEQ,IDIM,
     + IDEV,KBOUT,TITLE,NAMIN)
C   AUTHOR RODGER STADEN
C 20-12-90 Removed lines setting idim to 0. Could it have ever worked!
      CHARACTER TITLE*(*)
      CHARACTER SEQ(IDIM),TEMP(80),NAMIN*(*)
      EXTERNAL NMMTCH
10    CONTINUE
        READ(IDEV,1001,ERR=200,END=300)TITLE
        IF(TITLE(1:1).NE.'>')GO TO 10
        IF(NMMTCH(TITLE(5:),NAMIN).NE.0) GO TO 10
1001  FORMAT(A)
      CALL RDPIRS(SEQ,IDIM,TEMP,TITLE,IDEV,KBOUT,IOK)
      IF(IOK.EQ.1) GO TO 200
      IF(IOK.EQ.2) GO TO 300
      RETURN
200   CONTINUE
      CALL ERROM(KBOUT,'Error reading library file')
      IOK = 1
      IDIM = -9
      RETURN
300   CONTINUE
      CALL ERROM(KBOUT,'End of library file reached')
      IOK = 2
      IDIM = -9
      END
      SUBROUTINE RDPIRS(SEQ,IDIM,TEMP,TITLE,IDEV,KBOUT,IOK)
      CHARACTER SEQ(IDIM),TEMP(80),TITLE*(*)
      IDIMIN = IDIM
      IDIM = 0
      ISEQ = 0
      IOK = 0
      READ(IDEV,1005,ERR=200,END=300)TITLE
1005  FORMAT(A)
      NCHRS = 80
20    CONTINUE
      READ(IDEV,1000,ERR=200,END=300)TEMP
1000  FORMAT(80A1)
      DO 40 J=1,NCHRS
        IF(TEMP(J).NE.' ')THEN
C         IS THIS THE END OF THE ENTRY SHOWN BY A * ?
          IF(TEMP(J).EQ.'*')THEN
            IDIM = ISEQ
            RETURN
          END IF
          ISEQ = ISEQ + 1
          IF(ISEQ.GT.IDIMIN)THEN
            WRITE(KBOUT,1010)IDIMIN
1010        FORMAT(' Maximum sequence length (',I7,') reached',
     +      ' no more read')
            IDIM = IDIMIN
            RETURN
          END IF
          SEQ(ISEQ) = TEMP(J)
        END IF
40    CONTINUE
      GO TO 20
200   CONTINUE
      IOK = 1
      IDIM = -9
      RETURN
300   CONTINUE
      IDIM = -9
      IOK = 2
      END
      SUBROUTINE RPIR(SEQ,IDIM,IDEV,KBOUT,TEMP,IOK)
      CHARACTER SEQ(IDIM),TEMP(80)
      IDIMIN = IDIM
      ISEQ = 0
      IOK = 0
1000  FORMAT(80A1)
      READ(IDEV,1000,ERR=200,END=200)TEMP
      WRITE(KBOUT,1014)(TEMP(K),K=1,60)
1014  FORMAT(' ',60A)
20    CONTINUE
      READ(IDEV,1000,ERR=200,END=300)TEMP
      DO 40 J=1,80
        IF(TEMP(J).NE.' ')THEN
          IF(TEMP(J).EQ.'*')GO TO 100
          ISEQ=ISEQ+1
          IF(ISEQ.GT.IDIMIN)THEN
            WRITE(KBOUT,1010)IDIMIN
1010        FORMAT(' Maximum sequence length (',I6,') reached.',
     +      ' No more read')
            GO TO 400
          END IF
          SEQ(ISEQ)=TEMP(J)
        END IF
40    CONTINUE
      GO TO 20
100   CONTINUE
      IDIM=ISEQ
      RETURN
200   CONTINUE
      IOK = 1
      CALL ERROM(KBOUT,'Error reading file')
      RETURN
300   CONTINUE
      WRITE(KBOUT,*)' Warning: No * at end of entry'
      IDIM = ISEQ
      RETURN
400   CONTINUE
      IDIM=IDIMIN
      END
      SUBROUTINE RDFASP(SEQ,IDIM,FILNAM,IDEV,KBIN,KBOUT,
     +IHELPS,IHELPE,HELPF,IDEVH,IOK)
C   AUTHOR RODGER STADEN
      CHARACTER FILNAM*(*)
      CHARACTER SEQ(IDIM),TEMP(80),NAME*80,NAMIN*20,NEWNAM*20
      CHARACTER HELPF*(*)
      EXTERNAL NOTRL,NMMTCH
      SAVE NAMIN
      DATA NAMIN/' '/
      IF (NAMIN(1:1).EQ.' ')CALL GFASNM(IDEV,NAME,NAMIN)
      IDIMIN = IDIM
      IDIM = 0
      IOK = 1
          CALL YESNO(LIST,'Skip listing of entry names',
     +    IHELPS,IHELPE,HELPF,IDEVH,KBIN,KBOUT)
          IF(LIST.LT.0) RETURN
4         CONTINUE 
          LENGTH = NOTRL(NAMIN,20,' ')
          CALL GTSTR('Entry name',NAMIN,NEWNAM,
     +    LENGTH,KBOUT,KBIN,INFLAG)
          IF(INFLAG.EQ.2) RETURN
          IF(INFLAG.EQ.1) THEN
            CALL HELP2(IHELPS,IHELPE,HELPF,IDEVH,KBIN,KBOUT)
            GO TO 4
          END IF
          IF (LENGTH.GT.0) NAMIN = NEWNAM
          CALL CCASE(NAMIN,1)
1005      FORMAT(A)
10        CONTINUE
          READ(IDEV,1005,ERR=200,END=300)NAME
          IF(NAME(1:1).NE.'>')GO TO 10
          IF(LIST.EQ.1)WRITE(KBOUT,1003)NAME
1003      FORMAT(' ',A)
C         IS THIS THE ENTRY WE WANT?
C          IF(NAME(5:).NE.NAMIN(1:12))GO TO 10
          IF(NMMTCH(NAME(2:),NAMIN).NE.0) GO TO 10
          FILNAM = NAME(2:INDEX(NAME,' '))
          CALL RFAS(SEQ,IDIMIN,IDEV,KBOUT,TEMP,IOK)
          IDIM = IDIMIN
          RETURN
200   CONTINUE
      IOK = 1
      CALL ERROM(KBOUT,'Error reading file')
      RETURN
300   CONTINUE
      IOK = 1
      CALL ERROM(KBOUT,'Unexpected end of file')
      END
      SUBROUTINE GFASNM(IDEV,LINE,NAMIN)
      CHARACTER LINE*(*),NAMIN*(*)
      EXTERNAL NOTRL
 10   CONTINUE
      READ(IDEV,1000,END=30,ERR=30)LINE
      IF (LINE(1:1).NE.'>') GO TO 10
      I = MIN(INDEX(LINE,' ')-1,LEN(NAMIN))
      NAMIN  = LINE(2:I)
 30   CONTINUE
      REWIND(UNIT=IDEV)
 1000 FORMAT(A)
      END
      SUBROUTINE RDFASA(SEQ,IDIM,
     + IDEV,KBOUT,TITLE,FILNAM,LIST,NAMIN,IDEVL,ENTRYN)
C   AUTHOR RODGER STADEN
      CHARACTER TITLE*(*),FILNAM*(*)
      PARAMETER (NAMLEN = 10)
      CHARACTER SEQ(IDIM),TEMP*80,NAMIN*(*)
      INTEGER ENTRYN
      EXTERNAL NMMTCH
      SAVE TEMP
C
C for first entry we must find the entry name, but on subsequent visits
C we assume we have an entryname in title. This is because we have no
C terminator and have to keep reading until we meet >
C
      IF(LIST.EQ.1) THEN
        READ(IDEVL,1005,ERR=999,END=950)NAMIN
      END IF
      IF (ENTRYN.EQ.0) THEN
 5      CONTINUE
        READ(IDEV,1005,ERR=200,END=300)TITLE
1005    FORMAT(A)
        IF(TITLE(1:1).NE.'>')GO TO 5
      ELSE
        TITLE = TEMP
      END IF
      ENTRYN = 1
10    CONTINUE
C      write(*,*)namin
C      write(*,*)title
      IF(LIST.EQ.1)THEN
        IF(NMMTCH(TITLE(2:),NAMIN).NE.0) THEN
20        CONTINUE
          READ(IDEV,1005,ERR=200,END=300)TITLE
          IF(TITLE(1:1).NE.'>')GO TO 20
          IF(NMMTCH(TITLE(2:),NAMIN).NE.0) GO TO 20
        END IF
      END IF
      IF(LIST.EQ.-1)THEN
        IF(NMMTCH(TITLE(2:),NAMIN).EQ.0) THEN
30        CONTINUE
          READ(IDEVL,1005,ERR=999,END=900)NAMIN
C          write(*,*)'newnam',namin
40        CONTINUE
          READ(IDEV,1005,ERR=200,END=300)TITLE
          IF(TITLE(1:1).NE.'>')GO TO 40
C          write(*,*)'newtit',title
          IF(NMMTCH(TITLE(2:),NAMIN).EQ.0) GO TO 30
        END IF
      END IF
      L = INDEX(TITLE,' ')
      IF(L.NE.0) THEN
        L = L - 1
      ELSE
        L = NAMLEN
      END IF
      FILNAM = TITLE(2:L)
C   GOT WANTED SEQUENCE SO READ IT. FIRST READ 1 LINE TITLE
      CALL RDFASS(SEQ,IDIM,TEMP,TITLE,IDEV,KBOUT,IOK)
      IF(IOK.EQ.1) GO TO 200
      IF(IOK.EQ.2) GO TO 300
C      write(*,*)'process',filnam
      RETURN
200   CONTINUE
      CALL ERROM(KBOUT,'Error reading library file')
      IOK = 1
      IDIM = -9
      RETURN
300   CONTINUE
      CALL ERROM(KBOUT,'End of library file reached')
      IDIM = -9
      IOK = 2
      RETURN
900   CONTINUE
C
C end of names reached for list of excludes so set silly name
C and read to next entry (allows last entry to be excluded)
C
50    CONTINUE
      READ(IDEV,1005,ERR=200,END=300)TITLE
      IF(TITLE(1:1).NE.'>')GO TO 50
      NAMIN = ' RODGER'
      GO TO 10
999   CONTINUE
      CALL ERROM(KBOUT,'Error reading entry names file')
      IDIM = -9
      RETURN
 950  CONTINUE
      IOK = 3
      IDIM = -9
      END
      SUBROUTINE RDFASD(SEQ,IDIM,
     + IDEV,KBOUT,TITLE,NAMIN)
C   AUTHOR RODGER STADEN
      CHARACTER TITLE*(*)
      CHARACTER SEQ(IDIM),TEMP*80,NAMIN*(*)
      EXTERNAL NMMTCH
10    CONTINUE
        READ(IDEV,1001,ERR=200,END=300)TITLE
        IF(TITLE(1:1).NE.'>')GO TO 10
        IF(NMMTCH(TITLE(2:),NAMIN).NE.0) GO TO 10
1001  FORMAT(A)
      CALL RDFASS(SEQ,IDIM,TEMP,TITLE,IDEV,KBOUT,IOK)
      IF(IOK.EQ.1) GO TO 200
      IF(IOK.EQ.2) GO TO 300
      RETURN
200   CONTINUE
      CALL ERROM(KBOUT,'Error reading library file')
      IOK = 1
      IDIM = -9
      RETURN
300   CONTINUE
      CALL ERROM(KBOUT,'End of library file reached')
      IOK = 2
      IDIM = -9
      END
      SUBROUTINE RDFASS(SEQ,IDIM,TEMP,TITLE,IDEV,KBOUT,IOK)
      CHARACTER SEQ(IDIM),TEMP*(*),TITLE*(*)
      IDIMIN = IDIM
      IDIM = 0
      ISEQ = 0
      IOK = 0
      NCHRS = 80
20    CONTINUE
      READ(IDEV,1000,ERR=200,END=300)TEMP
1000  FORMAT(A)
C      write(*,1000)temp
      IF(TEMP(1:1).EQ.'>') THEN
        IDIM = ISEQ
C        write(*,*)idim
        RETURN
      END IF
      DO 40 J=1,NCHRS
        IF(TEMP(J:J).NE.' ')THEN
          ISEQ = ISEQ + 1
          IF(ISEQ.GT.IDIMIN)THEN
            WRITE(KBOUT,1010)IDIMIN
1010        FORMAT(' Maximum sequence length (',I7,') reached',
     +      ' no more read')
            IDIM = IDIMIN
            RETURN
          END IF
          SEQ(ISEQ) = TEMP(J:J)
        END IF
40    CONTINUE
      GO TO 20
200   CONTINUE
      IOK = 1
      IDIM = -9
      RETURN
300   CONTINUE
      IDIM = ISEQ
      END
      SUBROUTINE RFAS(SEQ,IDIM,IDEV,KBOUT,TEMP,IOK)
      CHARACTER SEQ(IDIM),TEMP(80)
      IDIMIN = IDIM
      ISEQ = 0
      IOK = 0
1000  FORMAT(80A1)
20    CONTINUE
      READ(IDEV,1000,ERR=200,END=100)TEMP
      IF(TEMP(1).EQ.'>')GO TO 100
      DO 40 J=1,80
        IF(TEMP(J).NE.' ')THEN
          ISEQ=ISEQ+1
          IF(ISEQ.GT.IDIMIN)THEN
            WRITE(KBOUT,1010)IDIMIN
1010        FORMAT(' Maximum sequence length (',I6,') reached.',
     +      ' No more read')
            GO TO 400
          END IF
          SEQ(ISEQ)=TEMP(J)
        END IF
40    CONTINUE
      GO TO 20
100   CONTINUE
      IDIM=ISEQ
      RETURN
200   CONTINUE
      IOK = 1
      CALL ERROM(KBOUT,'Error reading file')
      RETURN
400   CONTINUE
      IDIM=IDIMIN
      END
      SUBROUTINE RGCG(IDEV,SEQ,MAXSEQ,KBOUT)
      CHARACTER LINE*133,SEQ(MAXSEQ)
      INTEGER GCGDOT
      EXTERNAL GCGDOT
      IOK = GCGDOT(IDEV,LINE)
      IF(IOK.NE.0) THEN
        WRITE(KBOUT,*)'No .. line found'
        MAXSEQ = 0
        CLOSE(UNIT=IDEV)
        RETURN
      END IF
      CALL RGCGIN(SEQ,MAXSEQ,LINE,IDEV,KBOUT)
      CLOSE(UNIT=IDEV)
      END
      INTEGER FUNCTION GCGDOT(IDEV,LINE)
      CHARACTER LINE*(*)
      GCGDOT = 1
10    CONTINUE
      READ(IDEV,1000,ERR=100,END=100)LINE
1000  FORMAT(A)
      I = INDEX(LINE,'..')
      IF(I.EQ.0) GO TO 10
      GCGDOT = 0
      RETURN
100   CONTINUE
      END     
      SUBROUTINE RGCGIN(SEQ,MAXSEQ,LINE,IDEV,KBOUT)
      CHARACTER SEQ(MAXSEQ)
      PARAMETER (NBAD=11)
      CHARACTER LINE*(*),CHARB*(NBAD)
      PARAMETER (CHARB='0123456789 ')
      INTEGER COKBAD
      EXTERNAL COKBAD
      ISEQ = 0
10    CONTINUE
      READ(IDEV,1000,ERR=100,END=200)LINE
1000  FORMAT(A)
        DO 5 I = 1,LEN(LINE)
          IF(COKBAD(LINE(I:I),CHARB,NBAD).EQ.0) THEN
            ISEQ = ISEQ + 1
            IF(ISEQ.GT.MAXSEQ) THEN
              WRITE(KBOUT,1001)MAXSEQ
1001          FORMAT(' Maximum sequence length',I6,' exceeded')
              RETURN
            END IF
            SEQ(ISEQ) = LINE(I:I)
          END IF
5       CONTINUE
        GO TO 10
100   CONTINUE
      CALL ERROM(KBOUT,'Error reading file')
200   CONTINUE
      MAXSEQ = ISEQ
      END
      INTEGER FUNCTION COKBAD(CHAR,BADC,NBAD)
      CHARACTER CHAR,BADC*(*)
      COKBAD = 1
      DO 10 I = 1,NBAD
        IF(CHAR.EQ.BADC(I:I)) RETURN
10    CONTINUE
      COKBAD = 0
      END
C routines for handling embl cdrom format files
C
C RDLIB  opens cdrom format library and selects options
C CDROMS performs a number of jobs on a cdrom format library:
C        get a sequence, get annotations, get entryname from accession no
C OCDLBS gets the file names and types of a cdrom format lib for use by CDROMS
C CDROML performs a number of jobs on a cdrom format library relating
C        to its use when library searching: read the next entry off the
C        entryname file, and get the seq; get the next named seq; get
C        the next seq not on a list of excluded seqs; get a named seq
C OCDLBL opens a library for use by CDROML
C RDLB0  reads a list of libraries, gets the names of their descriptor files
C        and the prompt to appear on the screen
C RDLB1  gets a list of the file names and file types for a particular lib
      SUBROUTINE RDLB0(LTYPE,LOGNAM,PROMPT,MAXMEN,NAMLEN,MAXPRM,
     +IDEVLS,FILNLL,LINE,MAXLIN,KBOUT,ITEM,IOK)
      CHARACTER PROMPT(MAXMEN)*(*),LOGNAM(MAXMEN)*(*),LTYPE*(*)
      CHARACTER LINE*(*),FILNLL*(*)
C  Read file until the desired line is found
       CALL OPENRS(IDEVLS,FILNLL,IOK,LRECL,2)
       IF(IOK.NE.0) THEN
         CALL ERROM(KBOUT,'Unable to open file of library names')
         CALL ERROM(KBOUT,FILNLL)
         RETURN
      END IF
      LINENO = 0
      ITEM = 1
C  Read and parse until end of data
      CALL RDLB2(LTYPE,LOGNAM,PROMPT,MAXMEN,NAMLEN,MAXPRM,IDEVLS,
     +LINE,MAXLIN,LINENO,ITEM,IOK)
      IF(IOK.NE.0) 
     +WRITE(KBOUT,*)'Error in library menu file on line',LINENO
      ITEM = ITEM - 1
      CLOSE(UNIT=IDEVLS)
      END
      SUBROUTINE RDLB2(LTYPE,LOGNAM,PROMPT,MAXMEN,NAMLEN,MAXPRM,
     +IDEVM,LINE,MAXLIN,LINENO,ITEM,IOK)
      CHARACTER PROMPT(MAXMEN)*(*),LOGNAM(MAXMEN)*(*),LTYPE*(*)
      CHARACTER LINE*(*)
      IOK = 0
C  Read and parse until end of data
C  
10    CONTINUE
      LINENO = LINENO + 1
      READ(IDEVM,1000,ERR=100,END=200)LINE
1000  FORMAT(A)
        CALL RDLB3(LTYPE(ITEM:ITEM),LINE,MAXLIN,LOGNAM(ITEM),
     +  PROMPT(ITEM),
     +  NAMLEN,MAXPRM,IOK)
        IF(IOK.EQ.0) THEN
          ITEM = ITEM + 1
        END IF
      GO TO 10
100   CONTINUE
      IOK = 1
      CALL ERROM(KBOUT,'Error reading list of library files')
      RETURN
200   CONTINUE
      IOK = 0
      END
      SUBROUTINE RDLB3(LTYPE,LINE,MAXLIN,LOGNAM,PROMPT,NAMLEN,
     +MAXPRM,IOK)
      CHARACTER LTYPE,LINE*(*),LOGNAM*(*),PROMPT*(*),COMMNT
      PARAMETER (COMMNT = '!')
      EXTERNAL NOTILR,NOTIRL
C Extract LOGNAM, PROMPT
      IOK = 1
      MAXLN = INDEX(LINE,COMMNT) - 1
      IF(MAXLN.EQ.-1) MAXLN = MAXLIN
      IF(MAXLN.LT.6) RETURN
      LOGS = NOTILR(LINE(2:),MAXLN-1,' ') + 1
      IF(LOGS.EQ.MAXLN) RETURN
      LOGE = INDEX(LINE(LOGS:),' ')
      IF(LOGE.EQ.0) RETURN
      LOGE = LOGE - 1
      LOGEE = LOGE
      LOGE = MIN(NAMLEN,LOGE)
      LOGE = LOGE + LOGS - 1
      LOGNAM = ' '
      LOGNAM = LINE(LOGS:LOGE)
      LOGEE = LOGEE + LOGS - 1
      LOGS = NOTILR(LINE(LOGEE+1:),MAXLN,' ') + LOGEE
      IF(LOGS.EQ.MAXLN) RETURN
      LOGE = NOTIRL(LINE,MAXLN,' ')
      IF(LOGE.EQ.0) RETURN
      LOGE = MIN(MAXPRM,LOGE-LOGS+1) + LOGS - 1
      PROMPT = ' '
      PROMPT = LINE(LOGS:LOGE)
      LTYPE = LINE(1:1)
      IOK = 0
      END
      SUBROUTINE RDLB1(LIBNAM,MAXMEN,NAMLEN,IDEVLS,
     +FILNAM,
     +LINE,MAXLIN,FTYPE,LINENO,KBOUT,ITEM,IOK)
      CHARACTER LIBNAM(MAXMEN)*(*)
      CHARACTER LINE*(*),FTYPE*(*),FILNAM*(*)
      IOK = 0
C  ftype(i:i) stores the file type for libnam(i)
C  and libnam(i) stores the name of a library file
C  Read file until the desired line is found
      FTYPE = ' '
       CALL OPENRS(IDEVLS,FILNAM,IOK,LRECL,2)
       IF(IOK.NE.0) THEN
         CALL ERROM(KBOUT,'Unable to open file of library file names')
         CALL ERROM(KBOUT,FILNAM)
         RETURN
      END IF
      LINENO = 0
      ITEM = 1
C  Read and parse until end of data
C  
10    CONTINUE
      LINENO = LINENO + 1
      READ(IDEVLS,1000,ERR=100,END=200)LINE
1000  FORMAT(A)
        CALL RDLB4(LINE,MAXLIN,FTYPE(ITEM:ITEM),LIBNAM(ITEM),
     +  NAMLEN,IOK)
        IF(IOK.EQ.0) THEN
          ITEM = ITEM + 1
        END IF
      GO TO 10
100   CONTINUE
      IOK = 1
      CALL ERROM(KBOUT,'Error reading names of library files')
      RETURN
200   CONTINUE
      ITEM = ITEM - 1
      IOK = 0
      END
      SUBROUTINE RDLB4(LINE,MAXLIN,LTYPE,LIBNAM,NAMLEN,IOK)
      CHARACTER LINE*(*),LTYPE,LIBNAM*(*),COMMNT
      PARAMETER (COMMNT = '!')
      EXTERNAL NOTILR
C Extract LTYPE, LIBNAM
      IOK = 1
      MAXLN = INDEX(LINE,COMMNT) - 1
      IF(MAXLN.EQ.-1) MAXLN = MAXLIN
      IF(MAXLN.LT.6) RETURN
      LOGS = NOTILR(LINE(2:),MAXLN-1,' ') + 1
      IF(LOGS.EQ.MAXLN) RETURN
      LOGE = INDEX(LINE(LOGS:),' ')
      IF(LOGE.EQ.0) RETURN
      LOGE = LOGE - 1
      LOGEE = LOGE
      LOGE = MIN(NAMLEN,LOGE)
      LOGE = LOGE + LOGS - 1
      LIBNAM = ' '
      LIBNAM = LINE(LOGS:LOGE)
      LTYPE = LINE(1:1)
      IOK = 0
      END
      SUBROUTINE RDLIB(SEQ,IDIM,FILNAM,KBIN,KBOUT,
     +IHELPS,IHELPE,HELPF,IDEVH,IDEVOT,IDEVLL,IDEVEN,IDEVAN,IDEVDL,
     +IDEVLF,LIBIN,LIBLF,WORKI,MAXWOR,IOK)
C   AUTHOR RODGER STADEN
C IDEVLL = unit for library list LIBLF
C IDEVOT = unit for output of results 
C IDEVEN =          entry name file ENAMEF
C IDEVAN = unit for accession number files ANUMTF ANUMHF and keywords
C IDEVDL =          division lookup file DIVLUF
C IDEVLF =          actual library data files DATAF
C note to me: there is really no need to pass all these character strings
C down from main: you have to declare the size here anyway. So sort it out!
      PARAMETER (MAXNAM = 80,LENNAM = 10, MAXKWD = 5)
      PARAMETER (MAXPRM = 50,MAXMEN=20,MAXLIN=80,MAXLBF=9)
      CHARACTER*(MAXNAM) ENAMEF,DIVLUF,DATAF,ANUMTF,ANUMHF
      CHARACTER*(MAXNAM) AUTHHF,AUTHTF
      CHARACTER FILNAM*(*),LIBLF*(*)
      CHARACTER SEQ(IDIM),NAMIN*10,NEWNAM*10
      CHARACTER HELPF*(*),ACNUM*10
      CHARACTER PROMPT(MAXMEN)*(MAXPRM),LOGNAM(MAXMEN)*(MAXNAM)
      CHARACTER FTYPE*(MAXLBF),LINE*(MAXLIN),LIBNAM(MAXLBF)*(MAXNAM)
      CHARACTER LTYPE*(MAXMEN)
      CHARACTER*(MAXNAM) BRIEFF,KWRDTF,KWRDHF
      CHARACTER KEYWDS*80,TITLE*80
      INTEGER WORKI(MAXWOR)
      INTEGER KWS(MAXKWD),KWE(MAXKWD)
      INTEGER DIVCOD,ANNOFF,SEQOFF
      EXTERNAL NOTRL,NMMTCH
      IDIMIN = IDIM
      IDIM = 0
      NAMIN = ' '
      ACNUM = ' '
      NAMIN = ' '
      LIB = LIBIN
1     CONTINUE
      CALL RDLB0(LTYPE,LOGNAM,PROMPT,MAXMEN,MAXNAM,MAXPRM,
     +IDEVLL,LIBLF,LINE,MAXLIN,KBOUT,ITEM,IOK)
      IF(IOK.NE.0) RETURN
      IOK = 1
      CALL RADION('Select a library',PROMPT,ITEM,LIB,
     +IHELPS,IHELPE,HELPF,IDEVH,KBIN,KBOUT)
      IF(LIB.LT.1) RETURN
      FILNAM = LOGNAM(LIB)
      CALL RDLB1(LIBNAM,MAXLBF,MAXNAM,IDEVLL,FILNAM,
     +LINE,MAXLIN,FTYPE,LINENO,KBOUT,ITEM,IOK)
      IF(IOK.NE.0) RETURN
      IF(LTYPE(LIB:LIB).EQ.'A') THEN
        WRITE(KBOUT,*)'Library is in EMBL format with indexes'
        CALL OCDLBS(LIBNAM,MAXNAM,ITEM,FTYPE,
     +  ENAMEF,DIVLUF,ANUMTF,ANUMHF,BRIEFF,KWRDTF,KWRDHF,
     +  AUTHHF,AUTHTF,IOK)
      ELSE IF(LTYPE(LIB:LIB).EQ.'B') THEN
        WRITE(KBOUT,*)'Library is in CODATA format with indexes'
        CALL OCDLBS(LIBNAM,MAXNAM,ITEM,FTYPE,
     +  ENAMEF,DIVLUF,ANUMTF,ANUMHF,BRIEFF,KWRDTF,KWRDHF,
     +  AUTHHF,AUTHTF,IOK)
      ELSE IF(LTYPE(LIB:LIB).EQ.'C') THEN
        WRITE(KBOUT,*)'Library is in GenBank format with indexes'
        CALL OCDLBS(LIBNAM,MAXNAM,ITEM,FTYPE,
     +  ENAMEF,DIVLUF,ANUMTF,ANUMHF,BRIEFF,KWRDTF,KWRDHF,
     +  AUTHHF,AUTHTF,IOK)
      ELSE
        WRITE(KBOUT,*)'Unknown library type'
        RETURN
      END IF
2     CONTINUE
      IOPT = 1
      PROMPT(1) = 'Get a sequence'
      PROMPT(2) = 'Get annotations'
      PROMPT(3) = 'Get entry names from accession numbers'
      PROMPT(4) = 'Search author index'
      PROMPT(5) = 'Search text index for keywords'
      CALL RADION('Select a task',PROMPT,5,IOPT,
     +  IHELPS,IHELPE,HELPF,IDEVH,KBIN,KBOUT)
      IF(IOPT.LT.1) GO TO 1
3     CONTINUE
      IF((IOPT.EQ.1).OR.(IOPT.EQ.2)) THEN
C  get seq or annot
        LENGTH = NOTRL(NAMIN,LENNAM,' ')
        CALL GTSTR('Entry name',NAMIN,NEWNAM,
     +  LENGTH,KBOUT,KBIN,INFLAG)
        IF(INFLAG.EQ.2) GO TO 2
        IF(INFLAG.EQ.1) THEN
          CALL HELP2(IHELPS,IHELPE,HELPF,IDEVH,KBIN,KBOUT)
          GO TO 3
        END IF
        IF(LENGTH.GT.0) NAMIN = NEWNAM
        CALL CCASE(NAMIN,1)
        CALL CDROMS(IOPT,NAMIN,ACNUM,
     +  ENAMEF,DIVLUF,ANUMTF,ANUMHF,DATAF,BRIEFF,KWRDTF,KWRDHF,
     +  IDEVEN,IDEVAN,IDEVDL,IDEVLF,KBIN,AUTHHF,AUTHTF,
     +  KBOUT,DIVCOD,SEQOFF,ANNOFF,SEQ,IDIMIN,IDEVOT,
     +  KEYWDS,KWS,KWE,MAXKEY,LTYPE(LIB:LIB),WORKI,MAXWOR,TITLE,IOK)
        IF((IOPT.EQ.1).AND.(IOK.EQ.0)) IDIM = IDIMIN
        FILNAM = NAMIN
        CLOSE(UNIT=IDEVEN)
        CLOSE(UNIT=IDEVAN)
        CLOSE(UNIT=IDEVLF)
        CLOSE(UNIT=IDEVDL)
      ELSE IF(IOPT.EQ.3) THEN
 4      CONTINUE
        LENGTH = NOTRL(ACNUM,LENNAM,' ')
        CALL GTSTR('Accession number',ACNUM,NEWNAM,
     +  LENGTH,KBOUT,KBIN,INFLAG)
        IF(INFLAG.EQ.2) GO TO 2
        IF(INFLAG.EQ.1) THEN
          CALL HELP2(IHELPS,IHELPE,HELPF,IDEVH,KBIN,KBOUT)
          GO TO 3
        END IF
        IF(LENGTH.GT.0) ACNUM = NEWNAM
        CALL CCASE(ACNUM,1)
        CALL CDROMS(IOPT,NAMIN,ACNUM,
     +  ENAMEF,DIVLUF,ANUMTF,ANUMHF,DATAF,BRIEFF,KWRDTF,KWRDHF,
     +  IDEVEN,IDEVAN,IDEVDL,IDEVLF,KBIN,AUTHHF,AUTHTF,
     +  KBOUT,DIVCOD,SEQOFF,ANNOFF,SEQ,IDIMIN,IDEVOT,
     +  KEYWDS,KWS,KWE,MAXKEY,LTYPE(LIB:LIB),WORKI,MAXWOR,TITLE,IOK)
        CLOSE(UNIT=IDEVEN)
        CLOSE(UNIT=IDEVAN)
        CLOSE(UNIT=IDEVLF)
        CLOSE(UNIT=IDEVDL)
      ELSE IF(IOPT.EQ.4) THEN
        NKEYS = MAXKWD
        CALL SRCKEZ(KBIN,KBOUT,KEYWDS,KWS,KWE,NKEYS,'Authors',IOK)
        IF(IOK.NE.0) GO TO 2
        IF(NKEYS.LT.1) GO TO 2
        CALL CDROMS(IOPT,NAMIN,ACNUM,
     +  ENAMEF,DIVLUF,ANUMTF,ANUMHF,DATAF,BRIEFF,KWRDTF,KWRDHF,
     +  IDEVEN,IDEVAN,IDEVDL,IDEVLF,KBIN,AUTHHF,AUTHTF,
     +  KBOUT,DIVCOD,SEQOFF,ANNOFF,SEQ,IDIMIN,IDEVOT,
     +  KEYWDS,KWS,KWE,NKEYS,LTYPE(LIB:LIB),WORKI,MAXWOR,TITLE,IOK)
        CLOSE(UNIT=IDEVEN)
        GO TO 2
      ELSE IF(IOPT.EQ.5) THEN
        NKEYS = MAXKWD
        CALL SRCKEZ(KBIN,KBOUT,KEYWDS,KWS,KWE,NKEYS,'Keywords',IOK)
        IF(IOK.NE.0) GO TO 2
        IF(NKEYS.LT.1) GO TO 2
        CALL CDROMS(IOPT,NAMIN,ACNUM,
     +  ENAMEF,DIVLUF,ANUMTF,ANUMHF,DATAF,BRIEFF,KWRDTF,KWRDHF,
     +  IDEVEN,IDEVAN,IDEVDL,IDEVLF,KBIN,AUTHHF,AUTHTF,
     +  KBOUT,DIVCOD,SEQOFF,ANNOFF,SEQ,IDIMIN,IDEVOT,
     +  KEYWDS,KWS,KWE,NKEYS,LTYPE(LIB:LIB),WORKI,MAXWOR,TITLE,IOK)
        CLOSE(UNIT=IDEVEN)
        GO TO 2
      END IF
      IF(IOPT.EQ.1) RETURN
      GO TO 2
      END
      SUBROUTINE RDLIBB(SEQ,IDIM,FILNAM,KBIN,KBOUT,
     +IHELPS,IHELPE,HELPF,IDEVH,IDEVOT,IDEVLL,IDEVEN,IDEVAN,IDEVDL,
     +IDEVLF,LIBIN,LIBLF,WORKI,MAXWOR,IOK)
C   AUTHOR RODGER STADEN
C routine for lip
C IDEVLL = unit for library list LIBLF
C IDEVOT = unit for output of results 
C IDEVEN =          entry name file ENAMEF
C IDEVAN = unit for accession number files ANUMTF ANUMHF and keywords
C IDEVDL =          division lookup file DIVLUF
C IDEVLF =          actual library data files DATAF
C note to me: there is really no need to pass all these character strings
C down from main: you have to declare the size here anyway. So sort it out!
      PARAMETER (MAXNAM = 80,LENNAM = 10, MAXKWD = 5)
      PARAMETER (MAXPRM = 50,MAXMEN=20,MAXLIN=80,MAXLBF=9)
      CHARACTER*(MAXNAM) ENAMEF,DIVLUF,DATAF,ANUMTF,ANUMHF
      CHARACTER*(MAXNAM) AUTHHF,AUTHTF
      CHARACTER FILNAM*(*),LIBLF*(*)
      CHARACTER SEQ(IDIM),NAMIN*10,NEWNAM*14
      CHARACTER HELPF*(*),ACNUM*10
      CHARACTER PROMPT(MAXMEN)*(MAXPRM),LOGNAM(MAXMEN)*(MAXNAM)
      CHARACTER FTYPE*(MAXLBF),LINE*(MAXLIN),LIBNAM(MAXLBF)*(MAXNAM)
      CHARACTER LTYPE*(MAXMEN)
      CHARACTER*(MAXNAM) BRIEFF,KWRDTF,KWRDHF
      CHARACTER KEYWDS*80,TITLE*80,EXT*4
      INTEGER WORKI(MAXWOR)
      INTEGER KWS(MAXKWD),KWE(MAXKWD)
      INTEGER DIVCOD,ANNOFF,SEQOFF,GNFFOF
      EXTERNAL NOTRL,NMMTCH,GNFFOF
      NAMIN = ' '
      ACNUM = ' '
      NAMIN = ' '
      LIB = LIBIN
1     CONTINUE
      CALL RDLB0(LTYPE,LOGNAM,PROMPT,MAXMEN,MAXNAM,MAXPRM,
     +IDEVLL,LIBLF,LINE,MAXLIN,KBOUT,ITEM,IOK)
      IF(IOK.NE.0) RETURN
      IOK = 1
      CALL RADION('Select a library',PROMPT,ITEM,LIB,
     +IHELPS,IHELPE,HELPF,IDEVH,KBIN,KBOUT)
      IF(LIB.LT.1) RETURN
      FILNAM = LOGNAM(LIB)
      CALL RDLB1(LIBNAM,MAXLBF,MAXNAM,IDEVLL,FILNAM,
     +LINE,MAXLIN,FTYPE,LINENO,KBOUT,ITEM,IOK)
      IF(IOK.NE.0) RETURN
      IF(LTYPE(LIB:LIB).EQ.'A') THEN
        WRITE(KBOUT,*)'Library is in EMBL format with indexes'
        CALL OCDLBS(LIBNAM,MAXNAM,ITEM,FTYPE,
     +  ENAMEF,DIVLUF,ANUMTF,ANUMHF,BRIEFF,KWRDTF,KWRDHF,
     +  AUTHHF,AUTHTF,IOK)
      ELSE IF(LTYPE(LIB:LIB).EQ.'B') THEN
        WRITE(KBOUT,*)'Library is in CODATA format with indexes'
        CALL OCDLBS(LIBNAM,MAXNAM,ITEM,FTYPE,
     +  ENAMEF,DIVLUF,ANUMTF,ANUMHF,BRIEFF,KWRDTF,KWRDHF,
     +  AUTHHF,AUTHTF,IOK)
      ELSE IF(LTYPE(LIB:LIB).EQ.'C') THEN
        WRITE(KBOUT,*)'Library is in GenBank format with indexes'
        CALL OCDLBS(LIBNAM,MAXNAM,ITEM,FTYPE,
     +  ENAMEF,DIVLUF,ANUMTF,ANUMHF,BRIEFF,KWRDTF,KWRDHF,
     +  AUTHHF,AUTHTF,IOK)
      ELSE
        WRITE(KBOUT,*)'Unknown library type'
        RETURN
      END IF
2     CONTINUE
      PROMPT(1) = 'Sequence only in FASTA format'
      PROMPT(2) = 'Sequence only in STADEN format'
      PROMPT(3) = 'Annotation only'
      PROMPT(4) = 'Complete entries'
      JOPT = 1
      CALL RADION('Select a format',PROMPT,4,JOPT,
     +IHELPS,IHELPE,HELPF,IDEVH,KBIN,KBOUT)
      IF(JOPT.LT.1) RETURN
C      IOPT = 2
        FILNAM = ' '
        CALL OPENF1(IDEVLL,FILNAM,0,IOK,KBIN,KBOUT,
     +  'File of entry names',
     +  IHELPS,IHELPE,HELPF,IDEVH)
        IF(IOK.NE.0)RETURN
        IF (JOPT.EQ.1) THEN
          EXT = '.seq'
          IOPT = 1
        ELSE IF (JOPT.EQ.2) THEN
          EXT = '.SEQ'
          IOPT = 1
        ELSE IF (JOPT.EQ.3) THEN
          EXT = '.txt'
          IOPT = 2
        ELSE IF (JOPT.EQ.4) THEN
          EXT = '.all'
          IOPT = 6
        END IF
3     CONTINUE
        IOK = GNFFOF(IDEVLL,NAMIN)
        IF (IOK.EQ.1) GO TO 1
        IF (IOK.NE.0) GO TO 3
        NEWNAM = NAMIN
        K = INDEX(NEWNAM,' ')
        NEWNAM(K:K+3) = EXT
        CALL OPENRS(IDEVOT,NEWNAM,IOK,LRECL,1)
        IF(IOK.NE.0) THEN
          CALL ERROM(KBOUT,'Unable to open file')
          CALL ERROM(KBOUT,NEWNAM)
        ELSE
        CALL CCASE(NAMIN,1)
        IDIMIN = IDIM
        CALL CDROMS(IOPT,NAMIN,ACNUM,
     +  ENAMEF,DIVLUF,ANUMTF,ANUMHF,DATAF,BRIEFF,KWRDTF,KWRDHF,
     +  IDEVEN,IDEVAN,IDEVDL,IDEVLF,KBIN,AUTHHF,AUTHTF,
     +  KBOUT,DIVCOD,SEQOFF,ANNOFF,SEQ,IDIMIN,IDEVOT,
     +  KEYWDS,KWS,KWE,MAXKEY,LTYPE(LIB:LIB),WORKI,MAXWOR,TITLE,IOK)
        CLOSE(UNIT=IDEVEN)
        CLOSE(UNIT=IDEVAN)
        CLOSE(UNIT=IDEVLF)
        CLOSE(UNIT=IDEVDL)
        IF (JOPT.EQ.1) THEN
          CALL WRITFF(IDEVOT,SEQ,IDIMIN,NAMIN,TITLE)
        ELSE IF (JOPT.EQ.2) THEN
          WRITE(IDEVOT,1001)NAMIN
          WRITE(IDEVOT,1001)TITLE
 1001     FORMAT(';',A)
          CALL FMTDKN(IDEVOT,SEQ,IDIMIN)
        END IF
        CLOSE(UNIT=IDEVOT)
      END IF
      GO TO 3
      END
      SUBROUTINE OCDLBS(LIBNAM,NAMLEN,ITEMS,FTYPE,
     +ENAMEF,DIVLUF,ANUMTF,ANUMHF,BRIEFF,KWRDTF,KWRDHF,
     +AUTHHF,AUTHTF,IOK)
C Assigns libnam names to file name strings
      CHARACTER LIBNAM(ITEMS)*(*),AUTHHF*(*),AUTHTF*(*)
      CHARACTER ENAMEF*(*),DIVLUF*(*),BRIEFF*(*),KWRDTF*(*)
      CHARACTER ANUMTF*(*),ANUMHF*(*),FTYPE*(*),KWRDHF*(*)
      ENAMEF = ' '
      DIVLUF = ' '
      ANUMTF = ' '
      ANUMHF = ' '
      BRIEFF = ' '
      KWRDTF = ' '
      KWRDHF = ' '
      AUTHTF = ' '
      AUTHHF = ' '
      I = INDEX(FTYPE,'A')
      IF(I.NE.0) DIVLUF = LIBNAM(I)
      I = INDEX(FTYPE,'B')
      IF(I.NE.0) ENAMEF = LIBNAM(I)
      I = INDEX(FTYPE,'C')
      IF(I.NE.0) ANUMTF = LIBNAM(I)
      I = INDEX(FTYPE,'D')
      IF(I.NE.0) ANUMHF = LIBNAM(I)
      I = INDEX(FTYPE,'E')
      IF(I.NE.0) BRIEFF = LIBNAM(I)
      I = INDEX(FTYPE,'F')
      IF(I.NE.0) KWRDTF = LIBNAM(I)
      I = INDEX(FTYPE,'G')
      IF(I.NE.0) KWRDHF = LIBNAM(I)
      I = INDEX(FTYPE,'H')
      IF(I.NE.0) AUTHTF = LIBNAM(I)
      I = INDEX(FTYPE,'I')
      IF(I.NE.0) AUTHHF = LIBNAM(I)
      END
      SUBROUTINE CDROMS(JOB,ENAME,ACNUM,
     +ENAMEF,DIVLUF,ANUMTF,ANUMHF,LIBF,BRIEFF,KWRDTF,KWRDHF,
     +IDEVEN,IDEVAN,IDEVDL,IDEVLF,KBIN,AUTHHF,AUTHTF,
     +KBOUT,DIVCOD,SEQOFF,ANNOFF,SEQ,IDSEQ,IDEVOT,
     +KEYS,SS,SE,NKEYS,LTYPE,WORKI,MAXWOR,TITLE,IOK)
      CHARACTER ENAMEF*(*),DIVLUF*(*),LIBF*(*),KWRDTF*(*)
      CHARACTER ANUMTF*(*),ANUMHF*(*),BRIEFF*(*),KWRDHF*(*)
      CHARACTER ENAME*(*),ACNUM*(*),BARRAY*80,TERMA*5,TIT*5
      CHARACTER SEQ(IDSEQ)
      INTEGER DIVCOD,ANNOFF,SEQOFF,ENTFN,RDANUM,RSIZEN,OPENFU
      INTEGER CDSEQ,CDANN,CDANNT,OPENFF,SEEKRS,RDKWRD,CODSEQ
      CHARACTER KEYS*(*),LTYPE*1,AUTHHF*(*),AUTHTF*(*),TITLE*(*)
      INTEGER SS(NKEYS),SE(NKEYS),GENSEQ,WORKI(MAXWOR)
      EXTERNAL OPENFF,SEEKRS,IENAME,RDKWRD,CODSEQ,GENSEQ
      EXTERNAL ENTFN,RDANUM,IHEAD,OPENFU,CDSEQ,CDANN,CDANNT
C
C read lib data from cdrom format
C all files are opened and used as required
C job = 1 get a sequence ENAME and display its title
C job = 2 get annotation ENAME and display it
C job = 3 find entry names from accession number ACNUM
C job = 4 search author index for keywords
C job = 5 search keyword index for keywords
C job = 6 get complete entry ENAME
C
C
C  LTYPE = A means embl, B means codata, C means GenBank
C
      IF(LTYPE.EQ.'A') THEN
        TERMA = 'SQ   '
        TIT = 'DE   '
      ELSE IF(LTYPE.EQ.'B') THEN
        TERMA = 'SEQUE'
        TIT = 'TITLE'
      ELSE IF(LTYPE.EQ.'C') THEN
        TERMA = 'ORIGI'
        TIT = 'DEFIN'
      END IF
C
C open entryname file
C
      IOK = OPENFU(IDEVEN,ENAMEF)
      IF(IOK.NE.0) THEN
        CALL ERROM(KBOUT,'Error opening entry names index')
        CALL ERROM(KBOUT,ENAMEF)
        RETURN
      END IF
      IOK = IHEAD(IDEVEN,BARRAY,NRECEN,RSIZEN)
      IF(IOK.NE.0) THEN
        CALL ERROM(KBOUT,'Error reading entry name header')
        CALL ERROM(KBOUT,ENAMEF)
        RETURN
      END IF
      IF(JOB.EQ.1) THEN
C
C get a sequence
C
C
C open division lookup file
C
        IOK = OPENFF(IDEVDL,DIVLUF)
        IF(IOK.NE.0) THEN
          CALL ERROM(KBOUT,'Error opening division lookup file')
          CALL ERROM(KBOUT,DIVLUF)
          RETURN
        END IF
C
C get offsets and divcode
C
        IOK = IENAME(IDEVEN,NRECEN,RSIZEN,ENAME,ANNOFF,SEQOFF,
     +  DIVCOD,BARRAY)
        IF(IOK.NE.0) THEN
          WRITE(KBOUT,*)ENAME,' not found'
          RETURN
        END IF
C
C get division file name
C
        IOK = ENTFN(IDEVDL,DIVCOD,LIBF,BARRAY)
        CLOSE(UNIT=IDEVDL)
        IF(IOK.NE.0) THEN
          CALL ERROM(KBOUT,'Error reading division lookup file')
          CALL ERROM(KBOUT,DIVLUF)
          RETURN
        END IF
C
C open division
C
        IOK = OPENFF(IDEVLF,LIBF)
        IF(IOK.NE.0) THEN
          CALL ERROM(KBOUT,'Error opening division file')
          CALL ERROM(KBOUT,LIBF)
          RETURN
        END IF
C
C read seq, preceded by title
C
        IOK = CDANNT(IDEVLF,ANNOFF,BARRAY,TIT)
        IF(IOK.NE.0) THEN
          CALL ERROM(KBOUT,'Error locating title')
          RETURN
        END IF
        WRITE(KBOUT,*)BARRAY
        TITLE = BARRAY
        IOK = 99
        IF(LTYPE.EQ.'A') THEN
          IOK = CDSEQ(IDEVLF,SEQOFF,SEQ,IDSEQ,LTYPE)
        ELSE IF(LTYPE.EQ.'B') THEN
          IOK = CODSEQ(IDEVLF,SEQOFF,SEQ,IDSEQ,LTYPE)
        ELSE IF(LTYPE.EQ.'C') THEN
          IOK = GENSEQ(IDEVLF,SEQOFF,SEQ,IDSEQ,LTYPE)
        END IF
        IF(IOK.EQ.0) RETURN
        IF(IOK.EQ.2) THEN
          WRITE(KBOUT,*)
     +    'Only first ',IDSEQ,' characters of sequence read'
          IOK = 0
        END IF
        RETURN
      ELSE IF(JOB.EQ.2) THEN
C
C open division lookup file
C
        IOK = OPENFF(IDEVDL,DIVLUF)
        IF(IOK.NE.0) THEN
          CALL ERROM(KBOUT,'Error opening division lookup file')
          CALL ERROM(KBOUT,DIVLUF)
          RETURN
        END IF
C
C get offsets and divcode
C
        IOK = IENAME(IDEVEN,NRECEN,RSIZEN,ENAME,ANNOFF,SEQOFF,
     +  DIVCOD,BARRAY)
        IF(IOK.NE.0) THEN
          WRITE(KBOUT,*)ENAME,' not found'
          RETURN
        END IF
C
C get division file name
C
        IOK = ENTFN(IDEVDL,DIVCOD,LIBF,BARRAY)
        CLOSE(UNIT=IDEVDL)
        IF(IOK.NE.0) THEN
          CALL ERROM(KBOUT,'Error reading division lookup file')
          CALL ERROM(KBOUT,DIVLUF)
          RETURN
        END IF
C
C open division
C
        IOK = OPENFF(IDEVLF,LIBF)
        IF(IOK.NE.0) THEN
          CALL ERROM(KBOUT,'Error opening division file')
          CALL ERROM(KBOUT,LIBF)
          RETURN
        END IF
C
C read ann
C
        IOK = CDANN(IDEVLF,ANNOFF,BARRAY,IDEVOT,KBIN,KBOUT,TERMA)
        IF(IOK.EQ.2) IOK = 0
        IF(IOK.NE.0) THEN
          CALL ERROM(KBOUT,'Error reading annotation file')
        END IF
        RETURN
C
C deal with accession number start point
C
      ELSE IF(JOB.EQ.3) THEN
C
C read accession number
C
        IOK = RDANUM(IDEVAN,ANUMTF,ANUMHF,IDEVEN,NRECEN,RSIZEN,
     +  ACNUM,BARRAY,ENAME,IDEVOT,KBOUT)
        IF(IOK.NE.0) THEN
          CALL ERROM(KBOUT,'Error reading accession number files')
          RETURN
        END IF
C
C Do author search
C
      ELSE IF(JOB.EQ.4) THEN
C
C 
C
        MAXW2 = 1+(NRECEN-1)/32
        IF(MAXWOR/2.LT.MAXW2) THEN
          CALL ERROM(KBOUT,'Too many entries for bit files')
          RETURN
        END IF
        IOK = RDKWRD(IDEVAN,AUTHTF,AUTHHF,IDEVEN,NRECEN,RSIZEN,
     +  KEYS,SS,SE,NKEYS,BARRAY,ENAME,IDEVOT,KBIN,KBOUT,BRIEFF,
     +  WORKI,WORKI(MAXW2+1),MAXW2)
        IF(IOK.NE.0) THEN
          CALL ERROM(KBOUT,'Error searching author index for keywords')
          RETURN
        END IF
C
C Do text search
C
      ELSE IF(JOB.EQ.5) THEN
C
C 
C
        MAXW2 = 1+(NRECEN-1)/32
        IF(MAXWOR/2.LT.MAXW2) THEN
          CALL ERROM(KBOUT,'Too many entries for bit files')
          RETURN
        END IF
        IOK = RDKWRD(IDEVAN,KWRDTF,KWRDHF,IDEVEN,NRECEN,RSIZEN,
     +  KEYS,SS,SE,NKEYS,BARRAY,ENAME,IDEVOT,KBIN,KBOUT,BRIEFF,
     +  WORKI,WORKI(MAXW2+1),MAXW2)
        IF(IOK.NE.0) THEN
          CALL ERROM(KBOUT,'Error searching text index for keywords')
          RETURN
        END IF
      ELSE IF(JOB.EQ.6) THEN
C
C open division lookup file
C
        IOK = OPENFF(IDEVDL,DIVLUF)
        IF(IOK.NE.0) THEN
          CALL ERROM(KBOUT,'Error opening division lookup file')
          CALL ERROM(KBOUT,DIVLUF)
          RETURN
        END IF
C
C get offsets and divcode
C
        IOK = IENAME(IDEVEN,NRECEN,RSIZEN,ENAME,ANNOFF,SEQOFF,
     +  DIVCOD,BARRAY)
        IF(IOK.NE.0) THEN
          WRITE(KBOUT,*)ENAME,' not found'
          RETURN
        END IF
C
C get division file name
C
        IOK = ENTFN(IDEVDL,DIVCOD,LIBF,BARRAY)
        CLOSE(UNIT=IDEVDL)
        IF(IOK.NE.0) THEN
          CALL ERROM(KBOUT,'Error reading division lookup file')
          CALL ERROM(KBOUT,DIVLUF)
          RETURN
        END IF
C
C open division
C
        IOK = OPENFF(IDEVLF,LIBF)
        IF(IOK.NE.0) THEN
          CALL ERROM(KBOUT,'Error opening division file')
          CALL ERROM(KBOUT,LIBF)
          RETURN
        END IF
C
C read ann
C
        IOK = CDANN(IDEVLF,ANNOFF,BARRAY,IDEVOT,KBIN,KBOUT,'//')
        IF(IOK.EQ.2) IOK = 0
        IF(IOK.NE.0) THEN
          CALL ERROM(KBOUT,'Error reading annotation file')
        END IF
        RETURN
      END IF
      END
      SUBROUTINE SRCKEY(IDEVIN,IDEVOT,KBIN,KBOUT,IOK)
      CHARACTER STRING*80,NEW*80
      INTEGER J1(5),J2(5)
      INTEGER NOTLR
      EXTERNAL NOTLR
      WRITE(KBOUT,1000)
1000  FORMAT(' Search for keywords')
10    CONTINUE
      LENGTH = 0
      STRING = ' '
      NEW = ' '
      CALL GTSTR('Keywords',STRING,NEW,LENGTH,KBOUT,KBIN,INFLAG)
      LENGTH = 80
      IF(INFLAG.NE.0) RETURN
      STRING = NEW
C FIND SEPARATE WORDS
      I = 0
      I2 = 0
20    CONTINUE
        I = I + 1
        IF(I.LT.6)THEN
          I1 = I2 + 1
          LEFT = LENGTH - I1 + 1
          IF(LEFT.GT.0)THEN
            IT = NOTLR(STRING(I1:LENGTH),LEFT,' ')
            IF(IT.NE.0)THEN
              I1 = I1 + IT - 1
              IT = INDEX(STRING(I1:LENGTH),' ')
              I2 = I1 + IT - 2
              J1(I) = I1
              J2(I) = I2
              I2 = J2(I)
              IF(J2(I).LT.LENGTH) GO TO 20
            END IF
            GO TO 15
          END IF
          GO TO 15
        END IF
15    CONTINUE
      I = I - 1
      IF(I.GT.0) THEN
        CALL SRCTTL(IDEVIN,IDEVOT,STRING,J1,J2,I,KBIN,KBOUT,IOK)
      END IF
      END
      SUBROUTINE SRCKEZ(KBIN,KBOUT,STRING,J1,J2,I,PROMPT,IOK)
      CHARACTER STRING*80,NEW*80,UNDER,SPACE,PROMPT*(*)
      INTEGER J1(5),J2(5)
      INTEGER NOTLR
      EXTERNAL NOTLR
      SAVE UNDER,SPACE
      DATA UNDER/'_'/,SPACE/' '/
      WRITE(KBOUT,1000)PROMPT
1000  FORMAT(' Search for ',A)
10    CONTINUE
      LENGTH = 0
      STRING = ' '
      NEW = ' '
      CALL GTSTR(PROMPT,STRING,NEW,LENGTH,KBOUT,KBIN,INFLAG)
      LENGTH = 80
      IF(INFLAG.NE.0) RETURN
      STRING = NEW
C FIND SEPARATE WORDS
      I = 0
      I2 = 0
20    CONTINUE
        I = I + 1
        IF(I.LT.6)THEN
          I1 = I2 + 1
          LEFT = LENGTH - I1 + 1
          IF(LEFT.GT.0)THEN
            IT = NOTLR(STRING(I1:LENGTH),LEFT,' ')
            IF(IT.NE.0)THEN
              I1 = I1 + IT - 1
              IT = INDEX(STRING(I1:LENGTH),' ')
              I2 = I1 + IT - 2
              J1(I) = I1
              J2(I) = I2
              I2 = J2(I)
              IF(J2(I).LT.LENGTH) GO TO 20
            END IF
            GO TO 15
          END IF
          GO TO 15
        END IF
15    CONTINUE
C
C  keywords are in upper case
C
      CALL CCASE(STRING,1)
C
C  change uinderscore to space
C
      CALL EXCHNG(STRING,UNDER,SPACE)
      I = I - 1
C      IF(I.GT.0) I = 1
      IOK = 0
      END
      SUBROUTINE EXCHNG(STRING,FROM,TO)
      CHARACTER STRING*(*),FROM,TO
      DO 10 I=1,LEN(STRING)
        IF (STRING(I:I).EQ.FROM) STRING(I:I) = TO
 10     CONTINUE
      END
      SUBROUTINE SRCTTL(IDEVIN,IDEVOT,STRING,J1,J2,NSTRNG,
     +KBIN,KBOUT,IOK)
      CHARACTER LINE*80, STRING*(*),NAME*10,SLEN*4,LLINE*104
      INTEGER J1(NSTRNG),J2(NSTRNG),GETRS
      EXTERNAL GETRS
      EQUIVALENCE (LLINE(1:1),NAME),(LLINE(25:104),LINE)
      EQUIVALENCE(LLINE(21:24),SLEN)
      IOK = 0
      JPAGE = 0
      IFOUND = 0
      WRITE(KBOUT,1004)STRING(1:J2(NSTRNG))
1004  FORMAT(' Searching for ',A)
      CALL CCASE(STRING,2)
1001  FORMAT(A,' ',I7,' ',A)
 1002 FORMAT(' ',A,' ',I7,' ',A)
      IBYTE = 301
10    CONTINUE
      IOK = GETRS(IDEVIN,LLINE,104,IBYTE)
      IF(IOK.NE.0) GO TO 30
      IBYTE = IBYTE + 104
        CALL CCASE(LINE,2)
        DO 5 I = 1,NSTRNG
          IF(INDEX(LINE,STRING(J1(I):J2(I))).EQ.0)GO TO 10
5       CONTINUE
        IFOUND = IFOUND + 1
        CALL BSW4(SLEN,LENS)
        IF(IDEVOT.EQ.KBOUT)THEN
          CALL PAGER(KBIN,KBOUT,JPAGE,IOK)
          IF (IOK.NE.0) RETURN
        END IF
        IF(IDEVOT.EQ.KBOUT) THEN
          WRITE(IDEVOT,1002,ERR=20)NAME,LENS,LINE
        ELSE
          WRITE(IDEVOT,1001,ERR=20)NAME,LENS,LINE
        END IF
        GO TO 10
20    CONTINUE
      IOK = 1
      RETURN
30    CONTINUE
      WRITE(KBOUT,1003)IFOUND
1003  FORMAT(' ',I7,' entries found')
      CALL BPAUSE(KBIN,KBOUT,IEX)
      END
      SUBROUTINE PAGER(KBIN,KBOUT,JPAGE,IOK)
      PARAMETER (IPAGE = 22)
      JPAGE = JPAGE + 1
      IF(JPAGE.EQ.IPAGE)THEN
        CALL BPAUSE(KBIN,KBOUT,IOK)
        IF(IOK.NE.0)RETURN
        JPAGE = 0
      END IF
      IOK = 0
      END
      SUBROUTINE RDLIBL(FILNAM,KBIN,KBOUT,
     +IHELPS,IHELPE,HELPF,IDEVH,IDEVLL,IDEVEN,IDEVNL,
     +LIBLF,LIBIN,DIVDEV,MAXDIV,IDEVD,
     +LIST,ENAMEL,LIBTYP,LTYPEP,NDIV,RSIZEN,NRECEN,IOK)
C   AUTHOR RODGER STADEN
C IDEVLL = unit for library list LIBLF
C IDEVEN =          entry name file ENAMEF
C IDEVD    unit for division lookup file, and start for divdev
      CHARACTER FILNAM*(*),LIBLF*(*)
      CHARACTER HELPF*(*),LTYPEP
      PARAMETER (MAXNAM = 80,LENNAM = 10)
      PARAMETER (MAXPRM = 50,MAXMEN=20,MAXLIN=80)
      CHARACTER PROMPT(MAXMEN)*(MAXPRM),LOGNAM(MAXMEN)*(MAXNAM)
      CHARACTER FTYPE*(MAXMEN),LINE*(MAXLIN)
      CHARACTER LTYPE*(MAXMEN),ENAMEL*(LENNAM)
      INTEGER RSIZEN
      INTEGER DIVDEV(MAXDIV)
      IDIMIN = IDIM
      IDIM = 0
      LIB = LIBIN
1     CONTINUE
      CALL RDLB0(LTYPE,LOGNAM,PROMPT,MAXMEN,MAXNAM,MAXPRM,
     +IDEVLL,LIBLF,LINE,MAXLIN,KBOUT,ITEM,IOK)
      IF(IOK.NE.0) RETURN
      IOK = 1
      IF(ITEM.LT.MAXMEN) THEN
        ITEM = ITEM + 1
        PROMPT(ITEM) = 'Personal file in PIR format'
      END IF
      IF(ITEM.LT.MAXMEN) THEN
        ITEM = ITEM + 1
        PROMPT(ITEM) = 'Personal file in FASTA format'
      END IF
      CALL RADION('Select a library',PROMPT,ITEM,LIB,
     +IHELPS,IHELPE,HELPF,IDEVH,KBIN,KBOUT)
      IF(LIB.LT.1) RETURN
      IF(LIB.EQ.ITEM-1) THEN
        FILNAM = ' '
        CALL OPENF1(IDEVEN,FILNAM,0,IOK,KBIN,KBOUT,
     +  'Personal library in PIR format',
     +  IHELPS,IHELPE,HELPF,IDEVH)
        IF(IOK.NE.0)RETURN
        LIBTYP = 2
      ELSE IF(LIB.EQ.ITEM) THEN
        FILNAM = ' '
        CALL OPENF1(IDEVEN,FILNAM,0,IOK,KBIN,KBOUT,
     +  'Personal library in FASTA format',
     +  IHELPS,IHELPE,HELPF,IDEVH)
        IF(IOK.NE.0)RETURN
        LIBTYP = 3
      ELSE
        FILNAM = LOGNAM(LIB)
        CALL RDLB1(LOGNAM,MAXMEN,MAXNAM,IDEVLL,FILNAM,
     +  LINE,MAXLIN,FTYPE,LINENO,KBOUT,ITEM,IOK)
        IF(IOK.NE.0) RETURN
        LTYPEP = LTYPE(LIB:LIB)
        IF(LTYPEP.EQ.'A') THEN
          LIBTYP = 1
          WRITE(KBOUT,*)'Library is in EMBL format with indexes'
          CALL OCDLBL(LOGNAM,MAXNAM,FTYPE,FILNAM,DIVDEV,
     +    MAXDIV,NDIV,IDEVEN,RSIZEN,NRECEN,IDEVD,KBOUT,IOK)
          IF(IOK.NE.0) RETURN
        ELSE IF(LTYPEP.EQ.'B') THEN
          LIBTYP = 1
          WRITE(KBOUT,*)'Library is in CODATA format with indexes'
          CALL OCDLBL(LOGNAM,MAXNAM,FTYPE,FILNAM,DIVDEV,
     +    MAXDIV,NDIV,IDEVEN,RSIZEN,NRECEN,IDEVD,KBOUT,IOK)
          IF(IOK.NE.0) RETURN
        ELSE IF(LTYPE(LIB:LIB).EQ.'C') THEN
          LIBTYP = 1
          WRITE(KBOUT,*)'Library is in GenBank format with indexes'
          CALL OCDLBL(LOGNAM,MAXNAM,FTYPE,FILNAM,DIVDEV,
     +    MAXDIV,NDIV,IDEVEN,RSIZEN,NRECEN,IDEVD,KBOUT,IOK)
          IF(IOK.NE.0) RETURN
        ELSE
          WRITE(KBOUT,*)'Unknown library type'
          IOK = 1
        RETURN
        END IF
      END IF
2     CONTINUE
      IOPT = 1
      PROMPT(1) = 'Search whole library'
      PROMPT(2) = 'Search only a list of entries'
      PROMPT(3) = 'Search all but a list of entries'
      CALL RADION('Select a task',PROMPT,3,IOPT,
     +  IHELPS,IHELPE,HELPF,IDEVH,KBIN,KBOUT)
      IF(IOPT.LT.1) GO TO 1
      IF((IOPT.EQ.2).OR.(IOPT.EQ.3)) THEN
        FILNAM = ' '
        CALL OPENF1(IDEVNL,FILNAM,0,IOK,KBIN,KBOUT,
     +  'File of entry names',
     +  IHELPS,IHELPE,HELPF,IDEVH)
        IF(IOK.NE.0)RETURN
        IF(IOPT.EQ.3) THEN
C need to read in the first name for exclusion
C the rest handled by cdroml
          READ(IDEVNL,1000,ERR=100,END=200)ENAMEL
 1000     FORMAT(A)
        END IF
      END IF
      IFINEX = 0
      LIST = 0
      IF(IOPT.EQ.2) LIST = 1
      IF(IOPT.EQ.3) LIST = -1
      IOK = 0
      RETURN
 100  CONTINUE
      IOK = 1
      CALL ERROM(KBOUT,'Error in names file')
      RETURN
 200  CONTINUE
      IOK = 2
      CALL ERROM(KBOUT,'Empty names file')
      END
      SUBROUTINE OCDLBL(LIBNAM,MAXLBF,FTYPE,TEMPF,DIVDEV,
     +MAXDIV,NDIV,IDEVEN,RSIZEN,NRECEN,IDEVD,KBOUT,IOK)
C Assigns libnam names to file name strings, opens division lookup file,
C reads names of each division and opens the files
C seeks to first record in entry names file
      CHARACTER LIBNAM(MAXLBF)*(*)
      CHARACTER TEMPF*(*),BARRAY*10
      CHARACTER FTYPE*(*)
      INTEGER DIVDEV(MAXDIV),RSIZEN
      INTEGER OPENFU,IHEAD,OPENFF
      EXTERNAL OPENFU,IHEAD,OPENFF
      TEMPF = ' '
      TEMPF = LIBNAM(INDEX(FTYPE,'A'))
      IOK = OPENFF(IDEVD,TEMPF)
      IF(IOK.NE.0) THEN
        CALL ERROM(KBOUT,'Error opening division lookup file')
        CALL ERROM(KBOUT,TEMPF)
        RETURN
      END IF
      IOK = 2
      NDIV = 0
C use division as additions to idevd
 10   READ(IDEVD,1000,ERR=200,END=100)IDIV,TEMPF
 1000 FORMAT(I6,1X,A)
      IF((IDIV.GT.0).AND.(NDIV.LT.MAXDIV)) THEN
        NDIV = NDIV + 1
        DIVDEV(NDIV) = IDEVD + IDIV
        IF(OPENFF(DIVDEV(NDIV),TEMPF).NE.0) THEN
          CALL ERROM(KBOUT,'Error opening division file')
          CALL ERROM(KBOUT,TEMPF)
          RETURN
        END IF
        GO TO 10
      END IF
      IOK = 5
      RETURN
 100  CONTINUE
      CLOSE(UNIT=IDEVD)
      IOK = 3
      TEMPF = LIBNAM(INDEX(FTYPE,'B'))
      IF(OPENFU(IDEVEN,TEMPF).NE.0) THEN
        CALL ERROM(KBOUT,'Error opening entryname index')
        CALL ERROM(KBOUT,TEMPF)
        RETURN
      END IF
      IOK = 5
      IF(IHEAD(IDEVEN,BARRAY,NRECEN,RSIZEN).NE.0) THEN
        CALL ERROM(KBOUT,'Error reading entryname index header')
        CALL ERROM(KBOUT,TEMPF)
        RETURN
      END IF
      IOK = 0
      RETURN
 200  CONTINUE
      CALL ERROM(KBOUT,'Error reading division file')
      IOK = 4
      END
      SUBROUTINE CDROML(JOB,ENAME,ENAMEL,
     +IDEVEN,RSIZEN,NRECEN,IDEVNL,SEQ,IDSEQ,
     +DIVDEV,NDIV,ICREC,IFIN,TITLE,KBOUT,LTYPE,IOK)
      CHARACTER ENAME*(*),ENAMEL*(*),BARRAY*80,TITLE*(*)
      CHARACTER SEQ(IDSEQ),LTYPE,TIT*5
      INTEGER DIVCOD,ANNOFF,SEQOFF,RSIZEN,DIVDEV(NDIV)
      INTEGER CDSEQ,CDANNT,GNEXTN,CODSEQ,GENSEQ
      EXTERNAL IENAME
      EXTERNAL CDSEQ,CDANNT,GNEXTN,CODSEQ,GENSEQ
C
C read lib data from cdrom format
C
C all files are already open
C the entry names files is poised on the first entry, when we come in
C first time. Records are numbered 0 to nrecen.
C
C job = 0 process whole library, entry name by entryname
C       1 process a list of entry names
C       -1 process all but a list of entry names
C       2 get a single entry name
C
C error returns
C iok = 0 ok
C       -1 not found
C       -2 error
C       -3 end
C
C  LTYPE = A means embl, B means codata, C means GenBank
C
      IF (ICREC.EQ.NRECEN) THEN
        IOK = -3
        CALL ERROM(KBOUT,'End of library reached')
        RETURN
      END IF
      IF(LTYPE.EQ.'A') THEN
        TIT = 'DE   '
      ELSE IF(LTYPE.EQ.'B') THEN
        TIT = 'TITLE'
      ELSE IF(LTYPE.EQ.'C') THEN
        TIT = 'DEFIN'
      END IF
      IF(JOB.EQ.0) THEN
C
C get next sequence in entry name index
C
C get offsets and divcode (error as for getrs)
C
        IOK = GNEXTN(IDEVEN,RSIZEN,ENAME,ANNOFF,SEQOFF,DIVCOD,
     +  BARRAY,ICREC)
        IF(IOK.NE.0) THEN
          CALL ERROM(KBOUT,'Error reading entryname index')
          RETURN
        END IF
C
C read seq, preceded by title
C
        IOK = CDANNT(DIVDEV(DIVCOD),ANNOFF,BARRAY,TIT)
        IF(IOK.NE.0) THEN
          CALL ERROM(KBOUT,'Error reading title')
          RETURN
        END IF
        TITLE = BARRAY
        IOK = 99
        IF(LTYPE.EQ.'A') THEN
          IOK = CDSEQ(DIVDEV(DIVCOD),SEQOFF,SEQ,IDSEQ)
        ELSE IF(LTYPE.EQ.'B') THEN
          IOK = CODSEQ(DIVDEV(DIVCOD),SEQOFF,SEQ,IDSEQ)
        ELSE IF(LTYPE.EQ.'C') THEN
          IOK = GENSEQ(DIVDEV(DIVCOD),SEQOFF,SEQ,IDSEQ)
        END IF
        IF(IOK.EQ.0) RETURN
        IF(IOK.EQ.2) THEN
          WRITE(KBOUT,*)
     +    'Only first ',IDSEQ,' characters of sequence read'
          IOK = 0
          RETURN
        END IF
        CALL ERROM(KBOUT,'Error reading sequence')
        RETURN
      ELSE IF(JOB.EQ.2) THEN
C
C get entry name
C
C
C get offsets and divcode
C
        IOK = IENAME(IDEVEN,NRECEN,RSIZEN,ENAME,ANNOFF,SEQOFF,
     +  DIVCOD,BARRAY)
        IF(IOK.NE.0) THEN
          WRITE(KBOUT,*)ENAME,' not found'
          RETURN
        END IF
C
C read seq, preceded by title
C
        IOK = CDANNT(DIVDEV(DIVCOD),ANNOFF,BARRAY,TIT)
        IF(IOK.NE.0) THEN
          CALL ERROM(KBOUT,'Error reading title')
          RETURN
        END IF
        TITLE = BARRAY
        IOK = 99
        IF(LTYPE.EQ.'A') THEN
          IOK = CDSEQ(DIVDEV(DIVCOD),SEQOFF,SEQ,IDSEQ)
        ELSE IF(LTYPE.EQ.'B') THEN
          IOK = CODSEQ(DIVDEV(DIVCOD),SEQOFF,SEQ,IDSEQ)
        ELSE IF(LTYPE.EQ.'C') THEN
          IOK = GENSEQ(DIVDEV(DIVCOD),SEQOFF,SEQ,IDSEQ)
        END IF
        IF(IOK.EQ.0) RETURN
        IF(IOK.EQ.2) THEN
          WRITE(KBOUT,*)
     +    'Only first ',IDSEQ,' characters of sequence read'
          IOK = 0
          RETURN
        END IF
        CALL ERROM(KBOUT,'Error reading sequence')
        RETURN
      ELSE IF(JOB.EQ.1) THEN
C
C get next sequence in entry name list
C
 90      CONTINUE
         READ(IDEVNL,1000,ERR=100,END=200)ENAME
C
C get offsets and divcode
C
        IOK = IENAME(IDEVEN,NRECEN,RSIZEN,ENAME,ANNOFF,SEQOFF,
     +  DIVCOD,BARRAY)
        IF(IOK.NE.0) THEN
          WRITE(KBOUT,*)ENAME,' not found'
          GO TO 90
        END IF
C
C read seq, preceded by title
C
        IOK = CDANNT(DIVDEV(DIVCOD),ANNOFF,BARRAY,TIT)
        IF(IOK.NE.0) THEN
          CALL ERROM(KBOUT,'Error reading title')
          RETURN
        END IF
        TITLE = BARRAY
        IOK = 99
        IF(LTYPE.EQ.'A') THEN
          IOK = CDSEQ(DIVDEV(DIVCOD),SEQOFF,SEQ,IDSEQ)
        ELSE IF(LTYPE.EQ.'B') THEN
          IOK = CODSEQ(DIVDEV(DIVCOD),SEQOFF,SEQ,IDSEQ)
        ELSE IF(LTYPE.EQ.'C') THEN
          IOK = GENSEQ(DIVDEV(DIVCOD),SEQOFF,SEQ,IDSEQ)
        END IF
        IF(IOK.EQ.0) RETURN
        IF(IOK.EQ.2) THEN
          WRITE(KBOUT,*)
     +    'Only first ',IDSEQ,' characters of sequence read'
          IOK = 0
          RETURN
        END IF
        CALL ERROM(KBOUT,'Error reading sequence')
        RETURN
 100    CONTINUE
        IOK = -3
        CALL ERROM(KBOUT,'Error reading entrynames list')
        RETURN
 200    CONTINUE
        IOK = -1
        RETURN
      ELSE IF(JOB.EQ.-1) THEN
C
C get offsets and divcode
C
 250    CONTINUE
        IOK = GNEXTN(IDEVEN,RSIZEN,ENAME,ANNOFF,SEQOFF,DIVCOD,
     +  BARRAY,ICREC)
        IF(IOK.NE.0) THEN
          CALL ERROM(KBOUT,'Error reading entryname index')
          RETURN
        END IF
C
C is it the same as the one on the list?
C
        IF(ENAMEL.EQ.ENAME) THEN
          IF(IFIN.EQ.0) THEN
            READ(IDEVNL,1000,ERR=300,END=400)ENAMEL
 1000       FORMAT(A)
          END IF
          GO TO 250
        END IF
C
C read seq, preceded by title
C
        IOK = CDANNT(DIVDEV(DIVCOD),ANNOFF,BARRAY,TIT)
        IF(IOK.NE.0) THEN
          CALL ERROM(KBOUT,'Error reading title')
          RETURN
        END IF
        TITLE = BARRAY
        IOK = 99
        IF(LTYPE.EQ.'A') THEN
          IOK = CDSEQ(DIVDEV(DIVCOD),SEQOFF,SEQ,IDSEQ)
        ELSE IF(LTYPE.EQ.'B') THEN
          IOK = CODSEQ(DIVDEV(DIVCOD),SEQOFF,SEQ,IDSEQ)
        ELSE IF(LTYPE.EQ.'C') THEN
          IOK = GENSEQ(DIVDEV(DIVCOD),SEQOFF,SEQ,IDSEQ)
        END IF
        IF(IOK.EQ.0) RETURN
        IF(IOK.EQ.2) THEN
          WRITE(KBOUT,*)
     +    'Only first ',IDSEQ,' characters of sequence read'
          IOK = 0
          RETURN
        END IF
        CALL ERROM(KBOUT,'Error reading sequence')
        RETURN
 300    CONTINUE
        IOK = -3
        CALL ERROM(KBOUT,'Error reading entrynames list')
        RETURN
 400    CONTINUE
C
C end of exclude list reached, so include the rest
C
        IFIN = 1
        ENAMEL = ' '
        GO TO 250
      END IF
      END
      INTEGER FUNCTION IENAME(IDEVEN,NREC,RSIZE,
     +ENAME,ANNOFF,SEQOFF,DIVCOD,BARRAY)
      INTEGER ANNOFF,SEQOFF,DIVCOD,GETRS,NREC,RSIZE,SEEKRS
      INTEGER UB,B1,BEND
      INTEGER*2 STAR2
      CHARACTER BARRAY*(*),ENAME*(*)
      EXTERNAL GETRS,SEEKRS
      B1 = 0
      BEND = NREC
      LB = B1
      UB = BEND
 20   CONTINUE
      IF(UB.LT.LB) THEN
        IENAME = -1
        RETURN
      END IF
      IREC = (LB+UB)/2
      IBYTE = 301 + RSIZE*IREC
      IENAME = GETRS(IDEVEN,BARRAY,20,IBYTE)
      IF(IENAME.NE.0) RETURN
      IF(ENAME.LT.BARRAY(1:10)) THEN
        UB = IREC - 1
      ELSE IF(ENAME.GT.BARRAY(1:10)) THEN
        LB = IREC + 1
      ELSE
        CALL BSW4(BARRAY(11:),ANNOFF)
        CALL BSW4(BARRAY(15:),SEQOFF)
        CALL BSW2(BARRAY(19:),STAR2)
        DIVCOD = STAR2
        RETURN
      END IF
      GO TO 20
      END
      INTEGER FUNCTION CDSEQ(IDEV,SEQOFF,SEQ,IDSEQ)
C seek to seq and return it
      CHARACTER SEQ(IDSEQ),LT
      CHARACTER CR
      PARAMETER (CR=CHAR(13))
      INTEGER SEQOFF
      INTEGER SEEKRS
      EXTERNAL SEEKRS
      I = SEQOFF - 5
      CDSEQ = SEEKRS(IDEV,I)
      IF(CDSEQ.NE.0) RETURN
      NSEQ = 0
 10   CONTINUE
      LSEQ = MIN(NSEQ+60,IDSEQ)
      READ(IDEV,1000,ERR=100,END=200)LT,(SEQ(K),K=NSEQ+1,LSEQ)
 1000 FORMAT(A,4X,6(10A1,1X))
      IF(LT.EQ.'/') THEN
        CDSEQ = 0
C
C seq must have ended on previous line
C so look for carriage return
C
        DO 20 I=NSEQ-59,NSEQ
          IF(SEQ(I).EQ.CR) THEN
            J = I-1
            GO TO 21
          END IF
 20     CONTINUE
        J = NSEQ
 21     CONTINUE
C
C now look for a space
C
        DO 30 I=NSEQ-59,J
          IF(SEQ(I).EQ.' ') THEN
            IDSEQ = I-1
            RETURN
          END IF
 30     CONTINUE
        IDSEQ = J
        RETURN
      END IF
      NSEQ = LSEQ
      IF(NSEQ.LT.IDSEQ) GO TO 10
      CDSEQ = 2
      RETURN
 100  CONTINUE
      WRITE(*,*)'ERROR IN CDSEQ'
      CDSEQ = -2
      RETURN
 200  CONTINUE
      WRITE(*,*)'END IN CDSEQ'
      CDSEQ = -1
      END
      INTEGER FUNCTION GENSEQ(IDEV,SEQOFF,SEQ,IDSEQ)
C seek to seq and return it
      CHARACTER SEQ(IDSEQ),LT
      CHARACTER CR
      PARAMETER (CR=CHAR(13))
      INTEGER SEQOFF
      INTEGER SEEKRS
      EXTERNAL SEEKRS
C
C seek to beginning of line containing first seq data
C
      I = SEQOFF - 10
      GENSEQ = SEEKRS(IDEV,I)
      IF(GENSEQ.NE.0) RETURN
      NSEQ = 0
 10   CONTINUE
      LSEQ = MIN(NSEQ+60,IDSEQ)
      READ(IDEV,1000,ERR=100,END=200)LT,(SEQ(K),K=NSEQ+1,LSEQ)
 1000 FORMAT(A,9X,6(10A1,1X))
      IF(LT.EQ.'/') THEN
        GENSEQ = 0
C
C seq must have ended on previous line
C so look for carriage return
C
        DO 20 I=NSEQ-59,NSEQ
          IF(SEQ(I).EQ.CR) THEN
            J = I-1
            GO TO 21
          END IF
 20     CONTINUE
        J = NSEQ
 21     CONTINUE
C
C now look for a space
C
        DO 30 I=NSEQ-59,J
          IF(SEQ(I).EQ.' ') THEN
            IDSEQ = I-1
            RETURN
          END IF
 30     CONTINUE
        IDSEQ = J
        RETURN
      END IF
      NSEQ = LSEQ
      IF(NSEQ.LT.IDSEQ) GO TO 10
      GENSEQ = 2
      RETURN
 100  CONTINUE
      WRITE(*,*)'ERROR IN GENSEQ'
      GENSEQ = -2
      RETURN
 200  CONTINUE
      WRITE(*,*)'END IN GENSEQ'
      GENSEQ = -1
      END
      INTEGER FUNCTION CODSEQ(IDEV,SEQOFF,SEQ,IDSEQ)
C seek to seq and return it
      CHARACTER SEQ(IDSEQ),LT
      CHARACTER CR
      PARAMETER (CR=CHAR(13))
      INTEGER SEQOFF
      INTEGER SEEKRS
      EXTERNAL SEEKRS
C
C seek to beginning of line containing first seq data
C
      I = SEQOFF - 8
      CODSEQ = SEEKRS(IDEV,I)
      IF(CODSEQ.NE.0) RETURN
      NSEQ = 0
 10   CONTINUE
      LSEQ = MIN(NSEQ+30,IDSEQ)
      READ(IDEV,1000,ERR=100,END=200)LT,(SEQ(K),K=NSEQ+1,LSEQ)
 1000 FORMAT(A,7X,30(A1,1X))
      IF(LT.EQ.'/') THEN
        CODSEQ = 0
C
C seq must have ended on previous line
C so look for carriage return
C
        DO 20 I=NSEQ-29,NSEQ
          IF(SEQ(I).EQ.CR) THEN
            J = I-1
            GO TO 21
          END IF
 20     CONTINUE
        J = NSEQ
 21     CONTINUE
C
C now look for a space
C
        DO 30 I=NSEQ-29,J
          IF(SEQ(I).EQ.' ') THEN
            IDSEQ = I-1
            RETURN
          END IF
 30     CONTINUE
        IDSEQ = J
        RETURN
      END IF
      NSEQ = LSEQ
      IF(NSEQ.LT.IDSEQ) GO TO 10
      CODSEQ = 2
      RETURN
 100  CONTINUE
      WRITE(*,*)'ERROR IN CODSEQ'
      CODSEQ = -2
      RETURN
 200  CONTINUE
      WRITE(*,*)'END IN CODSEQ'
      CODSEQ = -1
      END
      INTEGER FUNCTION CDANN(IDEV,ANNOFF,BARRAY,IDEVOT,KBIN,KBOUT,TERM)
      CHARACTER BARRAY*(*),TERM*(*)
      INTEGER ANNOFF
      INTEGER SEEKRS,GETRSL
      EXTERNAL SEEKRS,GETRSL
      CDANN = SEEKRS(IDEV,ANNOFF)
      IF(CDANN.NE.0) RETURN
      JPAGE = 0
 10   CONTINUE
      IOK = GETRSL(IDEV,BARRAY,80)
      IF(IOK.LT.1) THEN
        CDANN = IOK
        RETURN
      END IF
      IF(IDEVOT.EQ.KBOUT) THEN
        CALL PAGER(KBIN,KBOUT,JPAGE,JOK)
        IF (JOK.NE.0) THEN
          CDANN = 2
          RETURN
        END IF
      END IF
      IF (IDEVOT.EQ.KBOUT) THEN
        WRITE(IDEVOT,1000)BARRAY(1:IOK)
      ELSE
        WRITE(IDEVOT,1001)BARRAY(1:IOK)
      END IF
      IF(BARRAY(1:LEN(TERM)).NE.TERM) GO TO 10
      CDANN = 0
 1000 FORMAT(' ',A)
 1001 FORMAT(A)
      END
      INTEGER FUNCTION CDANNT(IDEV,ANNOFF,BARRAY,TIT)
      CHARACTER BARRAY*(*),TIT*(*)
      INTEGER ANNOFF
      INTEGER SEEKRS,GETRSL
      EXTERNAL SEEKRS,GETRSL
      CDANNT = SEEKRS(IDEV,ANNOFF)
      IF(CDANNT.NE.0) RETURN
 10   CONTINUE
      IOK = GETRSL(IDEV,BARRAY,80)
      IF(IOK.LT.1) THEN
        CDANNT = IOK
        RETURN
      END IF
      IF(BARRAY(1:LEN(TIT)).NE.TIT) GO TO 10
      CDANNT = 0
      IF(IOK.LT.80) BARRAY(IOK+1:80) = ' '
      END
      INTEGER FUNCTION RDANUM(IDEVAN,ANUMTF,ANUMHF,IDEVEN,
     +NRECEN,RSIZEN,ACNUM,BARRAY,ENAME,IDEVOT,KBOUT)
      CHARACTER ANUMTF*(*),ANUMHF*(*),ACNUM*(*),BARRAY*(*)
      CHARACTER ENAME*(*)
      INTEGER OPENFU,ACNUMP,GETRS,ACNUMQ
      INTEGER ENAMEP,RSIZAN,RSIZEN
      EXTERNAL IHEAD,OPENFU,GETRS,IANUM
C start with accession number
C sent an accesion number acnum, return list of entry names
C
C open acnum.trg, 
C read until acnum found, get accession number pointer ACNUMP, number of hits
C NHITS
C close acnum.trg
C open acnum.hit, seek to ACNUMP
C read a record, write to screen, for each of NHITS
C close acnum.hit
C
C open accession number target file
C
C      WRITE(*,*)'OPENING'
C      WRITE(*,*)ANUMTF
      RDANUM = OPENFU(IDEVAN,ANUMTF)
      IF(RDANUM.NE.0) THEN
        CALL ERROM(KBOUT,'Error opening target file')
        CALL ERROM(KBOUT,ANUMTF)
        RETURN
       END IF
C      WRITE(*,*)'OPEN'
C
C read its header
C
C      WRITE(*,*)'READING'
      RDANUM = IHEAD(IDEVAN,BARRAY,NRECAN,RSIZAN)
      IF(RDANUM.NE.0) RETURN
C      WRITE(*,*)'NRECAN,RSIZAN',NRECAN,RSIZAN
C      WRITE(*,*)'READ'
C
C get the number of hits and the record number of the first hit
C
      RDANUM = IANUM(IDEVAN,ACNUM,NRECAN,RSIZAN,
     +       NHITS,ACNUMP,BARRAY)
      CLOSE(UNIT=IDEVAN)
C      WRITE(*,*)'NHITS,acnump',NHITS,ACNUMP
      IF(NHITS.LT.1) THEN
        WRITE(IDEVOT,*)ACNUM,' not found'
        RETURN
      END IF
      IF(RDANUM.NE.0) RETURN
C
C open the accession number hit file
C
C      WRITE(*,*)'READING'
      RDANUM = OPENFU(IDEVAN,ANUMHF)
      IF(RDANUM.NE.0) THEN
        CALL ERROM(KBOUT,'Error opening hit file')
        CALL ERROM(KBOUT,ANUMHF)
        RETURN
      END IF
C      WRITE(*,*)'READ'
C
C the records are 4 bytes and follow a 300 byte header
C
      DO 10 I = 1,NHITS
        ACNUMQ = 301 + (ACNUMP-1)*4
        RDANUM = GETRS(IDEVAN,BARRAY,4,ACNUMQ)
        IF(RDANUM.NE.0) RETURN
        CALL BSW4(BARRAY(1:),ENAMEP)
C        WRITE(*,*)'ENAMEP',ENAMEP
C
C the entry name file has records of size rsizen and the usual 300 byte header
C
        ENAMEP = 301 + (ENAMEP-1)*RSIZEN
        RDANUM = GETRS(IDEVEN,BARRAY,RSIZEN,ENAMEP)
        IF(RDANUM.NE.0) RETURN
        ENAME = BARRAY(1:10)
        WRITE(IDEVOT,*)'Entry name ',ENAME
C bug fix 18-10-91: added next line
        ACNUMP = ACNUMP + 1
 10   CONTINUE
      CLOSE(UNIT=IDEVAN)
      END
      INTEGER FUNCTION IANUM(IDEV,ACNUM,NREC,RSIZE,
     +NHITS,ACNUMP,BARRAY)
      INTEGER GETRS,NREC,RSIZE,ACNUMP
      INTEGER UB,B1,BEND
      CHARACTER BARRAY*(*),ACNUM*(*)
      EXTERNAL GETRS
C given an accession number acnum, does binary search.
C returns the number of hits NHITS and the record number of the first hit ACNUMP
      NHITS = 0
      B1 = 0
      BEND = NREC
 10   CONTINUE
      LB = B1
      UB = BEND
 20   CONTINUE
      IF(UB.LT.LB) THEN
        IANUM = 0
        RETURN
      END IF
      IREC = (LB+UB)/2
      IBYTE = 301 + RSIZE*IREC
C      WRITE(*,*)IBYTE
      IANUM = GETRS(IDEV,BARRAY,RSIZE,IBYTE)
C      WRITE(*,*)BARRAY(9:18)
      IF(IANUM.NE.0) RETURN
      IF(ACNUM.LT.BARRAY(9:18)) THEN
        UB = IREC - 1
      ELSE IF(ACNUM.GT.BARRAY(9:18)) THEN
        LB = IREC + 1
      ELSE
        CALL BSW4(BARRAY(1:),NHITS)
        CALL BSW4(BARRAY(5:),ACNUMP)
        RETURN
      END IF
      GO TO 20
      END
      INTEGER FUNCTION RDKWRD(IDEVKW,KWRDTF,KWRDHF,IDEVEN,
     +NRECEN,RSIZEN,KEYS,SS,SE,NKEYS,BARRAY,ENAME,IDEVOT,
     +KBIN,KBOUT,BRIEFF,BITAR0,BITAR1,MAXWRD)
      CHARACTER KWRDTF*(*),KWRDHF*(*),KEYS*(*),BARRAY*(*)
      CHARACTER ENAME*(*),BRIEFF*(*)
      INTEGER OPENFU,GETRS
      INTEGER ENAMEP,RSIZKW,RSIZEN
      INTEGER BITAR0(0:MAXWRD),BITAR1(0:MAXWRD)
      INTEGER NHITS(5),KWRDP(5),SS(NKEYS),SE(NKEYS)
      EXTERNAL IHEAD,OPENFU,GETRS,IKWRD
      RDKWRD = 1
C      MAXWRD = (NRECEN-1)/32
C start with keywords in KEYS
C sent nkeys keywords, return list of entry names
C
C open keyword.trg, 
C read until strings found, get keyword record pointer KWRDP,
C  number of hits NHITS
C close KEYWORD.trg
C open KEYWORD.hit, seek to KWRDP
C read a record, write to screen, for each of NHITS
C close KEYWORD.hit
      IFOUND = 0
C
C open KEYWORD target file
C
      RDKWRD = OPENFU(IDEVKW,KWRDTF)
      IF(RDKWRD.NE.0) THEN
        CALL ERROM(KBOUT,'Error opening target file')
        CALL ERROM(KBOUT,KWRDTF)
        RETURN
      END IF
C
C read its header
C
      RDKWRD = IHEAD(IDEVKW,BARRAY,NRECKW,RSIZKW)
      IF(RDKWRD.NE.0) THEN
        CLOSE(IDEVKW)
        RETURN
      END IF
C
C get start record number and nhits for each string
C
      DO 10 I=1,NKEYS
C
C get the number of hits and the record number of the first hit
C
        LS = SE(I) - SS(I) + 1
        RDKWRD = IKWRD(IDEVKW,KEYS(SS(I):),LS,NRECKW,RSIZKW,
     +       NHITS(I),KWRDP(I),BARRAY)
        IF(NHITS(I).LT.1) THEN
          WRITE(IDEVOT,*)KEYS(SS(I):SE(I)),' not found'
          CLOSE(UNIT=IDEVKW)
          RETURN
        END IF
        IF(RDKWRD.NE.0) THEN
          CLOSE(UNIT=IDEVKW)
          RETURN
        END IF
        WRITE(IDEVOT,*)KEYS(SS(I):SE(I)),' hits',NHITS(I)
 10     CONTINUE
        CLOSE(UNIT=IDEVKW)
        CALL BPAUSE(KBIN,KBOUT,IEX)
        IF(IEX.NE.0)RETURN
C
C open the KEYWORD hit file
C
      RDKWRD = OPENFU(IDEVKW,KWRDHF)
      IF(RDKWRD.NE.0) THEN
        CALL ERROM(KBOUT,'Error opening hit file')
        CALL ERROM(KBOUT,KWRDHF)
        RETURN
      END IF
C
C the records are 4 bytes and follow a 300 byte header
C
C For each hit for keyword 1 find its entryname record pointer and
C then find the entryname record pointers for each hit for each of
C the other keywords.
C
C kwrdp(1) is the number of the record in the hit file that contains
C the first hit for the first keyword
C      WRITE(*,*)'NHITS(1)',NHITS(1)
C
C  new stuff: clear bit arrays, set bits in bitar0 for first word
C  set bits in bitar1 for other words, then and/or them with array0
      CALL CABIT(BITAR0,MAXWRD)
      KWPR = KWRDP(1)
      DO 40 I = 1,NHITS(1)
        KWP = 301 + (KWPR-1)*4
C add to header and then read the entrynum of the first matching entry
        RDKWRD = GETRS(IDEVKW,BARRAY,4,KWP)
        IF(RDKWRD.NE.0) THEN
          CLOSE(UNIT=IDEVKW)
          RETURN
        END IF
        CALL BSW4(BARRAY(1:),ENAMEP)
C enamep is the record number of the first matching entry
C so set the corresponding bit
        CALL SABIT(BITAR0,MAXWRD,ENAMEP)
        KWPR = KWPR + 1
 40     CONTINUE
C
C now do the other keywords
C
        DO 25 J  = 2,NKEYS
          CALL CABIT(BITAR1,MAXWRD)
          KWPR = KWRDP(J)
          DO 30 I=1,NHITS(J)
            KWP = 301 + (KWPR-1)*4
            RDKWRD = GETRS(IDEVKW,BARRAY,4,KWP)
            IF(RDKWRD.NE.0) THEN
              CLOSE(UNIT=IDEVKW)
              RETURN
            END IF
            CALL BSW4(BARRAY(1:),ENAMEP)
C enamep is the record number of the first matching entry
C so set the corresponding bit
            CALL SABIT(BITAR1,MAXWRD,ENAMEP)
            KWPR = KWPR + 1
 30     CONTINUE
C
C and the bits with bitar0
C
          CALL AAWORD(BITAR0,BITAR1,MAXWRD)   
 25     CONTINUE
        CLOSE(UNIT=IDEVKW)
C
C open brief directory file
C
        IOK = OPENFU(IDEVKW,BRIEFF)
        IF(IOK.NE.0) THEN
          CALL ERROM(KBOUT,'Error opening brief directory file')
          CALL ERROM(KBOUT,BRIEFF)
          RETURN
        END IF
        CALL TBITAL(BITAR0,MAXWRD,IDEVKW,IDEVOT,KBIN,KBOUT,ENAME)
        CLOSE(UNIT=IDEVKW)
      END
      SUBROUTINE TBITAO(BITAR,MAXENT,IDEVEN,BARRAY,RSIZEN,IDEVOT,
     +ENAME)
C old routine
      CHARACTER BARRAY*(*)
      CHARACTER ENAME*(*)
      LOGICAL TBITRS
      INTEGER GETRS,BITAR(0:MAXENT)
      INTEGER ENAMEP,RSIZEN
      EXTERNAL GETRS,TBITRS
C
C the entry name file has records of size rsizen and the usual 300 byte header
C
      IFOUND = 0
      DO 10 I =0,MAXENT
        DO 5 J =0,31
          IF(TBITRS(BITAR(I),J)) THEN
            ENAMEP = I*32 + 1 + MOD(J,32)
            ENAMEP = 301 + (ENAMEP-1)*RSIZEN
            IOK = GETRS(IDEVEN,BARRAY,RSIZEN,ENAMEP)
            IF(IOK.NE.0) THEN
              RETURN
            END IF
            ENAME = BARRAY(1:10)
            IFOUND = IFOUND + 1
            WRITE(IDEVOT,*)ENAME,IFOUND
          END IF
 5      CONTINUE
 10   CONTINUE
      END
      SUBROUTINE TBITAL(BITAR,MAXENT,IDEVIN,IDEVOT,KBIN,KBOUT,ENAME)
      LOGICAL TBITRS
      INTEGER GETRS,BITAR(0:MAXENT)
      EXTERNAL GETRS,TBITRS,NOTIRL
      CHARACTER LINE*80,NAME*10,ACNUM*10,SLEN*4,LLINE*104,ENAME*(*)
      EQUIVALENCE (LLINE(1:1),NAME),(LLINE(25:104),LINE)
      EQUIVALENCE(LLINE(21:24),SLEN),(LLINE(11:20),ACNUM)
C
C the entry name file has records of size 104 and the usual 300 byte header
C
      IFOUND = 0
      DO 4 I =0,MAXENT
        IF (BITAR(I).NE.0) THEN
          DO 3 J =0,31
            IF(TBITRS(BITAR(I),J)) IFOUND = IFOUND + 1
 3        CONTINUE
        END IF
 4    CONTINUE
      WRITE(KBOUT,1004)IFOUND
 1004 FORMAT(' Different entries ',I7)
      IFOUND = 0
      JPAGE = 0
      DO 10 I =0,MAXENT
        IF (BITAR(I).NE.0) THEN
          DO 5 J =0,31
            IF(TBITRS(BITAR(I),J)) THEN
              IBYTE = I*32 + 1 + MOD(J,32)
              IBYTE = 301 + (IBYTE-1)*104
              IOK = GETRS(IDEVIN,LLINE,104,IBYTE)
              IF(IOK.NE.0) THEN
                RETURN
              END IF
              CALL BSW4(SLEN,LENS)
              IF(IDEVOT.EQ.KBOUT)THEN
                CALL PAGER(KBIN,KBOUT,JPAGE,IOK)
                IF (IOK.NE.0) RETURN
              END IF
              ENAME = NAME
              LINEND = NOTIRL(LINE,80,' ')
              IF(IDEVOT.EQ.KBOUT) THEN
                WRITE(IDEVOT,1002,ERR=20)
     +          NAME,ACNUM,LENS,LINE(1:LINEND)
              ELSE
                WRITE(IDEVOT,1001,ERR=20)
     +          NAME,ACNUM,LENS,LINE(1:LINEND)
 1001           FORMAT(A,' ',A,' ',I7,' ',A)
 1002         FORMAT(' ',A,' ',A,' ',I7,' ',A)
              END IF
              IFOUND = IFOUND + 1
            END IF
 5        CONTINUE
        END IF
 10   CONTINUE
 20   CONTINUE
      WRITE(KBOUT,1003)IFOUND
1003  FORMAT(' ',I7,' different entries found')
      CALL BPAUSE(KBIN,KBOUT,IEX)
      END
      INTEGER FUNCTION IKWRD(IDEV,KEYW,LS,NREC,RSIZE,
     +NHITS,KWRDP,BARRAY)
      INTEGER GETRS,NREC,RSIZE
      INTEGER UB,B1,BEND
      CHARACTER BARRAY*(*),KEYW*(*)
      EXTERNAL GETRS,IKWRDB
C given a KEYWORD KEYW OF LENGTH LSS, does binary search.
C returns the number of hits NHITS and the record number of the first hit KWRDP
C Note i make the assumption that the hit file will be ordered
C so that say all words beginning with sugar follow one another
C without interuption and in order. If not use the following:
C set string length to lss + 1 so we know the string found
C ends with a space: we wont find sugars if we search for sugar
C      LS = LSS + 1
      NHITS = 0
      B1 = 0
      BEND = NREC
 10   CONTINUE
      LB = B1
      UB = BEND
 20   CONTINUE
      IF(UB.LT.LB) THEN
        IKWRD = 0
        RETURN
      END IF
      IREC = (LB+UB)/2
      IBYTE = 301 + RSIZE*(IREC-1)
C      WRITE(*,*)'IBYTE',IBYTE
      IKWRD = GETRS(IDEV,BARRAY,RSIZE,IBYTE)
      IF(IKWRD.NE.0) RETURN
      IF(KEYW(1:LS).LT.BARRAY(9:8+LS)) THEN
        UB = IREC - 1
      ELSE IF(KEYW(1:LS).GT.BARRAY(9:8+LS)) THEN
        LB = IREC + 1
      ELSE
C a hit but it may not be the first! so look back until not a hit
C then go forward accumulating a count of hits until a nonmatch is found
C next 2 lines instead of call to ikwrdb if assumption mentioned above
C does not hold
C        CALL BSW4(BARRAY(5:),KWRDP)
C        CALL BSW4(BARRAY(1:),NHITS)
        IKWRD =  IKWRDB(IDEV,KEYW,LS,NREC,RSIZE,
     +  NHITS,KWRDP,BARRAY,IREC)
C        WRITE(*,*)'NHITS,KWRDP,IKWRD',NHITS,KWRDP,IKWRD
        RETURN
      END IF
      GO TO 20
      END
      INTEGER FUNCTION IKWRDB(IDEV,KEYW,LS,NREC,RSIZE,
     +NHITS,KWRDP,BARRAY,IREC)
      INTEGER GETRS,NREC,RSIZE
      CHARACTER BARRAY*(*),KEYW*(*)
      EXTERNAL GETRS
C given a KEYWORD KEYW OF LENGTH LS,  and a start record with
C a hit but it may not be the first! so look back until not a hit
C then go forward accumulating a count of hits until a nonmatch is found
      NHITS = 0
 10   CONTINUE
C      WRITE(*,*)'IREC IN IKWRDB',IREC
C SHOULD THIS BE 0 OR 1?
      IREC = IREC - 1
      IF(IREC.LT.0) THEN
        IKWRDB = 0
        RETURN
      END IF
      IBYTE = 301 + RSIZE*(IREC-1)
      IKWRDB = GETRS(IDEV,BARRAY,RSIZE,IBYTE)
      IF(IKWRDB.NE.0) RETURN
      IF(KEYW(1:LS).EQ.BARRAY(9:8+LS)) GO TO 10
C found first occurrence
      IREC = IREC + 1
      IBYTE = 301 + RSIZE*(IREC-1)
      IKWRDB = GETRS(IDEV,BARRAY,RSIZE,IBYTE)
      IF(IKWRDB.NE.0) RETURN
      CALL BSW4(BARRAY(5:),KWRDP)
 20   CONTINUE
      CALL BSW4(BARRAY(1:),JHITS)
C      WRITE(*,*)'JHITS,NHITS',JHITS,NHITS
      NHITS = NHITS + JHITS
      IREC = IREC + 1
C      WRITE(*,*)'IREC IN IKWRDB 2',IREC
      IF(IREC.GT.NREC) THEN
        IKWRDB = 0
        RETURN
      END IF
      IBYTE = 301 + RSIZE*(IREC-1)
C      WRITE(*,*)'IBYTE IN IKWRDB',IBYTE
      IKWRDB = GETRS(IDEV,BARRAY,RSIZE,IBYTE)
      IF(IKWRDB.NE.0) RETURN
      IF(KEYW(1:LS).EQ.BARRAY(9:8+LS)) GO TO 20
C      WRITE(*,*)'OFF END IN KWRDB'
      END
      INTEGER FUNCTION GNEXTN(IDEVEN,RSIZE,
     +ENAME,ANNOFF,SEQOFF,DIVCOD,BARRAY,ICREC)
C get next entry name from entrynam.idx leaving pointer at next name
      INTEGER ANNOFF,SEQOFF,DIVCOD,GETRS,RSIZE
      INTEGER*2 STAR2
      CHARACTER BARRAY*(*),ENAME*(*)
      EXTERNAL GETRS
C this routine numbers records 0 to nrec-1 (unlike all the others!!!!)
C sipl,nipl,pipl would need irec = 1 if we make this consistent and use irec-1
      IBYTE = 301 + RSIZE * ICREC
      GNEXTN = GETRS(IDEVEN,BARRAY,RSIZE,IBYTE)
      IF(GNEXTN.NE.0) RETURN
      CALL BSW4(BARRAY(11:),ANNOFF)
      CALL BSW4(BARRAY(15:),SEQOFF)
      CALL BSW2(BARRAY(19:),STAR2)
      DIVCOD = STAR2
      ENAME = BARRAY(1:10)
      ICREC = ICREC + 1
      END
      INTEGER FUNCTION ENTFN(IDEV,DIVCOD,LIBF,BARRAY)
      CHARACTER LIBF*(*),BARRAY*(*)
      INTEGER DIVCOD
      ENFTN = 1
      REWIND IDEV
 10   CONTINUE
      READ(IDEV,1000,ERR=100,END=200)I,BARRAY
 1000 FORMAT(I6,1X,A)
      IF(I.NE.DIVCOD) GO TO 10
      LIBF = BARRAY
      ENTFN = 0
      RETURN
 100  CONTINUE
      ENTFN = -2
      RETURN
 200  CONTINUE
      ENTFN = -1
      END
      INTEGER FUNCTION OPENFU(IDEV,NAME)
      CHARACTER NAME*(*)
      CLOSE(UNIT=IDEV)
      LRECL = 1
      CALL OPENRS(IDEV,NAME,IOK,LRECL,11)
      OPENFU = 0
      IF(IOK.EQ.0) RETURN
      OPENFU = 1
      END
      INTEGER FUNCTION OPENFF(IDEV,NAME)
      CHARACTER NAME*(*)
      CLOSE(UNIT=IDEV)
      CALL OPENRS(IDEV,NAME,IOK,LRECL,2)
      OPENFF = 0
      IF(IOK.EQ.0) RETURN
      OPENFF = 1
      END
      INTEGER FUNCTION IHEAD(IDEVEN,BARRAY,NREC,RSIZE)
      INTEGER NREC,RSIZE,GETRS
      INTEGER*2 IRSIZE
      CHARACTER BARRAY*(*)
      EXTERNAL GETRS
      IHEAD = GETRS(IDEVEN,BARRAY,10,1)
      IF(IHEAD.NE.0) RETURN
C      CALL BSW4(BARRAY(1:),FSIZE)
      CALL BSW4(BARRAY(5:),NREC)
      CALL BSW2(BARRAY(9:),IRSIZE)
      RSIZE = IRSIZE
      END
C below are routines for handling the embl and genbank feature tables
C Not all cases are dealt with, but those for automatic translation to
C protein are done for all the straightforward cases I can think of.
C Their robustness has yet to be tested!
      INTEGER FUNCTION EMBLFT(IDEV,KBOUT,KEYWRD,OPRATR,ISTRAN,
     +POSNS,MAXPOS,NPOS,NOBJ)
C interpret ft looking for keyword, operator and strand
C return list of end points, number of positions and
C number of objects
      CHARACTER LINEIN*80,KEYWRD*(*),OPRATR*(*)
      INTEGER POSNS(MAXPOS)
      INTEGER FTIN1,FTIN4,FTIN5,FTIN6,FTIN7,FTIN8
      INTEGER FTIN9,FTIN10,FTIN11,RDFTLN
      LOGICAL NUMBER,STRNGM,STRNGN,WSTRND
      EXTERNAL FTIN1,FTIN4,FTIN5,FTIN6,FTIN7,FTIN8
      EXTERNAL FTIN9,FTIN10,FTIN11,STRNGM,STRNGN,WSTRND,RDFTLN
      NPOS = 0
      NPOS1 = 0
C Length of opratr
      LENOP = INDEX(OPRATR,' ') - 1
C Find start
10    CONTINUE
      EMBLFT = RDFTLN(IDEV,LINEIN)
      IF(EMBLFT.LT.-1) RETURN
      IF(EMBLFT.NE.-1) GO TO 10
C For embl allow extra FH line
      IF(LINEIN(1:2).EQ.'FH') EMBLFT = RDFTLN(IDEV,LINEIN)
      IF(EMBLFT.NE.-1) RETURN
C Into ft
20    CONTINUE
      IERR = RDFTLN(IDEV,LINEIN)
C End of ft ?
      IF(IERR.EQ.-2) GO TO 400
C Error ?
      IF(IERR.NE.0) RETURN
25    CONTINUE
      IF(.NOT.STRNGM(LINEIN(6:),KEYWRD)) GO TO 20
      IF(LENOP.GT.0) THEN
        CALL CCASE(LINEIN,1)
        J = INDEX(LINEIN,OPRATR(1:LENOP))
        IF(J.EQ.0) GO TO 20
      END IF
C      IF(.NOT.STRNGN(LINEIN(22:),OPRATR)) GO TO 20
      IF(.NOT.WSTRND(LINEIN,ISTRAN)) GO TO 20
C
C Only get here if keyword, operator and strand are correct
C
      IF(NUMBER(LINEIN(22:22))) THEN
        EMBLFT = FTIN1(IDEV,POSNS,MAXPOS,NPOS,LINEIN)
      ELSE IF(LINEIN(22:22).EQ.'<') THEN
        EMBLFT = FTIN1(IDEV,POSNS,MAXPOS,NPOS,LINEIN)
      ELSE IF(LINEIN(22:22).EQ.'>') THEN
        EMBLFT = FTIN1(IDEV,POSNS,MAXPOS,NPOS,LINEIN)
      ELSE IF(LINEIN(22:22).EQ.'"') THEN
        WRITE(KBOUT,*)'Not handled'
        EMBLFT = FTIN4(IDEV,POSNS,MAXPOS,NPOS,LINEIN)
C
C note order of elses important
C
      ELSE IF(STRNGM(LINEIN(22:37),'JOIN(COMPLEMENT(')) THEN
        EMBLFT = FTIN8(IDEV,POSNS,MAXPOS,NPOS,LINEIN)
      ELSE IF(STRNGM(LINEIN(22:26),'JOIN(')) THEN
        EMBLFT = FTIN5(IDEV,POSNS,MAXPOS,NPOS,LINEIN)
      ELSE IF(STRNGM(LINEIN(22:38),'ORDER(COMPLEMENT(')) THEN
        EMBLFT = FTIN10(IDEV,POSNS,MAXPOS,NPOS,LINEIN)
      ELSE IF(STRNGM(LINEIN(22:27),'ORDER(')) THEN
        EMBLFT = FTIN9(IDEV,POSNS,MAXPOS,NPOS,LINEIN)
      ELSE IF(STRNGM(LINEIN(22:38),'COMPLEMENT(ORDER(')) THEN
        EMBLFT = FTIN11(IDEV,POSNS,MAXPOS,NPOS,LINEIN)
      ELSE IF(STRNGM(LINEIN(22:32),'COMPLEMENT(')) THEN
        EMBLFT = FTIN6(IDEV,POSNS,MAXPOS,NPOS,LINEIN)
      ELSE IF(LINEIN(22:22).EQ.'/') THEN
        WRITE(KBOUT,*)'Not handled'
        EMBLFT = FTIN7(IDEV,POSNS,MAXPOS,NPOS,LINEIN)
      ELSE 
         WRITE(KBOUT,*)'line of unknown type'
         EMBLFT = 9
      END IF
      IF(EMBLFT.NE.0) THEN
C
C -1 = start of ft
C -2 = end of entry or file
C -3 = error reading file
C -4 = error getting numbers from a line
C >0 = error in ftinN where error=N
C -9 = overflow of storage space
C
        WRITE(KBOUT,*)'Error no',EMBLFT
        WRITE(KBOUT,*)LINEIN
        RETURN
      END IF
      IF(NPOS.GT.NPOS1) THEN
        IF(NPOS+1.GT.MAXPOS) THEN
          WRITE(KBOUT,*)'Number of endpoints and objects exceeds',MAXPOS
          EMBLFT = -9
          RETURN
        END IF
        POSNS(NPOS+1) = NPOS - NPOS1
        NPOS = NPOS + 1
        NPOS1 = NPOS
        NOBJ = NOBJ + 1
      END IF
      GO TO 25
400   CONTINUE
      EMBLFT = 0
      WRITE(KBOUT,*)'End of entry'
      END
      LOGICAL FUNCTION WSTRND(LINEIN,WANTED)
C Return true if line is for wanted strand
C Assume complementary strands will contain string "complement"
C and that other strand wont
C Plus strand wanted = 0, minus strand wanted = 1
C (also works if wanted =2, meaning either strand)
      CHARACTER LINEIN*(*)
      INTEGER WANTED
      WSTRND = .FALSE.
      CALL CCASE(LINEIN,1)
      I = INDEX(LINEIN,'COMPLEMENT')
      IF((I.EQ.0).AND.(WANTED.EQ.1)) RETURN
      IF((I.NE.0).AND.(WANTED.EQ.0)) RETURN
      WSTRND = .TRUE.
      END
      LOGICAL FUNCTION STRNGN(S1,S2)
C return true if the first L chars of s1 and s2 match
C L is the length of the shortest string or the position of
C the first space char -1. Note if one is only spaces they will match.
      CHARACTER S1*(*),S2*(*),TUPPER
      EXTERNAL TUPPER
      STRNGN = .FALSE.
      L = MIN(LEN(S1),LEN(S2))
      I = MIN(INDEX(S1,' '),INDEX(S2,' '))
      L = MIN(L,MAX(0,I-1))
      DO 10 I = 1,L
        IF(TUPPER(S1(I:I)).NE.(TUPPER(S2(I:I)))) RETURN
 10     CONTINUE
      STRNGN = .TRUE.
      END
      INTEGER FUNCTION RDFTLN(IDEV,LINEIN)
      CHARACTER LINEIN*(*)
C
C -1 = start of ft
C -2 = end of entry or file
C -3 = error reading file
C
C read ft lines
      READ(IDEV,1000,END=200,ERR=300)LINEIN
C check for start of data
      IF((LINEIN(1:2).EQ.'FH').OR.(LINEIN(1:8).EQ.'FEATURES')) THEN
        RDFTLN = -1
        RETURN
      END IF
C check for end of data
      IF((LINEIN(1:2).EQ.'SQ').OR.(LINEIN(1:2).EQ.'//')) GO TO 200
C      WRITE(*,*)LINEIN
      RDFTLN = 0
      RETURN
1000  FORMAT(A)
200   CONTINUE
      RDFTLN = -2
      RETURN
300   CONTINUE
      RDFTLN = -3
      END
      INTEGER FUNCTION FTIN1(IDEV,POSNS,MAXPOS,NPOS,LINEIN)
      INTEGER POSNS(MAXPOS)
      CHARACTER LINEIN*(*)
      INTEGER FTPAIR,RDFTLN
      EXTERNAL FTPAIR,RDFTLN
C  467
C  340..565
C  102.110
C  123^124
C  145^177
C  find start n1, end n2 of first number
C  find start n3, end n3 of second number
C  decode into posns
      FTIN1 = 1
      IF(NPOS+2.GT.MAXPOS) RETURN
      N1 = 22
      FTIN1 = FTPAIR(LINEIN,N1,J1,J2)
      IF(FTIN1.NE.0) RETURN
      POSNS(NPOS+1) = J1
      POSNS(NPOS+2) = J2
      NPOS = NPOS + 2
      FTIN1 = RDFTLN(IDEV,LINEIN)
      END
      INTEGER FUNCTION FTIN6(IDEV,POSNS,MAXPOS,NPOS,LINEIN)
      INTEGER POSNS(MAXPOS)
      CHARACTER LINEIN*(*)
      INTEGER FTPAIR,RDFTLN
      LOGICAL NUMBER
      EXTERNAL FTPAIR,NUMBER,RDFTLN
C  only deal with simplest case:
C  complement(123..345)
      FTIN6 = 6
      N1 = 33
10    CONTINUE
      IF(NPOS+2.GT.MAXPOS) RETURN
      FTIN6 = FTPAIR(LINEIN,N1,J1,J2)
      IF(FTIN6.NE.0) THEN
        IERR = RDFTLN(IDEV,LINEIN)
        RETURN
      END IF
      POSNS(NPOS+1) = J1
      POSNS(NPOS+2) = J2
      NPOS = NPOS + 2
C  allow next position to be  ')'
      IF(LINEIN(N1+1:N1+1).EQ.')') THEN
        FTIN6 = RDFTLN(IDEV,LINEIN)
        RETURN
      ELSE
        IERR = RDFTLN(IDEV,LINEIN)
        FTIN6 = 6
      END IF
      FTIN6 = 6
      END
      INTEGER FUNCTION FTIN5(IDEV,POSNS,MAXPOS,NPOS,LINEIN)
      INTEGER POSNS(MAXPOS)
      CHARACTER LINEIN*(*)
      INTEGER FTPAIR,RDFTLN
      LOGICAL NUMBER
      EXTERNAL FTPAIR,NUMBER,RDFTLN
C  only deal with simplest cases:
C  join(123..345,456..666)
C  join(123..345,456..666,
C  789..899)
C  assume continues lines end with ", "
C  always terminate at first closing bracket
      FTIN5 = 5
      N1 = 27
10    CONTINUE
      IF(NPOS+2.GT.MAXPOS) RETURN
      FTIN5 = FTPAIR(LINEIN,N1,J1,J2)
      IF(FTIN5.NE.0) THEN
        IERR = RDFTLN(IDEV,LINEIN)
        RETURN
      END IF
      POSNS(NPOS+1) = J1
      POSNS(NPOS+2) = J2
      NPOS = NPOS + 2
C  allow next position to be ',' or ')'
      IF(LINEIN(N1+1:N1+1).EQ.')') THEN
        FTIN5 = RDFTLN(IDEV,LINEIN)
        RETURN
      ELSE IF(LINEIN(N1+1:N1+1).EQ.',') THEN
        IF(NUMBER(LINEIN(N1+2:N1+2))) THEN
          N1 = N1 + 2
C if ,number
          GO TO 10
        ELSE
          IF(LINEIN(N1+2:N1+2).NE.' ') THEN
            IERR = RDFTLN(IDEV,LINEIN)
C if ,notspace
            FTIN5 = 5
            RETURN
          END IF
        END IF
      END IF
      IERR = RDFTLN(IDEV,LINEIN)
      IF(IERR.NE.0) THEN
        FTIN5  = IERR
        RETURN
      END IF
      IF(LINEIN(6:8).EQ.'  ') THEN
        N1 = 22
        GO TO 10
      END IF
      FTIN5 = 5
      END
      INTEGER FUNCTION FTIN8(IDEV,POSNS,MAXPOS,NPOS,LINEIN)
      INTEGER POSNS(MAXPOS)
      CHARACTER LINEIN*(*)
      INTEGER FTPAIR,RDFTLN
      LOGICAL NUMBER,STRNGM
      EXTERNAL FTPAIR,NUMBER,STRNGM,RDFTLN
C  only deal with simplest cases:
C  join(complement(123..345),complement(59..67))
C  join(complement(123..345),complement(59..67),
C  complement(44..55))
C  assume continues lines end with ", "
C  always terminate at first closing bracket
C  remember start
      NPOS1 = NPOS
      FTIN8 = 8
      N1 = 38
10    CONTINUE
      IF(NPOS+2.GT.MAXPOS) RETURN
      FTIN8 = FTPAIR(LINEIN,N1,J1,J2)
      IF(FTIN8.NE.0) THEN
        IERR = RDFTLN(IDEV,LINEIN)
      END IF
      POSNS(NPOS+1) = J1
      POSNS(NPOS+2) = J2
      NPOS = NPOS + 2
C  allow next position to be '),COMPLEMENT' or ')) '
      IF(LINEIN(N1+1:N1+2).EQ.'))') THEN
        FTIN8 = RDFTLN(IDEV,LINEIN)
        CALL REVFTP(POSNS(NPOS1+1),NPOS-NPOS1)
        RETURN
      ELSE IF(STRNGM(LINEIN(N1+1:N1+12),'),COMPLEMENT')) THEN
          N1 = N1 + 14
C if ,number
          GO TO 10
        ELSE
          IF(LINEIN(N1+1:N1+3).NE.'), ') THEN
            IERR = RDFTLN(IDEV,LINEIN)
C if ,notspace
            FTIN8 = 8
            RETURN
C          END IF
        END IF
      END IF
      IERR = RDFTLN(IDEV,LINEIN)
      IF(IERR.NE.0) THEN
        FTIN8 = IERR
        RETURN
      END IF
      IF(LINEIN(6:8).EQ.'  ') THEN
        N1 = 33
        GO TO 10
      END IF
      FTIN8 = 8
      END
      SUBROUTINE REVFTP(POSNS,NPOS)
C Reverses posns for join(complement()) and order(complement())
      INTEGER POSNS(NPOS)
      DO 10 I = 1,NPOS-1,2
        J = POSNS(I)
        POSNS(I) = POSNS(I+1)
        POSNS(I+1) = J
 10     CONTINUE
      DO 20 I = 1,NPOS/2
        J = POSNS(I)
        POSNS(I) = POSNS(NPOS-I+1)
        POSNS(NPOS-I+1) = J
 20     CONTINUE
      END
      INTEGER FUNCTION FTIN9(IDEV,POSNS,MAXPOS,NPOS,LINEIN)
      INTEGER POSNS(MAXPOS)
      CHARACTER LINEIN*(*)
      INTEGER FTPAIR,RDFTLN
      LOGICAL NUMBER
      EXTERNAL FTPAIR,NUMBER,RDFTLN
C  only deal with simplest cases:
C  order(123..345,456..666)
C  order(123..345,456..666,
C  789..899)
C  assume continues lines end with ", "
C  always terminate at first closing bracket
      FTIN9 = 9
      N1 = 28
10    CONTINUE
      IF(NPOS+2.GT.MAXPOS) RETURN
      FTIN9 = FTPAIR(LINEIN,N1,J1,J2)
      IF(FTIN9.NE.0) THEN
        IERR = RDFTLN(IDEV,LINEIN)
        RETURN
      END IF
      POSNS(NPOS+1) = J1
      POSNS(NPOS+2) = J2
      NPOS = NPOS + 2
C  allow next position to be ',' or ')'
      IF(LINEIN(N1+1:N1+1).EQ.')') THEN
        FTIN9 = RDFTLN(IDEV,LINEIN)
        RETURN
      ELSE IF(LINEIN(N1+1:N1+1).EQ.',') THEN
        IF(NUMBER(LINEIN(N1+2:N1+2))) THEN
          N1 = N1 + 2
C if ,number
          GO TO 10
        ELSE
          IF(LINEIN(N1+2:N1+2).NE.' ') THEN
            IERR = RDFTLN(IDEV,LINEIN)
C if ,notspace
            FTIN9 = 9
            RETURN
          END IF
        END IF
      END IF
      IERR = RDFTLN(IDEV,LINEIN)
      IF(IERR.NE.0) THEN
        FTIN9 = IERR
        RETURN
      END IF
      IF(LINEIN(6:8).EQ.'  ') THEN
        N1 = 22
        GO TO 10
      END IF
      FTIN9 = 9
      END
      INTEGER FUNCTION FTIN10(IDEV,POSNS,MAXPOS,NPOS,LINEIN)
      INTEGER POSNS(MAXPOS)
      CHARACTER LINEIN*(*)
      INTEGER FTPAIR,RDFTLN
      LOGICAL NUMBER,STRNGM
      EXTERNAL FTPAIR,NUMBER,STRNGM,RDFTLN
C  only deal with simplest cases:
C  order(complement(123..345),complement(59..67))
C  order(complement(123..345),complement(59..67),
C  complement(44..55))
C  assume continues lines end with ", "
C  always terminate at first closing bracket
C  remember start
      NPOS1 = NPOS
      FTIN10 = 10
      N1 = 39
10    CONTINUE
      IF(NPOS+2.GT.MAXPOS) RETURN
      FTIN10 = FTPAIR(LINEIN,N1,J1,J2)
      IF(FTIN10.NE.0) THEN
        IERR = RDFTLN(IDEV,LINEIN)
        RETURN
      END IF
      POSNS(NPOS+1) = J1
      POSNS(NPOS+2) = J2
      NPOS = NPOS + 2
C  allow next position to be '),COMPLEMENT' or ')) '
      IF(LINEIN(N1+1:N1+2).EQ.'))') THEN
        FTIN10 = RDFTLN(IDEV,LINEIN)
        CALL REVFTP(POSNS(NPOS1+1),NPOS-NPOS1)
        RETURN
      ELSE IF(STRNGM(LINEIN(N1+1:N1+12),'),COMPLEMENT')) THEN
          N1 = N1 + 13
C if ,number
          GO TO 10
        ELSE
          IF(LINEIN(N1+1:N1+3).NE.'), ') THEN
            IERR = RDFTLN(IDEV,LINEIN)
C if ,notspace
            FTIN10 = 10
            RETURN
C          END IF
        END IF
      END IF
      IERR = RDFTLN(IDEV,LINEIN)
      IF(IERR.NE.0) THEN
        FTIN10 = IERR
        RETURN
      END IF
      IF(LINEIN(6:8).EQ.'  ') THEN
        N1 = 33
        GO TO 10
      END IF
      FTIN10 = 10
      END
      INTEGER FUNCTION FTIN11(IDEV,POSNS,MAXPOS,NPOS,LINEIN)
      INTEGER POSNS(MAXPOS)
      CHARACTER LINEIN*(*)
      INTEGER FTPAIR,RDFTLN
      LOGICAL NUMBER,STRNGM
      EXTERNAL FTPAIR,NUMBER,STRNGM,RDFTLN
C  only deal with simplest cases:
C  complement(order(123..345,456..666))
C  complement(order(123..345,456..666,
C  789..899))
C  assume continues lines end with ", "
C  always terminate at first closing bracket
      FTIN11 = 11
      N1 = 39
10    CONTINUE
      IF(NPOS+2.GT.MAXPOS) RETURN
      FTIN11 = FTPAIR(LINEIN,N1,J1,J2)
      IF(FTIN11.NE.0) THEN
        IERR = RDFTLN(IDEV,LINEIN)
        RETURN
      END IF
      POSNS(NPOS+1) = J1
      POSNS(NPOS+2) = J2
      NPOS = NPOS + 2
C  allow next position to be ',' or ')) '
      IF(LINEIN(N1+1:N1+2).EQ.'))') THEN
        FTIN11 = RDFTLN(IDEV,LINEIN)
        RETURN
      ELSE IF(LINEIN(N1+1:N1+1).EQ.',') THEN
        IF(NUMBER(LINEIN(N1+2:N1+2))) THEN
          N1 = N1 + 2
C if ,number
          GO TO 10
        ELSE
          IF(LINEIN(N1+2:N1+2).NE.' ') THEN
            IERR = RDFTLN(IDEV,LINEIN)
C if ,notspace
            FTIN11 = 11
            RETURN
          END IF
        END IF
      END IF
      IERR = RDFTLN(IDEV,LINEIN)
      IF(IERR.NE.0) THEN
        FTIN11 = IERR
        RETURN
      END IF
      IF(LINEIN(6:8).EQ.'  ') THEN
        N1 = 22
        GO TO 10
      END IF
      FTIN11 = 11
      END
      INTEGER FUNCTION FTPAIR(LINEIN,N1,J1,J2)
      CHARACTER LINEIN*(*)
      INTEGER FIRSTN
      EXTERNAL LASTN,FIRSTN
      FTPAIR = -4
C  find first and last digits in each of two numbers (n1,n2 and n3,n4)
C  then encode them
      J = INDEX(LINEIN(N1:),'.')
C  Could be cases 1,4,5 but for now error
      IF(J.EQ.0) RETURN
      J = FIRSTN(LINEIN(N1:),1)
      IF(J.EQ.0) RETURN
      N1 = N1 + J - 1
      J = INDEX(LINEIN(N1:),'.')
      N2 = N1 + J - 2
      K = N2 + 2
      J = INDEX(LINEIN(K:),'.')
      IF(J.EQ.0) RETURN
      IF(J.NE.1) RETURN
      N3 = J + K
      J = FIRSTN(LINEIN(N3:),1)
      IF(J.EQ.0) RETURN
      N3 = N3 + J - 1
      K = N3
      J = LASTN(LINEIN,K)
      IF(J.EQ.0) RETURN
      N4 = J
      READ(LINEIN(N1:N2),1000,ERR=100)J1
1000  FORMAT(I7)
      READ(LINEIN(N3:N4),1000,ERR=100)J2
      FTPAIR = 0
      N1 = N4
      RETURN
100   CONTINUE
C      WRITE(*,*)'Scream: ftpair'
      END
      INTEGER FUNCTION FTIN7(IDEV,POSNS,MAXPOS,NPOS,LINEIN)
      INTEGER POSNS(MAXPOS),RDFTLN
      CHARACTER LINEIN*(*)
      EXTERNAL RDFTLN
      FTIN7 = RDFTLN(IDEV,LINEIN)
      END
      INTEGER FUNCTION FTIN4(IDEV,POSNS,MAXPOS,NPOS,LINEIN)
      INTEGER POSNS(MAXPOS),RDFTLN
      CHARACTER LINEIN*(*)
      EXTERNAL RDFTLN
      FTIN4 = RDFTLN(IDEV,LINEIN)
      END
