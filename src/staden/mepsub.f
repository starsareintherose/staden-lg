C     Subroutines for program MEP
C     author Rodger Staden
C     22-jun-1990 changed makeds to send work
C     9-7-90 removed menu routine
C     14-11-90 Replaced all radio by radion
C     2-3-92 set filnam = ' ' for some calls to openf1
C
C   CCODE
C   DCODE
C   DCODEI
C   DICIO
C   DRAWPN
C   EXAMDM
C   EXAMDW
C   GETCM
C   GETCOM
C   GETV
C   HAIRPD
C   HAIRPN
C   HARPN
C   KTONUM
C   LISTN
C   LWRAP2
C   MAKEDH
C   MAKEDM
C   MAKEDS
C   MAKEDW
C   MENU
C   NCODE
C   NCODES
C   OUTP
C   PLOTH
C   PLOTP
C   RDDIC
C   RDIC
C   RINF
C   RSEQ
C   RSEQ1
C   RSEQ2
C   SHLSRT
C   SHOWST
C   SQFIT7
C   SQFIT8
C   STRNIN
C   TCOMP
C   WDCODE
C   WDIC
C   WFUZ2
C   WRTDIC
C   WRTWMT
C   WTMCON
C
      SUBROUTINE RSEQ(IDEV1,IDEV2,FILNAM,SEQ,MAXCHR,MAXSEQ,LENSEQ,
     +NFILE,LMAX,KBIN,KBOUT,IHELPS,IHELPE,HELPF,IDEVH,IOK)
      INTEGER LENSEQ(MAXSEQ)
      CHARACTER SEQ(MAXCHR),FILNAM*(*),HELPF*(*)
      PARAMETER (MAXPRM = 30)
      CHARACTER PROMPT(2)*(MAXPRM)
      IOK = 1
      IB = 1
      PROMPT(1) = 'Read file of aligned sequences'
      PROMPT(2) = 'Use file of file names'
      CALL RADION('Select input mode',PROMPT,2,IB,
     +IHELPS,IHELPE,HELPF,IDEVH,KBIN,KBOUT)
      IF(IB.LT.1) RETURN
      IF(IB.EQ.2)THEN
        FILNAM = ' '
        CALL OPENF1(IDEV2,FILNAM,0,IOK,KBIN,KBOUT,
     +  'File of sequence names',
     +  HELPS,HELPE,HELPF,IDEVH)
        IF(IOK.NE.0) RETURN
        CALL RSEQ1(SEQ,MAXCHR,MAXSEQ,LENSEQ,
     +  IDEV2,IDEV1,KBIN,KBOUT,FILNAM,NFILE,LMAX,IOK)
        RETURN
      END IF
      IF(IB.EQ.1) THEN
        FILNAM = ' '
        CALL OPENF1(IDEV2,FILNAM,0,IOK,KBIN,KBOUT,
     +  'File of aligned sequences',
     +  IHELPS,HELPE,HELPF,IDEVH)
        IF(IOK.NE.0) RETURN
        CALL RSEQ2(SEQ,MAXCHR,MAXSEQ,LENSEQ,
     +  IDEV2,KBIN,KBOUT,FILNAM,NFILE,LMAX,IOK)
      END IF
      END
      SUBROUTINE RSEQ1(SEQ,MAXCHR,MAXSEQ,LENSEQ,
     +IDEVN,IDEVSQ,KBIN,KBOUT,FILNAM,NFILE,LMAX,IOK)
      INTEGER LENSEQ(MAXSEQ)
      CHARACTER SEQ(MAXCHR),FILNAM*(*)
C  CONCATENATE SEQUENCES IN SEQ, PUT THE START OF SEQ(J) IN LENSEQ(J)
C  SO THE LENGTH OF SEQ(J) IS LENSEQ(J+1) - LENSEQ(J)
      LMAX = 0
      NFILE = 1
      LENSEQ(1) = 1
      ISEQ = 1
      REWIND IDEVN
10    CONTINUE
      IF(NFILE.LT.MAXSEQ)THEN
        READ(IDEVN,1000,END=20,ERR=30)FILNAM
1000    FORMAT(A)
        CALL OPENRS(IDEVSQ,FILNAM,IOK,LRECL,2)
        IF(IOK.NE.0)THEN
          WRITE(KBOUT,1002)FILNAM
1002      FORMAT(' Error opening file ',A)
          GO TO 10
        END IF
        WRITE(KBOUT,1001)FILNAM
1001    FORMAT(' Reading file ',A)
        IDIM = MAXCHR - ISEQ + 1
        CALL ARRFIL(IDEVSQ,SEQ(ISEQ),IDIM,KBOUT)
        CLOSE(UNIT=IDEVSQ)
        NFILE = NFILE + 1
        ISEQ = ISEQ + IDIM
        LENSEQ(NFILE) = ISEQ
        LMAX = MAX(LMAX,IDIM)
        GO TO 10
      END IF
20    CONTINUE
      CLOSE(UNIT=IDEVN)
      IOK = 0
      WRITE(KBOUT,*)'Number of files ',NFILE - 1
      RETURN
30    CONTINUE
      WRITE(KBOUT,*)' Error reading file of file names'
      IOK = 1
      END
      SUBROUTINE RSEQ2(SEQ,MAXCHR,MAXSEQ,LENSEQ,
     +IDEVSQ,KBIN,KBOUT,FILNAM,NFILE,LMAX,IOK)
      INTEGER LENSEQ(MAXSEQ)
      CHARACTER SEQ(MAXCHR),LINE*80
      INTEGER NOTIRL
      EXTERNAL NOTIRL
      LMAX = 0
      NFILE = 0
      ISEQ = 1
10    CONTINUE
      IF(NFILE.LT.MAXSEQ)THEN
        NFILE = NFILE + 1
        LENSEQ(NFILE) = ISEQ
        READ(IDEVSQ,1000,ERR=40,END=30)LINE
1000    FORMAT(1X,A)
        IDIM = NOTIRL(LINE,80,' ')
        LMAX = MAX(LMAX,IDIM)
        K = MAXCHR - ISEQ + 1
        IDIM = MIN(IDIM,K)
        K = ISEQ
        DO 20 I = 1,IDIM
          SEQ(K) = LINE(I:I)
          K = K + 1
20      CONTINUE
        ISEQ = K
        GO TO 10
      END IF
30    CONTINUE
      CLOSE(UNIT=IDEVSQ)
      IOK = 0
      WRITE(KBOUT,*)'Number of files ',NFILE - 1
      RETURN
40    CONTINUE
      WRITE(KBOUT,*)' Error reading file'
      IOK = 1
      CLOSE(UNIT=IDEVSQ)
      END
      SUBROUTINE PLOTH(SEQ,MAXCHR,LENSEQ,NFILE,HIS,LMAX,KBIN,KBOUT,
     +MARGL,MARGR,MARGB,MARGT,ISXMAX,ISYMAX,I1,I2,
     +IHELPS,IHELPE,HELPF,IDEVH)
      PARAMETER (MAXSTR = 11)
      CHARACTER SEQ(MAXCHR),STRING(MAXSTR),HELPF*(*)
      INTEGER LENSEQ(NFILE),HIS(LMAX)
30    CONTINUE
      IDIM2 = 0
      CALL GETSTR('Word to search for',SEQ,STRING,MAXSTR,IDIM2,
     +KBOUT,KBIN,INFLAG)
      IF(INFLAG.EQ.1) THEN
        CALL HELP2(IHELPS,IHELPE,HELPF,IDEVH,KBIN,KBOUT)
        GO TO 30
      END IF
      IF(INFLAG.NE.0) RETURN
      MN = 1
      MX = IDIM2
      MINMAT = IDIM2
      CALL GETINT(MN,MX,MINMAT,'Minimum match',
     +  IVAL,KBIN,KBOUT,IHELPS,IHELPE,HELPF,IDEVH,IOK)
        IF(IOK.NE.0) RETURN
      MINMAT = IVAL
      CALL YESONO(MOPT,'Join dots','Separate dots',
     +IHELPS,IHELPE,HELPF,IDEVH,KBIN,KBOUT)
      IF(MOPT.LT.0)RETURN
      XMAX=LMAX
      XMIN=1
      YMIN=0
      CALL FILLI(HIS,LMAX,0)
      DO 40 I = 1,NFILE-1
        K = LENSEQ(I)
        I4 = LENSEQ(I+1) - K
        I3 = MIN(I2,I4)
        CALL SQFIT7(SEQ(K),I4,STRING,IDIM2,HIS,LMAX,
     +  I1,I3,MINMAT,1)
40    CONTINUE
      MAXHIS = 0
      DO 50 I = 1,LMAX
        IF(HIS(I).GT.MAXHIS)MAXHIS = HIS(I)
50    CONTINUE
      YMAX = MAXHIS
      WRITE(KBOUT,*)' Maximum in histogram=',MAXHIS
      CALL VECTOM
      CALL FRAME(MARGL,MARGR,MARGB,MARGT,ISXMAX,ISYMAX)
      XF=XMIN
      YF=YMIN
      DO 100 I = 1,LMAX
        XT = I
        YT = HIS(I)
        IF(MOPT.EQ.0)CALL LINE(XF,XT,YF,YT,XMAX,XMIN,YMAX,YMIN,
     +  MARGL,MARGR,MARGB,MARGT,ISXMAX,ISYMAX)
        IF(MOPT.EQ.1)CALL POINT(XT,YT,XMAX,XMIN,YMAX,YMIN,
     +  MARGL,MARGR,MARGB,MARGT,ISXMAX,ISYMAX)
        XF=XT
        YF=YT
100   CONTINUE
      CALL VT100M
      END
C      SQFIT7
      SUBROUTINE SQFIT7(SEQ,IDIM1,STRING,IDIM2,ITOTEL,ITOTID,
     +IS,IE,MINS,KSTART)
C   AUTHOR: RODGER STADEN
C   COMPARE POINTERS, NOT ACTUAL CHARACTERS
      CHARACTER SEQ(IDIM1),STRING(IDIM2)
      INTEGER ITOTEL(ITOTID)
      INTEGER CTONUM
      EXTERNAL CTONUM
      IDIF=(IE-IS+2)-IDIM2
      IPSEQ=IS-KSTART+1
      DO 200 I=1,IDIF
        NTOT = 0
        IP=IPSEQ
        DO 100 J=1,IDIM2
          IF(STRING(J).EQ.'-')THEN
            NTOT = NTOT + 1
          ELSE
            K = CTONUM(STRING(J))
            IF(K.EQ.CTONUM(SEQ(IP)))NTOT = NTOT + 1
          END IF
          IP=IP+1
100     CONTINUE
        IF(NTOT.GE.MINS)THEN
          K = IP - IDIM2+KSTART-1
          ITOTEL(K) = ITOTEL(K) + 1
        END IF
        IPSEQ=IPSEQ+1
200   CONTINUE
      END
      SUBROUTINE PLOTP(SEQ,MAXCHR,LENSEQ,NFILE,HIS,LMAX,KBIN,KBOUT,
     +MARGL,MARGR,MARGB,MARGT,ISXMAX,ISYMAX,I1,I2,IDEV,
     +IHELPS,IHELPE,HELPF,IDEVH)
      PARAMETER (MAXSTR=40)
      CHARACTER SEQ(MAXCHR),STRING(MAXSTR),HELPF*(*)
      INTEGER LENSEQ(NFILE),HIS(LMAX)
30    CONTINUE
      IDIM2 = 0
      CALL GETSTR('Word to search for',SEQ,STRING,MAXSTR,IDIM2,
     +KBOUT,KBIN,INFLAG)
      IF(INFLAG.EQ.1) THEN
        CALL HELP2(IHELPS,IHELPE,HELPF,IDEVH,KBIN,KBOUT)
        GO TO 30
      END IF
      IF(INFLAG.NE.0) RETURN
      MN = 1
      MX = IDIM2
      MINMAT = IDIM2
      CALL GETINT(MN,MX,MINMAT,'Minimum match',
     +  IVAL,KBIN,KBOUT,IHELPS,IHELPE,HELPF,IDEVH,IOK)
        IF(IOK.NE.0) RETURN
      MINMAT = IVAL
      IGON = 0
      CALL YESONO(IGON,'Plot results','List results',
     +IHELPS,IHELPE,HELPF,IDEVH,KBIN,KBOUT)
      IF(IGON.LT.0)RETURN
C  CHECK RANGE
      XMAX=LMAX
      XMIN=1
      YMIN=0
      YMAX = 1
      IF(IGON.EQ.0)THEN
        CALL VECTOM
        CALL FRAME(MARGL,MARGR,MARGB,MARGT,ISXMAX,ISYMAX)
        XMARG = REAL(MARGT)/(NFILE-1)
        MARGT1 = NINT(XMARG)
        MARGB1 = MARGB
      END IF
      NMATT = 0
      DO 200 I = 1,NFILE-1
        CALL FILLI(HIS,LMAX,0)
        K = LENSEQ(I)
        I4 = LENSEQ(I+1) - K
        I3 = MIN(I2,I4)
        CALL SQFIT8(SEQ(K),I4,STRING,IDIM2,HIS,LMAX,
     +  I1,I3,MINMAT,NMAT,1)
        NMATT = NMATT + NMAT
        YF=YMIN
        YT = YMAX
        DO 100 J = 1,NMAT
          XT = HIS(J)
          XF = XT
          IF(IGON.EQ.0)THEN
            CALL LINE(XF,XT,YF,YT,XMAX,XMIN,YMAX,YMIN,
     +        MARGL,MARGR,MARGB1,MARGT1,ISXMAX,ISYMAX)
          END IF
          IF(IGON.EQ.1)THEN
            K1 = LENSEQ(I) + HIS(J) - 1
            K2 = K1 + IDIM2 - 1
            WRITE(IDEV,2000)I,HIS(J),(SEQ(K),K=K1,K2)
2000          FORMAT(' ',I6,I6,' ',20A1)
           END IF
100     CONTINUE
        MARGB1 = MARGB1 + MARGT1
200   CONTINUE
      CALL VT100M
      END
C      SQFIT8
      SUBROUTINE SQFIT8(SEQ,IDIM1,STRING,IDIM2,ITOTEL,ITOTID,
     +IS,IE,MINS,ITOTP,KSTART)
C   AUTHOR: RODGER STADEN
C   COMPARE POINTERS, NOT ACTUAL CHARACTERS
      CHARACTER SEQ(IDIM1),STRING(IDIM2)
      INTEGER ITOTEL(ITOTID)
      INTEGER CTONUM
      EXTERNAL CTONUM
      IDIF=(IE-IS+2)-IDIM2
      IPSEQ=IS-KSTART+1
      ITOTP=0
      DO 200 I=1,IDIF
        NTOT = 0
        IP=IPSEQ
        DO 100 J=1,IDIM2
          IF(STRING(J).EQ.'-')THEN
            NTOT = NTOT + 1
          ELSE
            K = CTONUM(STRING(J))
            IF(K.EQ.CTONUM(SEQ(IP)))NTOT = NTOT + 1
          END IF
          IP=IP+1
100     CONTINUE
        IF(NTOT.GE.MINS)THEN
          ITOTP=ITOTP+1
          IF(ITOTP.GT.ITOTID)RETURN
C          ITOT(ITOTP)=NTOT
          ITOTEL(ITOTP)=IP-IDIM2+KSTART-1
        END IF
        IPSEQ=IPSEQ+1
200   CONTINUE
      END
      SUBROUTINE HAIRPN(SEQ,MAXCHR,LENSEQ,NFILE,LMAX,
     +  ITOT,ITOTEL,ITOTAL,IDTOT,
     +  MARGL,MARGR,MARGB,MARGT,ISXMAX,ISYMAX,IDEV,KBIN,KBOUT,
     +  IHELPS,IHELPE,HELPF,IDEVH)
      CHARACTER HELPF*(*)
C   AUTHOR: RODGER STADEN
      INTEGER SCORES(30)
      CHARACTER SEQ(MAXCHR)
      INTEGER LENSEQ(NFILE)
      INTEGER ITOT(IDTOT),ITOTEL(IDTOT),ITOTAL(IDTOT)
      DO 1 I=1,30
        SCORES(I)=0
1     CONTINUE
      SCORES(16)=1
      SCORES(21)=1
      SCORES(22)=1
      SCORES(8)=1
      SCORES(9)=1
      SCORES(14)=1
      LEVEL=2
      IS = 1
      IN = LMAX
      MINLPI = 0
      MAXLPI = 10
      MINLP = 3
      MINLPX = 1
      MAXLPX = 20
      MAXLP = 6
      MINBP = 1
      MAXBP = 20
      MININ = 6
      CALL HAIRPD(IS,IN,KBIN,KBOUT,
     +MINLPI,MAXLPI,MINLP,MINLPX,MAXLPX,MAXLP,MINBP,MAXBP,MININ,
     +IGON,IOK,
     +IHELPS,IHELPE,HELPF,IDEVH)
      IF(IOK.NE.0) RETURN
      XMARG = REAL(MARGT)/(NFILE-1)
      MARGT1 = NINT(XMARG)
      MARGB1 = MARGB
      WRITE(KBOUT,*)' Searching'
      IF(IGON.EQ.0) THEN
        CALL VECTOM
        CALL FRAME(MARGL,MARGR,MARGB,MARGT,ISXMAX,ISYMAX)
      END IF
      DO 900 II = 1,NFILE-1
        CALL FILLI(ITOT,IDTOT,0)
        CALL FILLI(ITOTEL,IDTOT,0)
        CALL FILLI(ITOTAL,IDTOT,0)
        K = LENSEQ(II)
        IDIM = LENSEQ(II+1) - LENSEQ(II)
        CALL HARPN(SEQ(K),IDIM,ITOT,ITOTEL,ITOTAL,IDTOT,
     +  1,IDIM,KBOUT,IP,NOUT,MINLP,MAXLP,MININ,SCORES)
        IF(IP.GT.0)THEN
          XMAX=LMAX
          XMIN=1
C         PLOT ON SCALE 0 TO 4*MININ IN Y
          YMIN=0.
          YMAX=MININ*4
          YF=0.
        DO 800 I=NOUT+1,IP
          IF(IGON.EQ.0)CALL VECTOM
          XF=ITOTEL(I)+ITOTAL(I)/2
          XT=XF
          YT=ITOT(I)
          IF(IGON.EQ.0)CALL LINE(XF,XT,YF,YT,
     +    XMAX,XMIN,YMAX,YMIN,
     +    MARGL,MARGR,MARGB1,MARGT1,ISXMAX,ISYMAX)
          IF(IGON.EQ.1)THEN
            WRITE(IDEV,1000)II,ITOTEL(I)-ITOT(I)+1
1000        FORMAT(/,' Sequence',I6,I6)
            CALL DRAWPN(
     +      SEQ(K),IDIM,ITOT(I),ITOTAL(I),ITOTEL(I),
     +      IDEV,IDIM)
          END IF
800     CONTINUE
      END IF
      MARGB1 = MARGB1 + MARGT1
900   CONTINUE
      CALL VT100M
      END
      SUBROUTINE HARPN(SEQ,IDIM,ITOT,ITOTEL,ITOTAL,IDTOT,
     +IS,IN,KBOUT,IP,NOUT,MIN,MAX,MININ,SCORES)
C   AUTHOR: RODGER STADEN
      INTEGER SCORES(30)
      CHARACTER SEQ(IDIM)
      INTEGER ITOT(IDTOT),ITOTEL(IDTOT),ITOTAL(IDTOT)
      INTEGER CTONUM
      EXTERNAL CTONUM,LWRAP2
      IDO=IN-IS+1
      IF(IN.LE.IS)IDO=IDIM+IDO
      I=IS-1
      IP=0
      DO 301 K=1,IDO
        I=I+1
        DO 300 J=MIN,MAX
         N=0
100      CONTINUE
           IMN=I-N
           IPNPJ=I+N+J
           IT=CTONUM(SEQ(LWRAP2(IN,IMN)))+
     +     5*CTONUM(SEQ(LWRAP2(IN,IPNPJ)))
           IT=SCORES(IT)
           IF(IT.NE.0)THEN
             N=N+IT
             GO TO 100
           END IF
           IF(N.GE.MININ)THEN
             IP=IP+1
             IF(IP.GT.IDTOT)THEN
               WRITE(KBOUT,1234)IP,I
1234           FORMAT(' Maximum of',I5,
     +         ' loops found up to',I6,', no more searching')
               GO TO 302
             END IF
             ITOT(IP)=N
             ITOTEL(IP)=LWRAP2(IN,I)
             ITOTAL(IP)=J-1
           END IF
300      CONTINUE
301      CONTINUE
302   CONTINUE
      IF(IP.GT.0)THEN
        NOUT=0
        DO 700 J=1,IP
          JP1=J+1
           D1=FLOAT(ITOTEL(J))+FLOAT(ITOTAL(J))/2.
           DO 690 I=JP1,IP
             D2=FLOAT(ITOTEL(I))+FLOAT(ITOTAL(I))/2.
             IF(D1.NE.D2)GO TO 690
             LP2=ITOTAL(I)+2*ITOT(I)
             IF(ITOTAL(J).GT.LP2)GO TO 690
             ITOT(J)=0
             ITOTAL(J)=0
             ITOTEL(J)=0
             NOUT=NOUT+1
             GO TO 700
690        CONTINUE
700     CONTINUE
        CALL BUB3AS(ITOTEL,ITOT,ITOTAL,IP)
      END IF
      END
C
C   subroutine to draw hairpin loops on device idev
C   hairpins are defined by:
C   1) nstem the number of basepairs in the stem
C   2) nloop the number of bases in the loop
C   3) ip the position of the rightmost base in the left side of the stem
C
C   draw the stems very simply, just standing up with a few bases
C   before and after
C   maximum size is 20 up and 14 across
      SUBROUTINE DRAWPN(SEQ,IDIM,NSTEM,NLOOP,IP,IDEV,IN)
C   AUTHOR: RODGER STADEN
      CHARACTER SEQ(IDIM),ARRAY(14,20),SCORE(5,5),SPACE
      INTEGER CTONUM
      EXTERNAL CTONUM
      DATA SPACE/' '/
C   CHECK FOR WRAP AROUND
      IF((IP-NSTEM).LT.0)RETURN
      IF((IP+NLOOP+NSTEM+1).GT.IN)RETURN
      DO 1 I=1,5
      DO 1 J=1,5
        SCORE(I,J)=' '
1     CONTINUE
      SCORE(1,3)='-'
      SCORE(3,1)='-'
      SCORE(2,4)='-'
      SCORE(4,2)='-'
      SCORE(1,4)='.'
      SCORE(4,1)='.'
      DO 2 I=1,14
      DO 2 J=1,20
        ARRAY(I,J)=SPACE
2     CONTINUE
C   point to left stem start
      IPL=IP-NSTEM
C   point to right stem start
      IPR=IP+NSTEM+NLOOP+1
C   point to temporary array
      IAL=6
      IAR=8
      KA=1
C   fill in stem and loop
      DO 10 I=1,MIN(NSTEM+NLOOP/2,19)
        IPL=IPL+1
        IPR=IPR-1
        KA=KA+1
        ARRAY(IAL,KA)=SEQ(IPL)
        ARRAY(IAR,KA)=SEQ(IPR)
C       basepaired?
        ARRAY(IAL+1,KA)=
     +  SCORE(CTONUM(SEQ(IPL)),CTONUM(SEQ(IPR)))
10    CONTINUE
C     odd number of bases in loop?
      IF(MOD(NLOOP,2).NE.0)THEN
C       put in extra base
        IF((KA+1).LE.20)ARRAY((IAL+1),KA+1)=SEQ(IPL+1)
      END IF
C     now do 6 bases before the stem and 7 after
      IPL=IP-NSTEM-6
      DO 30 I=1,6
        IPL=IPL+1
30    IF(IPL.GT.0)ARRAY(I,1)=SEQ(IPL)
      IPR=IP+NSTEM+NLOOP
      DO 40 I=8,14
        IPR=IPR+1
        IF(IPR.LT.IN)ARRAY(I,1)=SEQ(IPR)
40    CONTINUE
      DO 60 I=1,20
        K=21-I
        DO 50 JEMPT = 1,14
          IF(ARRAY(JEMPT,K).NE.' ') THEN
            WRITE(IDEV,1000)(ARRAY(L,K),L=1,14)
            GO TO 51
          END IF
50      CONTINUE
51      CONTINUE
60    CONTINUE
1000  FORMAT(' ',5X,14A1)
1001  FORMAT(' ',5X,I6)
      WRITE(IDEV,1001)IP-NSTEM
      END
      SUBROUTINE SHOWST(NFUZ,I1,I2,KCOMP,ICOMP,IDICW,IDICS,
     +IDICM,IDICH,MASK,LMASK,KBOUT)
      LOGICAL MASK(LMASK)
      CHARACTER STRING*25
      STRING = ' '
      DO 10 I=1,LMASK
        IF (MASK(I)) THEN
          STRING(I:I) = 'x'
        ELSE
          STRING(I:I) = '-'
        END IF
 10   CONTINUE
      WRITE(KBOUT,1001)STRING(1:LMASK),NFUZ,I1,I2
1001  FORMAT(' Current mask         = ',A,/,
     +       ' Number of mismatches =',I4,/,
     +       ' Start position       =',I6,/,
     +       ' End position         =',I6)
      IF(KCOMP.EQ.0)WRITE(KBOUT,*)' Input strand only'
      IF(KCOMP.EQ.1)WRITE(KBOUT,*)' Complementary strand only'
      IF(KCOMP.EQ.2)WRITE(KBOUT,*)' Both strands'
      IF(ICOMP.EQ.0)WRITE(KBOUT,*)' Even composition'
      IF(ICOMP.EQ.1)WRITE(KBOUT,*)' Observed composition'
      IF(ICOMP.EQ.2)WRITE(KBOUT,*)' Entered composition'
      IF(IDICW.EQ.0)WRITE(KBOUT,*)' Dictionary Dw made'
      IF(IDICW.NE.0)WRITE(KBOUT,*)' Dictionary Dw unmade'
      IF(IDICS.NE.0)WRITE(KBOUT,*)' Dictionary Ds unmade'
      IF(IDICS.EQ.0)WRITE(KBOUT,*)' Dictionary Ds made'
      IF(IDICM.EQ.0)WRITE(KBOUT,*)' Dictionary Dm unmade'
      IF(IDICM.EQ.1)WRITE(KBOUT,*)' Dictionary Dm made from Dw'
      IF(IDICM.EQ.2)WRITE(KBOUT,*)' Dictionary Dm made from Ds'
      IF(IDICH.EQ.0)WRITE(KBOUT,*)' Dictionary Dh unmade'
      IF(IDICH.EQ.1)WRITE(KBOUT,*)' Dictionary Dh made from Dw'
      IF(IDICH.EQ.2)WRITE(KBOUT,*)' Dictionary Dh made from Ds'
      END
      SUBROUTINE GETCM(COMP,KBIN,KBOUT,SEQN,LENSEQ,ICOMP,
     +IHELPS,IHELPE,HELPF,IDEVH)
      INTEGER SEQN(LENSEQ)
      REAL COMP(5),COMPT(4)
      CHARACTER BASE(4),HELPF*(*),STRING*22
      PARAMETER (MAXPRM = 20)
      CHARACTER PROMPT(3)*(MAXPRM)
      SAVE BASE
      DATA BASE/'T','C','A','G'/
      NOPT = ICOMP + 1
      PROMPT(1) = 'Even composition'
      PROMPT(2) = 'Observed composition'
      PROMPT(3) = 'Type in composition'
        CALL RADION('Select composition',PROMPT,3,NOPT,
     +  IHELPS,IHELPE,HELPF,IDEVH,KBIN,KBOUT)
        IF(NOPT.LT.1) RETURN
        ICOMP = NOPT - 1
      IF(NOPT.EQ.1) THEN
        CALL FILLR(COMP,5,0.25)
        RETURN
      ELSE IF (NOPT.EQ.2) THEN
        CALL GETCOM(SEQN,LENSEQ,COMP)
        RETURN
      ELSE IF (NOPT.EQ.3) THEN
        RMN = 0.
        TOT = 0.
        DO 30 I = 1,4
          RMX = 1.0 - TOT
          RMM = MIN(0.25,RMX)
          WRITE(STRING,1002)BASE(I)
1002      FORMAT('Composition for base ',A1)
          CALL GETRL(RMN,RMX,RMM,STRING,VAL,KBIN,KBOUT,
     +    IHELPS,IHELPE,HELPF,IDEVH,IOK)
          IF(IOK.NE.0) RETURN
          TOT = TOT + VAL
          COMPT(I) = VAL
30      CONTINUE
        CALL COPYR(COMPT,COMP,4)
        RETURN
      END IF
      END
      SUBROUTINE EXAMDW(DICTOT,WINDEX,
     +MAXPOS,LENGTH,KBIN,KBOUT,WT,NFUZ,IOK,COMP,NFILE,IDEV,
     +TEMPC,TEMPI,TEMPP,TEMPN,MAXLIS,MASK,LMASK)
      INTEGER DICTOT(MAXPOS),WINDEX(MAXPOS),WT(4,LENGTH)
      CHARACTER DCODE*8,STRING*8,STRNIN*8,UNMASK*25,SOUT*25,ST*8
      CHARACTER MMASK*25
      REAL COMP(5)
      INTEGER TEMPC(MAXLIS),TEMPP(MAXLIS),TEMPI(MAXLIS),TEMPN(MAXLIS)
      LOGICAL MASK(LMASK)
      EXTERNAL DCODE,NCODES,STRNIN,RINF,UNMASK,MMASK
      IOK = 1
      IFILL = 4 * LENGTH
30    CONTINUE
      L = 0
      CALL GTSTR('Word to examine',' ',ST,L,KBOUT,KBIN,INFLAG)
      IF(INFLAG.EQ.1) THEN
        CALL HELP2(IHELPS,IHELPE,HELPF,IDEVH,KBIN,KBOUT)
        GO TO 30
      END IF
      IF(INFLAG.NE.0) RETURN
      STRING = MMASK(ST,MASK,LMASK,L)      
      IF(L.NE.LENGTH) RETURN
      CALL FILLI(WT,IFILL,1)
      CALL WFUZ2(STRING,LENGTH,NFUZ,WINDEX,MAXPOS,WT)
      I = NCODES(STRING,LENGTH)
      R = RINF(WT,LENGTH,COMP)
      ST = STRNIN(STRING,LENGTH)
      SOUT = UNMASK(ST,MASK,LMASK)
      WRITE(IDEV,*)SOUT(1:LMASK),' ',DICTOT(I),R
      CALL WRTWMT(WT,WT,4,LENGTH,IDEV,1)
      GO TO 30
      END
      SUBROUTINE EXAMDM(DICTOT,WINDEX,
     +MAXPOS,LENGTH,KBIN,KBOUT,WT,NFUZ,IOK,COMP,NFILE,IDEV,
     +TEMPC,TEMPI,TEMPP,TEMPN,MAXLIS,MASK,LMASK)
      INTEGER DICTOT(MAXPOS),WINDEX(MAXPOS),WT(4,LENGTH)
      CHARACTER DCODE*8,STRING*8,STRNIN*8
      REAL COMP(5)
      LOGICAL MASK(LMASK)
      INTEGER TEMPC(MAXLIS),TEMPP(MAXLIS),TEMPI(MAXLIS),TEMPN(MAXLIS)
      PARAMETER (MAXPRM = 19)
      CHARACTER PROMPT(2)*(MAXPRM)
      EXTERNAL DCODE,NCODES,STRNIN,RINF
      MAXTOT = 0
      IOK = 1
      WRITE(KBOUT,*)'Looking for highest scoring words'
      IFILL = 4 * LENGTH
      DO 10 I = 1,MAXPOS
        IF(DICTOT(I).GT.MAXTOT)MAXTOT = DICTOT(I)
10    CONTINUE
15    CONTINUE
      WRITE(KBOUT,*)
     +'The highest word score = ',MAXTOT
      MN = 0
      MX = MAXTOT
      I = MIN(NFILE/2,MAXTOT)
      CALL GETINT(MN,MX,I,'Minimum word score',
     +  IVAL,KBIN,KBOUT,IHELPS,IHELPE,HELPF,IDEVH,IOK)
        IF(IOK.NE.0) RETURN
      MINTOT = IVAL
      RI = 0.
      RMN = 0.
      RMX = 1.0
      CALL GETRL(RMN,RMX,RI,'Minimum information',
     +  VAL,KBIN,KBOUT,IHELPS,IHELPE,HELPF,IDEVH,IOK)
        IF(IOK.NE.0) RETURN
      XINF = VAL
      I = 1
      PROMPT(1) = 'Sort on information'
      PROMPT(2) = 'Sort word scores'
      CALL RADION('Select sort mode',PROMPT,2,I,
     +IHELPS,IHELPE,HELPF,IDEVH,KBIN,KBOUT)
      IF(I.LT.1) RETURN
      ISORT = I - 1
      MN = 0
      MX = MAXLIS
      I = MAXLIS
      CALL GETINT(MN,MX,I,'Maximum number to list',
     +  IVAL,KBIN,KBOUT,IHELPS,IHELPE,HELPF,IDEVH,IOK)
        IF(IOK.NE.0) RETURN
      MAXL = IVAL
      IF((MINTOT.GT.0).OR.(XINF.GT.0.0))THEN
        IF(MAXTOT.GE.MINTOT)THEN
          WRITE(KBOUT,*)'The words are'
          N = 0
          XINFM = 0.
          DO 20 I = 1,MAXPOS
            IF(DICTOT(I).GE.MINTOT)THEN
              STRING = DCODE(I,LENGTH)
              CALL FILLI(WT,IFILL,1)
              CALL WFUZ2(STRING,LENGTH,NFUZ,WINDEX,MAXPOS,WT)
              R = RINF(WT,LENGTH,COMP)
              IF(R.GE.XINF)THEN
                XINFM = MAX(XINFM,R)
C                WRITE(IDEV,*)STRNIN(STRING,LENGTH),DICTOT(I),R
                N = N + 1
                IF(N.LE.MAXL) THEN
                  TEMPI(N) = R * 10000.
                  TEMPC(N) = DICTOT(I)
                  TEMPP(N) = I
                END IF
              END IF
            END IF
20        CONTINUE
          WRITE(KBOUT,*)' Total words=',N,' Maximum information=',XINFM
          MLIS = MIN(MAXL,N)
          INCDEC = -1
          IF(ISORT.EQ.0) THEN
            CALL SHLSRT(TEMPI,MLIS,TEMPN,INCDEC)
            CALL OUTP(TEMPI,TEMPC,TEMPP,TEMPN,MLIS,LENGTH,IDEV,MASK,
     +      LMASK)
          ELSE IF(ISORT.EQ.1) THEN
            CALL SHLSRT(TEMPC,MLIS,TEMPN,INCDEC)
            CALL OUTP(TEMPI,TEMPC,TEMPP,TEMPN,MLIS,LENGTH,IDEV,MASK,
     +      LMASK)
          END IF
          GO TO 15
        END IF
      END IF
      END
      CHARACTER*(*) FUNCTION STRNIN(STRING,LENGTH)
      CHARACTER STRING*(*),TUPPER
      EXTERNAL TUPPER,KTONUM
      CALL CCASE(STRING,2)
      STRNIN(1:LENGTH) = STRING(1:LENGTH)
      LO2 = LENGTH / 2
      J = LENGTH
      DO 10 I = 1,LO2
        K = KTONUM(STRING(I:I)) + KTONUM(STRING(J:J))
        IF (K.EQ.5) THEN
          STRNIN(I:I) = TUPPER(STRING(I:I))
          STRNIN(J:J) = TUPPER(STRING(J:J))
        END IF
        J = J - 1
10    CONTINUE
      END
      INTEGER FUNCTION KTONUM(CHAR)
      CHARACTER CHAR
      INTEGER CTONUM,MAP(5)
      EXTERNAL CTONUM
      SAVE MAP
      DATA MAP/1,2,4,3,5/
      KTONUM = MAP(CTONUM(CHAR))
      END
      SUBROUTINE MAKEDW(SEQ,MAXCHR,WINDEX,TMPDIC,MAXPOS,
     +LENSEQ,MAXSEQ,NFILE,LENGTH,KCOMP,I1,I2,MASK,LMASK)
      INTEGER SEQ(MAXCHR),WINDEX(MAXPOS),TMPDIC(MAXPOS)
      INTEGER LENSEQ(MAXSEQ),CCODEM
      LOGICAL MASK(LMASK)
      EXTERNAL NCODEM,CCODEM
      CALL FILLI(WINDEX,MAXPOS,0)
      IF((KCOMP.EQ.0).OR.(KCOMP.EQ.2))THEN
        DO 10 I = 1,NFILE-1
          I11 = LENSEQ(I) + I1 - 1
          I22 = MIN(LENSEQ(I) + I2 - 1, LENSEQ(I+1)) - LMASK
          DO 5 J = I11,I22
            N = NCODEM(SEQ(J),LENGTH,MASK,LMASK)
            IF(N.NE.0) WINDEX(N) = WINDEX(N) + 1
5         CONTINUE
10      CONTINUE
      END IF
      IF((KCOMP.EQ.1).OR.(KCOMP.EQ.2)) THEN
        DO 20 I = 1,NFILE-1
          I11 = LENSEQ(I) + I1 - 1
          I22 = MIN(LENSEQ(I) + I2 - 1, LENSEQ(I+1)) - LMASK
          DO 15 J = I11,I22
            N = CCODEM(SEQ(J),LENGTH,MASK,LMASK)
            IF(N.NE.0) WINDEX(N) = WINDEX(N) + 1
15        CONTINUE
20      CONTINUE
      END IF
      END
      INTEGER FUNCTION CCODE(STRING,LENGTH)
      IMPLICIT INTEGER (A-Z)
      DIMENSION CONSTS(8),STARTS(8),STRING(LENGTH),COMP(5)
      SAVE CONSTS,STARTS,COMP
      DATA CONSTS/1,4,16,64,256,1024,4096,16384/
      DATA STARTS/0,-4,-20,-84,-340,-1364,-5460,-21844/
      DATA COMP/3,4,1,2,5/
      CCODE = 0
      N = STARTS(LENGTH)
      L = LENGTH
      DO 10 I = LENGTH,1,-1
        J = STRING(I)
        IF(J.NE.5) THEN
          N = N + COMP(J) * CONSTS(L)
          L = L - 1
        ELSE
          RETURN
        END IF
10    CONTINUE
      CCODE = N
      END
      INTEGER FUNCTION CCODEM(STRING,LENGTH,MASK,LMASK)
      IMPLICIT INTEGER (A-Z)
      DIMENSION CONSTS(8),STARTS(8),STRING(LENGTH),COMP(5)
      LOGICAL MASK(LMASK)
      SAVE CONSTS,STARTS,COMP
      DATA CONSTS/1,4,16,64,256,1024,4096,16384/
      DATA STARTS/0,-4,-20,-84,-340,-1364,-5460,-21844/
      DATA COMP/3,4,1,2,5/
      CCODEM = 0
      N = STARTS(LENGTH)
      L = LENGTH
      DO 10 I = LENGTH,1,-1
        IF (MASK(I)) THEN
          J = STRING(I)
          IF(J.NE.5) THEN
            N = N + COMP(J) * CONSTS(L)
            L = L - 1
          ELSE
            RETURN
          END IF
        END IF
10    CONTINUE
      CCODEM = N
      END
      INTEGER FUNCTION NCODEM(STRING,LENGTH,MASK,LMASK)
      IMPLICIT INTEGER (A-Z)
      DIMENSION CONSTS(8),STARTS(8),STRING(LENGTH)
      LOGICAL MASK(LMASK)
      SAVE CONSTS,STARTS
      DATA CONSTS/1,4,16,64,256,1024,4096,16384/
      DATA STARTS/0,-4,-20,-84,-340,-1364,-5460,-21844/
      NCODEM = 0
      N = STARTS(LENGTH)
      L = LENGTH
      DO 10 I = 1,LMASK
        IF (MASK(I)) THEN
          J = STRING(I)
          IF(J.NE.5) THEN
            N = N + J * CONSTS(L)
            L = L - 1
          ELSE
            RETURN
          END IF
        END IF
10    CONTINUE
      NCODEM = N
      END
      INTEGER FUNCTION NCODE(STRING,LENGTH)
      IMPLICIT INTEGER (A-Z)
      DIMENSION CONSTS(8),STARTS(8),STRING(LENGTH)
      SAVE CONSTS,STARTS
      DATA CONSTS/1,4,16,64,256,1024,4096,16384/
      DATA STARTS/0,-4,-20,-84,-340,-1364,-5460,-21844/
      NCODE = 0
      N = STARTS(LENGTH)
      L = LENGTH
      DO 10 I = 1,LENGTH
        J = STRING(I)
        IF(J.NE.5) THEN
          N = N + J * CONSTS(L)
          L = L - 1
        ELSE
          RETURN
        END IF
10    CONTINUE
      NCODE = N
      END
      INTEGER FUNCTION NCODES(STRING,LENGTH)
      INTEGER CONSTS(8),STARTS(8)
      CHARACTER STRING*(*)
      INTEGER CTONUM
      EXTERNAL CTONUM
      SAVE CONSTS,STARTS
      DATA CONSTS/1,4,16,64,256,1024,4096,16384/
      DATA STARTS/0,-4,-20,-84,-340,-1364,-5460,-21844/
      NCODES = STARTS(LENGTH)
      DO 10 I = 1,LENGTH
        NCODES = NCODES + CTONUM(STRING(I:I)) * CONSTS(LENGTH-I+1)
10    CONTINUE
      END
      SUBROUTINE WFUZ2(WORD,LENGTH,NFUZ,WINDEX,MAXPOS,WT)
      INTEGER STRING(8),WINDEX(MAXPOS)
      INTEGER DIF(3,4),WT(4,LENGTH)
      INTEGER CTONUM
      CHARACTER WORD*(*),CHAR
      EXTERNAL CTONUM,NCODE
      SAVE DIF
      DATA DIF/2,3,4,
     +         1,3,4,
     +         1,2,4,
     +         1,2,3/
      DO 1 I = 1,LENGTH
        CHAR = WORD(I:I)
        STRING(I) = CTONUM(CHAR)
1     CONTINUE      
C  KEEP ADDING COUNTS UNTIL LEVEL OF FUZ EXCEEDED
      NALL = NCODE(STRING(1),LENGTH)
        IF(WINDEX(NALL).NE.0) 
     +    CALL WDCODE(WT,STRING,LENGTH,WINDEX(NALL))
          IF(NFUZ.EQ.0)RETURN
C  ONLY ONE POSITION MUST CHANGE, CHANGE THEM ALL IN TURN
          N = 1
          DO 10 I1 = 1,LENGTH
            DO 5 I2 = 1,3
            N = N + 1
            IT = STRING(I1)
            NC1 = DIF(I2,STRING(I1))
            STRING(I1) = NC1
            NALL = NCODE(STRING(1),LENGTH)
            IF(WINDEX(NALL).NE.0)
     +      CALL WDCODE(WT,STRING,LENGTH,WINDEX(NALL))
            STRING(I1) = IT
5         CONTINUE
10      CONTINUE
        IF(NFUZ.EQ.1)RETURN
C  TWO POSITIONS MUST CHANGE, CHANGE THEM ALL IN TURN
        DO 40 I1 = 1,LENGTH
          DO 30 I2 = I1+1,LENGTH
            DO 25 I3 = 1,3
              IT1 = STRING(I1)
              NC1 = DIF(I3,STRING(I1))
              STRING(I1) = NC1
              DO 24 I4 = 1,3
                IT2 = STRING(I2)
                NC2 = DIF(I4,STRING(I2))
                STRING(I2) = NC2
                N = N + 1
                NALL = NCODE(STRING(1),LENGTH)
                IF(WINDEX(NALL).NE.0)
     +           CALL WDCODE(WT,STRING,LENGTH,WINDEX(NALL))
                STRING(I2) = IT2
24            CONTINUE
              STRING(I1) = IT1
25          CONTINUE
30        CONTINUE
40      CONTINUE
      END
      SUBROUTINE MAKEDM(WINDEX,DM,LENGTH,NFUZ,MAXPOS)
      INTEGER STRING(8),WINDEX(MAXPOS),DM(MAXPOS),DIF(3,4)
      SAVE DIF
      EXTERNAL NCODE
      DATA DIF/2,3,4,
     +         1,3,4,
     +         1,2,4,
     +         1,2,3/
C      IF(NFUZ.EQ.0)THEN
      DO 100 IWORD = 1,MAXPOS
C CHANGED NEXT LINE 16-7-89
C        DICTOT(IWORD) = DICTOT(IWORD) + WINDEX(IWORD)
        DM(IWORD) = WINDEX(IWORD)
100   CONTINUE
      IF(NFUZ.EQ.0) RETURN
      DO 200 IWORD = 1,MAXPOS
C  ONLY ONE POSITION MUST CHANGE, CHANGE THEM ALL IN TURN
C  IF THIS WORD EXISTS IN SEQ
        KT = WINDEX(IWORD)
        IF(KT.NE.0)THEN
          CALL DCODEI(IWORD,LENGTH,STRING)
          DO 10 I1 = 1,LENGTH
            DO 5 I2 = 1,3
              IT = STRING(I1)
              NC1 = DIF(I2,STRING(I1))
              STRING(I1) = NC1
              JWORD = NCODE(STRING(1),LENGTH)
              DM(JWORD) = DM(JWORD) + KT
              STRING(I1) = IT
5           CONTINUE
10        CONTINUE
        END IF
200   CONTINUE
      IF(NFUZ.EQ.1)RETURN
        DO 300 IWORD = 1,MAXPOS
C  TWO POSITIONS MUST CHANGE, CHANGE THEM ALL IN TURN
          KT = WINDEX(IWORD)
          IF(KT.NE.0)THEN
            CALL DCODEI(IWORD,LENGTH,STRING)
            DO 40 I1 = 1,LENGTH
              DO 30 I2 = I1+1,LENGTH
                DO 25 I3 = 1,3
                  IT1 = STRING(I1)
                  NC1 = DIF(I3,STRING(I1))
                  STRING(I1) = NC1
                  DO 24 I4 = 1,3
                    IT2 = STRING(I2)
                    NC2 = DIF(I4,STRING(I2))
                    STRING(I2) = NC2
                    JWORD = NCODE(STRING(1),LENGTH)
                    DM(JWORD) = DM(JWORD) + KT
                    STRING(I2) = IT2
24                CONTINUE
                  STRING(I1) = IT1
25              CONTINUE
30            CONTINUE
40          CONTINUE
          END IF
300     CONTINUE
        RETURN
C      END IF
      END
      SUBROUTINE MAKEDH(TMPDIC,DICTOT,LENGTH,NFUZ,MAXPOS)
      INTEGER STRING(8),TMPDIC(MAXPOS),DICTOT(MAXPOS)
      INTEGER GETV
      EXTERNAL GETV
      DO 100 IWORD = 1,MAXPOS
        TMPDIC(IWORD) = DICTOT(IWORD)
100   CONTINUE
      IF(NFUZ.EQ.0) RETURN
      DO 200 IWORD = 1,MAXPOS
C  ONLY ONE POSITION MUST CHANGE, CHANGE THEM ALL IN TURN
C  IF THIS WORD EXISTS IN SEQ
        IF(DICTOT(IWORD).NE.0)THEN
          CALL DCODEI(IWORD,LENGTH,STRING)
          IV = GETV(IWORD,LENGTH,STRING,DICTOT,MAXPOS,NFUZ)
          IF(IV.GT.TMPDIC(IWORD))TMPDIC(IWORD) = 0
        END IF
200   CONTINUE
      IF(NFUZ.EQ.1)RETURN
      DO 300 IWORD = 1,MAXPOS
C  TWO POSITIONS MUST CHANGE, CHANGE THEM ALL IN TURN
        IF(DICTOT(IWORD).NE.0)THEN
          CALL DCODEI(IWORD,LENGTH,STRING)
          IV = GETV(IWORD,LENGTH,STRING,DICTOT,MAXPOS,NFUZ)
          IF(IV.GT.TMPDIC(IWORD))TMPDIC(IWORD) = 0
        END IF
300   CONTINUE
      RETURN
      END
      INTEGER FUNCTION GETV(IWORD,LENGTH,STRING,DICTOT,MAXPOS,NFUZ)
      INTEGER DICTOT(MAXPOS),STRING(8)
      INTEGER DIF(3,4)
      EXTERNAL NCODE
      SAVE DIF
      DATA DIF/2,3,4,
     +         1,3,4,
     +         1,2,4,
     +         1,2,3/
      IV = DICTOT(IWORD)
      DO 10 I1 = 1,LENGTH
        DO 5 I2 = 1,3
          IT = STRING(I1)
          NC1 = DIF(I2,STRING(I1))
          STRING(I1) = NC1
          JWORD = NCODE(STRING(1),LENGTH)
          IV = MAX(IV,DICTOT(JWORD))
          STRING(I1) = IT
5       CONTINUE
10    CONTINUE
      IF(NFUZ.EQ.1)THEN
        GETV = IV
        RETURN
      END IF
      DO 40 I1 = 1,LENGTH
        DO 30 I2 = I1+1,LENGTH
          DO 25 I3 = 1,3
            IT1 = STRING(I1)
            NC1 = DIF(I3,STRING(I1))
            STRING(I1) = NC1
            DO 24 I4 = 1,3
              IT2 = STRING(I2)
              NC2 = DIF(I4,STRING(I2))
              STRING(I2) = NC2
              JWORD = NCODE(STRING(1),LENGTH)
              IV = MAX(IV,DICTOT(JWORD))
              STRING(I2) = IT2
24          CONTINUE
            STRING(I1) = IT1
25        CONTINUE
30      CONTINUE
40    CONTINUE
      GETV = IV
      END
      SUBROUTINE WDCODE(WT,STRING,LENGTH,IADD)
      INTEGER WT(4,LENGTH),STRING(LENGTH)
      DO 10 I = 1,LENGTH
        WT(STRING(I),I) = WT(STRING(I),I) + IADD
10    CONTINUE
      END
      REAL FUNCTION RINF(WT,LENGTH,COMP)
      INTEGER WT(4,LENGTH)
      REAL COMP(5)
      HSS = 0.
      T = 0.
      DO 1 I = 1,4
        T = T + WT(I,1)
1     CONTINUE
      DO 10 I = 1,LENGTH
        DO 5 J = 1,4
          HS = 0.
          Y = COMP(J)
          X = REAL(WT(J,I))/T
          HS = X * LOG(X/Y)
          HSS = HSS + HS
5       CONTINUE
10    CONTINUE
      RINF = HSS/REAL(LENGTH)
      END
      SUBROUTINE DCODEI(A,LENGTH,STRING)
      INTEGER A,CONSTS(8),STRING(LENGTH)
      SAVE CONSTS
      DATA CONSTS/1,4,16,64,256,1024,4096,16384/
      K = 0
      J = A
      DO 10 I = LENGTH,1,-1
        J1 = (J-1) / CONSTS(I)
        K = K + 1
        STRING(K) = J1 + 1
        J = J - J1*CONSTS(I)
10    CONTINUE
      END
      SUBROUTINE WRTWMT(WEIGHT,IWT,MAXCHR,LENGTH,IDEV,IFLAG)
      REAL WEIGHT(MAXCHR,LENGTH)
      INTEGER IWT(MAXCHR,LENGTH)
      CHARACTER STRING*8
      CHARACTER WTMCON
      EXTERNAL WTMCON
      DO 10 I = 1,MAXCHR
        IF(IFLAG.EQ.0)
     +  WRITE(IDEV,1000,ERR=100)(WEIGHT(I,J),J=1,LENGTH)
        IF(IFLAG.EQ.1)
     +  WRITE(IDEV,1001,ERR=100)(IWT(I,J),J=1,LENGTH)
10    CONTINUE
1000  FORMAT(' ',20F5.2)
1001  FORMAT(' ',20I6)
100   CONTINUE
      STRING = ' '
      DO 20 I = 1,LENGTH
        STRING(I:I) = WTMCON(IWT,LENGTH,MAXCHR,I)
20    CONTINUE
      WRITE(IDEV,1002)STRING
1002  FORMAT(' ',A)
      END
      CHARACTER*1 FUNCTION WTMCON(IWT,LENGTH,MAXCHR,IP)
      INTEGER IWT(MAXCHR,LENGTH),C(5)
      CHARACTER STRING(5,5)
      SAVE STRING
      DATA STRING/
     +'T','Y','W','K','T',
     +'Y','C','M','S','C',
     +'W','M','A','R','A',
     +'K','S','R','G','G',
     +'T','C','A','G','-'/            
      N = 0
      DO 10 J = 1,MAXCHR
        I = IWT(J,IP)
        IF (I.GT.N) N = I
10    CONTINUE
      NC = 0
      DO 20 J = 1,MAXCHR
        C(J) = 0
        I = IWT(J,IP)
        IF (I.EQ.N) THEN
          M = J
          NC = NC + 1
          C(NC) = J
        END IF
20    CONTINUE
      IF(NC.EQ.1)THEN
        J = C(1)
        WTMCON = STRING(J,J)
        RETURN
      END IF
      IF(NC.EQ.2)THEN
        J = C(1)
        I = C(2)
        WTMCON = STRING(I,J)
        RETURN
      END IF
      IF(NC.EQ.3)THEN
        J = 0
        DO 30 I = 1,3
          J = J + C(I)
30      CONTINUE
        IF(J.EQ.6) WTMCON = 'H'
        IF(J.EQ.7) WTMCON = 'B'
        IF(J.EQ.8) WTMCON = 'D'
        IF(J.EQ.9) WTMCON = 'V'
        RETURN
      END IF
      WTMCON = 'N'
      END
      CHARACTER*(*) FUNCTION DCODE(A,LENGTH)
      IMPLICIT INTEGER (A-Z)
      DIMENSION CONSTS(8)
      CHARACTER BASE(5)
      SAVE CONSTS,BASE
      DATA BASE/'T','C','A','G','-'/
      DATA CONSTS/1,4,16,64,256,1024,4096,16384/
      K = 0
      J = A
      DCODE = ' '
      DO 10 I = LENGTH,1,-1
        J1 = (J-1) / CONSTS(I)
        K = K + 1
        DCODE(K:K) = BASE(J1+1)
        J = J - J1*CONSTS(I)
10    CONTINUE
      END
      SUBROUTINE GETCOM(SEQN,IDIM,COMP)
      INTEGER SEQN(IDIM)
      REAL COMP(5)
C NB ADD 1 TO EACH BASE FOR LATER
      CALL FILLR(COMP,5,1.)
      DO 10 I = 1,IDIM
        J = SEQN(I)
        COMP(J) = COMP(J) + 1.
10    CONTINUE
      X = REAL(IDIM) + 4.
      DO 20 I = 1,4
        COMP(I) = COMP(I)/X
20    CONTINUE
      END
      SUBROUTINE TCOMP(COMP,KCOMP)
      REAL COMP(5)
      IF(KCOMP.EQ.1)THEN
        T1 = COMP(1)
        COMP(1) = COMP(3)
        COMP(3) = T1
        T1 = COMP(2)
        COMP(2) = COMP(4)
        COMP(4) = T1
      ELSE IF (KCOMP.EQ.2)THEN
        T1 = COMP(1)
        T2 = COMP(3)
        T1 = (T1 + T2) / 2.
        COMP(1) = T1
        COMP(3) = T1
        T1 = COMP(2)
        T2 = COMP(4)
        T1 = (T1 + T2) / 2.
        COMP(2) = T1
        COMP(4) = T1
      END IF
      END
      SUBROUTINE WRTDIC(IDEV,FILNAM,DIC,MAXPOS,IOK)
      INTEGER DIC(MAXPOS)
      CHARACTER FILNAM*(*)
      CALL OPENRS(IDEV,FILNAM,IOK,LRECL,6)
      IF(IOK.NE.0)RETURN
      WRITE(IDEV)DIC
      CLOSE(UNIT=IDEV)
      END
      SUBROUTINE RDDIC(IDEV,FILNAM,DIC,MAXPOS,IOK)
      INTEGER DIC(MAXPOS)
      CHARACTER FILNAM*(*)
      CALL OPENRS(IDEV,FILNAM,IOK,LRECL,7)
      IF(IOK.NE.0)RETURN
      READ(IDEV)DIC
      CLOSE(UNIT=IDEV)
      END
      SUBROUTINE RDIC(IDEV,FILNAM,DIC,MAXPOS,KBIN,KBOUT,IOK,
     +IHELPS,IHELPE,HELPF,IDEVH)
      INTEGER DIC(MAXPOS)
      CHARACTER FILNAM*(*),HELPF*(*)
30    CONTINUE
      L = 0
      CALL GTSTR(
     +'Name of dictionary file',' ',FILNAM,L,KBOUT,KBIN,INFLAG)
      IF(INFLAG.EQ.1) THEN
        CALL HELP2(IHELPS,IHELPE,HELPF,IDEVH,KBIN,KBOUT)
        GO TO 30
      END IF
      IF(INFLAG.NE.0) RETURN
      CALL RDDIC(IDEV,FILNAM,DIC,MAXPOS,IOK)
      END
      SUBROUTINE WDIC(IDEV,FILNAM,DIC,MAXPOS,KBIN,KBOUT,IOK,
     +IHELPS,IHELPE,HELPF,IDEVH)
      INTEGER DIC(MAXPOS)
      CHARACTER FILNAM*(*),HELPF*(*)
30    CONTINUE
      L = 0
      CALL GTSTR(
     +'Name for dictionary file',' ',FILNAM,L,KBOUT,KBIN,INFLAG)
      IF(INFLAG.EQ.1) THEN
        CALL HELP2(IHELPS,IHELPE,HELPF,IDEVH,KBIN,KBOUT)
        GO TO 30
      END IF
      IF(INFLAG.NE.0) RETURN
      CALL WRTDIC(IDEV,FILNAM,DIC,MAXPOS,IOK)
      END
      SUBROUTINE DICIO(KBIN,KBOUT,NOPT,IHELPS,IHELPE,HELPF,IDEVH)
      CHARACTER HELPF*(*)
      PARAMETER (MAXPRM = 10)
      CHARACTER PROMPT(8)*(MAXPRM)
      WRITE(KBOUT,*)'Save or restore a dictionary'
      NOPT = 0
      IB = 1
      PROMPT(1) = 'Save Dw'
      PROMPT(2) = 'Save Ds'
      PROMPT(3) = 'Save Dm'
      PROMPT(4) = 'Save Dh'
      PROMPT(5) = 'Restore Dw'
      PROMPT(6) = 'Restore Ds'
      PROMPT(7) = 'Restore Dm'
      PROMPT(8) = 'Restore Dh'
      CALL RADION('Select save or restore mode',PROMPT,8,IB,
     +IHELPS,IHELPE,HELPF,IDEVH,KBIN,KBOUT)
      IF(IB.LT.1) RETURN
      NOPT = IB
      END
      SUBROUTINE SHLSRT(KEY,N,IPOINT,INCDEC)
C     SHELL SORT
C     REFERENCES:  D.L. SHELL, CACM 2, 32 (JULY 1959)
C                  D.E. KNUTH, TAOCP III, SECT. 5.2.1
C     CALLING SEQUENCE:
C
C     KEY    IS AN ARRAY OF KEYS ON WHICH TO SORT
C     N      IS THE NUMBER OF ITEMS
C     IPOINT IS THE ARRAY OF POINTERS
C            (ONLY THE POINTERS WILL MOVE)
C     INCDEC .GE. 0 FOR SORTING INTO INCREASING ORDER;
C            .LT. 0 FOR SORTING INTO DECREASING ORDER
C
      INTEGER KEY(N),IPOINT(N),K
      INTEGER H,S,T
      IPOINT(1) = 1
      IF(N.LT.2)RETURN
C     INITIALIZE POINTER ARRAY
      DO 2 I = 2,N
        IPOINT(I) = I
    2 CONTINUE
C
C     CHOICE OF SEQUENCE OF INCREMENTS SUGGESTED
C     BY KNUTH III, EQ. 8, P. 95.   HIS FORMULA
C     IS EQUIVALENT TO:
C
C            H(S) = (3**S - 1)/2
C            INITIAL VALUE OF S IS MINIMAL INTEGER
C              SUCH THAT H(S+2) .GE. N
C
C
C     SMAX = (ALOG(2N + 1)/ALOG(3)) - 2 + 1
      S = INT( (ALOG(FLOAT(2*N+1))/1.09861229) - 0.95 )
      S = MAX(S,1)
      H = (3**S - 1)/2
      DO 7 T = 1,S
        JMIN = H + 1
        DO 6 J = JMIN,N
          I = J - H
          JJ = IPOINT(J)
          K = KEY(JJ)
          IPT = IPOINT(J)
   3      CONTINUE
          II = IPOINT(I)
          IF((K-KEY(II)).LE.0)THEN
            IPLUSH = I + H
            IPOINT(IPLUSH) = IPOINT(I)
            I = I - H
            IF(I.GT.0) GO TO 3
          END IF
          IPLUSH = I + H
          IPOINT(IPLUSH) = IPT
    6   CONTINUE
C     CHANGE INCREMENT
        IF(H.LT.2) GO TO 8
        H = (H-1)/3
7     CONTINUE
C      CHECK INCDEC: IF NEGATIVE, SWITCH POINTER ARRAY
8     CONTINUE
      IF(INCDEC.LT.0)THEN
        M = N/2
        NP1MI = N
        DO 10 I = 1,M
          NTEMP = IPOINT(I)
          IPOINT(I) = IPOINT(NP1MI)
          IPOINT(NP1MI) = NTEMP
          NP1MI = NP1MI - 1
  10    CONTINUE
      END IF
      END
      SUBROUTINE MAKEDS(SEQ,MAXCHR,DS,DT,MAXPOS,
     +LENSEQ,MAXSEQ,NFILE,LENGTH,KCOMP,I1,I2,TEMP,MASK,LMASK)
      INTEGER SEQ(MAXCHR),DS(MAXPOS),DT(MAXPOS)
      INTEGER LENSEQ(MAXSEQ),CCODEM,TEMP(MAXCHR)
      LOGICAL MASK(LMASK)
      EXTERNAL NCODEM,CCODEM
      CALL FILLI(DS,MAXPOS,0)
      CALL FILLI(DT,MAXPOS,0)
      ITEMP = 0
      IF((KCOMP.EQ.0).OR.(KCOMP.EQ.2))THEN
        DO 10 I = 1,NFILE-1
          I11 = LENSEQ(I) + I1 - 1
          I22 = MIN(LENSEQ(I) + I2 - 1, LENSEQ(I+1)) - LMASK
          IF(ITEMP.GT.0) THEN
            DO 4 J = 1,ITEMP
              DT(TEMP(J)) = 0
4           CONTINUE
            ITEMP = 0
          END IF
          DO 5 J = I11,I22
            N = NCODEM(SEQ(J),LENGTH,MASK,LMASK)
            IF(N.NE.0)THEN
              IF(DT(N).EQ.0) THEN
                DT(N) = 1
                ITEMP = ITEMP + 1
                TEMP(ITEMP) = N
                DS(N) = DS(N) + 1
              END IF
            END IF
5         CONTINUE
10      CONTINUE
      END IF
      IF((KCOMP.EQ.1).OR.(KCOMP.EQ.2)) THEN
        DO 20 I = 1,NFILE-1
          I11 = LENSEQ(I) + I1 - 1
          I22 = MIN(LENSEQ(I) + I2 - 1, LENSEQ(I+1)) - LMASK
          IF(ITEMP.GT.0) THEN
            DO 14 J = 1,ITEMP
              DT(TEMP(J)) = 0
14           CONTINUE
            ITEMP = 0
          END IF
          DO 15 J = I11,I22
            N = CCODEM(SEQ(J),LENGTH,MASK,LMASK)
            IF(N.NE.0)THEN
              IF(DT(N).EQ.0) THEN
                DT(N) = 1
                ITEMP = ITEMP + 1
                TEMP(ITEMP) = N
                DS(N) = DS(N) + 1
              END IF
            END IF
15        CONTINUE
20      CONTINUE
      END IF
      END
      SUBROUTINE OUTP(TEMPI,TEMPC,TEMPP,TEMPN,MLIS,LENGTH,IDEV,
     +MASK,LMASK)
      INTEGER TEMPI(MLIS),TEMPC(MLIS),TEMPP(MLIS),TEMPN(MLIS)
      CHARACTER*8 STRING,DCODE
      CHARACTER UNMASK*25,SOUT*25
      LOGICAL MASK(LMASK)
      EXTERNAL DCODE,UMASK
      DO 100 I = 1,MLIS
        N = TEMPN(I)
        NW = TEMPP(N)
        STRING = DCODE(NW,LENGTH)
        SOUT = UNMASK(STRING,MASK,LMASK)
        WRITE(IDEV,1000)SOUT(1:LMASK),TEMPC(N),REAL(TEMPI(N))/10000.
1000    FORMAT(' ',A,' ',I6,F10.5)
100   CONTINUE
      END
      SUBROUTINE HAIRPD(IS,IN,KBIN,KBOUT,
     +MINLPI,MAXLPI,MINLP,MINLPX,MAXLPX,MAXLP,MINBP,MAXBP,MININ,
     +IGON,IOK,
     +IHELPS,IHELPE,HELPF,IDEVH)
      CHARACTER HELPF*(*)
C   AUTHOR: RODGER STADEN
      WRITE(KBOUT,1004)
1004  FORMAT(' Define the range of loop sizes')
      CALL GETINT(MINLPI,MAXLPI,MINLP,'Minimum loop size',
     +IVAL,KBIN,KBOUT,IHELPS,IHELPE,HELPF,IDEVH,IOK)
      IF(IOK.NE.0) RETURN
      MINLP = IVAL
      MINLPX = MAX(MINLPX,MINLP)
      MAXLP = MINLPX
      CALL GETINT(MINLPX,MAXLPX,MAXLP,'Maximum loop size',
     +IVAL,KBIN,KBOUT,IHELPS,IHELPE,HELPF,IDEVH,IOK)
      IF(IOK.NE.0) RETURN
      MAXLP = IVAL
      MINLP = MINLP + 1
      MAXLP = MAXLP + 1
      CALL GETINT(MINBP,MAXBP,MININ,'Minimum number of basepairs',
     +IVAL,KBIN,KBOUT,IHELPS,IHELPE,HELPF,IDEVH,IOK)
      IF(IOK.NE.0) RETURN
      MININ = IVAL
      CALL YESONO(IGON,'Plot results','List results',
     +IHELPS,IHELPE,HELPF,IDEVH,KBIN,KBOUT)
C      IF(IGON.LT.0)RETURN
      END
C   lwrap2
      INTEGER FUNCTION LWRAP2(IDIM,I)
C   AUTHOR: RODGER STADEN
C   test for end of array,ifso wrap around
      LWRAP2=I
      IF(LWRAP2.LT.1)LWRAP2=LWRAP2+IDIM+1
      IF(LWRAP2.GT.IDIM)LWRAP2=LWRAP2-IDIM
      END
      SUBROUTINE LISTN(SEQ,MAXCHR,LENSEQ,NFILE,LMAX,LINLEN,I1,I2,
     +IDEV,KBOUT)
      CHARACTER SEQ(MAXCHR)
      INTEGER LENSEQ(NFILE)
C     SET WIDTH FOR LAST PAGE
      LPAGE=MOD(I2-I1+1,LINLEN)
C     HOW MANY PAGE WIDTHS?
      NPAGE=1+(I2-I1+1)/LINLEN
      IF(MOD(I2-I1+1,LINLEN).EQ.0)THEN
        NPAGE=NPAGE-1
        LPAGE=LINLEN
      END IF
      ISTART=I1-LINLEN
      DO 50 I=1,NPAGE
        ISTART=ISTART+LINLEN
        IF(I.EQ.NPAGE)LINLEN=LPAGE
        WRITE(IDEV,1006)(K,K=ISTART+9,ISTART+LINLEN-1,10)
        DO 40 J=1,NFILE-1
          KF = ISTART + LENSEQ(J) - 1
          KT = MIN(KF+LINLEN,LENSEQ(J+1)) - 1
          WRITE(IDEV,1003,ERR=60)J,(SEQ(K),K=KF,KT)
1003      FORMAT(' ',I4,'  ',100A1)
1006      FORMAT('       ',10I10)
40      CONTINUE
        WRITE(IDEV,1008)
1008    FORMAT(/)
50    CONTINUE
      RETURN
60    CONTINUE
      WRITE(KBOUT,*)' Error writing file'
      END
      SUBROUTINE GMASK(KBIN,KBOUT,MASK,LMASK1,LENGTH,
     +IHELPS,IHELPE,HELPF,IDEVH)
      CHARACTER CMASK*25,HELPF*(*),TUPPER
      LOGICAL MASK(LMASK1)
      EXTERNAL TUPPER
 1    CONTINUE
      LMASK = LMASK1
      DO 2 I=1,LMASK
        MASK(I) = .TRUE.
 2    CONTINUE
      LMASK = LENGTH
5     CONTINUE
        WRITE(KBOUT,1002)
1002    FORMAT(' x means use, - means ignore',/,
     +         ' e.g. xx-x---x-x means use positions 1,2,4,8,10')
        LIN = 0
        CALL GTSTR('Mask',' ',CMASK,LIN,KBOUT,KBIN,INFLAG)
        IF(INFLAG.EQ.1) THEN
          CALL HELP2(IHELPS,IHELPE,HELPF,IDEVH,KBIN,KBOUT)
          GO TO 5
        END IF
        IF(INFLAG.EQ.2) RETURN
        IF(LIN.EQ.0) RETURN
        LMASK = LIN
        LENGTH = 0
        DO 70 I=1,LIN
          IF(TUPPER(CMASK(I:I)).EQ.'X') THEN
            LENGTH = LENGTH + 1
            MASK(I) = .TRUE.
          ELSE
            MASK(I) = .FALSE.
          END IF
70      CONTINUE
      IF (LENGTH.GT.8) THEN
        WRITE(KBOUT,*)'More than 8 active bases'
        GO TO 1
      END IF
      LMASK1 = LMASK
      END
      CHARACTER*(*) FUNCTION UNMASK(STRING,MASK,LMASK)
      CHARACTER STRING*(*)
      LOGICAL MASK(LMASK)
      UNMASK = ' '
      DO 5 I=1,LMASK
        UNMASK(I:I) = '-'
 5    CONTINUE
      J = 0
      DO 10 I=1,LMASK
        IF (MASK(I)) THEN
          J  = J + 1
          UNMASK(I:I) = STRING(J:J)
        END IF
 10   CONTINUE
      END
      CHARACTER*(*) FUNCTION MMASK(STRING,MASK,LMASK,J)
      CHARACTER STRING*(*)
      LOGICAL MASK(LMASK)
      J = 0
      DO 10 I=1,LMASK
        IF (MASK(I)) THEN
          J  = J + 1
          MMASK(J:J) = STRING(I:I)
        END IF
 10   CONTINUE
      END
