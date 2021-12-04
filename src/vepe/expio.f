C routines to read and write data to the experiment files
C rexgel read gel
C rexcvn read cloning vector name
C rexsvn read sequencing vector name
C rexsvc read cloning site
C rexsvp read primer site
C rexpdp read poor data positions
C rexsvp read sequencing vector positions in reading
C wexsvp write sequencing vector positions
C wexcvp write cloning vector positions
C
C
C
      INTEGER FUNCTION REXGEL(IDEV,GEL,LENGTH)
      CHARACTER GEL(LENGTH)
      INTEGER EXPRSA
      EXTERNAL EXPRSA
C rexgel read gel
      JOB = 22
      REXGEL = EXPRSA(IDEV,JOB,GEL,LENGTH)
      JOB = LENGTH
      DO 10 I=1,JOB
        IF(GEL(I).EQ.' ') THEN
          LENGTH = I-1
          RETURN
        END IF
 10     CONTINUE
      END
      INTEGER FUNCTION REXCVN(IDEV,NAME)
      CHARACTER NAME*(*)
      INTEGER EXPRS
      EXTERNAL EXPRS
C rexcvn read cloning vector name
      JOB = 3
      REXCVN = EXPRS(IDEV,JOB,NAME)
      END
      INTEGER FUNCTION REXCVF(IDEV,NAME)
      CHARACTER NAME*(*)
      INTEGER EXPRS
      EXTERNAL EXPRS
C rexcvf read cloning vector file name
      JOB = 0
      REXCVF = EXPRS(IDEV,JOB,NAME)
      END
      INTEGER FUNCTION REXSVF(IDEV,NAME)
      CHARACTER NAME*(*)
      INTEGER EXPRS
      EXTERNAL EXPRS
C rexsvf read sequencing vector file name
      JOB = 18
      REXSVF = EXPRS(IDEV,JOB,NAME)
      END
      INTEGER FUNCTION REXSVN(IDEV,NAME)
      CHARACTER NAME*(*)
      INTEGER EXPRS
      EXTERNAL EXPRS
C rexsvn read sequencing vector name
      JOB = 25
      REXSVN = EXPRS(IDEV,JOB,NAME)
      END
      INTEGER FUNCTION REXSVC(IDEV,ICSITE)
      INTEGER EXPRI
      EXTERNAL EXPRI
C rexsvc read cloning site
      JOB = 17
      REXSVC = EXPRI(IDEV,JOB,ICSITE)
      IF(REXSVC.NE.0) ICSITE = 0
      END
      INTEGER FUNCTION REXSVQ(IDEV,IPSITE)
      INTEGER EXPRI
      EXTERNAL EXPRI
C rexsvq read primer site
      JOB = 21
      REXSVQ = EXPRI(IDEV,JOB,IPSITE)
      IF(REXSVQ.NE.0) IPSITE = 0
      END
      INTEGER FUNCTION REXPDP(IDEV,LEFT,IRIGHT)
      INTEGER EXPRI
      EXTERNAL EXPRI
C rexpdp read poor data positions
      LEFT = 0
      JOB = 16
      REXPDP = EXPRI(IDEV,JOB,IRIGHT)
      IF(REXPDP.NE.0) THEN
        IRIGHT = 0
      END IF
      JOB = 27
      REXPDP = EXPRI(IDEV,JOB,LEFT)
      IF(REXPDP.NE.0) THEN
        LEFT = 0
      END IF
      END
      INTEGER FUNCTION REXSVP(IDEV,LEFT,IRIGHT)
      INTEGER EXPRI
      EXTERNAL EXPRI
C rexsvp read sequencing vector positions
      JOB = 20
      REXSVP = EXPRI(IDEV,JOB,LEFT)
      IF(REXSVP.NE.0) THEN
        LEFT = 0
      END IF
      JOB = 23
      REXSVP = EXPRI(IDEV,JOB,IRIGHT)
      IF(REXSVP.NE.0) THEN
        IRIGHT = 0
      END IF
      END
      INTEGER FUNCTION REXCVP(IDEV,LEFT,IRIGHT)
      INTEGER EXPRR
      EXTERNAL EXPRR
C rexcvp read cloning vector positions
      JOB = 2
      REXCVP = EXPRR(IDEV,JOB,LEFT,IRIGHT)
      IF(REXCVP.NE.0) THEN
        LEFT = 0
        IRIGHT = 0
      END IF
      END
      INTEGER FUNCTION WEXCVP(IDEV,LEFT,IRIGHT)
      INTEGER EXPWR
      EXTERNAL EXPWR
C rexpst write cloning vector positions
      JOB = 2
      WEXCVP = EXPWR(IDEV,JOB,LEFT,IRIGHT)
      END
      INTEGER FUNCTION WEXSVP(IDEV,LEFT,IRIGHT)
      INTEGER EXPWI
      EXTERNAL EXPWI
C rexpst write sequencing vector positions
      JOB = 20
      WEXSVP = EXPWI(IDEV,JOB,LEFT)
      IF(WEXSVP.NE.0) RETURN
      JOB = 23
      WEXSVP = EXPWI(IDEV,JOB,IRIGHT)
      END
