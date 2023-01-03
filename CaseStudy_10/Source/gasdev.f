**==gasdev.spg  processed by SPAG 4.52O  at 09:34 on 20 Jun 1996
      DOUBLE PRECISION FUNCTION GASDEV(Sigma, Iseed)
      IMPLICIT NONE
c
c     normally distributed with variance sigma and mean zero
c
      DOUBLE PRECISION r, v1, v2, fac, gset, RANF
      DOUBLE PRECISION Sigma
      INTEGER iset, Iseed
 
      SAVE gset, iset
      DATA iset/0/
 100  IF (iset.EQ.0) THEN
         v1 = 2.D0*RANF(Iseed) - 1.D0
         v2 = 2.D0*RANF(Iseed) - 1.D0
         r = v1**2 + v2**2
         IF (r.GE.1) GOTO 100
         fac = SQRT(-2.D0*LOG(r)/r)
         gset = v1*fac
         GASDEV = v2*fac
         iset = 1
      ELSE
         GASDEV = gset
         iset = 0
      END IF
      GASDEV = GASDEV*Sigma
      RETURN
c-------------------------------------------------------c
      END
