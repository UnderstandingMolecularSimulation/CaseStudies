**==gauss.spg  processed by SPAG 4.52O  at 11:03 on 18 Oct 1996
 
      SUBROUTINE GAUSS(Sigma, L0, L)
c     generates l normally distributed with variance sigma
c     and  mean l0
c     Algorithm based on "gasdev" from 'Numerical recipes'
c     pg. 203.
 
      IMPLICIT NONE
      INTEGER iset
      DOUBLE PRECISION Sigma, L0, L, RANF, gset, fac, v1, v2, r
      SAVE gset, iset
      DATA iset/0/
 100  IF (iset.EQ.0) THEN
         v1 = 2*RANF() - 1
         v2 = 2*RANF() - 1
         r = v1**2 + v2**2
         IF (r.GE.1) GOTO 100
         fac = SQRT(-2*LOG(r)/r)
         gset = v1*fac
         L = L0 + Sigma*v2*fac
         iset = 1
      ELSE
         L = L0 + Sigma*gset
         iset = 0
      END IF
      RETURN
      END
