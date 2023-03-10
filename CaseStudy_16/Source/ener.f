**==ener.spg  processed by SPAG 4.52O  at 18:10 on 19 Jul 1996
      SUBROUTINE ENER(En, Vir, R2, Ib)
c  ---calculates energy (en) and virial (vir) for given
c     distance squared between (r2) two particles
 
      IMPLICIT NONE
      DOUBLE PRECISION R2, r2i, r6i, En, Vir
      INTEGER Ib
      INCLUDE 'potential.inc'
 
      IF (R2.LE.RC2(Ib)) THEN
         r2i = SIG2/R2
         r6i = r2i*r2i*r2i
         IF (SHIFT) THEN
            En = EPS4*(r6i*r6i-r6i) - ECUT(Ib)
         ELSE
            En = EPS4*(r6i*r6i-r6i)
         END IF
         Vir = EPS48*(r6i*r6i-0.5D0*r6i)
      ELSE
         En = 0.D0
         Vir = 0.D0
      END IF
      RETURN
      END
