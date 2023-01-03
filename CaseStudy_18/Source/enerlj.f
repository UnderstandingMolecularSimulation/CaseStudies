**==enerlj.spg  processed by SPAG 4.52O  at 11:03 on 18 Oct 1996
      SUBROUTINE ENERLJ(En, Vir, R2)
c  ---calculates energy (en) and virial (vir) for given
c     distance squared between (r2) two particles
 
      IMPLICIT NONE
      DOUBLE PRECISION R2, r2i, r6i, En, Vir
      INCLUDE 'potential.inc'
 
      IF (R2.LE.RC2) THEN
         r2i = 1/R2
         r6i = r2i*r2i*r2i
         IF (SHIft) THEN
            En = 4*(r6i*r6i-r6i) - ECUT
         ELSE
            En = 4*(r6i*r6i-r6i)
         END IF
         Vir = 48*(r6i*r6i-0.5D0*r6i)
      ELSE
         En = 0.D0
         Vir = 0.D0
      END IF
      RETURN
      END
