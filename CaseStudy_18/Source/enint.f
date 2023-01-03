**==enint.spg  processed by SPAG 4.52O  at 11:03 on 18 Oct 1996
      SUBROUTINE ENINT(Xn, Yn, Zn, Evib, Virvib)
      IMPLICIT NONE
      INCLUDE 'particle.inc'
      INTEGER j
      DOUBLE PRECISION uvib, r, dx, dy, dz, Evib, Virvib, vir
      DOUBLE PRECISION Xn(*), Yn(*), Zn(*)
 
      Evib = 0
      Virvib = 0
      DO j = 2, ELL
c     ---internal interactions:
         dx = Xn(j) - Xn(j-1)
         dy = Yn(j) - Yn(j-1)
         dz = Zn(j) - Zn(j-1)
         r = SQRT(dx*dx+dy*dy+dz*dz)
         CALL VIB(uvib, vir, r)
         Evib = Evib + uvib
         Virvib = Virvib + vir
      END DO
      RETURN
      END
