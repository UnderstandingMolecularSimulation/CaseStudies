**==vib.spg  processed by SPAG 4.52O  at 11:03 on 18 Oct 1996
      SUBROUTINE VIB(Uvib, Vir, L)
c     ---bond vibration potential
      IMPLICIT NONE
      DOUBLE PRECISION Uvib, L, Vir
      INCLUDE 'par.inc'
      Uvib = 0.5D0*KV*(L-L0)**2
      Vir = -KV*(L-L0)*L
      RETURN
      END
