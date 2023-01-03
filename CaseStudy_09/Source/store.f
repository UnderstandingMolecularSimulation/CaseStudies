**==store.spg  processed by SPAG 4.52O  at 18:49 on  6 Jun 1996
      SUBROUTINE STORE(Iout, Dr)
c
c     writes configuration to disk
c
      IMPLICIT NONE
      INCLUDE 'parameter.inc'
      INCLUDE 'conf.inc'
      INCLUDE 'system.inc'
      INTEGER Iout, i
      DOUBLE PRECISION Dr
 
      WRITE (Iout, *) BOX
      WRITE (Iout, *) NPART
      WRITE (Iout, *) Dr
      DO i = 1, NPART
         WRITE (Iout, *) X(i), Y(i), Z(i)
      END DO
      REWIND (Iout)
      RETURN
      END
