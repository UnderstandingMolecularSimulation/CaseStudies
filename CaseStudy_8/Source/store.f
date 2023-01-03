**==store.spg  processed by SPAG 4.52O  at 10:45 on  5 Jun 1996
      SUBROUTINE STORE(Iout, Dr, Vmax)
c     writes configuration to disk
      IMPLICIT NONE
      INCLUDE 'parameter.inc'
      INCLUDE 'conf.inc'
      INCLUDE 'system.inc'
      INTEGER Iout, i
      DOUBLE PRECISION Dr, Vmax
 
      WRITE (Iout, *) BOX, HBOX
      WRITE (Iout, *) NPART
      WRITE (Iout, *) Dr, Vmax
      DO i = 1, NPART
         WRITE (Iout, *) X(i), Y(i), Z(i)
      END DO
      REWIND (Iout)
      RETURN
      END
