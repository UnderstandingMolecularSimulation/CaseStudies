**==store.spg  processed by SPAG 4.52O  at 10:35 on 12 Jul 1996
      SUBROUTINE STORE(Iout, Dr)
c     writes configuration to disk
      IMPLICIT NONE
      INCLUDE 'parameter.inc'
      INCLUDE 'conf.inc'
      INCLUDE 'system.inc'
      INTEGER Iout, i
      DOUBLE PRECISION Dr
 
      WRITE (Iout, *) BOX, HBOX
      WRITE (Iout, *) NPART
      WRITE (Iout, *) Dr
      DO i = 1, NPART
         WRITE (Iout, *) X(i), Y(i), Z(i)
      END DO
      REWIND (Iout)
      RETURN
      END
