**==store.spg  processed by SPAG 4.52O  at 15:46 on 28 Mar 1996
      SUBROUTINE STORE(Iout)
c
c     writes configuration to disk
c
c  Iout (input) file number
c
      IMPLICIT NONE
      INCLUDE 'parameter.inc'
      INCLUDE 'conf.inc'
      INCLUDE 'veloc.inc'
      INCLUDE 'system.inc'
      INTEGER Iout, i
 
      WRITE (Iout, *) BOX, HBOX
      WRITE (Iout, *) NPART
      DO i = 1, NPART
         WRITE (Iout, *) X(i), Y(i), Z(i), VX(i), VY(i), VZ(i)
      END DO
      REWIND (Iout)
      RETURN
      END
