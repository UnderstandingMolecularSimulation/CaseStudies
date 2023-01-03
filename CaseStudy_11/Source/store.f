**==store.spg  processed by SPAG 4.52O  at 11:24 on 20 Jun 1996
 
 
      SUBROUTINE STORE(Iout)
c     writes configuration to disk
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
      WRITE (Iout, *) S, PS
      REWIND (Iout)
      RETURN
      END
