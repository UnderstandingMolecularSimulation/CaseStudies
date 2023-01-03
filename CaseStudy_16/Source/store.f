**==store.spg  processed by SPAG 4.52O  at 18:10 on 19 Jul 1996
      SUBROUTINE STORE(Iout, Dr, Vmax)
c     writes configuration to disk
      IMPLICIT NONE
      INCLUDE 'parameter.inc'
      INCLUDE 'conf.inc'
      INCLUDE 'system.inc'
      INTEGER Iout, i
      DOUBLE PRECISION Dr, Vmax
 
      WRITE (Iout, *) BOX(1), HBOX(1), BOX(2), HBOX(2)
      WRITE (Iout, *) NPART, NPBOX(1), NPBOX(2)
      WRITE (Iout, *) Dr, Vmax
      DO i = 1, NPART
         WRITE (Iout, *) X(i), Y(i), Z(i), ID(i)
      END DO
      REWIND (Iout)
      RETURN
      END
