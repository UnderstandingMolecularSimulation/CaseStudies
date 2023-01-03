**==store.spg  processed by SPAG 4.52O  at 11:03 on 18 Oct 1996
      SUBROUTINE STORE(Iout, Dr, Dv)
c     writes configuration to disk
      IMPLICIT NONE
      INCLUDE 'particle.inc'
      INCLUDE 'par.inc'
      INTEGER Iout, i, j
      DOUBLE PRECISION Dr, Dv
 
      WRITE (Iout, *) BOX, HBOX
      WRITE (Iout, *) NPART, ELL
      WRITE (Iout, *) Dr, Dv
      DO i = 1, NPART
         DO j = 1, ELL
            WRITE (Iout, *) X(i, j), Y(i, j), Z(i, j)
         END DO
      END DO
      REWIND (Iout)
      RETURN
      END
