**==lattice.spg  processed by SPAG 4.52O  at 10:35 on 12 Jul 1996
      SUBROUTINE LATTICE
c     ---place `npart' particles on a lattice with density 'rho'
      IMPLICIT NONE
      INCLUDE 'parameter.inc'
      INCLUDE 'conf.inc'
      INCLUDE 'system.inc'
      INTEGER i, j, k, itel, n
      DOUBLE PRECISION dx, dy, dz, del
 
      n = INT(NPART**(1./3.)) + 1
      IF (n.EQ.0) n = 1
      del = BOX/DBLE(n)
      itel = 0
      dx = -del
      DO i = 1, n
         dx = dx + del
         dy = -del
         DO j = 1, n
            dy = dy + del
            dz = -del
            DO k = 1, n
               dz = dz + del
               IF (itel.LT.NPART) THEN
                  itel = itel + 1
                  X(itel) = dx
                  Y(itel) = dy
                  Z(itel) = dz
               END IF
            END DO
         END DO
      END DO
      WRITE (6, 99001) itel
      RETURN
99001 FORMAT (' Initialisation on lattice: ', /, i10, 
     &        ' particles placed on a lattice')
      END
