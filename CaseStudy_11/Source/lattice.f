**==lattice.spg  processed by SPAG 4.52O  at 11:24 on 20 Jun 1996
 
      SUBROUTINE LATTICE()
c     ---place `npart' particles on a lattice with density 'rho'
      IMPLICIT NONE
      INCLUDE 'parameter.inc'
      INCLUDE 'conf.inc'
      INCLUDE 'system.inc'
      INTEGER i, j, k, itel, n
      DOUBLE PRECISION del
 
      del = (BOX**3)**(1.D0/3.D0)
c     ---put particels on a simple cubic lattice
      WRITE (6, *) ' generate simple cubic lattice'
      n = INT(NPART**(1.D0/3.D0)) + 1
      IF (n.EQ.0) n = 1
      del = del/DBLE(n)
      itel = 0
      DO i = 0, n - 1
         DO j = 0, n - 1
            DO k = 0, n - 1
               IF (itel.LT.NPART) THEN
                  itel = itel + 1
                  X(itel) = k*del
                  Y(itel) = j*del
                  Z(itel) = i*del
               END IF
            END DO
         END DO
      END DO
      WRITE (6, 99001) itel
      RETURN
99001 FORMAT (' Initialisation on lattice: ', /, i10, 
     &        ' particles placed on a lattice')
      END
