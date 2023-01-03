**==lattice.spg  processed by SPAG 4.52O  at 10:45 on  5 Jun 1996
 
      SUBROUTINE LATTICE(Struc)
c     ---place `npart' particles on a lattice with density 'rho'
      IMPLICIT NONE
      INCLUDE 'parameter.inc'
      INCLUDE 'conf.inc'
      INCLUDE 'system.inc'
      INTEGER i, j, k, itel, n
      DOUBLE PRECISION del
      CHARACTER Struc*3
 
      del = (BOX**3)**(1.D0/3.D0)
      IF (Struc.EQ.'fcc') THEN
c        ---put particles on a cubic fcc-lattice
         n = NINT((NPART/4.D0)**(1.D0/3.D0))
         WRITE (6, *) ' generate FCC-lattice '
         IF (n*n*n*4.NE.NPART) THEN
            WRITE (6, *) 
     &                  ' Number of particles incomp. with fcc lattice '
     &                  , NPART, n
            STOP 'lattice'
         END IF
         del = del/n
         PRINT *, del, BOX
         itel = 0
         DO i = 0, n - 1
            DO j = 0, n - 1
               DO k = 0, n - 1
                  X(itel+1) = k*del
                  Y(itel+1) = j*del
                  Z(itel+1) = i*del
 
                  X(itel+2) = (k+0.5D0)*del
                  Y(itel+2) = (j+0.5D0)*del
                  Z(itel+2) = i*del
 
                  X(itel+3) = k*del
                  Y(itel+3) = (j+0.5D0)*del
                  Z(itel+3) = (i+0.5D0)*del
 
                  X(itel+4) = (k+0.5D0)*del
                  Y(itel+4) = j*del
                  Z(itel+4) = (i+0.5D0)*del
                  itel = itel + 4
               END DO
            END DO
         END DO
      ELSE
c        ---put particels on a simple cubic lattice
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
      END IF
      WRITE (6, 99001) itel
      RETURN
99001 FORMAT (' Initialisation on lattice: ', /, i10, 
     &        ' particles placed on a lattice')
      END
