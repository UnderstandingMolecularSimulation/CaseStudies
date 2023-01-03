**==lattice.spg  processed by SPAG 4.52O  at 18:10 on 19 Jul 1996
 
      SUBROUTINE LATTICE(Struc)
c     ---place `npart' particles on a lattice with density 'rho'
c      --half the number in box 1 and the other half in box 2
      IMPLICIT NONE
      INCLUDE 'parameter.inc'
      INCLUDE 'conf.inc'
      INCLUDE 'system.inc'
      INTEGER i, j, k, itel, n, ib
      DOUBLE PRECISION del
      CHARACTER Struc*3
 
      del = (BOX(1)**3)**(1.D0/3.D0)
      NPBOX(1) = NPART/2
      NPBOX(2) = NPBOX(1)
      IF (NPBOX(1)+NPBOX(2).NE.NPART) THEN
         STOP ' error npart'
      END IF
      IF (Struc.EQ.'fcc') THEN
c        ---put particles on a cubic fcc-lattice
         n = NINT((NPART/8)**(1.D0/3.D0))
         WRITE (6, *) ' generate FCC-lattice '
         IF (n*n*n*8.NE.NPART) THEN
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
                  DO ib = 1, 2
                     X(itel+1) = k*del
                     Y(itel+1) = j*del
                     Z(itel+1) = i*del
                     ID(itel+1) = ib
 
                     X(itel+2) = (k+0.5D0)*del
                     Y(itel+2) = (j+0.5D0)*del
                     Z(itel+2) = i*del
                     ID(itel+1) = ib
 
                     X(itel+3) = k*del
                     Y(itel+3) = (j+0.5D0)*del
                     Z(itel+3) = (i+0.5D0)*del
                     ID(itel+1) = ib
 
                     X(itel+4) = (k+0.5D0)*del
                     Y(itel+4) = j*del
                     Z(itel+4) = (i+0.5D0)*del
                     ID(itel+1) = ib
                     itel = itel + 4
                  END DO
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
                  DO ib = 1, 2
                     IF (itel.LT.NPART) THEN
                        itel = itel + 1
                        X(itel) = k*del
                        Y(itel) = j*del
                        Z(itel) = i*del
                        ID(itel) = ib
                     END IF
                  END DO
               END DO
            END DO
         END DO
      END IF
      WRITE (6, 99001) itel
      RETURN
99001 FORMAT (' Initialisation on lattice: ', /, i10, 
     &        ' particles placed on a lattice')
      END
