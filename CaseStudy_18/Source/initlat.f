**==initlat.spg  processed by SPAG 4.52O  at 11:03 on 18 Oct 1996
 
      SUBROUTINE INITLAT
c
c     initialice chains on an fcc  lattice
c     all chains start on x,y plane and point there
c     ell atoms in the z direction.
c
      IMPLICIT NONE
      INCLUDE 'particle.inc'
      INCLUDE 'par.inc'
      INTEGER ix, iy, iz, itel
      DOUBLE PRECISION x0, y0, z0, a
 
      itel = 0
      a = SQRT(0.5D0)
      z0 = -a*2*(ELL/2+1)
      DO WHILE (itel.LT.NPART)
         z0 = z0 + a*2*(ELL/2+1)
         IF (z0.GT.BOX-a) THEN
            STOP 'error: lattice'
         END IF
         x0 = -2*a
         DO ix = 0, NPART
            x0 = x0 + 2*a
            IF (x0.LE.BOX-3*a) THEN
               y0 = -a
               DO iy = 0, NPART
                  y0 = y0 + a
                  IF (y0.LE.BOX-2*a) THEN
                     IF (itel.LT.NPART) THEN
                        itel = itel + 1
                        DO iz = 1, ELL
                           X(itel, iz) = x0 + a*(MOD(iy,2)+MOD(iz,2))
                           Y(itel, iz) = y0
                           Z(itel, iz) = z0 + a*(iz-1)
                        END DO
                     END IF
                  END IF
               END DO
            END IF
         END DO
      END DO
      WRITE (6, *) ' Placed ', itel, ' chains on a lattice '
      RETURN
      END
