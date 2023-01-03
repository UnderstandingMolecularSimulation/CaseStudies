**==eneri.spg  processed by SPAG 4.52O  at 15:46 on 28 Mar 1996
      SUBROUTINE ENERI(Xi, Yi, Zi, I, Jb, En, Vir)
c
c    calculates the energy of particle I with particles j=jb,npart
c
c  Xi (input) x coordinate particle I
c  Yi (input) y coordinate particle I
c  Zi (input) z coordinate particle I
c  I  (input) particle number
c  Jb (input) = 0 calculates energy particle I with all other particle
c             = jb calculates energy particle I with all particles j > jb
c  En  (output) energy particle i
c  Vir (output) virial particle i
c
      IMPLICIT NONE
      INCLUDE 'parameter.inc'
      INCLUDE 'conf.inc'
      INCLUDE 'potential.inc'
      INCLUDE 'system.inc'
 
      DOUBLE PRECISION Xi, Yi, Zi, En, dx, dy, dz, r2, Vir, virij, enij, 
     &                 r2i, r6i
 
      INTEGER I, j, Jb
c
      En = 0.D0
      Vir = 0.D0
      DO j = Jb, NPART
         IF (j.NE.I) THEN
            dx = Xi - X(j)
            dy = Yi - Y(j)
            dz = Zi - Z(j)
c           ---periodic boundary conditions
            dx = dx - BOX*ANINT(dx/BOX)
            dy = dy - BOX*ANINT(dy/BOX)
            dz = dz - BOX*ANINT(dz/BOX)
            r2 = dx*dx + dy*dy + dz*dz
            IF (r2.LE.RC2) THEN
               r2i = 1/r2
               r6i = r2i*r2i*r2i
               enij = 4*(r6i*r6i-r6i) - ECUT
               virij = 48*(r6i*r6i-0.5D0*r6i)
               En = En + enij
               Vir = Vir + virij
            END IF
         END IF
      END DO
      RETURN
      END
