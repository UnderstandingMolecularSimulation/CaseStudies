**==lattice.spg  processed by SPAG 4.52O  at 16:41 on 22 Jul 1996
 
 
 
 
 
      SUBROUTINE LATTICE(Fcc, Bcc, Nx, Ny, Nz)
c     ---place `npart' particles on an fcc lattice with density 'rho'
      IMPLICIT NONE
      INCLUDE 'parameter.inc'
      INCLUDE 'conf.inc'
      INCLUDE 'system.inc'
      INTEGER ix, iy, iz, ip, Nx, Ny, Nz, ii
      DOUBLE PRECISION a0, a1
      LOGICAL Fcc, Bcc
 
      IF (NPART.NE.(Nx*Ny*Nz)) THEN
         WRITE (6, *) 'error "lattice" input npart.ne.(nx*ny*nz)'
         STOP
      END IF
      ip = 0
      IF (Fcc) THEN
c        ---fcc structure 4 particles per unit cells
c            nx x ny : number of atoms in close-packed plane
c            nz      : number of stacked close-packed planes
c        ---determine box sizes
         a1 = (VOL/(Nx*Ny*Nz/4.D0))**(1.D0/3.D0)
         a0 = SQRT(a1*a1/2.D0)
         BOXX = a0*Nx
         BOXY = a0*Ny
         BOXZ = (a1/2.D0)*Nz
         DO iz = 0, Nz - 1
            DO ix = 0, Nx - 1
               DO iy = 0, Ny - 1
                  ip = ip + 1
                  X(ip) = a0*ix + (a0/2.D0)*MOD(iz, 2)
                  Y(ip) = a0*iy + (a0/2.D0)*MOD(iz, 2)
                  Z(ip) = iz*a1/2.D0
               END DO
            END DO
         END DO
      ELSE IF (Bcc) THEN
c        ---bcc structure:
c        nx x ny : atoms in
c        nz/2    : equivalent planes
c        ---determine box sizes:
         IF (MOD(Nz,2).NE.0) THEN
            STOP 'nz should be even'
         END IF
         a0 = (VOL/(Nx*Ny*(Nz/2)))**(1.D0/3.D0)
         PRINT *, ' lattice: a0 ', a0, VOL
         BOXX = a0*Nx
         BOXY = a0*Ny
         BOXZ = a0*Nz/2.D0
         DO iz = 0, Nz/2 - 1
            DO iy = 0, Ny - 1
               DO ix = 0, Nx - 1
                  DO ii = 0, 1
                     ip = ip + 1
                     X(ip) = a0*ix + MOD(ii, 2)*a0/2.D0
                     Y(ip) = a0*iy + MOD(ii, 2)*a0/2.D0
                     Z(ip) = a0*iz + MOD(ii, 2)*a0/2.D0
                  END DO
               END DO
            END DO
         END DO
      ELSE
         STOP 'LATTICE: wrong: structure'
      END IF
      WRITE (6, 99001) ip
      IF (ip.NE.NPART) THEN
         STOP 'LATTICE: npart '
      END IF
      RETURN
99001 FORMAT (' Initialisation on lattice: ', /, i10, 
     &        ' particles placed on a lattice')
      END
