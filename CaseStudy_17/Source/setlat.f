**==setlat.spg  processed by SPAG 4.52O  at 16:41 on 22 Jul 1996
      SUBROUTINE SETLAT(Fcc, Bcc, Nx, Ny, Nz, Lambda)
c sets reference lattice for free energy calculation
c --currently only for fcc lattice
      IMPLICIT NONE
      INCLUDE 'parameter.inc'
      INCLUDE 'conf.inc'
      INCLUDE 'einst.inc'
      INCLUDE 'system.inc'
      DOUBLE PRECISION xcm, ycm, zcm, a1, a0, xcm0, ycm0, zcm0, Lambda
      INTEGER ix, iy, iz, Nx, Ny, Nz, ip, ii
      LOGICAL Fcc, Bcc
 
      WRITE (6, *) ' Set up Einstein crystal lambda: ', SNGL(Lambda)
      xcm = 0.D0
      ycm = 0.D0
      zcm = 0.D0
      xcm0 = 0.D0
      ycm0 = 0.D0
      zcm0 = 0.D0
c     ---set up Einstein lattice
      ip = 0
      IF (Fcc) THEN
c        ---fcc lattice
         WRITE (6, *) ' FCC-lattice: '
         a1 = (VOL/(Nx*Ny*Nz/4.D0))**(1.D0/3.D0)
         a0 = SQRT(a1*a1/2.D0)
         DO iz = 0, Nz - 1
            DO ix = 0, Nx - 1
               DO iy = 0, Ny - 1
                  ip = ip + 1
                  X0(ip) = a0*ix + (a0/2.D0)*MOD(iz, 2)
                  Y0(ip) = a0*iy + (a0/2.D0)*MOD(iz, 2)
                  Z0(ip) = (a1/2.D0)*iz
c                 ---centre of mass reference lattice
                  xcm0 = xcm0 + X0(ip)
                  ycm0 = ycm0 + Y0(ip)
                  zcm0 = zcm0 + Z0(ip)
c                 ---initial centre of mass solid
                  xcm = xcm + X(ip)
                  ycm = ycm + Y(ip)
                  zcm = zcm + Z(ip)
               END DO
            END DO
         END DO
      ELSE IF (Bcc) THEN
c        ---bcc lattice
         WRITE (6, *) ' BCC-lattice: '
         a0 = (VOL/(Nx*Ny*(Nz/2)))**(1.D0/3.D0)
         PRINT *, ' setlat: a0 ', a0, VOL
         DO iz = 0, Nz/2 - 1
            DO iy = 0, Ny - 1
               DO ix = 0, Nx - 1
                  DO ii = 0, 1
                     ip = ip + 1
                     X0(ip) = a0*ix + MOD(ii, 2)*a0/2.D0
                     Y0(ip) = a0*iy + MOD(ii, 2)*a0/2.D0
                     Z0(ip) = a0*iz + MOD(ii, 2)*a0/2.D0
c                    ---centre of mass reference lattice
                     xcm0 = xcm0 + X0(ip)
                     ycm0 = ycm0 + Y0(ip)
                     zcm0 = zcm0 + Z0(ip)
c                    ---initial centre of mass solid
                     xcm = xcm + X(ip)
                     ycm = ycm + Y(ip)
                     zcm = zcm + Z(ip)
                  END DO
               END DO
            END DO
         END DO
      ELSE
         STOP 'setlat: wrong structure'
      END IF
      IF (ip.NE.NPART) THEN
         WRITE (6, *) ip, ' lattice points added'
         STOP ' error: SETLAT'
      END IF
      xcm = xcm/NPART
      ycm = ycm/NPART
      zcm = zcm/NPART
c     ---centre of mass of the Einstein lattice:
      xcm0 = xcm0/NPART
      ycm0 = ycm0/NPART
      zcm0 = zcm0/NPART
      PRINT *, ' Centre of mass Einstein: ', SNGL(xcm0), SNGL(ycm0), 
     &      SNGL(zcm0)
      PRINT *, ' Centre of mass         : ', SNGL(xcm), SNGL(ycm), 
     &      SNGL(zcm)
c     ---determine: rcm-rcm0
      DXCM = xcm - xcm0
      DYCM = ycm - ycm0
      DZCM = zcm - zcm0
      RETURN
      END
