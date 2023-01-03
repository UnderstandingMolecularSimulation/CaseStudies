**==readdat.spg  processed by SPAG 4.52O  at 16:41 on 22 Jul 1996
      SUBROUTINE READDAT(Equil, Prod, Nsamp, Ndispl, Dr, Lambda, Succ, 
     &                   Cmc, Iseed)
C     ---read input data and model parameters
      IMPLICIT NONE
      INCLUDE 'parameter.inc'
      INCLUDE 'system.inc'
      INCLUDE 'conf.inc'
      INCLUDE 'nlist.inc'
      INCLUDE 'method.inc'
 
      INTEGER ibeg, Equil, Prod, i, Ndispl, Nsamp, nx, ny, nz, Iseed
      DOUBLE PRECISION rc, Dr, rho, Lambda, Succ
      LOGICAL fcc, Cmc, bcc
 
 
c     ---read simulation data
      READ (15, *)
      READ (15, *) ibeg, Equil, Prod, Nsamp, Iseed
      READ (15, *)
      READ (15, *) Dr, Succ
      READ (15, *)
      READ (15, *) Ndispl
      READ (15, *)
      READ (15, *) NPART, TEMP, rho
      READ (15, *)
      READ (15, *) nx, ny, nz, fcc, bcc, Cmc
      WRITE (6, *) '**************** MC_NVT_SOLID ***************'
c     ---initialise and test random number generator
      CALL RANTEST(Iseed)
 
      IF (NPART.GT.NPMax) THEN
         WRITE (6, *) ' ERROR: number of particles too large'
         STOP
      END IF
      VOL = NPART/rho
      IF (ibeg.EQ.0) THEN
c        ---generate configuration form lattice
         CALL LATTICE(fcc, bcc, nx, ny, nz)
      ELSE
         WRITE (6, *) ' read conf from disk '
         READ (11, *) BOXX, BOXY, BOXZ
         READ (11, *) NPART
         READ (11, *) Dr
         DO i = 1, NPART
            READ (11, *) X(i), Y(i), Z(i)
         END DO
         REWIND (11)
      END IF
      HBOX = 0.5D0*MIN(BOXX, BOXY, BOXZ)
      IF (ABS(VOL-BOXX*BOXY*BOXZ).GT.1D-8) THEN
         WRITE (6, *) ' (VOL .ne. BOXX*BOXY*BOXZ ', VOL, BOXX*BOXY*BOXZ
         STOP 'error: '
      END IF
c     ---write input data
      WRITE (6, 99001) Equil, Prod, Nsamp
      WRITE (6, 99002) Ndispl, Dr
      WRITE (6, 99003) NPART, TEMP, rho, BOXX, BOXY, BOXZ
c     ---calculate parameters:
      BETA = 1.D0/TEMP
c     ---read method for energy calculation
      READ (15, *)
      READ (15, *) NEIGHLIST, Lambda
      IF (NEIGHLIST) THEN
c        ---parameters Neighbour-list
         WRITE (6, 99004)
c        ---determine cell size
         rc = 1.D0
         NCELX = INT(BOXX/rc)
         NCELY = INT(BOXY/rc)
         NCELZ = INT(BOXZ/rc)
         NCELT = NCELX*NCELY*NCELZ
         FACTX = NCELX/BOXX
         FACTY = NCELY/BOXY
         FACTZ = NCELZ/BOXZ
         WRITE (6, 99005) 1.D0/FACTX, rc, NCELT, NCELX
         WRITE (6, 99005) 1.D0/FACTY, rc, NCELT, NCELY
         WRITE (6, 99005) 1.D0/FACTZ, rc, NCELT, NCELZ
         IF (NCELX.LE.3.OR.NCELT.GT.CELmax) THEN
            WRITE (6, *) ' ERROR: cells'
            WRITE (6, *) ' ncel, celmax: ', NCELX, NCELY, NCELZ, NCELT, 
     &                   CELmax
            STOP
         END IF
c        ---make Neighbour list
         CALL NEW_NLIST
      END IF
      IF (.NOT.NEIGHLIST) THEN
         WRITE (6, 99006)
      END IF
c ---set up Einstein crystal:
      IF (Lambda.GT.0.D0) CALL SETLAT(fcc, bcc, nx, ny, nz, Lambda)
 
      RETURN
99001 FORMAT ('  Number of equilibration cycles             :', i10, /, 
     &        '  Number of production cycles                :', i10, /, 
     &        '  Sample frequency                           :', i10, /)
99002 FORMAT ('  Number of att. to displ. a part. per cycle :', i10, /, 
     &        '  Maximum displacement                       :', f10.3, 
     &        //)
99003 FORMAT ('  Number of particles                        :', i10, /, 
     &        '  Temperature                                :', f10.3, 
     &        /, '  Density                                    :', 
     &        f10.3, /, '  Box length-x                               :'
     &        , f10.3, /, 
     &        '  Box length-y                               :', f10.3, 
     &        /, '  Box length-z                               :', 
     &        f10.3, /)
99004 FORMAT (' Energy calculation using Neighbour list ', /)
99005 FORMAT (' Parameters Neighbour-list : ', /, '      cell-size : ', 
     &        f5.3, ' (cut-off radius: ', f5.3, ')', /, 
     &        '    num. cells  : ', i5, ' (= ', i5, '^3)')
99006 FORMAT (' Simple N^2 algorithm for energy calculation ', /)
      END
