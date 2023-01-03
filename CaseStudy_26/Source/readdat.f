**==readdat.spg  processed by SPAG 4.52O  at 10:32 on 18 Oct 1996
      SUBROUTINE READDAT(Equil, Prod, Nsamp, Ndispl, Dr, Iseed)
C     ---read input data and model parameters
      IMPLICIT NONE
      INCLUDE 'parameter.inc'
      INCLUDE 'system.inc'
      INCLUDE 'potential.inc'
      INCLUDE 'conf.inc'
      INCLUDE 'nlist.inc'
      INCLUDE 'verlet.inc'
      INCLUDE 'method.inc'
 
      INTEGER ibeg, Equil, Prod, i, Ndispl, Nsamp, Iseed
      DOUBLE PRECISION eps, sig, CORU, CORP, vir, boxf, hboxf, rhof, Dr, 
     &                 rho
 
 
c     ---read simulation data
c
c input parameters
c      ibeg    = 0 start simulation from lattice
c              = 1 continue sumlation from disk
c      equil   = number of equilibration cycles
c      prod    = number of produktion cycles
c      nsample = sample frequency; sample after nsample cycles
c      iseed   = seed random number
c      dr      = maximum displacement (only for ibeg=0)
c      ndispl  = number of attempts to displace a particle per cycle
c      npart   = total number of particles (only for ibeg=0)
c      temp    = temperture
c      rho     = initial density  (only for ibeg=0)
c
      READ (15, *)
      READ (15, *) ibeg, Equil, Prod, Nsamp, Iseed
      READ (15, *)
      READ (15, *) Dr
      READ (15, *)
      READ (15, *) Ndispl
      READ (15, *)
      READ (15, *) NPART, TEMP, rho
      IF (NPART.GT.NPMax) THEN
         WRITE (6, *) ' ERROR: number of particles too large'
         STOP
      END IF
      BOX = (NPART/rho)**(1.D0/3.D0)
      HBOX = 0.5D0*BOX
c     ---read model parameters
      READ (25, *)
      READ (25, *) TAILCO, SHIFT
      READ (25, *)
      READ (25, *) eps, sig, MASS, RC
c     ---read/generate configuration
      IF (ibeg.EQ.0) THEN
c        ---generate configuration form lattice
         CALL LATTICE
      ELSE
         WRITE (6, *) ' read conf from disk '
         READ (11, *) boxf, hboxf
         READ (11, *) NPART
         READ (11, *) Dr
         IF (ABS(boxf-BOX).GT.1.D-3) THEN
            WRITE (6, 99007) rho, rhof
         END IF
         DO i = 1, NPART
            READ (11, *) X(i), Y(i), Z(i)
            X(i) = X(i)*BOX/boxf
            Y(i) = Y(i)*BOX/boxf
            Z(i) = Z(i)*BOX/boxf
         END DO
         REWIND (11)
      END IF
c     ---write input data
      WRITE (6, 99001) Equil, Prod, Nsamp
      WRITE (6, 99002) Ndispl, Dr
      WRITE (6, 99003) NPART, TEMP, rho, BOX
      WRITE (6, 99004) eps, sig, MASS
c     ---calculate parameters:
      BETA = 1.D0/TEMP
      PI = 4.D0*ATAN(1.D0)
c     ---calculate cut-off radius potential
      RC = MIN(RC, HBOX)
      RC2 = RC*RC
      EPS4 = 4*eps
      EPS48 = 48*eps
      SIG2 = sig*sig
      IF (SHIFT) THEN
c     ---calculate energy of the shift
         ECUT = 0
         CALL ENER(ECUT, vir, RC2)
         WRITE (6, 99005) RC, ECUT
      END IF
      IF (TAILCO) THEN
         WRITE (6, 99006) RC, CORU(RC, rho), CORP(RC, rho)
      END IF
c     ---read method for energy calculation
      READ (15, *)
      READ (15, *) NEIGHLIST, VERLETLIST
      READ (15, *)
      READ (15, *) RV
      IF (NEIGHLIST) THEN
c        ---parameters Neighbour-list
         IF (VERLETLIST) THEN
            WRITE (6, 99012)
         ELSE
            WRITE (6, 99008)
         END IF
c        ---determine cell size
         IF (VERLETLIST) THEN
            NCEL = INT(BOX/RV)
         ELSE
            NCEL = INT(BOX/RC)
         END IF
         NCELT = NCEL*NCEL*NCEL
         FACTN = NCEL/BOX
         WRITE (6, 99009) 1/FACTN, RC, NCELT, NCEL
         IF (NCEL.LE.3.OR.NCELT.GT.CELmax) THEN
            WRITE (6, *) ' ERROR: cells'
            WRITE (6, *) ' ncel, celmax: ', NCEL, NCELT, CELmax
            STOP
         END IF
c        ---make Neighbour list
         CALL NEW_NLIST
      END IF
      IF (VERLETLIST) THEN
c        ---parameters Verlet-list
         RV2 = RV*RV
         SKIN = RV - RC
         SKIN2 = SKIN*SKIN
         WRITE (6, 99011) RV
         IF (SKIN.LE.0) THEN
            WRITE (6, *) ' ERROR: skin < 0 ', RV, RC
            STOP
         END IF
c        ---make Verlet list
         CALL NEW_VLIST
      END IF
      IF (.NOT.VERLETLIST.AND..NOT.NEIGHLIST) THEN
         WRITE (6, 99010)
      END IF
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
     &        f10.3, /, '  Box length                                 :'
     &        , f10.3, /)
99004 FORMAT ('  Model parameters: ', /, '     epsilon: ', f5.3, /, 
     &        '     sigma  : ', f5.3, /, '     mass   : ', f5.3)
99005 FORMAT (' Simulations with TRUNCATED AND SHIFTED potential: ', /, 
     &        ' Potential truncated at :', f10.3, /, 
     &        ' Energy shif            :', f10.3, //)
99006 FORMAT (' Simulations with tail correction: ', /, 
     &        ' Potential truncated at  Rc = ', f10.3, /, 
     &        ' Tail corrections:   energy = ', f10.3, ' pressure ', 
     &        f10.3, /, /)
99007 FORMAT (' Requested density: ', f5.2, 
     &        ' different from density on disk: ', f5.2, /, 
     &        ' Rescaling of coordinates!')
99008 FORMAT (' Energy calculation using Neighbour list ', /)
99009 FORMAT (' Parameters Neighbour-list : ', /, '      cell-size : ', 
     &        f5.3, ' (cut-off radius: ', f5.3, ')', /, 
     &        '    num. cells  : ', i5, ' (= ', i5, '^3)')
99010 FORMAT (' Simple N^2 algorithm for energy calculation ', /)
99011 FORMAT (' Energy calculation using Verlet list ', /, 
     &        '  Verlet radius           Rv = ', f10.3)
99012 FORMAT (' Energy calculation using combination Neighbour list ', 
     &        'and Verlet list ', /)
      END
