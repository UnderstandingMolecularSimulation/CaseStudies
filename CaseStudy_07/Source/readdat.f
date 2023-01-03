**==readdat.spg  processed by SPAG 4.52O  at 10:45 on  5 Jun 1996
      SUBROUTINE READDAT(Equil, Prod, Nsamp, Ndispl, Dr, Nvol, Vmax, 
     &                   Iseed, Succ)
C     reads input data and model parameters
c
c     ---input parameters: file: fort.15
c    ibeg  =  0 : initialize from a lattice
c             1 : read configuration from disk
c    Equil      : number of Monte Carlo cycles during equilibration
c    Prod       : number of Monte Carlo cycles during production
c    Nsamp      : number of Monte Carlo cycles between two sampling periods
c    Iseed      : seed random number generator
c    Dr         : maximum displacement
c    Vmax       : maximum volume change
c    Succ       : optimal percentance of accepted attemps
c                 the program adjusts Vmax or Dr in just a way that
c                 on average succ% of the moves are accepted
c    Ndispl     : number of attemps to displace a particle per MC cycle
c    Nvol       : number of attemps to change the volume  per MC cycle
c    NPART      : total numbero fo particles
c    TEMP       : temperature
c    rho        : density
c    P          : pressure
c    struc      : structure to initalize the lattice
c               = 'fcc' fcc-lattice (face centered cubic)
c                 otherwise simple cubic bcc-lattice (body centered cubic)
c
c
c     ---input parameters: file: fort.25
c    TAILCO = .true. : tail corrections are applied
c             .false.: no-tail corrections
c    SHIFT  = .true. : potential is shifted
c             .false.: potential is NOT-shifted
c    eps    = epsilon Lennard-Jones potential
c    sig    = sigma Lennard-Jones potential
c    MASS   = mass of the particle
c    RC     = cut-off radius of the potential
c
c     ---input parameters: file: fort.11 (restart file
c                to continue a simulation from disk)
c    boxf   = Box length old configuration (if this one
c             does not correspond to the requested density, the positions
c             of the particles are rescaled!
c    NPART  = number of particles (over rules fort.15!!)
c    Dr     = optimized maximum displacement old configurations
c    Vmax   = optimized maximum volume change old configurations
c    X(1),Y(1),Z(1)            : position first particle 1
c        ...
c    X(NPART),Y(NPART),Z(NPART): position particle last particle
 
      IMPLICIT NONE
      INCLUDE 'parameter.inc'
      INCLUDE 'system.inc'
      INCLUDE 'potential.inc'
      INCLUDE 'conf.inc'
      INCLUDE 'npt.inc'
      INTEGER ibeg, Equil, Prod, i, Ndispl, Nsamp, Nvol, Iseed
      DOUBLE PRECISION eps, sig, CORU, CORP, vir, boxf, hboxf, rhof, 
     &                 rho, Dr, Vmax, Succ
      CHARACTER struc*3
 
 
c     ---read simulation data
      READ (15, *)
      READ (15, *) ibeg, Equil, Prod, Nsamp, Iseed
      READ (15, *)
      READ (15, *) Dr, Vmax, Succ
      READ (15, *)
      READ (15, *) Ndispl, Nvol
      READ (15, *)
      READ (15, *) NPART, TEMP, rho, P, struc
c     ---initialise and test random number generator
      CALL RANTEST(Iseed)
 
      IF (NPART.GT.NPMax) THEN
         WRITE (6, *) ' ERROR: number of particles too large'
         STOP
      END IF
c     ---read model parameters
      READ (25, *)
      READ (25, *) TAILCO, SHIFT
      READ (25, *)
      READ (25, *) eps, sig, MASS, RC
c     ---read/generate configuration
      BOX = (NPART/rho)**(1.D00/3.D00)
      HBOX = 0.5D00*BOX
      IF (ibeg.EQ.0) THEN
c        ---generate configuration form lattice
         CALL LATTICE(struc)
      ELSE
         WRITE (6, *) ' read conf from disk '
         READ (11, *) boxf, hboxf
         READ (11, *) NPART
         READ (11, *) Dr, Vmax
         rhof = NPART/boxf**3
         IF (ibeg.EQ.2) THEN
c           ---scale density to input value
            IF (ABS(boxf-BOX).GT.1.D-6) THEN
               WRITE (6, 99007) rho, rhof
            END IF
         ELSE
c           ---use density from disk
            rho = rhof
            BOX = boxf
            HBOX = hboxf
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
      WRITE (6, 99002) Ndispl, Dr, Nvol, Vmax
      WRITE (6, 99003) NPART, TEMP, P, rho, BOX
      WRITE (6, 99004) eps, sig, MASS
c     ---calculate parameters:
      BETA = 1.D0/TEMP
      PI = 4.D0*ATAN(1.D0)
c     ---calculate cut-off radius potential
      RC = MIN(RC, HBOX)
      RC2 = RC*RC
      EPS4 = 4.D0*eps
      EPS48 = 48.D0*eps
      SIG2 = sig*sig
      IF (SHIFT) THEN
c     ---calculate energy of the shift (dirty trick)
         ECUT = 0.D0
         CALL ENER(ECUT, vir, RC2)
         WRITE (6, 99005) RC, ECUT
      END IF
      IF (TAILCO) THEN
         WRITE (6, 99006) RC, CORU(RC, rho), CORP(RC, rho)
      END IF
      RETURN
99001 FORMAT ('  Number of equilibration cycles             :', i10, /, 
     &        '  Number of production cycles                :', i10, /, 
     &        '  Sample frequency                           :', i10, /)
99002 FORMAT ('  Number of att. to displ. a part. per cycle :', i10, /, 
     &        '  Maximum displacement                       :', f10.3, 
     &        /, '  Number of att. to change volume  per cycle :', i10, 
     &        /, '  Maximum change volume                      :', 
     &        f10.3, //)
99003 FORMAT ('  Number of particles                        :', i10, /, 
     &        '  Temperature                                :', f10.3, 
     &        /, '  Pressure                                   :', 
     &        f10.3, /, '  Density                                    :'
     &        , f10.3, /, 
     &        '  Box length                                 :', f10.3, 
     &        /)
99004 FORMAT ('  Model parameters: ', /, '     epsilon: ', f5.3, /, 
     &        '     sigma  : ', f5.3, /, '     mass   : ', f5.3)
99005 FORMAT (' Simulations with TRUNCATED AND SHIFTED potential: ', /, 
     &        ' Potential truncated at :', f10.3, /, 
     &        ' Energy shif            :', f10.6, //)
99006 FORMAT (' Simulations with tail correction: ', /, 
     &        ' Potential truncated at  Rc =:', f10.3, /, 
     &        ' Tail corrections:   energy = ', f10.3, ' pressure ', 
     &        f10.3, /, /)
99007 FORMAT (' Requested density: ', f5.2, 
     &        ' different from density on disk: ', f5.2, /, 
     &        ' Rescaling of coordinates!')
      END
