**==readdat.spg  processed by SPAG 4.52O  at 18:10 on 19 Jul 1996
      SUBROUTINE READDAT(Equil, Prod, Nsamp, Ndispl, Dr, Nvol, Vmax, 
     &                   Nswap, Iseed, Succ)
C     reads input data and model parameters
c
c     ---input parameters: file: fort.15
c    ibeg  =  0 : initilaize from a lattice
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
c    Nswap      : number of attemps to swap particle between the two boxes per MC cycle
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
c    rcc    = cut-off radius of the potential
c
c     ---input parameters: file: fort.11 (restart file
c                to continue a simulation from disk)
c    BOX(1)   = length box 1 old configuration
c    HBOX(1)  = BOX(1)/2
c    BOX(2)   = length box 2 old configuration
c    HBOX(2)  = BOX(2)/2
c    NPART    = total number of particles (over rules fort.15!!)
c    NPBOX(1) = number of particles in box 1
c    NPBOX(2) = number of particles in box 2
c    Dr     = optimized maximum displacement old configurations
c    Vmax   = optimized maximum volume change old configurations
c    X(1),Y(1),Z(1)            : position first particle 1
c                  ,ID(1)   = 1 particle in box 1
c                  ,ID(1)   = 2 particle in box 2
c       ....
c    X(NPART),Y(NPART),Z(NPART): position particle last particle
      IMPLICIT NONE
      INCLUDE 'parameter.inc'
      INCLUDE 'system.inc'
      INCLUDE 'potential.inc'
      INCLUDE 'conf.inc'
      INCLUDE 'npt.inc'
      INTEGER ibeg, Equil, Prod, i, Ndispl, Nsamp, Nvol, Iseed, ib, 
     &        Nswap
      DOUBLE PRECISION eps, sig, CORU, CORP, vir, rho, Dr, Vmax, Succ, 
     &                 rcc
      CHARACTER struc*3
 
 
c     ---read simulation data
      READ (15, *)
      READ (15, *) ibeg, Equil, Prod, Nsamp, Iseed
      READ (15, *)
      READ (15, *) Dr, Vmax, Succ
      READ (15, *)
      READ (15, *) Ndispl, Nvol, Nswap
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
      READ (25, *) eps, sig, MASS, rcc
c     ---read/generate configuration
      BOX(1) = (NPART/(2*rho))**(1.D00/3.D00)
      HBOX(1) = 0.5D00*BOX(1)
      BOX(2) = BOX(1)
      HBOX(2) = HBOX(1)
      IF (ibeg.EQ.0) THEN
c        ---generate configuration form lattice
         CALL LATTICE(struc)
      ELSE
         WRITE (6, *) ' read conf from disk '
         READ (11, *) BOX(1), HBOX(1), BOX(2), HBOX(2)
         READ (11, *) NPART, NPBOX(1), NPBOX(2)
         READ (11, *) Dr, Vmax
         DO i = 1, NPART
            READ (11, *) X(i), Y(i), Z(i), ID(i)
         END DO
         REWIND (11)
      END IF
c     ---write input data
      WRITE (6, 99001) Equil, Prod, Nsamp
      WRITE (6, 99002) Ndispl, Dr, Nvol, Vmax, Nswap
      WRITE (6, 99003) NPART, TEMP, P, NPBOX(1)/BOX(1)**3, BOX(1), 
     &                 NPBOX(2)/BOX(2)**3, BOX(2)
      WRITE (6, 99004) eps, sig, MASS
c     ---calculate parameters:
      BETA = 1.D0/TEMP
      PI = 4.D0*ATAN(1.D0)
      EPS4 = 4.D0*eps
      EPS48 = 48.D0*eps
      SIG2 = sig*sig
c     ---calculate cut-off radius potential
      DO ib = 1, 2
         RC(ib) = MIN(rcc, HBOX(ib))
         RC2(ib) = RC(ib)*RC(ib)
         IF (TAILCO) THEN
            WRITE (6, 99006) RC(ib), CORU(RC(ib), rho), 
     &                       CORP(RC(ib), rho)
         END IF
         IF (SHIFT) THEN
c           ---calculate energy of the shift (dirty trick)
            ECUT(ib) = 0.D0
            CALL ENER(ECUT(ib), vir, RC2(ib), ib)
            WRITE (6, 99005) RC(ib), ECUT(ib)
         END IF
      END DO
      RETURN
99001 FORMAT ('  Number of equilibration cycles             :', i10, /, 
     &        '  Number of production cycles                :', i10, /, 
     &        '  Sample frequency                           :', i10, /)
99002 FORMAT ('  Number of att. to displ. a part. per cycle :', i10, /, 
     &        '  Maximum displacement                       :', f10.3, 
     &        /, '  Number of att. to change volume  per cycle :', i10, 
     &        /, '  Maximum change volume                      :', 
     &        f10.3, /, '  Number of att. to exch part.  per cycle    :'
     &        , i10, //)
99003 FORMAT ('  Number of particles                        :', i10, /, 
     &        '  Temperature                                :', f10.3, 
     &        /, '  Pressure                                   :', 
     &        f10.3, /, '  Density box 1                              :'
     &        , f10.3, /, 
     &        '  Box 1 length                               :', f10.3, 
     &        /, '  Density box 1                              :', 
     &        f10.3, /, '  Box 2 length                               :'
     &        , f10.3, /)
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
