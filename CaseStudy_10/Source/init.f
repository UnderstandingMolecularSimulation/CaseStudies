**==init.spg  processed by SPAG 4.52O  at 09:34 on 20 Jun 1996
      SUBROUTINE INIT(Delt, Tmax, Tequil, Temprqs, Iseed)
C     reads input data and model parameters
c
c    Delt    (output) : time step MD simulation
c    Tmax    (output) : total simulation time
c    Tequil  (output) : total equilibration time
c    Temprqs (output) : requisted temperature
c    Iseed   (output) : seed random number generator
c
      IMPLICIT NONE
      INCLUDE 'parameter.inc'
      INCLUDE 'system.inc'
      INCLUDE 'potential.inc'
      INCLUDE 'conf.inc'
      INCLUDE 'veloc.inc'
      INCLUDE 'samp.inc'
      INTEGER ibeg, i, Iseed
      DOUBLE PRECISION rho, Delt, Tmax, rc, sumvx, sumvy, sumvz, sumv2, 
     &                 temp, Tequil, Temprqs
 
 
c     ---input parameters: file: fort.15
c    ibeg  =  0 : initialize from a lattice
c             1 : read configuration from disk
c    Delt       : time step MD simulation
c    Tmax       : total simulation time
c    Tequil     : total equilibration time
c    nsamp      : frequency with which subroutine sample is called
c
c    NPART      : total number of particles
c    temp       : initial temperature
c    rho        : density
c    rc         : cut-off radius
c    Iseed      : seed random number generator
c
c
c    NU         : parameter Poisson distribution
c    Temprqs    : requisted temperature
c
c     IOUT1     : fortran file number output radial distribution function
c     IGR       : sample frequency radial distribution function
c                 (total sample frequency is: nsamp*igr)
c     IOUT2     : fortran file number output diffusion results
c     NTVACF    : sample frequency velocity autocorrelation function
c                 (total sample frequency is: nsamp*ntvacf)
c     IT0       : frequency for a new t=0 in velocity autocorrelation function
c     ITSTRESS0 : frequency for a new t=0 in stress tensor correlation
c     IOUT3     : fortran file number output stress tensor correlation
c
      READ (15, *)
      READ (15, *) ibeg, Delt, Tmax, Tequil, NSAMP
      READ (15, *)
      READ (15, *) NPART, temp, rho, rc, Iseed
      READ (15, *)
      READ (15, *) NU, Temprqs
      READ (15, *)
      READ (15, *) IOUT1, IGR, IOUT2, NTVACF, IT0, ITSTRESS0, IOUT3
 
c     ---initialise and test random number generator
      CALL RANTEST(Iseed)
 
      IF (NPART.GT.NPMax) THEN
         WRITE (6, *) ' ERROR: number of particles too large'
         STOP
      END IF
c     ---read/generate configuration
      IF (ibeg.EQ.0) THEN
c        ---generate configuration form lattice
         BOX = (NPART/rho)**(1.D00/3.D00)
         HBOX = 0.5D00*BOX
         CALL LATTICE()
         CALL SETVEL(temp, Iseed, sumvx, sumvy, sumvz)
      ELSE
         WRITE (6, *) ' read conf from disk '
         READ (11, *) BOX, HBOX
         READ (11, *) NPART
         rho = NPART/BOX**3
         sumv2 = 0
         sumvx = 0
         sumvy = 0
         sumvz = 0
         DO i = 1, NPART
            READ (11, *) X(i), Y(i), Z(i), VX(i), VY(i), VZ(i)
            sumv2 = sumv2 + VX(i)**2 + VY(i)**2 + VZ(i)**2
            sumvx = sumvx + VX(i)
            sumvy = sumvy + VY(i)
            sumvz = sumvz + VZ(i)
         END DO
         temp = sumv2/DBLE(3*NPART)
         sumvx = sumvx/DBLE(NPART)
         sumvy = sumvy/DBLE(NPART)
         sumvz = sumvz/DBLE(NPART)
         REWIND (11)
      END IF
c     ---calculate cut-off radius potential
      rc = MIN(rc, HBOX)
      RC2 = rc*rc
      ECUT = 4*(1/RC2**6-1/RC2**3)
c     ---write input data
      WRITE (6, 99001) NPART, rho, BOX
      WRITE (6, 99003) temp, sumvx, sumvy, sumvz
      WRITE (6, 99002) Delt, Tmax, Tequil, NSAMP, IGR
      WRITE (6, 99004) rc, ECUT
      WRITE (6, 99006) NU*Delt, Temprqs
c
      PI = 4*ATAN(1.D0)
      RETURN
99001 FORMAT ('  Number of particles                        :', i10, /, 
     &        '  Density                                    :', f10.3, 
     &        /, '  Box length                                 :', 
     &        f10.3, /)
99002 FORMAT ('  Time step                                   :', f10.3, 
     &        /, '  Total simulation time                       : ', 
     &        f10.2, /, 
     &        '  Equilibration                               : ', f10.2, 
     &        /, '  Number of timesteps between two samples     : ', 
     &        i10, /, '  Number of timesteps between two samples g(r): '
     &        , i10)
99003 FORMAT ('  Initial Temperature                         :', f10.3, 
     &        /, '    velocity centre of mass x-dir            :', 
     &        f10.3, /, '    velocity centre of mass y-dir            :'
     &        , f10.3, /, 
     &        '    velocity centre of mass z-dir            :', f10.3)
99004 FORMAT (' Simulations with TRUNCATED AND SHIFTED potential: ', /, 
     &        ' Potential truncated at                      :', f10.3, 
     &        /, ' Energy shift                                :', 
     &        f10.6, //)
99005 FORMAT (' Time step                                   :', f10.3, 
     &        /, ' Maximum simulation time                     :', 
     &        f10.3, /, ' Number of timesteps between two samples     :'
     &        , f10.3)
 
99006 FORMAT (' =====> Andersen thermostat <=========== ', /, 
     &        '  collision frequency per timestep: ', f10.3, /, 
     &        '  requested temperature           : ', f10.3)
      END
