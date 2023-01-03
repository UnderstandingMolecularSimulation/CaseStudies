**==readdat.spg  processed by SPAG 4.52O  at 18:49 on  6 Jun 1996
      SUBROUTINE READDAT(Equil, Prod, Nsamp, Ndispl, Dr, Nexch, Iseed)
c
C     reads input data and model parameters
c
      IMPLICIT NONE
      INCLUDE 'parameter.inc'
      INCLUDE 'system.inc'
      INCLUDE 'potential.inc'
      INCLUDE 'conf.inc'
      INCLUDE 'grand.inc'
      INTEGER ibeg, Equil, Prod, i, Ndispl, Nsamp, Nexch, Iseed
      DOUBLE PRECISION eps, sig, CORU, CORP, vir, rho, Dr, pid
 
 
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
c      nexch   = number of attempts to exchange particles per cycle
c      npart   = total number of particles
c      temp    = temperture
c      rho     = initial density  (only for ibeg=0)
c      pid     = ideal gas pressure reservoir (transfered to chemical potential)
c
      READ (15, *)
      READ (15, *) ibeg, Equil, Prod, Nsamp, Iseed
      READ (15, *)
      READ (15, *) Dr
      READ (15, *)
      READ (15, *) Ndispl, Nexch
      READ (15, *)
      READ (15, *) NPART, TEMP, rho, pid
      IF (NPART.GT.NPMax) THEN
         WRITE (6, *) ' ERROR: number of particles too large'
         STOP
      END IF
      BOX = (NPART/rho)**(1.D00/3.D00)
      HBOX = 0.5D00*BOX
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
         READ (11, *) BOX
         READ (11, *) NPART
         READ (11, *) Dr
         rho = NPART/BOX**3
         HBOX = 0.5D0*BOX
         WRITE (6, 99007) rho
         DO i = 1, NPART
            READ (11, *) X(i), Y(i), Z(i)
         END DO
         REWIND (11)
      END IF
c     ---calculate parameters:
      BETA = 1.D0/TEMP
c     ---grand canonical coupled to an ideal gas bath with pressure: pid
c        chemical potential bath: mu^b = mu^0 + ln(beta*pid)/beta
c                                      = ln (beta*pid*Lamda^3)/beta
c        zz is defined as         zz   = exp(beta*mu^b)/Lamda^3
c                                      = beta*pid
c        excess chemical pot.     muex = mu^b -mu^id
c                                      = mu^0 + ln(beta*pid)/beta - mu^0 - ln(rho)
c                                      = ln(zz)/beta - ln <rho>
      ZZ = BETA*pid
      PI = 4.D0*ATAN(1.D0)
c     ---write input data
      WRITE (6, 99001) Equil, Prod, Nsamp
      WRITE (6, 99002) Ndispl, Dr, Nexch
      WRITE (6, 99003) NPART, TEMP, pid, rho, BOX
      WRITE (6, 99004) eps, sig, MASS
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
     &        /, '  Number of att. to exchange part. per cycle :', i10, 
     &        //)
99003 FORMAT ('  Number of particles                        :', i10, /, 
     &        '  Temperature                                :', f10.3, 
     &        /, '  Ideal gas pressure reservoir               :', 
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
