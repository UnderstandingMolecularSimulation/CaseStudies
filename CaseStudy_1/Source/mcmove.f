**==mcmove.spg  processed by SPAG 4.52O  at 18:54 on 27 Mar 1996
      SUBROUTINE MCMOVE(En, Vir, Attempt, Nacc, Dr, Iseed)
c
c     attempts to displace a randomly selected particle
c
c
c  Ener   (input/output) : total energy
c  Vir    (input/output) : total virial
c  Attemp (input/output) number of attemps that have been
c                  performed to displace a particle
c  Nacc   (input/output) number of successful attemps
c                  to displace a particle
c  Dr     (input) maximum displacement
c  Iseed  (input) seed random number (not used in present
c                  random number generator)
c
      IMPLICIT NONE
      INCLUDE 'parameter.inc'
      INCLUDE 'conf.inc'
      INCLUDE 'system.inc'
      DOUBLE PRECISION enn, eno, En, RANF, xn, yn, zn, viro, virn, Vir, 
     &                 Dr
      INTEGER o, Attempt, Nacc, jb, Iseed
 
      Attempt = Attempt + 1
      jb = 1
c     ---select a particle at random
      o = INT(NPART*RANF(Iseed)) + 1
c     ---calculate energy old configuration
      CALL ENERI(X(o), Y(o), Z(o), o, jb, eno, viro)
c     ---give particle a random displacement
      xn = X(o) + (RANF(Iseed)-0.5D0)*Dr
      yn = Y(o) + (RANF(Iseed)-0.5D0)*Dr
      zn = Z(o) + (RANF(Iseed)-0.5D0)*Dr
c     ---calculate energy new configuration:
      CALL ENERI(xn, yn, zn, o, jb, enn, virn)
c     ---acceptance test
      IF (RANF(Iseed).LT.EXP(-BETA*(enn-eno))) THEN
c        --accepted
         Nacc = Nacc + 1
         En = En + (enn-eno)
         Vir = Vir + (virn-viro)
c        ---put particle in simulation box
         IF (xn.LT.0) xn = xn + BOX
         IF (xn.GT.BOX) xn = xn - BOX
         IF (yn.LT.0) yn = yn + BOX
         IF (yn.GT.BOX) yn = yn - BOX
         IF (zn.LT.0) zn = zn + BOX
         IF (zn.GT.BOX) zn = zn - BOX
         X(o) = xn
         Y(o) = yn
         Z(o) = zn
      END IF
      RETURN
      END
