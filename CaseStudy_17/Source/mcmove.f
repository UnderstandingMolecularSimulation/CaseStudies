**==mcmove.spg  processed by SPAG 4.52O  at 16:41 on 22 Jul 1996
 
 
      SUBROUTINE MCMOVE(En, Vir, Attempt, Nacc, Dr, Enla, Lambda, Cmc, 
     &                  Iseed)
c     ---attempts to displace a randomly selected particle
c        using Linked-lists
c        for this case particles are NOT put back in origional box
      IMPLICIT NONE
      INCLUDE 'parameter.inc'
      INCLUDE 'conf.inc'
      INCLUDE 'system.inc'
      INCLUDE 'method.inc'
      INCLUDE 'einst.inc'
 
      DOUBLE PRECISION enn, En, RANF, xn, yn, zn, virn, Vir, del, Dr, 
     &                 Enla, enlan, Lambda, xo, yo, zo
      INTEGER o, Attempt, Nacc, jb, CELL, Iseed
      LOGICAL overlap, Cmc
 
      Attempt = Attempt + 1
      jb = 1
c     ---select a particle at random
      o = INT(NPART*RANF(Iseed)) + 1
c     ---give particle a random displacement
      xn = X(o) + (RANF(Iseed)-0.5D0)*Dr
      yn = Y(o) + (RANF(Iseed)-0.5D0)*Dr
      zn = Z(o) + (RANF(Iseed)-0.5D0)*Dr
c     ---put particle NOT back simulation box
c     ---test overlap
      CALL ENERI(overlap, xn, yn, zn, o, jb, enn, virn)
c     if hard-core overlap return
      IF (overlap) THEN
         RETURN
      END IF
c     ---calculate energy with lattice
      CALL ENLAT(.TRUE., o, xn, yn, zn, enlan, Lambda, Cmc)
      del = enlan - Enla
c     ---acceptance test
      IF (RANF(Iseed).LT.EXP(-BETA*del)) THEN
c        --accepted
         Nacc = Nacc + 1
         Enla = enlan
c        ---update shift centre of mass
         DXCM = DXCM + (xn-X(o))/NPART
         DYCM = DYCM + (yn-Y(o))/NPART
         DZCM = DZCM + (zn-Z(o))/NPART
         xo = X(o)
         yo = Y(o)
         zo = Z(o)
         X(o) = xn
         Y(o) = yn
         Z(o) = zn
         IF (NEIGHLIST) THEN
c           ---- test whether new position is in a different cell:
            IF (CELL(xo,yo,zo).NE.CELL(xn,yn,zn)) CALL NEW_NLIST
         END IF
      END IF
      RETURN
      END
