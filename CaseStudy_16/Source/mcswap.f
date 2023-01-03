**==mcswap.spg  processed by SPAG 4.52O  at 18:10 on 19 Jul 1996
      SUBROUTINE MCSWAP(En, Vir, Attempt, Acc, Iseed)
c     ---exchange a particle bewteen the two boxes
 
      IMPLICIT NONE
 
      INCLUDE 'parameter.inc'
      INCLUDE 'conf.inc'
      INCLUDE 'potential.inc'
      INCLUDE 'system.inc'
      INCLUDE 'chem.inc'
 
      DOUBLE PRECISION En, Vir, RANF, xn, yn, zn, enn, virn, eno, viro, 
     &                 arg, CORU, vola, vold, rhoan, rhoao, rhodn, 
     &                 rhodo, xo, yo, zo, dele, dtaila, dtaild
      INTEGER Attempt, Iseed, o, iadd, idel, jb, idi, Acc
      DIMENSION En(*), Vir(*)
 
 
      Attempt = Attempt + 1
c     ===select a box at random
      IF (RANF(Iseed).LT.0.5D0) THEN
         iadd = 1
         idel = 2
      ELSE
         iadd = 2
         idel = 1
      END IF
      vola = BOX(iadd)**3
      vold = BOX(idel)**3
c     ---add a particle to box iadd
      xn = BOX(iadd)*RANF(Iseed)
      yn = BOX(iadd)*RANF(Iseed)
      zn = BOX(iadd)*RANF(Iseed)
c     ---calculate energy of this particle
      jb = 1
      o = NPART + 1
      CALL ENERI(xn, yn, zn, o, jb, enn, virn, iadd)
c     ---calculate contibution to the chemical potential:
      arg = -BETA*enn
      IF (TAILCO) THEN
         rhoan = (NPBOX(iadd)+1)/vola
         arg = -BETA*(enn+2*CORU(RC(iadd),rhoan))
      END IF
      CHP(iadd) = CHP(iadd) + vola*EXP(arg)/DBLE(NPBOX(iadd)+1)
      IF (NPBOX(iadd).EQ.NPART) CHP(iadd) = CHP(iadd) + vola*EXP(arg)
     &    /DBLE(NPBOX(iadd)+1)
      ICHP(iadd) = ICHP(iadd) + 1
 
c     ---delete particle from box b:
      IF (NPBOX(idel).EQ.0) THEN
         RETURN
      END IF
      idi = 0
      DO WHILE (idi.NE.idel)
         o = INT(NPART*RANF(Iseed)) + 1
         idi = ID(o)
      END DO
      xo = X(o)
      yo = Y(o)
      zo = Z(o)
      CALL ENERI(xo, yo, zo, o, jb, eno, viro, idel)
 
c     ---acceptence test:
      dele = enn - eno + LOG(vold*(NPBOX(iadd)+1)/(vola*NPBOX(idel)))
     &       /BETA
      IF (TAILCO) THEN
c        ---tail corrections:
         rhoao = NPBOX(iadd)/vola
         dtaila = (NPBOX(iadd)+1)*CORU(RC(iadd), rhoan) - NPBOX(iadd)
     &            *CORU(RC(iadd), rhoao)
         rhodn = (NPBOX(idel)-1)/vold
         rhodo = NPBOX(idel)/vold
         dtaild = (NPBOX(idel)-1)*CORU(RC(idel), rhodn) - NPBOX(idel)
     &            *CORU(RC(idel), rhodo)
         dele = dele + dtaila + dtaild
      END IF
      IF (RANF(Iseed).LT.EXP(-BETA*dele)) THEN
c        ---accepted:
         Acc = Acc + 1
         NPBOX(iadd) = NPBOX(iadd) + 1
         X(o) = xn
         Y(o) = yn
         Z(o) = zn
         ID(o) = iadd
         En(iadd) = En(iadd) + enn
         IF (TAILCO) En(iadd) = En(iadd) + dtaila
         Vir(iadd) = Vir(iadd) + virn
         NPBOX(idel) = NPBOX(idel) - 1
         En(idel) = En(idel) - eno
         IF (TAILCO) En(idel) = En(idel) + dtaild
         Vir(idel) = Vir(idel) - viro
      END IF
      RETURN
      END
