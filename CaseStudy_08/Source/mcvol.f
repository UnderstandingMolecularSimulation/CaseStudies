**==mcvol.spg  processed by SPAG 4.52O  at 10:45 on  5 Jun 1996
      SUBROUTINE MCVOL(En, Vir, Attempt, Acc, Vmax, Iseed)
c     attempts to change the volume
      IMPLICIT NONE
      INCLUDE 'parameter.inc'
      INCLUDE 'conf.inc'
      INCLUDE 'potential.inc'
      INCLUDE 'system.inc'
      INCLUDE 'npt.inc'
      DOUBLE PRECISION enn, En, RANF, Vir, virn, boxn, yy, vo, vn, CORU, 
     &                 lnvn, Vmax, rhon, rhoo, rcn, f, arg, eno, virt
      INTEGER Attempt, Acc, i, Iseed
 
      Attempt = Attempt + 1
c     ---calulate new volume by making random walk in ln V
      vo = BOX*BOX*BOX
      lnvn = LOG(vo) + (RANF(Iseed)-0.5D0)*Vmax
      vn = EXP(lnvn)
      boxn = vn**(1.D0/3.D0)
      IF (.NOT.SHIFT) THEN
c        ---calculate new energy using scaling
         IF (TAILCO) THEN
c           ---substract tail correction
            rhoo = NPART/vo
            eno = En - NPART*CORU(RC, rhoo)
         END IF
         yy = (vo/vn)*(vo/vn)
         enn = eno*yy*(2-yy) - Vir*yy*(1-yy)/6
         virn = -12*eno*yy*(yy-1) + Vir*yy*(2*yy-1)
         IF (TAILCO) THEN
c            ---add tail correction
            rhon = NPART/vn
            rcn = RC*boxn/BOX
            enn = enn + NPART*CORU(rcn, rhon)
         END IF
      ELSE
c        ---determine new coordinates
         f = boxn/BOX
         DO i = 1, NPART
            X(i) = f*X(i)
            Y(i) = f*Y(i)
            Z(i) = f*Z(i)
         END DO
         BOX = boxn
         HBOX = 0.5D0*BOX
         RC = RC*f
         RC2 = RC*RC
c        ---recalculate new energy shift (dirty trick)
         ECUT = 0.D0
         CALL ENER(ECUT, virt, RC2)
c        ---calculate new total energy
         CALL TOTERG(enn, virn)
      END IF
c     ---acceptance:
      arg = EXP(-BETA*(P*(vn-vo)-(NPART+1)*LOG(vn/vo)/BETA+enn-En))
      IF (RANF(Iseed).LT.arg) THEN
c        ---accepted
         Acc = Acc + 1
         En = enn
         Vir = virn
         IF (SHIFT) THEN
            RETURN
         END IF
c        ---rescale coordinates
         f = boxn/BOX
         DO i = 1, NPART
            X(i) = f*X(i)
            Y(i) = f*Y(i)
            Z(i) = f*Z(i)
         END DO
         BOX = boxn
         HBOX = BOX/2
         RC = RC*f
         RC2 = RC*RC
      ELSE IF (SHIFT) THEN
c        ---restore the old configuration
         f = 1/f
         DO i = 1, NPART
            X(i) = f*X(i)
            Y(i) = f*Y(i)
            Z(i) = f*Z(i)
         END DO
         BOX = boxn*f
         HBOX = 0.5D0*BOX
         RC = RC*f
         RC2 = RC*RC
c        ---recalculate energy shift (dirty trick)
         ECUT = 0
         CALL ENER(ECUT, virt, RC2)
      END IF
      RETURN
      END
