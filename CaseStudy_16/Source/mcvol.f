**==mcvol.spg  processed by SPAG 4.52O  at 18:10 on 19 Jul 1996
      SUBROUTINE MCVOL(En, Vir, Attempt, Acc, Vmax, Iseed)
c     attempts to change the volume
      IMPLICIT NONE
      INCLUDE 'parameter.inc'
      INCLUDE 'conf.inc'
      INCLUDE 'potential.inc'
      INCLUDE 'system.inc'
      DOUBLE PRECISION enn(2), En, RANF, Vir, virn(2), yy, CORU, Vmax, 
     &                 rhon, rhoo, f(2), arg, virt, volo(2), volt, dlnv, 
     &                 voln(2), dele1, dele2, dlnv1, dlnv2, enold
      DIMENSION En(*), Vir(*)
      INTEGER Attempt, Acc, i, Iseed, ib, idi
 
      Attempt = Attempt + 1
c     ---calulate new volume by making random walk in ln V
      volo(1) = BOX(1)**3
      volo(2) = BOX(2)**3
      volt = volo(1) + volo(2)
      dlnv = LOG(volo(1)/volo(2)) + (RANF(Iseed)-0.5D0)*Vmax
      voln(1) = EXP(dlnv)*volt/(1+EXP(dlnv))
      voln(2) = volt - voln(1)
      DO ib = 1, 2
         BOX(ib) = voln(ib)**(1.D0/3.D0)
         f(ib) = BOX(ib)/volo(ib)**(1D0/3D0)
         HBOX(ib) = BOX(ib)/2
         RC(ib) = f(ib)*RC(ib)
         RC2(ib) = RC(ib)**2
      END DO
c     ---determine new coordinates
      DO i = 1, NPART
         idi = ID(i)
         X(i) = f(idi)*X(i)
         Y(i) = f(idi)*Y(i)
         Z(i) = f(idi)*Z(i)
      END DO
      IF (.NOT.SHIFT) THEN
c        ---calculate new energy using scaling
         DO ib = 1, 2
            IF (TAILCO) THEN
c              ---substract tail correction
               rhoo = NPBOX(ib)/volo(ib)
               enold = En(ib) - NPBOX(ib)*CORU(RC(ib)/f(ib), rhoo)
            ELSE
               enold = En(ib)
            END IF
            yy = (volo(ib)/voln(ib))**2
            enn(ib) = enold*yy*(2-yy) - Vir(ib)*yy*(1-yy)/6
            virn(ib) = -12*enold*yy*(yy-1) + Vir(ib)*yy*(2*yy-1)
            IF (TAILCO) THEN
c               ---add tail correction
               rhon = NPBOX(ib)/voln(ib)
               enn(ib) = enn(ib) + NPBOX(ib)*CORU(RC(ib), rhon)
            END IF
         END DO
      ELSE
c        ---calculate new total energy
         DO ib = 1, 2
c           ---recalculate new energy shift (dirty trick)
            ECUT(ib) = 0.D0
            CALL ENER(ECUT(ib), virt, RC2(ib), ib)
            CALL TOTERG(enn(ib), virn(ib), ib)
         END DO
      END IF
c     ---acceptance:
      dele1 = enn(1) - En(1)
      dele2 = enn(2) - En(2)
      dlnv1 = LOG(voln(1)/volo(1))
      dlnv2 = LOG(voln(2)/volo(2))
      arg = EXP(-BETA*(dele1+dele2-(NPBOX(1)+1)*dlnv1/BETA-(NPBOX(2)+1)
     &      *dlnv2/BETA))
      IF (RANF(Iseed).LT.arg) THEN
c        ---accepted
         Acc = Acc + 1
         DO ib = 1, 2
            En(ib) = enn(ib)
            Vir(ib) = virn(ib)
         END DO
      ELSE
c        ---restore the old configuration
         DO ib = 1, 2
            f(ib) = 1/f(ib)
            BOX(ib) = BOX(ib)*f(ib)
            HBOX(ib) = 0.5D0*BOX(ib)
            RC(ib) = f(ib)*RC(ib)
            RC2(ib) = RC(ib)**2
         END DO
         DO i = 1, NPART
            idi = ID(i)
            X(i) = f(idi)*X(i)
            Y(i) = f(idi)*Y(i)
            Z(i) = f(idi)*Z(i)
         END DO
      END IF
      RETURN
      END
