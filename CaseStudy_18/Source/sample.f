**==sample.spg  processed by SPAG 4.52O  at 11:03 on 18 Oct 1996
      SUBROUTINE SAMPLE(Switch, En, Enlj, Vir)
      IMPLICIT NONE
      INCLUDE 'particle.inc'
      INCLUDE 'par.inc'
      INTEGER Switch, nsamp, bin, BINmax, i, j
      DOUBLE PRECISION factrg, factbl, dx, dy, dz, r, bl, rg, En, Vir, 
     &                 press, vol, enp, Enlj, enljp, rho, virh
      PARAMETER (BINmax=100)
      INTEGER hrg(0:BINmax), hbl(0:BINmax)
      SAVE hrg, hbl, nsamp, bl, rg, factrg, factbl
 
      IF (Switch.EQ.0) THEN
c     ---initialize sampling
         bl = 0
         rg = 0
         nsamp = 0
         DO bin = 0, BINmax
            hrg(bin) = 0
            hbl(bin) = 0
         END DO
c     ---radius of gyration: max: ell*l0
c                            min: 0
         factrg = DBLE(BINmax)/(L0*ELL)
         factbl = DBLE(BINmax)/1.5D0
      ELSE IF (Switch.EQ.1) THEN
c     ---perform sampling
         nsamp = nsamp + 1
c --- determine distribution of bond lengths
c               and radius of gyration
c
         DO i = 1, NPART
            dx = (X(i,1)-X(i,ELL))**2
            dy = (Y(i,1)-Y(i,ELL))**2
            dz = (Z(i,1)-Z(i,ELL))**2
            r = SQRT(dx+dy+dz)
            rg = rg + r
            bin = INT(r*factrg)
            hrg(bin) = hrg(bin) + 1
            DO j = 2, ELL
               dx = (X(i,j)-X(i,j-1))**2
               dy = (Y(i,j)-Y(i,j-1))**2
               dz = (Z(i,j)-Z(i,j-1))**2
               r = SQRT(dx+dy+dz)
               bl = bl + r
               bin = INT(r*factbl)
               hbl(bin) = hbl(bin) + 1
            END DO
         END DO
         IF (NPART.NE.0) THEN
            enp = En/DBLE(NPART)
            enljp = Enlj/DBLE(NPART*ELL)
            vol = BOX**3
            press = (NPART*ELL/vol)/BETA + Vir/(3*vol)
            rho = NPART*ELL/vol
            virh = Vir/(3*vol)
         ELSE
            enp = 0
            press = 0
         END IF
         WRITE (66, *) nsamp, NPART/vol, virh, press, rho
 
      ELSE IF (Switch.EQ.2) THEN
c     ---print results
         IF (nsamp.EQ.0) THEN
            WRITE (6, 99001)
         ELSE
            WRITE (6, 99002) nsamp
            WRITE (6, 99003) rg/(NPART*nsamp), 
     &                       bl/(nsamp*(ELL-1.D0)*NPART)
            DO bin = 0, BINmax
               WRITE (16, 99004) (bin+0.5D0)/factrg, DBLE(hrg(bin))
     &                           /nsamp
               WRITE (17, 99004) (bin+0.5D0)/factbl, DBLE(hbl(bin))
     &                           /(nsamp*(ELL-1.D0))
            END DO
         END IF
      ELSE
         STOP 'error: switch'
      END IF
      RETURN
99001 FORMAT (' ==========> No samples!!!')
99002 FORMAT (' results based on ', i7, ' samples')
99003 FORMAT (' End-to-end distance : ', f6.3, /, 
     &        ' Average bond length : ', f6.3)
99004 FORMAT (2x, f5.3, 2x, f9.3)
      END
