**==sample.spg  processed by SPAG 4.52O  at 09:34 on 20 Jun 1996
      SUBROUTINE SAMPLE(Switch, Is, En, Vir, Enk, Delt)
c     writes quantities to file
      IMPLICIT NONE
      INCLUDE 'parameter.inc'
      INCLUDE 'conf.inc'
      INCLUDE 'system.inc'
      INCLUDE 'potential.inc'
      INCLUDE 'veloc.inc'
      INCLUDE 'samp.inc'
      INTEGER Is, Switch, NHIsmax, i, j, ig, nhgr, TMAx, t, T0Max, 
     &        tvacf, ngr, t0, dt
      PARAMETER (NHIsmax=250, TMAx=5000, T0Max=100)
      INTEGER nvacf(TMAx), tstress0, tstress, nstress(TMAx), tt0(T0Max), 
     &        ttel, ttv0(T0Max)
      DOUBLE PRECISION En, enp, Vir, press, vol, rho, Enk, temp
      DOUBLE PRECISION tempav, delg, delgi, g(NHIsmax), r, e, p, r6i, 
     &                 r2, dx, dy, dz, xi, yi, zi, dr, Delt, CORP
      DOUBLE PRECISION vxt0(NPMax, T0Max), vyt0(NPMax, T0Max), 
     &                 vzt0(NPMax, T0Max), vacf(TMAx), dif, vtime, dtime
      DOUBLE PRECISION r2t(TMAx), rx0(NPMax, T0Max), ry0(NPMax, T0Max), 
     &                 rz0(NPMax, T0Max), ri(NHIsmax)
      DOUBLE PRECISION sxyt(TMAx), sxzt(TMAx), syzt(TMAx), sxy0(T0Max), 
     &                 sxz0(T0Max), syz0(T0Max), sxy, sxz, syz, 
     &                 dstresstime, ethaxy, ethaxz, ethayz, dstime, fact
 
      DOUBLE PRECISION fy, fz, virij, fr, r2i, sxy00, sxz00, syz00, ent, 
     &                 factemp, factvel, v0, factvel2, tempavv, tempav2, 
     &                 v2av, v4av, v2, sum, rc
      INTEGER itt, binvel(NHIsmax), bintemp(NHIsmax), ivel, itemp, 
     &        binvel2(NHIsmax)
      SAVE ngr, tempav, delg, delgi, g, nhgr
      SAVE vacf, nvacf, t0, vxt0, vyt0, vzt0, tvacf, dtime, r2t, rx0, 
     &   ry0, rz0, tt0, ttv0
      SAVE sxyt, sxzt, syzt, sxy0, sxz0, syz0, tstress0, dstresstime, 
     &   nstress, sxy00, sxz00, syz00, tstress
      SAVE itt, binvel, bintemp, factemp, factvel, v0, binvel2, 
     &   factvel2, tempavv, tempav2, v2av, v4av
 
      IF (Switch.EQ.1) THEN
c        ---Sample averages
         IF (NPART.NE.0) THEN
            enp = (En-Enk)/DBLE(NPART)
            ent = En/NPART
            temp = 2*Enk/DBLE(3*NPART)
            vol = BOX**3
            rho = NPART/vol
            rc = SQRT(RC2)
            press = rho*temp + Vir/(3.D0*vol) + CORP(rc, rho)
c           ---temperature fluctuations
            itemp = INT(temp*factemp)
            bintemp(itemp) = bintemp(itemp) + 1
            DO i = 1, NPART
               ivel = INT((VX(i)+v0)*factvel)
               binvel(ivel) = binvel(ivel) + 1
               ivel = INT((VY(i)+v0)*factvel)
               binvel(ivel) = binvel(ivel) + 1
               ivel = INT((VZ(i)+v0)*factvel)
               binvel(ivel) = binvel(ivel) + 1
 
               v2 = VX(i)**2 + VY(i)**2 + VZ(i)**2
               v2av = v2av + v2
               v4av = v4av + v2*v2
               ivel = INT(v2*factvel2) + 1
               binvel2(ivel) = binvel2(ivel) + 1
            END DO
            itt = itt + 1
            tempavv = tempavv + temp
            tempav2 = tempav2 + temp*temp
         ELSE
            rho = 0.D0
            enp = 0.D0
            press = 0.D0
         END IF
c         WRITE (66, *) Is, SNGL(ent), SNGL(enp), SNGL(enk/npart)
         WRITE (66, *) Is, SNGL(temp), SNGL(press), SNGL(enp)
         IF (MOD(Is,IGR).EQ.0) THEN
c           ---sample radial distribution function and stress tensor
            ngr = ngr + 1
            tempav = tempav + temp
            sxy = 0
            sxz = 0
            syz = 0
            DO i = 1, NPART
               xi = X(i)
               yi = Y(i)
               zi = Z(i)
               DO j = i + 1, NPART
                  dx = xi - X(j)
                  dy = yi - Y(j)
                  dz = zi - Z(j)
c                 ---periodic boundary conditions
                  dx = dx - BOX*ANINT(dx/BOX)
                  dy = dy - BOX*ANINT(dy/BOX)
                  dz = dz - BOX*ANINT(dz/BOX)
                  r2 = dx*dx + dy*dy + dz*dz
                  IF (r2.LE.HBOX*HBOX) THEN
                     r = SQRT(r2)
                     ig = INT(r*delgi)
                     g(ig) = g(ig) + 1
c                    ---stress tensor
                     IF (r2.LE.RC2) THEN
                        r2i = 1/r2
                        r6i = r2i*r2i*r2i
                        virij = 48*(r6i*r6i-0.5D0*r6i)
                        fr = -virij*r2i
                        fy = fr*dy
                        fz = fr*dz
                        sxy = sxy + dx*fy
                        sxz = sxz + dx*fz
                        syz = syz + dy*fz
                     END IF
                  END IF
               END DO
               sxy = sxy + VX(i)*VY(i)
               sxz = sxz + VX(i)*VZ(i)
               syz = syz + VY(i)*VZ(i)
            END DO
            sxy00 = sxy00 + sxy*sxy
            sxz00 = sxz00 + sxz*sxz
            syz00 = syz00 + syz*syz
c           ---sample stress tensor correlation function
            tstress = tstress + 1
            IF (MOD(tstress,ITSTRESS0).EQ.0) THEN
c            --new t=0
               tstress0 = tstress0 + 1
               ttel = MOD(tstress0-1, T0Max) + 1
               tt0(ttel) = tstress
               sxy0(ttel) = sxy
               sxz0(ttel) = sxz
               syz0(ttel) = syz
            END IF
            DO t = 1, MIN(tstress0, T0Max)
               dt = tstress - tt0(t) + 1
               IF (dt.LT.TMAx) THEN
                  nstress(dt) = nstress(dt) + 1
                  sxyt(dt) = sxyt(dt) + sxy*sxy0(t)
                  sxzt(dt) = sxzt(dt) + sxz*sxz0(t)
                  syzt(dt) = syzt(dt) + syz*syz0(t)
               END IF
            END DO
         END IF
         IF (MOD(Is,NTVACF).EQ.0) THEN
            tvacf = tvacf + 1
c           ---sample velocity auto correlation function and
c            --mean square displacement
            IF (MOD(tvacf,IT0).EQ.0) THEN
c            --new t=0
               t0 = t0 + 1
               ttel = MOD(t0-1, T0Max) + 1
               ttv0(ttel) = tvacf
               DO i = 1, NPART
                  rx0(i, ttel) = X(i)
                  ry0(i, ttel) = Y(i)
                  rz0(i, ttel) = Z(i)
                  vxt0(i, ttel) = VX(i)
                  vyt0(i, ttel) = VY(i)
                  vzt0(i, ttel) = VZ(i)
               END DO
            END IF
            DO t = 1, MIN(t0, T0Max)
               dt = tvacf - ttv0(t) + 1
               IF (dt.LT.TMAx) THEN
                  nvacf(dt) = nvacf(dt) + 1
                  DO i = 1, NPART
                     vacf(dt) = vacf(dt) + VX(i)*vxt0(i, t) + VY(i)
     &                          *vyt0(i, t) + VZ(i)*vzt0(i, t)
                     r2t(dt) = r2t(dt) + (X(i)-rx0(i,t))
     &                         **2 + (Y(i)-ry0(i,t))
     &                         **2 + (Z(i)-rz0(i,t))**2
                  END DO
               END IF
            END DO
         END IF
      ELSE IF (Switch.EQ.0) THEN
c        ---Initialize
c        ---radial distribution function:
         ngr = 0
         nhgr = NHIsmax
         delg = HBOX/nhgr
         delgi = 1/delg
         DO i = 1, nhgr
            g(i) = 0
c           ---fluctuations temp and vel.
            bintemp(i) = 0
            binvel(i) = 0
            binvel2(i) = 0
         END DO
         itt = 0
         v0 = 10.D0
         factemp = NHIsmax/5.D0
         factvel = NHIsmax/(6*v0)
         factvel2 = NHIsmax/v0**2
         tempavv = 0
         tempav2 = 0
         v2av = 0
         v4av = 0
c        ---stress tensor
         tstress0 = 0
         dstresstime = NSAMP*Delt*IGR
         sxy00 = 0
         sxz00 = 0
         syz00 = 0
 
c        ---velocity auto-correlation function and
c         --mean square displacement
         t0 = 0
         tvacf = 0
         dtime = NSAMP*Delt*NTVACF
         DO i = 1, TMAx
            r2t(i) = 0
            nvacf(i) = 0
            vacf(i) = 0
            nstress(i) = 0
            sxyt(i) = 0
            sxzt(i) = 0
            syzt(i) = 0
         END DO
 
      ELSE IF (Switch.EQ.2) THEN
c        ---write results to file
c        ---radial distribution function
         IF (ngr.NE.0) THEN
            rho = NPART/BOX**3
            tempav = tempav/ngr
            e = 0
            p = 0
            DO i = 1, nhgr
               r = (i+0.5D0)*delg
               dr = 4*PI*delg**3*((i+1)**3-i**3)/3
               g(i) = 2*g(i)/(ngr*dr*rho*NPART)
               WRITE (IOUT1, *) r, g(i)
               IF (r*r.LT.RC2) THEN
                  r6i = 1/r**6
                  e = e + g(i)*2*PI*rho*r**2*(4*(r6i*r6i-r6i)-ECUT)*delg
                  p = p + g(i)*2*PI*rho**2*r**2*48*(r6i*r6i-0.5D0*r6i)
     &                *delg/3
                  g(i) = g(i)*2*PI*rho*r**2*(4*(r6i*r6i-r6i)-ECUT)
               ELSE
                  g(i) = 0
               END IF
               ri(i) = i*delg
            END DO
            WRITE (6, 99001) ngr, tempav, e, p + rho*tempav
         END IF
c        ---velocity auto-correlation function
         IF (tvacf.NE.0) THEN
            dif = 0
            DO i = 1, TMAx
               vtime = dtime*(i-1)
               IF (nvacf(i).NE.0) THEN
                  vacf(i) = vacf(i)/(NPART*nvacf(i))
                  r2t(i) = r2t(i)/(NPART*nvacf(i))
                  dif = dif + vacf(i)*dtime
                  WRITE (IOUT2, *) SNGL(vtime), SNGL(vacf(i)), 
     &                             SNGL(r2t(i)), SNGL(dif/3)
               END IF
            END DO
            WRITE (6, 99002) tvacf, t0, dtime, dtime*IT0, dif/3
         END IF
c        ---stress tensor
         IF (tstress.NE.0) THEN
            ethaxy = 0
            ethaxz = 0
            ethayz = 0
            vol = BOX**3
            DO i = 1, TMAx
               dstime = dstresstime*(i-1)
               IF (nstress(i).NE.0) THEN
                  fact = 1/(tempav*vol*nstress(i))
                  sxyt(i) = sxyt(i)*fact
                  sxzt(i) = sxzt(i)*fact
                  syzt(i) = syzt(i)*fact
                  ethaxy = ethaxy + sxyt(i)*dstresstime
                  ethaxz = ethaxz + sxzt(i)*dstresstime
                  ethayz = ethayz + syzt(i)*dstresstime
                  WRITE (IOUT3, '(10(2x,f8.3))') SNGL(dstime), 
     &                   SNGL(sxyt(i)), SNGL(sxzt(i)), SNGL(syzt(i)), 
     &                   SNGL(sxyt(i)+sxzt(i)+syzt(i))/3, 
     &                   SNGL((ethaxy+ethaxz+ethayz)/3)
               END IF
            END DO
            WRITE (6, 99003) ethaxy, ethaxz, ethayz, 
     &                       (ethaxy+ethaxz+ethayz)/3
            PRINT *, SNGL(sxy00/(tempav*vol*ngr)), SNGL(sxyt(1))
            PRINT *, SNGL(sxz00/(tempav*vol*ngr)), SNGL(sxzt(1))
            PRINT *, SNGL(syz00/(tempav*vol*ngr)), SNGL(syzt(1))
         END IF
         sum = 0
         IF (itt.NE.0) THEN
c           ---temp fluc
            PRINT *, ' itt samples : ', itt
            sum = 0
            DO i = 1, NHIsmax
               sum = sum + (binvel(i)/DBLE(NPART*itt*3))/factvel
            END DO
            sum = 1/sum
            DO i = 1, NHIsmax
               IF (bintemp(i).NE.0) THEN
                  WRITE (IOUT3+1, '(2(2x,f8.3))') (i+0.5D0)/factemp, 
     &                   (bintemp(i)/DBLE(itt))/factemp
               END IF
               IF (binvel(i).NE.0) THEN
                  WRITE (IOUT3+2, '(2(2x,f8.3))') (i+0.5D0)/factvel - 
     &                   v0, sum*(binvel(i)/DBLE(NPART*itt*3))
               END IF
               IF (binvel2(i).NE.0) THEN
                  WRITE (IOUT3+4, '(2(2x,f10.3))') (i-0.5D0)/factvel2, 
     &                   (binvel2(i)/DBLE(NPART*itt))/factvel2
               END IF
            END DO
            tempavv = tempavv/itt
            PRINT *, ' temperature ', SNGL(tempavv)
            tempav2 = tempav2/itt
            PRINT *, ' sigma(T) ', 
     &            SNGL(SQRT((tempav2-tempavv**2)/tempavv**2)), 
     &            SNGL(SQRT(NPART*(tempav2-tempavv**2)/tempavv**2))
            v2av = v2av/(itt*NPART)
            v4av = v4av/(itt*NPART)
            PRINT *, ' Velocity v^2 av', SNGL(v2av)
            PRINT *, ' sigma(v) ', SNGL(SQRT((v4av-v2av**2)/v2av**2))
            PRINT *, ' sum ', sum
         END IF
      ELSE
         STOP 'Error (sample.f) switch'
      END IF
 
      RETURN
99001 FORMAT (' Energy and pressure calculated from g(r) ', /, 
     &        '     Number of samples     : ', i8, /, 
     &        '     Average temperature   : ', f8.3, /, 
     &        '     Energy                : ', f8.3, /, 
     &        '     Pressure              : ', f8.3, /)
99002 FORMAT (
     &       ' Velocity auto correlation function and mean square dis.:'
     &       , /, '   Number of samples       : ', i8, /, 
     &       '   Number of t=0           : ', i8, /, 
     &       '   Timestep between samples: ', f8.3, /, 
     &       '   Timestep between t=0    : ', f8.5, /, 
     &       '   Diffusion coef.         : ', f8.5)
99003 FORMAT (' Shear viscosity: ', /, '  xy component : ', f12.3, /, 
     &        '  xz component : ', f12.3, /, '  yz component : ', f12.3, 
     &        /, '  Average      : ', f12.3, /)
 
      END
