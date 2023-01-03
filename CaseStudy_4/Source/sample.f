**==sample.spg  processed by SPAG 4.52O  at 15:46 on 28 Mar 1996
      SUBROUTINE SAMPLE(Switch, Is, En, Vir, Enk, Delt)
c
c     Samples averages:
c        a) density, pressure, potenial energy
c        b) stress tensor correlation functions
c        c) velocity autocorrelation function
c        d) mean square displacement (conventional algorithm)
c
c    Switch (input) = 1: sample averages
c                   = 0: initialize varibales
c                   = 2: writes results to disk
c    Is      = total number of time steps since start simulation
c    En     (input) total energy (potenial + kinetic)
c    Vir    (input) total virial
c    Delt   (input) time step md simulation
c
      IMPLICIT NONE
      INCLUDE 'parameter.inc'
      INCLUDE 'conf.inc'
      INCLUDE 'system.inc'
      INCLUDE 'potential.inc'
      INCLUDE 'veloc.inc'
      INCLUDE 'samp.inc'
      INTEGER Is, Switch, NHIsmax, i, j, ig, nhgr, TMAx, t, T0Max, 
     &        tvacf, ngr, t0, dt, iout, nbl, ihbmax
      PARAMETER (NHIsmax=250, TMAx=1000, T0Max=200)
      INTEGER nvacf(TMAx), tstress0, tstress, nstress(TMAx), tt0(T0Max), 
     &        ttel, ttv0(T0Max)
      DOUBLE PRECISION En, enp, Vir, press, vol, rho, Enk, temp
      DOUBLE PRECISION tempav, delg, delgi, g(NHIsmax), r, e, p, r6i, 
     &                 r2, dx, dy, dz, xi, yi, zi, dr, Delt, r2asum, r2a
      DOUBLE PRECISION vxt0(NPMax, T0Max), vyt0(NPMax, T0Max), 
     &                 vzt0(NPMax, T0Max), vacf(TMAx), dif, vtime, dtime
      DOUBLE PRECISION r2t(TMAx), rx0(NPMax, T0Max), ry0(NPMax, T0Max), 
     &                 rz0(NPMax, T0Max), thmax
      DOUBLE PRECISION sxyt(TMAx), sxzt(TMAx), syzt(TMAx), sxy0(T0Max), 
     &                 sxz0(T0Max), syz0(T0Max), sxy, sxz, syz, 
     &                 dstresstime, ethaxy, ethaxz, ethayz, dstime, 
     &                 fact, tau0, tauc, errvacf
 
      DOUBLE PRECISION fy, fz, virij, fr, r2i, sxy00, sxz00, syz00
      SAVE ngr, tempav, delg, delgi, g, nhgr
      SAVE vacf, nvacf, t0, vxt0, vyt0, vzt0, tvacf, dtime, r2t, rx0, 
     &   ry0, rz0, tt0, ttv0
      SAVE sxyt, sxzt, syzt, sxy0, sxz0, syz0, tstress0, dstresstime, 
     &   nstress, sxy00, sxz00, syz00, tstress
 
      IF (Switch.EQ.1) THEN
c        ---Sample averages
         IF (NPART.NE.0) THEN
            enp = (En-Enk)/DBLE(NPART)
            temp = 2*Enk/DBLE(3*NPART)
            vol = BOX**3
            rho = NPART/vol
            press = rho*temp + Vir/(3.D0*vol)
         ELSE
            rho = 0.D0
            enp = 0.D0
            press = 0.D0
         END IF
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
               IF (dt.LT.TMAx.AND.dt*dtime.LE.TDIFMAX) THEN
                  nvacf(dt) = nvacf(dt) + 1
                  r2asum = 0
                  DO i = 1, NPART
                     vacf(dt) = vacf(dt) + VX(i)*vxt0(i, t) + VY(i)
     &                          *vyt0(i, t) + VZ(i)*vzt0(i, t)
                     r2a = (X(i)-rx0(i,t))**2 + (Y(i)-ry0(i,t))
     &                     **2 + (Z(i)-rz0(i,t))**2
                     r2t(dt) = r2t(dt) + r2a
                     r2asum = r2asum + r2a
                  END DO
                  r2asum = r2asum/NPART
c                 ---print mean square displacement to file for t=1,10,100,etc
                  nbl = 1
                  iout = 49
                  DO WHILE (nbl.LE.dt)
                     iout = iout + 1
                     IF (nbl-dt+1.EQ.0) WRITE (iout, *) (dt-1)*dtime, 
     &                   r2asum
                     nbl = 10*nbl
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
         END DO
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
            END DO
            WRITE (6, 99003) ngr, tempav, e, p + rho*tempav
         END IF
c        ---velocity auto-correlation function
         IF (tvacf.NE.0) THEN
c           ---correlation time (for error estimate)
            tauc = 0
            DO i = 1, TMAx
               IF (nvacf(i)*NPART.NE.0) THEN
                  tauc = tauc + (vacf(i)/(NPART*nvacf(i)))**2*dtime
               END IF
            END DO
c           ---normalisation
            IF (nvacf(1)*NPART.NE.0)
     &          tauc = tauc/(vacf(1)/(NPART*nvacf(1))**2)
            WRITE (6, 99002) tauc
            dif = 0
c           ---total averaging time:
            tau0 = dtime*IT0*t0
            errvacf = SQRT(2*tauc*(vacf(1)/(NPART*nvacf(1))**2/tau0))
            thmax = 0
            ihbmax = 0
            DO i = 1, TMAx
               vtime = dtime*(i-1)
               IF (nvacf(i).NE.0) THEN
                  vacf(i) = vacf(i)/(NPART*nvacf(i))
                  r2t(i) = r2t(i)/(NPART*nvacf(i))
                  dif = dif + vacf(i)*dtime
                  WRITE (IOUT2, *) SNGL(vtime), SNGL(vacf(i)), 
     &                             SNGL(r2t(i)), SNGL(dif/3)
                  IF (MOD(i,60).EQ.0) WRITE (IOUT4, *) SNGL(vtime), 
     &                SNGL(vacf(i)), SNGL(errvacf)
                  IF (vtime.GT.thmax) THEN
                     ihbmax = nvacf(i)
                     thmax = vtime
                  END IF
               END IF
            END DO
            WRITE (6, 99004) tvacf, t0, dtime, dtime*IT0, dif/3
 
            WRITE (6, *) 
     &                 ' Diffusion calculated with conventional scheme '
            WRITE (6, 99001) 2*dtime, nvacf(3), thmax, ihbmax
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
            WRITE (6, 99005) ethaxy, ethaxz, ethayz, 
     &                       (ethaxy+ethaxz+ethayz)/3
            PRINT *, SNGL(sxy00/(tempav*vol*ngr)), SNGL(sxyt(1))
            PRINT *, SNGL(sxz00/(tempav*vol*ngr)), SNGL(sxzt(1))
            PRINT *, SNGL(syz00/(tempav*vol*ngr)), SNGL(syzt(1))
         END IF
      ELSE
         STOP 'Error (sample.f) switch'
      END IF
 
      RETURN
99001 FORMAT (' Number of samples for tmin = ', f8.3, ' is : ', i10, /, 
     &        ' Number of samples for tmax = ', f8.3, ' is : ', i10)
99002 FORMAT ('  Decorrelation time ', e12.4)
99003 FORMAT (' Energy and pressure calculated from g(r) ', /, 
     &        '     Number of samples     : ', i8, /, 
     &        '     Average temperature   : ', f8.3, /, 
     &        '     Energy                : ', f8.3, /, 
     &        '     Pressure              : ', f8.3, /)
99004 FORMAT (
     &       ' Velocity auto correlation function and mean square dis.:'
     &       , /, '   Number of samples       : ', i8, /, 
     &       '   Number of t=0           : ', i8, /, 
     &       '   Timestep between samples: ', f8.3, /, 
     &       '   Timestep between t=0    : ', f8.5, /, 
     &       '   Diffusion coef.         : ', f8.5)
99005 FORMAT (' Shear viscosity: ', /, '  xy component : ', f12.3, /, 
     &        '  xz component : ', f12.3, /, '  yz component : ', f12.3, 
     &        /, '  Average      : ', f12.3, /)
 
      END
