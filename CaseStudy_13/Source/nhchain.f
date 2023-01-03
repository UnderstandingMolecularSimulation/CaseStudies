**==nhchain.spg  processed by SPAG 4.52O  at 14:19 on 24 Jun 1996
      SUBROUTINE NHCHAIN(X, Ux, Delt, T, Isamp)
c
c    integrate equations of motion:
c    Nose-Hoover thermostat
c
      IMPLICIT NONE
      DOUBLE PRECISION X, en, fx, Ux, Delt, T, enk
      DOUBLE PRECISION hn, fxx
      INTEGER i, Isamp, iter
      INCLUDE 'nosehoover.inc'
      INCLUDE 'nvt.inc'
      DOUBLE PRECISION a2, a3, a0, qa, ra, fa, uxn, uxo, delth, d1, h1, 
     &                 err, c1, b1
      DOUBLE PRECISION psco(MMAx), g(MMAx), f(MMAx), h(MMAx), e(MMAx), 
     &                 pscn(MMAx), delpsc(MMAx), fsc(MMAx)
      LOGICAL ready
      SAVE fx
      DATA fx/0/
 
c     ---first step velocity Verlet
      fxx = (fx-Ux*PSC(1))
      IF (M.EQ.1) THEN
         fsc(1) = (Ux**2-FH*TEMP)/QH(1)
      ELSE
         fsc(1) = (Ux**2-FH*TEMP)/QH(1) - PSC(1)*PSC(2)
         DO i = 2, M
            IF (i.LT.M) THEN
               fsc(i) = (QH(i-1)*PSC(i-1)**2-TEMP)/QH(i) - PSC(i)
     &                  *PSC(i+1)
            ELSE
               fsc(i) = (QH(i-1)*PSC(i-1)**2-TEMP)/QH(i)
            END IF
         END DO
      END IF
      X = X + Ux*Delt + fxx*Delt*Delt/2
      Ux = Ux + fxx*Delt/2
      DO i = 1, M
         SC(i) = SC(i) + PSC(i)*Delt + fsc(i)*Delt*Delt/2
         PSC(i) = PSC(i) + fsc(i)*Delt/2
      END DO
c     ---determine forces
      CALL FORCE(fx, X, en)
 
c     ---second step velocity Verlet
      IF (M.EQ.1) THEN
c        -- Solve a0 ux**3 + a2 ux + a3 = 0
         a0 = Delt**2/(4*QH(1))
         a2 = 1 + (Delt/2)*(PSC(1)-TEMP*FH*Delt/(2*QH(1)))
         a2 = a2/a0
         a3 = (-Ux-fx*Delt/2)/a0
         qa = -a2/3
         ra = a3/2
         fa = (ABS(ra)+SQRT(ra*ra-qa*qa*qa))**(1.D0/3.D0)
         Ux = -SIGN(1.D0, ra)*(fa+qa/fa)
         PSC(1) = PSC(1) + (Ux**2-FH*TEMP)*Delt/(2*QH(1))
      ELSE
c        ---
         uxn = Ux
         DO i = 1, M
            pscn(i) = PSC(i)
         END DO
         err = 1.D-10
         delth = Delt/2
         ready = .FALSE.
         iter = 0
c        ---start iteration
         DO WHILE (.NOT.ready.AND.iter.LE.100)
            iter = iter + 1
            uxo = uxn
            DO i = 1, M
               psco(i) = pscn(i)
            END DO
c           ---coefficients of the M x M tridiagonal Matrix
            g(1) = -psco(2)*delth - 1
            e(1) = -psco(1)*delth
            h(1) = -(PSC(1)+((uxo**2-FH*TEMP)/QH(1)-psco(1)*psco(2))
     &             *delth-psco(1))
            DO i = 2, M - 1
               f(i) = Delt*QH(i-1)*psco(i-1)/QH(i)
               g(i) = -psco(i+1)*delth - 1
               e(i) = -psco(i)*delth
               h(i) = -(PSC(i)+((QH(i-1)*psco(i-1)**2-TEMP)/QH(i)-psco(i
     &                )*psco(i+1))*delth-psco(i))
            END DO
            g(M) = -1
            f(M) = Delt*QH(M-1)*psco(M-1)/QH(M)
            h(M) = -(PSC(M)+((QH(M-1)*psco(M-1)**2-TEMP)/QH(M))
     &             *delth-psco(M))
            d1 = -psco(1)*delth - 1
            c1 = -uxn*delth
            b1 = Delt*uxn/QH(1)
            g(1) = g(1) - b1*c1/d1
            h1 = Ux + (fx-psco(1)*uxo)*delth - uxo
            h(1) = h(1) - b1*h1/d1
c           ---solve
            CALL TRIDAG(f, g, e, h, delpsc, M)
            DO i = 1, M
               pscn(i) = psco(i) + delpsc(i)
            END DO
            uxn = uxo + (-h1-c1*delpsc(1))/d1
c           ---test for convergence
            ready = .TRUE.
            i = 0
            DO WHILE (ready.AND.i.LE.M)
               IF (i.EQ.0) THEN
                  IF (ABS((uxn-uxo)/uxn).GT.err) ready = .FALSE.
               ELSE
                  IF (ABS((pscn(i)-psco(i))/pscn(i)).GT.err)
     &                ready = .FALSE.
               END IF
               i = i + 1
            END DO
         END DO
         DO i = 1, M
            PSC(i) = pscn(i)
         END DO
         Ux = uxn
      END IF
      enk = 0.5D0*Ux**2
      T = T + Delt
      hn = en + enk
      DO i = 1, M
         IF (i.EQ.1) THEN
            hn = hn + PSC(i)*PSC(i)*QH(i)/2 + FH*TEMP*SC(i)
         ELSE
            hn = hn + PSC(i)*PSC(i)*QH(i)/2 + TEMP*SC(i)
         END IF
      END DO
      IF (MOD(INT(T/Delt),Isamp).EQ.0)
     &     WRITE (21, '(2(2x,f8.4),10(2x,f10.5))') X, Ux, hn
      RETURN
      END
