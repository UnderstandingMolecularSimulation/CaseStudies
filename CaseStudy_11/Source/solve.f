**==solve.spg  processed by SPAG 4.52O  at 11:24 on 20 Jun 1996
 
      SUBROUTINE SOLVE(Switch, Fx, Fy, Fz, Enkin, Delt, Iseed, Temp, 
     &                 Enpot, H)
c
c     using Velocity Verlet algorithm: Swope et al JCP 76 (1982) 637
c
c     switch = 1 : update position + first step velocity
c     switch = 2 : update second step velocity
 
      IMPLICIT NONE
      INCLUDE 'parameter.inc'
      INCLUDE 'conf.inc'
      INCLUDE 'veloc.inc'
 
      DOUBLE PRECISION Fx(*), Fy(*), Fz(*), Enkin, Delt, delth, delt2, 
     &                 Temp, vxn(NPMax), vyn(NPMax), vzn(NPMax), 
     &                 vxo(NPMax), vyo(NPMax), vzo(NPMax), pso, psn, 
     &                 err, sumv2, Enpot, H, ri, di, bx(NPMax), 
     &                 by(NPMax), bz(NPMax), delps
      INTEGER i, Switch, Iseed, iter
      LOGICAL ready
 
      IF (Switch.EQ.1) THEN
c        --input: force and velocity at time t
c        --out: position time t+1 and part velocity
c        ===solve equations of motion
         delt2 = Delt*Delt/2
         delth = Delt/2
         sumv2 = 0
         DO i = 1, NPART
            X(i) = X(i) + Delt*VX(i) + delt2*(Fx(i)-PS*VX(i))
            Y(i) = Y(i) + Delt*VY(i) + delt2*(Fy(i)-PS*VY(i))
            Z(i) = Z(i) + Delt*VZ(i) + delt2*(Fz(i)-PS*VZ(i))
            sumv2 = sumv2 + VX(i)**2 + VY(i)**2 + VZ(i)**2
            VX(i) = VX(i) + delth*(Fx(i)-PS*VX(i))
            VY(i) = VY(i) + delth*(Fy(i)-PS*VY(i))
            VZ(i) = VZ(i) + delth*(Fz(i)-PS*VZ(i))
         END DO
         S = S + PS*Delt + (sumv2-G*Temp)*delt2/Q
         PS = PS + (sumv2-G*Temp)*delth/Q
      ELSE IF (Switch.EQ.2) THEN
c        ---final update velocity
         delth = Delt/2
         err = 1.D-10
         sumv2 = 0
         DO i = 1, NPART
            vxn(i) = VX(i)
            vyn(i) = VY(i)
            vzn(i) = VZ(i)
            sumv2 = sumv2 + vxn(i)*vxn(i) + vyn(i)*vyn(i) + vzn(i)
     &              *vzn(i)
         END DO
         psn = PS
         ready = .FALSE.
         iter = 0
         DO WHILE (.NOT.ready.AND.iter.LT.100)
            iter = iter + 1
            pso = psn
            delps = 0
            DO i = 1, NPART
               vxo(i) = vxn(i)
               vyo(i) = vyn(i)
               vzo(i) = vzn(i)
               bx(i) = -delth*(Fx(i)-pso*vxo(i)) - (VX(i)-vxo(i))
               ri = vxo(i)*Delt/Q
               delps = delps + ri*bx(i)
               by(i) = -delth*(Fy(i)-pso*vyo(i)) - (VY(i)-vyo(i))
               ri = vyo(i)*Delt/Q
               delps = delps + ri*by(i)
               bz(i) = -delth*(Fz(i)-pso*vzo(i)) - (VZ(i)-vzo(i))
               ri = vzo(i)*Delt/Q
               delps = delps + ri*bz(i)
            END DO
            di = -(pso*delth+1)
            delps = delps - di*((-sumv2+G*Temp)*delth/Q-(PS-pso))
            delps = delps/(-Delt*delth*sumv2/Q+di)
            sumv2 = 0
            DO i = 1, NPART
               vxn(i) = vxn(i) + (bx(i)+delth*vxo(i)*delps)/di
               vyn(i) = vyn(i) + (by(i)+delth*vyo(i)*delps)/di
               vzn(i) = vzn(i) + (bz(i)+delth*vzo(i)*delps)/di
               sumv2 = sumv2 + vxn(i)*vxn(i) + vyn(i)*vyn(i) + vzn(i)
     &                 *vzn(i)
            END DO
            psn = pso + delps
c           ---test for convergence
            ready = .TRUE.
            i = 0
            DO WHILE (i.LE.NPART.AND.ready)
               i = i + 1
               IF (i.LE.NPART) THEN
                  IF (ABS((vxn(i)-vxo(i))/vxn(i)).GT.err)
     &                ready = .FALSE.
                  IF (ABS((vyn(i)-vyo(i))/vyn(i)).GT.err)
     &                ready = .FALSE.
                  IF (ABS((vzn(i)-vzo(i))/vzn(i)).GT.err)
     &                ready = .FALSE.
               ELSE
                  IF (ABS((psn-pso)/psn).GT.err) ready = .FALSE.
               END IF
            END DO
         END DO
         DO i = 1, NPART
            VX(i) = vxn(i)
            VY(i) = vyn(i)
            VZ(i) = vzn(i)
         END DO
         PS = psn
         Enkin = sumv2/2
         H = Enkin + Enpot + (PS**2*Q)/2 + G*Temp*S
      ELSE
         STOP 'error switch'
      END IF
      RETURN
      END
