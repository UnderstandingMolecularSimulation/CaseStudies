**==solve.spg  processed by SPAG 4.52O  at 09:34 on 20 Jun 1996
 
      SUBROUTINE SOLVE(Switch, Fx, Fy, Fz, Enkin, Delt, Iseed, Temp)
c
c     using Velocity Verlet algorithm: Swope et al JCP 76 (1982) 637
c
c     switch = 1 : update position + first step velocity
c     switch = 2 : update second step velocity
 
      IMPLICIT NONE
      INCLUDE 'parameter.inc'
      INCLUDE 'conf.inc'
      INCLUDE 'veloc.inc'
 
      DOUBLE PRECISION Fx(*), Fy(*), Fz(*), v2, Enkin, Delt, delth, 
     &                 delt2, sigma, GASDEV, Temp, RANF
      INTEGER i, Switch, Iseed
 
      IF (Switch.EQ.1) THEN
c        --input: force and velocity at time t
c        --out: position time t+1 and part velocity
c        ===solve equations of motion
         delt2 = Delt*Delt/2
         delth = Delt/2
         DO i = 1, NPART
            X(i) = X(i) + Delt*VX(i) + delt2*Fx(i)
            Y(i) = Y(i) + Delt*VY(i) + delt2*Fy(i)
            Z(i) = Z(i) + Delt*VZ(i) + delt2*Fz(i)
            VX(i) = VX(i) + delth*Fx(i)
            VY(i) = VY(i) + delth*Fy(i)
            VZ(i) = VZ(i) + delth*Fz(i)
         END DO
      ELSE IF (Switch.EQ.2) THEN
c        ---final update velocity
         delth = Delt/2
         v2 = 0.D0
         DO i = 1, NPART
            VX(i) = VX(i) + delth*Fx(i)
            VY(i) = VY(i) + delth*Fy(i)
            VZ(i) = VZ(i) + delth*Fz(i)
            v2 = v2 + VX(i)**2 + VY(i)**2 + VZ(i)**2
         END DO
         Enkin = v2/2
c        ===perform collisions with Heat bath
         sigma = SQRT(Temp)
         DO i = 1, NPART
c           ---test for collision
            IF (RANF(Iseed).LT.NU*Delt) THEN
c           ---particle has a collision
               VX(i) = GASDEV(sigma, Iseed)
               VY(i) = GASDEV(sigma, Iseed)
               VZ(i) = GASDEV(sigma, Iseed)
            END IF
         END DO
      ELSE
         STOP 'error switch'
      END IF
      RETURN
      END
