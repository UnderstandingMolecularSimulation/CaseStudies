**==solve.spg  processed by SPAG 4.52O  at 15:46 on 28 Mar 1996
      SUBROUTINE SOLVE(Fx, Fy, Fz, Enkin, Delt)
c
c   Solve the equations of motion
c
c  Fx    (input) array: x component of the force acting on the particles
c  Fy    (input) array: y component of the force acting on the particles
c  Fz    (input) array: z component of the force acting on the particles
c  Enkin (ouput)      : total kinetic energy
c  Delt  (input)      : time step MD simulation
c
 
      IMPLICIT NONE
      INCLUDE 'parameter.inc'
      INCLUDE 'conf.inc'
      INCLUDE 'veloc.inc'
      DOUBLE PRECISION Fx(*), Fy(*), Fz(*), v2, vxt, vyt, vzt, Enkin, 
     &                 Delt
      INTEGER i
 
      v2 = 0.D0
c     ===solve equations of motion
      DO i = 1, NPART
c        ===leap frog alogithm
         vxt = VX(i)
         vyt = VY(i)
         vzt = VZ(i)
         VX(i) = VX(i) + Delt*Fx(i)
         VY(i) = VY(i) + Delt*Fy(i)
         VZ(i) = VZ(i) + Delt*Fz(i)
         X(i) = X(i) + Delt*VX(i)
         Y(i) = Y(i) + Delt*VY(i)
         Z(i) = Z(i) + Delt*VZ(i)
         v2 = v2 + (VX(i)+vxt)**2/4 + (VY(i)+vyt)**2/4 + (VZ(i)+vzt)
     &        **2/4
      END DO
      Enkin = v2/2
      RETURN
      END
