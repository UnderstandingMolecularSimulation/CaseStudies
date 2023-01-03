**==toterg.spg  processed by SPAG 4.52O  at 15:46 on 28 Mar 1996
      SUBROUTINE TOTERG(Ener, Vir, Enk)
c
c     calculates total energy
c
c  Ener (output) : total (potential + kinetic) energy
c  Vir  (output) : total virial
c  Enk  (ouput)  : total kinetic energy
c
      IMPLICIT NONE
      INCLUDE 'parameter.inc'
      INCLUDE 'conf.inc'
      INCLUDE 'veloc.inc'
 
      DOUBLE PRECISION xi, yi, zi, Ener, eni, viri, Vir, Enk
      INTEGER i, jb
 
      Ener = 0
      Vir = 0
      Enk = 0
      DO i = 1, NPART
         xi = X(i)
         yi = Y(i)
         zi = Z(i)
         jb = i + 1
         CALL ENERI(xi, yi, zi, i, jb, eni, viri)
         Ener = Ener + eni
         Vir = Vir + viri
         Enk = Enk + (VX(i)*VX(i)+VY(i)*VY(i)+VZ(i)*VZ(i))
      END DO
c     --kinetic energy
      Enk = 0.5D0*Enk
      Ener = Ener + Enk
      RETURN
      END
