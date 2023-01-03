**==ranor.spg  processed by SPAG 4.52O  at 11:03 on 18 Oct 1996
      SUBROUTINE RANOR(Bx, By, Bz)
c ---generate vector random on unit sphere
      IMPLICIT NONE
      DOUBLE PRECISION Bx, By, Bz, RANF, ransq, ranh
 
      ransq = 2
      DO WHILE (ransq.GE.1)
         Bx = 1 - 2*RANF()
         By = 1 - 2*RANF()
         ransq = Bx*Bx + By*By
      END DO
      ranh = 2*SQRT(1-ransq)
      Bx = Bx*ranh
      By = By*ranh
      Bz = (1-2*ransq)
      RETURN
c-----------------------------------------------------------c
      END
