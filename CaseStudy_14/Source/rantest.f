**==rantest.spg  processed by SPAG 4.52O  at 10:35 on 12 Jul 1996
      SUBROUTINE RANTEST(Iseed)
c     ---test and initialize the random number generator
      IMPLICIT NONE
      INTEGER Iseed, i
      DOUBLE PRECISION RANF
 
      CALL RANSET(0)
      PRINT *, ' ******** test random numbers ***********'
      DO i = 1, 5
         PRINT *, ' i,ranf() ', i, RANF()
      END DO
      RETURN
      END
