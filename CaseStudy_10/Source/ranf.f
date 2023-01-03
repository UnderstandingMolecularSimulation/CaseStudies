**==ranf.spg  processed by SPAG 4.52O  at 18:54 on 27 Mar 1996
      FUNCTION RANF(Idum)
c
c     random number generator
c
c     Idum (input): can be used as seed (not used in present
c                   random number generator.
 
      IMPLICIT NONE
      INTEGER Idum
      DOUBLE PRECISION RANF, RCARRY
      RANF = RCARRY()
      RETURN
C ----------------------------------------------------C
      END
**==randx.spg  processed by SPAG 4.52O  at 18:54 on 27 Mar 1996
 
      FUNCTION RANDX(Iseed)
C----------------------------------------------------------------------C
C  Random number generator, fast and rough, machine independent.
C  Returns an uniformly distributed deviate in the 0 to 1 interval.
C  This random number generator is portable, machine-independent and
C  reproducible, for any machine with at least 32 bits / real number.
C  REF: Press, Flannery, Teukolsky, Vetterling, Numerical Recipes (1986)
C----------------------------------------------------------------------C
      IMPLICIT NONE
      INTEGER IA, IC, Iseed, M1
      DOUBLE PRECISION RANDX, RM
      PARAMETER (M1=714025, IA=1366, IC=150889, RM=1.D+0/M1)
c
      Iseed = MOD(IA*Iseed+IC, M1)
      RANDX = Iseed*RM
      IF (RANDX.LT.0.D+0) THEN
         STOP '*** Random number is negative ***'
      END IF
c
      RETURN
      END
**==ranset.spg  processed by SPAG 4.52O  at 18:54 on 27 Mar 1996
 
      SUBROUTINE RANSET(Iseed)
c     --- initializes random number generator
      IMPLICIT NONE
      INTEGER Iseed
 
      CALL RSTART(Iseed)
      RETURN
      END
**==rstart.spg  processed by SPAG 4.52O  at 18:54 on 27 Mar 1996
 
      SUBROUTINE RSTART(Iseeda)
C----------------------------------------------------------------------C
C       Initialize Marsaglia list of 24 random numbers.
C----------------------------------------------------------------------C
      IMPLICIT NONE
      DOUBLE PRECISION CARRY, ran, RANDX, SEED
      INTEGER i, I24, ISEED, Iseeda, J24
      COMMON /RANDOM/ SEED(24), CARRY, I24, J24, ISEED
 
      I24 = 24
      J24 = 10
      CARRY = 0.D+0
      ISEED = Iseeda
c
c       get rid of initial correlations in rand by throwing
c       away the first 100 random numbers generated.
c
      DO i = 1, 100
         ran = RANDX(ISEED)
      END DO
c
c       initialize the 24 elements of seed
c
 
      DO i = 1, 24
         SEED(i) = RANDX(ISEED)
      END DO
 
      RETURN
      END
**==rcarry.spg  processed by SPAG 4.52O  at 18:54 on 27 Mar 1996
 
 
      FUNCTION RCARRY()
C----------------------------------------------------------------------C
C       Random number generator from Marsaglia.
C----------------------------------------------------------------------C
      IMPLICIT NONE
      DOUBLE PRECISION CARRY, RCARRY, SEED, TWOm24, TWOp24, uni
      INTEGER I24, ISEED, J24
      PARAMETER (TWOp24=16777216.D+0, TWOm24=1.D+0/TWOp24)
      COMMON /RANDOM/ SEED(24), CARRY, I24, J24, ISEED
c
c       F. James Comp. Phys. Comm. 60, 329  (1990)
c       algorithm by G. Marsaglia and A. Zaman
c       base b = 2**24  lags r=24 and s=10
c
      uni = SEED(I24) - SEED(J24) - CARRY
      IF (uni.LT.0.D+0) THEN
         uni = uni + 1.D+0
         CARRY = TWOm24
      ELSE
         CARRY = 0.D+0
      END IF
      SEED(I24) = uni
      I24 = I24 - 1
      IF (I24.EQ.0) I24 = 24
      J24 = J24 - 1
      IF (J24.EQ.0) J24 = 24
      RCARRY = uni
 
      RETURN
      END
