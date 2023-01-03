**==tridag.spg  processed by SPAG 4.52O  at 14:19 on 24 Jun 1996
      SUBROUTINE TRIDAG(A, B, C, R, U, N)
      IMPLICIT NONE
      INTEGER N, NMAx
      DOUBLE PRECISION A(N), B(N), C(N), R(N), U(N)
      PARAMETER (NMAx=500)
      INTEGER j
      DOUBLE PRECISION bet, gam(NMAx)
      IF (B(1).EQ.0.) THEN
         STOP 'tridag: rewrite equations'
      END IF
      bet = B(1)
      U(1) = R(1)/bet
      DO j = 2, N
         gam(j) = C(j-1)/bet
         bet = B(j) - A(j)*gam(j)
         IF (bet.EQ.0.) THEN
            STOP 'tridag failed'
         END IF
         U(j) = (R(j)-A(j)*U(j-1))/bet
      END DO
      DO j = N - 1, 1, -1
         U(j) = U(j) - gam(j+1)*U(j+1)
      END DO
      RETURN
      END
C  (C) Copr. 1986-92 Numerical Recipes Software +3Y.
