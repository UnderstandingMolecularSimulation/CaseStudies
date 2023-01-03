**==select.spg  processed by SPAG 4.52O  at 11:03 on 18 Oct 1996
 
 
      SUBROUTINE SELECT(W, Sumw, N)
c     ---select a trial position n with probability
c     ----p(i) = w(i)/sumw
      IMPLICIT NONE
      INTEGER N
      DOUBLE PRECISION Sumw, cumw, ws, RANF
      DOUBLE PRECISION W(*)
 
      ws = RANF()*Sumw
      cumw = W(1)
      N = 1
      DO WHILE (cumw.LT.ws)
         N = N + 1
         cumw = cumw + W(N)
      END DO
      RETURN
      END
