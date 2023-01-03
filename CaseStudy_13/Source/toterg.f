**==toterg.spg  processed by SPAG 4.52O  at 14:19 on 24 Jun 1996
      SUBROUTINE TOTERG(X, Ux, Ent)
c
c   Determin total energy:
c
      IMPLICIT NONE
      DOUBLE PRECISION X, Ux, Ent, en, enk, fx
      CALL FORCE(fx, X, en)
      enk = 0.5D0*Ux*Ux
      Ent = enk + en
      RETURN
      END
