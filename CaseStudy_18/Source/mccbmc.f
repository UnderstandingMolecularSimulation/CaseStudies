**==mccbmc.spg  processed by SPAG 4.52O  at 11:03 on 18 Oct 1996
      SUBROUTINE MCCBMC(K, Move, Acc, Etot, Evib, Enlj, Vir)
c ---perform a cbmc move
      IMPLICIT NONE
      INCLUDE 'particle.inc'
      LOGICAL newconf
      INTEGER K, o, Move, idel, i, ib
      DOUBLE PRECISION wn, wo, RANF, Etot, ennew, enold, Evib, Enlj, 
     &                 evibnew, evibold, Vir, virvibold, virvibnew, 
     &                 virnew, virold
      DOUBLE PRECISION xn(ELLmax), yn(ELLmax), zn(ELLmax)
      INTEGER Acc(*)
 
c     ---perform a cbmc move in which a chain
c     ----is regrown at a random position
      Move = Move + 1
c     ---old configuration
      newconf = .FALSE.
      CALL GROW(newconf, wo, K, xn, yn, zn, o, ib, enold, virold)
c        ---determine internal energy of the selected chain
      CALL ENINT(xn, yn, zn, evibold, virvibold)
      idel = o
c     ---new configuration
      newconf = .TRUE.
      CALL GROW(newconf, wn, K, xn, yn, zn, o, ib, ennew, virnew)
      CALL ENINT(xn, yn, zn, evibnew, virvibnew)
c     ---acceptance test
      IF (RANF().LT.(wn/wo)) THEN
c        ---accepted
         Acc(ib) = Acc(ib) + 1
         DO i = ib, ELL
            X(idel, i) = xn(i)
            Y(idel, i) = yn(i)
            Z(idel, i) = zn(i)
         END DO
         Evib = Evib + (evibnew-evibold)
         Enlj = Enlj + (ennew-enold)
         Etot = Evib + Enlj
         Vir = Vir + (virnew-virold) + (virvibnew-virvibold)
      END IF
      RETURN
      END
