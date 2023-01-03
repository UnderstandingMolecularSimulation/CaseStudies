**==new_nlist.spg  processed by SPAG 4.52O  at 16:41 on 22 Jul 1996
 
      SUBROUTINE NEW_NLIST
c     makes a noew neigh-bour list using the linked-list
c     algorithm
      IMPLICIT NONE
      INTEGER i, ic, CELL
      INCLUDE 'parameter.inc'
      INCLUDE 'conf.inc'
      INCLUDE 'nlist.inc'
 
c     ---initialize the head-of-chain
      DO ic = 0, NCELT - 1
         HOC(ic) = 0
      END DO
c     ---make linked list:
      DO i = 1, NPART
c        ---determine celnumber
         ic = CELL(X(i), Y(i), Z(i))
c        ---update linked-list and head of chain
         LL(i) = HOC(ic)
         HOC(ic) = i
      END DO
      RETURN
      END
