**==adjust.spg  processed by SPAG 4.52O  at 18:49 on  6 Jun 1996
      SUBROUTINE ADJUST(Attemp, Nacc, Dr)
c     adjusts maximum displacement such that 50% of the
c     movels will be accepted
      IMPLICIT NONE
      INCLUDE 'system.inc'
      INTEGER Attemp, Nacc, attempp, naccp
      DOUBLE PRECISION dro, frac, Dr
      SAVE naccp, attempp
 
      IF (Attemp.EQ.0.OR.attempp.GE.Attemp) THEN
         naccp = Nacc
         attempp = Attemp
      ELSE
         frac = DBLE(Nacc-naccp)/DBLE(Attemp-attempp)
         dro = Dr
         Dr = Dr*ABS(frac/0.5D0)
c        ---limit the change:
         IF (Dr/dro.GT.1.5D0) Dr = dro*1.5D0
         IF (Dr/dro.LT.0.5D0) Dr = dro*0.5D0
         IF (Dr.GT.HBOX/2.D0) Dr = HBOX/2.D0
         WRITE (6, 99001) Dr, dro, frac, Attemp - attempp, Nacc - naccp
c        ---store nacc and attemp for next use
         naccp = Nacc
         attempp = Attemp
      END IF
      RETURN
99001 FORMAT (' Max. displ. set to : ', f6.3, ' (old : ', f6.3, ')', /, 
     &        ' Frac. acc.: ', f4.2, ' attempts: ', i7, ' succes: ', i7)
      END
