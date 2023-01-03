**==adjust.spg  processed by SPAG 4.52O  at 18:10 on 19 Jul 1996
      SUBROUTINE ADJUST(Attemp, Nacc, Dr, Attv, Accv, Vmax, Succ)
c     sets maximum displacement and maximum volume change
c     such that 50% of the move will be accepted
      IMPLICIT NONE
      INCLUDE 'system.inc'
      INTEGER Attemp, Nacc, attempp, naccp, accvp, Attv, Accv, attvp
      DOUBLE PRECISION dro, frac, vmaxo, Dr, Vmax, Succ
      SAVE naccp, attempp, attvp, accvp
 
c     ---displacement:
      IF (Attemp.EQ.0.OR.attempp.GE.Attemp) THEN
         naccp = Nacc
         attempp = Attemp
      ELSE
         frac = DBLE(Nacc-naccp)/DBLE(Attemp-attempp)
         dro = Dr
         Dr = Dr*ABS(frac/(Succ/100.D0))
c        ---limit the change:
         IF (Dr/dro.GT.1.5D0) Dr = dro*1.5D0
         IF (Dr/dro.LT.0.5D0) Dr = dro*0.5D0
         IF (Dr.GT.HBOX(1)/2.D0) Dr = HBOX(1)/2.D0
         WRITE (6, 99001) Dr, dro, frac, Attemp - attempp, Nacc - naccp
c        ---store nacc and attemp for next use
         naccp = Nacc
         attempp = Attemp
      END IF
c     ---volume:
      IF (Attv.EQ.0.OR.attvp.GE.Attv) THEN
         accvp = Accv
         attvp = Attv
      ELSE
         frac = DBLE(Accv-accvp)/DBLE(Attv-attvp)
         vmaxo = Vmax
         Vmax = Vmax*ABS(frac/(Succ/100.D0))
c        ---limit the change:
         IF (Vmax/vmaxo.GT.1.5D0) Vmax = vmaxo*1.5D0
         IF (Vmax/vmaxo.LT.0.5D0) Vmax = vmaxo*0.5D0
         WRITE (6, 99002) Vmax, vmaxo, frac, Attv - attvp, Accv - accvp
c        ---store nacc and attemp for next use
         accvp = Accv
         attvp = Attv
      END IF
      RETURN
99001 FORMAT (' Max. displ. set to      : ', f6.3, ' (old : ', f6.3, 
     &        ')', /, ' Frac. acc.: ', f4.2, ' attempts: ', i7, 
     &        ' succes: ', i7)
99002 FORMAT (' Max. vol. chan. set to: ', f6.3, ' (old : ', f6.3, ')', 
     &        /, ' Frac. acc.: ', f4.2, ' attempts: ', i7, ' succes: ', 
     &        i7)
      END
