      Subroutine Integrate(Cnn1)
      Implicit None
 
      Include 'system.inc'
 
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C     Integrate The Equations Of Motion For An Nve System  C
C     Velocity Verlet Integrator                           C
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      Integer I
      Double Precision U,F,Consv,Cnul,Cnn1,Cnn2
 
      Call Force(Xpos,U,F)

      Cnn1 = 0.0d0
      Cnn2 = 0.0d0
      Cnul = 0.0d0

      Do I=1,Nstep
         Xpos = Xpos             + Vpos*Tstep      + 0.5d0*F*Tstep*Tstep
         Vpos = Vpos             + 0.5d0*Tstep*F

         Call Force(Xpos,U,F)

         Vpos  = Vpos            + 0.5d0*Tstep*F
         Consv = 0.5d0*Vpos*Vpos + U

         If(I.Eq.1) Then
            Cnul = Consv
         Else
            If(.Not.(I.Gt.5.And.
     &               ((Xpos.Lt.0.0d0.And.Vpos.Lt.0.0d0).Or.
     &                (Xpos.Gt.1.0d0.And.Vpos.Gt.0.0d0)))) Then

               Cnn1 = Cnn1 + Dabs((Consv-Cnul)/Cnul)
               Cnn2 = Cnn2 + 1.0d0
            Endif
         Endif

         Call Sample(2,I)

Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C     Check If The Particle Passed The Boundary Or If It Can Never Recross The   C
C     Boundary Again...                                                          C
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      Enddo

      If(Cnn2.Gt.0.5d0) Then
         Cnn1 = Cnn1/Cnn2
      Else
         Cnn1 = 0.0d0
      Endif

      Return
      End
