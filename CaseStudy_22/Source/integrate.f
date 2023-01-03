      Subroutine Integrate(Step)
      Implicit None
 
      Include 'system.inc'
 
Cccccccccccccccccccccccccccccccccccccccccc
C     Integrate The Equations Of Motion  C
Cccccccccccccccccccccccccccccccccccccccccc
 
      Integer I,J,Step,Noiter
      Double Precision Rtstep,Htstep,Hlstep,Vxt,Vyt,
     &     Vzt,Chi,Twochi
 
      Rtstep = 1.0d0/Tstep
      Htstep = 0.5d0*Tstep
      Hlstep = 0.5d0*Dble(Nrespa)*Tstep
 
      Do I = 1,Npart
         Rxf(I) = Rxx(I)
         Ryf(I) = Ryy(I)
         Rzf(I) = Rzz(I)
 
         Vxf(I) = Vxx(I)
         Vyf(I) = Vyy(I)
         Vzf(I) = Vzz(I)
      Enddo
 
      Ukin = 0.0d0
 
      If (Step.Le.Ninit) Then
 
Cccccccccccccccccccccccc
C     Initialisation   C
Cccccccccccccccccccccccc
 
         Do I = 1,Npart
            Vxt = Vxf(I) + Htstep*(Fxxl(I) + Fxxs(I))
            Vyt = Vyf(I) + Htstep*(Fyyl(I) + Fyys(I))
            Vzt = Vzf(I) + Htstep*(Fzzl(I) + Fzzs(I))
 
            Ukin = Ukin + Vxt*Vxt + Vyt*Vyt + Vzt*Vzt
         Enddo
 
         Ukin = Ukin*0.5d0
 
         If (Lcons) Then
            Noiter = 2
         Else
            Noiter = 1
         Endif
 
         Do J = 1,Noiter
 
            Chi = Dsqrt(0.5d0*Temp*Dble(Nfree)/Ukin)
            Twochi = 2.0d0*Chi - 1.0d0
 
            Do I = 1,Npart
               Vxx(I) = Vxf(I)*Twochi + 
     &              Chi*Tstep*(Fxxl(I) + Fxxs(I))
               Vyy(I) = Vyf(I)*Twochi + 
     &              Chi*Tstep*(Fyyl(I) + Fyys(I))
               Vzz(I) = Vzf(I)*Twochi + 
     &              Chi*Tstep*(Fzzl(I) + Fzzs(I))
 
               Rxx(I) = Rxf(I) + Tstep*Vxx(I)
               Ryy(I) = Ryf(I) + Tstep*Vyy(I)
               Rzz(I) = Rzf(I) + Tstep*Vzz(I)
            Enddo
 
            If (Lcons) Then
               Call Shake
 
               Do I = 1,Npart
                  Vxx(I) = (Rxx(I) - Rxf(I))*Rtstep
                  Vyy(I) = (Ryy(I) - Ryf(I))*Rtstep
                  Vzz(I) = (Rzz(I) - Rzf(I))*Rtstep
 
                  Fxxl(I) = (Vxx(I) - Vxf(I))*Rtstep
                  Fyyl(I) = (Vyy(I) - Vyf(I))*Rtstep
                  Fzzl(I) = (Vzz(I) - Vzf(I))*Rtstep
 
                  Fxxs(I) = 0.0d0
                  Fyys(I) = 0.0d0
                  Fzzs(I) = 0.0d0
               Enddo
            Endif
 
            Ukin = 0.0d0
 
            Do I = 1,Npart
               Vxt = Vxf(I) + Vxx(I)
               Vyt = Vyf(I) + Vyy(I)
               Vzt = Vzf(I) + Vzz(I)
 
               Ukin = Ukin + Vxt*Vxt + Vyt*Vyt + Vzt*Vzt
            Enddo
 
            Ukin = 0.125d0*Ukin
         Enddo
 
      Elseif (Lrespa) Then
 
Cccccccccccccccccccccccc
C     Respa            C
Cccccccccccccccccccccccc
 
         Do J = 1,Nrespa
 
            If (J.Eq.1) Then
               Do I = 1,Npart
                  Vxx(I) = Vxx(I) + Hlstep*Fxxl(I)
                  Vyy(I) = Vyy(I) + Hlstep*Fyyl(I)
                  Vzz(I) = Vzz(I) + Hlstep*Fzzl(I)
               Enddo
            Endif
 
            Do I = 1,Npart
               Vxx(I) = Vxx(I) + Htstep*Fxxs(I)
               Vyy(I) = Vyy(I) + Htstep*Fyys(I)
               Vzz(I) = Vzz(I) + Htstep*Fzzs(I)
 
               Rxx(I) = Rxx(I) + Vxx(I)*Tstep
               Ryy(I) = Ryy(I) + Vyy(I)*Tstep
               Rzz(I) = Rzz(I) + Vzz(I)*Tstep
            Enddo
 
            If (J.Eq.Nrespa) Then
               Call Force(.True.,.True.)
            Else
               Call Force(.True.,.False.)
            Endif
 
            Do I = 1,Npart
               Vxx(I) = Vxx(I) + Htstep*Fxxs(I)
               Vyy(I) = Vyy(I) + Htstep*Fyys(I)
               Vzz(I) = Vzz(I) + Htstep*Fzzs(I)
            Enddo
 
            If (J.Eq.Nrespa) Then
 
               Do I = 1,Npart
                  Vxx(I) = Vxx(I) + Hlstep*Fxxl(I)
                  Vyy(I) = Vyy(I) + Hlstep*Fyyl(I)
                  Vzz(I) = Vzz(I) + Hlstep*Fzzl(I)
 
                  Ukin = Ukin + Vxx(I)*Vxx(I) + 
     &                 Vyy(I)*Vyy(I) + Vzz(I)*Vzz(I)
               Enddo
            Endif
 
         Enddo
 
         Ukin = 0.5d0*Ukin
 
      Else
 
Cccccccccccccccccccccccc
C     Normal Nve       C
Cccccccccccccccccccccccc
 
         Do I = 1,Npart
            Vxx(I) = Vxf(I) + Tstep*(Fxxl(I) + Fxxs(I))
            Vyy(I) = Vyf(I) + Tstep*(Fyyl(I) + Fyys(I))
            Vzz(I) = Vzf(I) + Tstep*(Fzzl(I) + Fzzs(I))
 
            Rxx(I) = Rxf(I) + Tstep*Vxx(I)
            Ryy(I) = Ryf(I) + Tstep*Vyy(I)
            Rzz(I) = Rzf(I) + Tstep*Vzz(I)
         Enddo
 
         If (Lcons) Then
 
            Call Shake
 
            Do I = 1,Npart
               Vxx(I) = (Rxx(I) - Rxf(I))*Rtstep
               Vyy(I) = (Ryy(I) - Ryf(I))*Rtstep
               Vzz(I) = (Rzz(I) - Rzf(I))*Rtstep
            Enddo
         Endif
 
         Do I = 1,Npart
            Vxt = Vxx(I) + Vxf(I)
            Vyt = Vyy(I) + Vyf(I)
            Vzt = Vzz(I) + Vzf(I)
 
            Ukin = Ukin + Vxt*Vxt + Vyt*Vyt + Vzt*Vzt
         Enddo
 
         Ukin = 0.125d0*Ukin
      Endif
 
      Return
      End
