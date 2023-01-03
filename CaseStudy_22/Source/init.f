      Subroutine Init
      Implicit None
 
      Include 'system.inc'
 
C     Generates Initial Positions/Velocities
 
      Integer I,J,Screwup
      Logical Lovrlap
 
      Double Precision Ranf,Unew,Fxos(Maxpart),Fyos(Maxpart),
     &     Fzos(Maxpart),Uold,Testje,Place,Fxol(Maxpart),Fyol(Maxpart),
     &     Fzol(Maxpart),X,Y,Z,Impx,Impy,Impz,Rangauss
 
C     Calculate Velocities; Set Impulse To Zero
 
      Impx = 0.0d0
      Impy = 0.0d0
      Impz = 0.0d0
      Ukin = 0.0d0
      Place = 0.001d0*Box

      If (.Not.Linit) Then
         Do I = 1,Npart
            Read (22,*) Rxx(I),Ryy(I),Rzz(I),
     &           Vxx(I),Vyy(I),Vzz(I)
         Enddo
      Endif
 
      Do I = 1,Npart - 1,2
 
         J = I + 1
 
         If (Linit) Then
 
            Vxx(I) = Rangauss()
            Vyy(I) = Rangauss()
            Vzz(I) = Rangauss()
 
            If (Lcons) Then
               Vxx(J) = Vxx(I)
               Vyy(J) = Vyy(I)
               Vzz(J) = Vzz(I)
            Else
               Vxx(J) = Rangauss()
               Vyy(J) = Rangauss()
               Vzz(J) = Rangauss()
            Endif
         Endif
 
         Impx = Impx + Vxx(I) + Vxx(J)
         Impy = Impy + Vyy(I) + Vyy(J)
         Impz = Impz + Vzz(I) + Vzz(J)
 
      Enddo
 
      Impx = Impx/Dble(Npart)
      Impy = Impy/Dble(Npart)
      Impz = Impz/Dble(Npart)
 
      Do I = 1,Npart
         Vxx(I) = Vxx(I) - Impx
         Vyy(I) = Vyy(I) - Impy
         Vzz(I) = Vzz(I) - Impz
 
         Ukin = Ukin + Vxx(I)*Vxx(I) + 
     &        Vyy(I)*Vyy(I) + Vzz(I)*Vzz(I)
      Enddo
 
      Testje = Dsqrt(Temp*Dble(Nfree)/Ukin)
 
      Impx = 0.0d0
      Impy = 0.0d0
      Impz = 0.0d0
 
      Do I = 1,Npart
         Vxx(I) = Testje*Vxx(I)
         Vyy(I) = Testje*Vyy(I)
         Vzz(I) = Testje*Vzz(I)
 
         Impx = Impx + Vxx(I)
         Impy = Impy + Vyy(I)
         Impz = Impz + Vzz(I)
      Enddo
 
      Impx = Dabs(Impx)/Dble(Npart)
      Impy = Dabs(Impy)/Dble(Npart)
      Impz = Dabs(Impz)/Dble(Npart)
      Impx = Max(Impx,Impy,Impz)
 
      Write (6,*) 'Ininial Impulse       : ',Impx
 
      If (.Not.Linit) Then
         Write (6,*)
         Return
      Endif
 
 100  Screwup = 0
 
      Do I = 1,Npart - 1,2

         J = I + 1
 
 150     Rxx(I) = Ranf()*Box
         Ryy(I) = Ranf()*Box
         Rzz(I) = Ranf()*Box
 
         Call Ransphere(X,Y,Z)
 
         Rxx(J) = Rxx(I) + X*Bondl
         Ryy(J) = Ryy(I) + Y*Bondl
         Rzz(J) = Rzz(I) + Z*Bondl
 
         Call Checkdis(Lovrlap,I)
 
         If (Lovrlap) Then
            Screwup = Screwup + 1
 
            If (Screwup.Lt.(2*Npart)) Goto 150
            Goto 100
         Endif
 
      Enddo

      Do I=1,Npart
         Xoud(I) = Rxx(I) + 2.0d0*Box
         Youd(I) = Ryy(I) + 2.0d0*Box
         Zoud(I) = Rzz(I) + 2.0d0*Box
      Enddo
 
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C     Calculate Better Positions Using A Steepest Decent Algorithm     C
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
 
      Do J = 1,500
 
         If (J.Eq.1) Then
            Call Force(.True.,.True.)
            Uold = Upotl + Upots
            Write (6,*) 'Initial Energy        : ',Uold
 
            Place = 0.001d0*Box
         Endif
 
         Testje = 0.0d0
 
         Do I = 1,Npart
            Rxf(I) = Rxx(I)
            Ryf(I) = Ryy(I)
            Rzf(I) = Rzz(I)
 
            Fxos(I) = Fxxs(I)
            Fyos(I) = Fyys(I)
            Fzos(I) = Fzzs(I)
 
            Fxol(I) = Fxxl(I)
            Fyol(I) = Fyyl(I)
            Fzol(I) = Fzzl(I)
 
            Testje = Max(Testje,Dabs(Fxxl(I) + Fxxs(I)),
     &           Dabs(Fyyl(I) + Fyys(I)),Dabs(Fzzl(I) + Fzzs(I)))
         Enddo
 
         Testje = Place/Testje
 
         Do I = 1,Npart
            Rxx(I) = Rxx(I) + Testje*(Fxxl(I) + Fxxs(I))
            Ryy(I) = Ryy(I) + Testje*(Fyyl(I) + Fyys(I))
            Rzz(I) = Rzz(I) + Testje*(Fzzl(I) + Fzzs(I))
         Enddo
 
         If (Lcons) Call Shake
 
         Call Force(.True.,.True.)
 
         Unew = Upots + Upotl
 
         If (Unew.Lt.Uold) Then
            Uold = Unew
            Place = Place*1.2d0

            If (Place.Gt.Hbox) Place = Hbox
 
         Else

            Do I = 1,Npart
               Fxxs(I) = Fxos(I)
               Fyys(I) = Fyos(I)
               Fzzs(I) = Fzos(I)
 
               Fxxl(I) = Fxol(I)
               Fyyl(I) = Fyol(I)
               Fzzl(I) = Fzol(I)
 
               Rxx(I) = Rxf(I)
               Ryy(I) = Ryf(I)
               Rzz(I) = Rzf(I)

               Xoud(I) = Rxx(I) + 2.0d0*Box
               Youd(I) = Ryy(I) + 2.0d0*Box
               Zoud(I) = Rzz(I) + 2.0d0*Box
            Enddo
            Place = Place*0.2d0
         Endif
      Enddo

      Do I = 1,Npart
         Xoud(I) = Rxx(I) + 2.0d0*Box
         Youd(I) = Ryy(I) + 2.0d0*Box
         Zoud(I) = Rzz(I) + 2.0d0*Box
      Enddo      
 
      Write (6,*) 'Final Energy          : ',Uold
      Write (6,*)
      Write (6,*)
 
      Return
      End
