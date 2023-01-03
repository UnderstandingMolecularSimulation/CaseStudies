      Subroutine Sample_Gyra(Switch)
      Implicit None

      Include 'system.inc'

Cccccccccccccccccccccccccccccccccccccc
C     Sample The Radius Of Gyration  C
Cccccccccccccccccccccccccccccccccccccc

      Integer Switch,Bin,Maxx,Ichain

      Parameter (Maxx = 500)
      
      Double Precision Deltax,R,Dsamp,Hrg(Maxx),Maxgyra

      Parameter (Maxgyra = 50.0d0)

      Save Hrg,Dsamp,Deltax
 
      If (Switch.Eq.1) Then

Cccccccccccccccccccc
C     Initialize   C
Cccccccccccccccccccc

         Dsamp  = 0.0d0
         Deltax = Dble(Maxx-1)/Maxgyra
 
         Do Bin = 1,Maxx
            Hrg(Bin) = 0.0d0
         Enddo

      Elseif (Switch.Eq.2) Then

Cccccccccccccccccccccccccc
C     Sample Some Stuff  C
Cccccccccccccccccccccccccc

         Do Ichain = 1,Nchain
            R        = Dsqrt((X(1,Ichain) - X(Ell,Ichain))**2 +
     &                       (Y(1,Ichain) - Y(Ell,Ichain))**2 +
     &                       (Z(1,Ichain) - Z(Ell,Ichain))**2)

            Dsamp    = Dsamp  + 1.0d0
            Bin      = 1      + Idint(R*Deltax)

            If(Bin.Ge.1.And.Bin.Le.Maxx) Hrg(Bin) = Hrg(Bin) + 1.0d0
         Enddo
          
      Else

Ccccccccccccccccccccccc
C     Write Results   C
Ccccccccccccccccccccccc
         
         If (Dsamp.Gt.0.5d0) Then
            
            Open(24,File="gyra.dat")

            Do Bin = 1,Maxx
               Write (24,*) (Dble(Bin)-0.5d0)/Deltax,Hrg(Bin)/Dsamp
            Enddo

            Close(24)

         Endif
      Endif
     
      Return
      End
