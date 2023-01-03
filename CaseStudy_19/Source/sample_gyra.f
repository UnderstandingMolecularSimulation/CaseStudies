      Subroutine Sample_Gyra(Switch)
      Implicit None

      Include 'system.inc'

Cccccccccccccccccccccccccccccccccccccc
C     Sample The Radius Of Gyration  C
Cccccccccccccccccccccccccccccccccccccc

      Integer Switch,Bin,Maxx

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

         R = Dsqrt((X(1) - X(Ell))**2 +
     &             (Y(1) - Y(Ell))**2 +
     &             (Z(1) - Z(Ell))**2)
         
         Dsamp    = Dsamp  + 1.0d0
         Bin      = 1      + Idint(R*Deltax)

         If(Bin.Ge.1.And.Bin.Le.Maxx) 
     &        Hrg(Bin) = Hrg(Bin) + 1.0d0
                   
      Else

Ccccccccccccccccccccccc
C     Write Results   C
Ccccccccccccccccccccccc
         
         If (Dsamp.Gt.0.5d0) Then
            
            Open(21,File="gyra.dat")

            Do Bin = 1,Maxx
               Write (21,*) 
     &              (Dble(Bin)-0.5d0)/Deltax,Hrg(Bin)/Dsamp
            Enddo

            Close(21)

         Endif
      Endif
     
      Return
      End
