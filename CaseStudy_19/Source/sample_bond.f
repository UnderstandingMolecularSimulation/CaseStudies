      Subroutine Sample_Bond(Switch)
      Implicit None

      Include 'system.inc'

Cccccccccccccccccccccccccccccccccccccc
C     Sample The Radius Of Gyration  C
Cccccccccccccccccccccccccccccccccccccc

      Integer Switch,Bin,Maxx,I

      Parameter (Maxx = 500)
      
      Double Precision Deltax,R,Dsamp,Hrg(Maxx),Maxgyra

      Parameter (Maxgyra = 2.0d0)

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

         Do I=2,Ell
            R = Dsqrt((X(I) - X(I-1))**2 +
     &                (Y(I) - Y(I-1))**2 +
     &                (Z(I) - Z(I-1))**2)
         
            Dsamp    = Dsamp  + 1.0d0
            Bin      = 1      + Idint(R*Deltax)

            If(Bin.Ge.1.And.Bin.Le.Maxx) 
     &           Hrg(Bin) = Hrg(Bin) + 1.0d0
         Enddo
                   
      Else

Ccccccccccccccccccccccc
C     Write Results   C
Ccccccccccccccccccccccc
         
         If (Dsamp.Gt.0.5d0) Then
            
            Open(21,File="bond.dat")

            Do Bin = 1,Maxx
               Write (21,*) 
     &              (Dble(Bin)-0.5d0)/Deltax,Hrg(Bin)/Dsamp
            Enddo

            Close(21)

         Endif
      Endif
     
      Return
      End
