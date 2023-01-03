      Subroutine Minim
      Implicit None

      Include 'maxarray.inc'
      Include 'system.inc'

      Integer I,J
      Double Precision Uold,Testje,Place,
     &     Fxo(Maxtotal),Fyo(Maxtotal),
     &     Fzo(Maxtotal),Xf(Maxtotal),
     &     Yf(Maxtotal),Zf(Maxtotal),
     &     Gammaold,Sigmaold

Cccccccccccccccccccccccccccccccccccccccccccc
C     Minimize The Energy                  C
C     Steepest Decent Algorithm            C
C                                          C
C     A Bit Silly, But Very Effective      C
C     Set Random/Replusion Force To Zero   C
Cccccccccccccccccccccccccccccccccccccccccccc

      Gammaold = Gamma
      Sigmaold = Sigmat
      Gamma    = 0.0d0
      Sigmat   = 0.0d0
      Upot     = 0.0d0
      Uold     = 0.0d0
      Place    = 0.05d0*Min(Boxlx,Boxly,Boxlz)

Cccccccccccccccccccccccccccccccccc
C     Loop Over Initial Steps    C
Cccccccccccccccccccccccccccccccccc

      Call Force_Minim

      Uold = Upot
      Write (6,*) 'Initial Energy             : ',Uold

      Do J = 1,300
         Testje = 0.0d0
 
Ccccccccccccccccccccccccccccccccccccccccccccc
C     Calculate Maximum Downhill Gradient   C
Ccccccccccccccccccccccccccccccccccccccccccccc
 
         Do I = 1,Npart
            Xf(I)  = Xx(I)
            Yf(I)  = Yy(I)
            Zf(I)  = Zz(I)
            Fxo(I) = Fx(I)
            Fyo(I) = Fy(I)
            Fzo(I) = Fz(I)
 
            Testje = Max(Testje,
     &           Dabs(Fx(I)),Dabs(Fy(I)),Dabs(Fz(I)))
         Enddo
 
         If(Testje.Gt.0.00001d0) Then
            Testje = Place/Testje
         Else
            Testje = 0.00001d0
         Endif
 
         Do I = 1,Npart
            Xx(I) = Xx(I) + Testje*Fx(I)
            Yy(I) = Yy(I) + Testje*Fy(I)
            Zz(I) = Zz(I) + Testje*Fz(I)
 
            If (Xx(I).Ge.Boxlx) Then
               Xx(I) = Xx(I) - Boxlx
            Elseif (Xx(I).Lt.0.0d0) Then
               Xx(I) = Xx(I) + Boxlx
            Endif

            If (Yy(I).Ge.Boxly) Then
               Yy(I) = Yy(I) - Boxly
            Elseif (Yy(I).Lt.0.0d0) Then
               Yy(I) = Yy(I) + Boxly
            Endif

            If (Zz(I).Ge.Boxlz) Then
               Zz(I) = Zz(I) - Boxlz
            Elseif (Zz(I).Lt.0.0d0) Then
               Zz(I) = Zz(I) + Boxlz
            Endif
         Enddo
 
Ccccccccccccccccccccccccccccccccccccccccccccccccc
C     Calculate New Potential Energy/Forces     C
Ccccccccccccccccccccccccccccccccccccccccccccccccc
 
         Call Force_Minim
 
Cccccccccccccccccccccccccccccccccccccccccccccccccccccc
C     Check If The New Positions Are Acceptable      C
C     If Not, Make Stepsize Smaller And Try Again    C
C     If So, Accept And Increase Stepsize            C
Cccccccccccccccccccccccccccccccccccccccccccccccccccccc
 
         If (Upot.Lt.Uold) Then
            Uold  = Upot
            Place = Place*1.2d0
            Place = Min(Place,
     &           0.1d0*Min(Boxlx,Boxly,Boxlz))
         Else
            Do I = 1,Npart
               Fx(I) = Fxo(I)
               Fy(I) = Fyo(I)
               Fz(I) = Fzo(I)
               Xx(I) = Xf(I)
               Yy(I) = Yf(I)
               Zz(I) = Zf(I)
            Enddo
            Place = Place*0.1d0
         Endif
      Enddo
 
      Write (6,*) 'Final Energy               : ',Uold
 
Cccccccccccccccccccccccccccccccccc
C     Restore Gamma And Sigma    C
Cccccccccccccccccccccccccccccccccc

      Gamma  = Gammaold
      Sigmat = Sigmaold

      Return
      End
