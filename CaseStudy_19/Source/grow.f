      Subroutine Grow(Lold,Weight,Xn,Yn,Zn,Etot)
      Implicit None

      Include 'system.inc'

Ccccccccccccccccccccccccccccccccccccccccc
C     Grow A Chain Using Cbmc           C
C     The Potential Is An Arbitrary     C
C     Continuous Potential That         C
C     Is Defined In Epair               C
Ccccccccccccccccccccccccccccccccccccccccc
      
      Logical          Lold
 
      Integer          Ibead,Ichoi

      Double Precision Weight,Bx,By,Bz,Sumw,Etot,U,Wt(Kmax),
     &                 Ent(Kmax),Xn(Ellmax),Yn(Ellmax),
     &                 Zn(Ellmax),Xt(Kmax),Yt(Kmax),Zt(Kmax)
       
Cccccccccccccccccccccccccc
C     Initialize Arrays  C
Cccccccccccccccccccccccccc

      Weight = 1.0d0
      Etot   = 0.0d0
      
      Do Ibead=1,Ell
         Xn(Ibead) = X(Ibead)
         Yn(Ibead) = Y(Ibead)
         Zn(Ibead) = Z(Ibead)
      Enddo
      
Ccccccccccccccccccccccccccccc
C     Loop Over All Beads   C
Ccccccccccccccccccccccccccccc

      Do Ibead=2,Ell

         Sumw = 0.0d0

         Do Ichoi=1,Nchoi

Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C     Generate Trial Position And Calculate The Energy   C
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

            If(Lold.And.Ichoi.Eq.1) Then
               Xt(1) = X(Ibead)
               Yt(1) = Y(Ibead)
               Zt(1) = Z(Ibead)

               U = 0.5d0*Kv*
     &              ((Dsqrt((X(Ibead) - X(Ibead-1))**2 + 
     &                      (Y(Ibead) - Y(Ibead-1))**2 + 
     &                      (Z(Ibead) - Z(Ibead-1))**2) - 1.0d0)**2)
            Else
               Call Sphere(Bx,By,Bz)
                     
               Xt(Ichoi) = Xn(Ibead-1) + Bx
               Yt(Ichoi) = Yn(Ibead-1) + By
               Zt(Ichoi) = Zn(Ibead-1) + Bz

               U = 0.5d0*Kv*
     &              ((Dsqrt((Xt(Ichoi) - Xn(Ibead-1))**2 + 
     &                      (Yt(Ichoi) - Yn(Ibead-1))**2 + 
     &                      (Zt(Ichoi) - Zn(Ibead-1))**2) - 1.0d0)**2)
            Endif
               
            Ent(Ichoi) = U
            Wt(Ichoi)  = -Beta*U
            Sumw       = Sumw + Dexp(Wt(Ichoi))
         Enddo
            
         If(Ladvanced) Then
            Sumw = 0.0d0
            Do Ichoi=1,Nchoi
               Wt(Ichoi) = 0.0d0
            Enddo
         Endif

         Weight = Weight*Sumw
            
Cccccccccccccccccccccccccccccccccccccccccccc
C     Choose A Trial Position At Random    C
C     The First For The Old Config         C
Cccccccccccccccccccccccccccccccccccccccccccc

         If(Lold) Then
            Ichoi = 1
         Else
            Call Select(Wt,Ichoi,Nchoi)
         Endif
            
         Etot = Etot + Ent(Ichoi)
         
         Xn(Ibead) = Xt(Ichoi)
         Yn(Ibead) = Yt(Ichoi)
         Zn(Ibead) = Zt(Ichoi)
      Enddo

      Return
      End
