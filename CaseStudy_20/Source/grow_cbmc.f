      Subroutine Grow_cbmc(Lovrlap,Lold,Weight,Xn,Yn,Zn,Ichain,Etot)
      Implicit None

      Include 'system.inc'

Ccccccccccccccccccccccccccccccccccccccccc
C     Grow A Chain Using Cbmc           C
C     The Potential Is An Arbitrary     C
C     Continuous Potential That         C
C     Is Defined In Epair               C
Ccccccccccccccccccccccccccccccccccccccccc
      
      Logical          Lold,Lovrlap
 
      Integer          Ibead,Ichain,Ichoi,Nchoi(Ellmax)

      Double Precision Ran_Uniform,Weight,Bx,By,Bz,Sumw,Etot,En,
     &                 Wt(Kmax),Ent(Kmax),Xn(Ellmax),Yn(Ellmax),
     &                 Zn(Ellmax),Xt(Kmax),Yt(Kmax),Zt(Kmax)
       
Cccccccccccccccccccccccccc
C     Initialize Arrays  C
Cccccccccccccccccccccccccc

      Weight    = 1.0d0
      Etot      = 0.0d0
      Lovrlap   = .False.

      Do Ibead=1,Ellmax
         Xn(Ibead) = 0.0d0
         Yn(Ibead) = 0.0d0
         Zn(Ibead) = 0.0d0
      Enddo

      Do Ichoi=1,Kmax
         Xt(Ichoi)  = 0.0d0
         Yt(Ichoi)  = 0.0d0
         Zt(Ichoi)  = 0.0d0
         Wt(Ichoi)  = 0.0d0
         Ent(Ichoi) = 0.0d0
      Enddo

      If(Ichain.Ge.1.And.Ichain.Le.Nchain) Then
         Do Ibead=1,Ell
            Xn(Ibead) = X(Ibead,Ichain)
            Yn(Ibead) = Y(Ibead,Ichain)
            Zn(Ibead) = Z(Ibead,Ichain)
         Enddo
      Endif

Cccccccccccccccccccccccccccccccccccccccccccccccccc
C     Generate The Number Of Trial Directions    C
Cccccccccccccccccccccccccccccccccccccccccccccccccc

      Do Ibead=1,Ell
         If(Ibead.Eq.1) Then
            Nchoi(Ibead) = 1
         Else
            Nchoi(Ibead) = D_Second
         Endif
      Enddo

Ccccccccccccccccccccccccccccc
C     Loop Over All Beads   C
Ccccccccccccccccccccccccccccc

      Do Ibead=1,Ell

         Sumw = 0.0d0

         Do Ichoi=1,Nchoi(Ibead)

Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C     Generate Trial Position And Calculate The Energy   C
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

            If(Lold.And.Ichoi.Eq.1) Then
               Xt(1) = X(Ibead,Ichain)
               Yt(1) = Y(Ibead,Ichain)
               Zt(1) = Z(Ibead,Ichain)
            Else
               If(Ibead.Eq.1) Then
                  Xt(Ichoi) = Ran_Uniform()*Box
                  Yt(Ichoi) = Ran_Uniform()*Box
                  Zt(Ichoi) = Ran_Uniform()*Box

               Elseif(Ibead.Eq.2) Then

                  Call Ran_Sphere(Bx,By,Bz)
                     
                  Xt(Ichoi) = Xn(Ibead-1) + Bx
                  Yt(Ichoi) = Yn(Ibead-1) + By
                  Zt(Ichoi) = Zn(Ibead-1) + Bz

               Else

                  Bx = Xn(Ibead-2) - Xn(Ibead-1)
                  By = Yn(Ibead-2) - Yn(Ibead-1)
                  Bz = Zn(Ibead-2) - Zn(Ibead-1)

                  Call Ran_Cone(Bx,By,Bz)

                  Xt(Ichoi) = Xn(Ibead-1) + Bx
                  Yt(Ichoi) = Yn(Ibead-1) + By
                  Zt(Ichoi) = Zn(Ibead-1) + Bz

               Endif
            Endif
               
            Call Echoice(Ichain,Ibead,Xt(Ichoi),
     &           Yt(Ichoi),Zt(Ichoi),
     &           Xn,Yn,Zn,En)
               
            Ent(Ichoi) = En
            Wt(Ichoi)  = -Beta*En
            Sumw       = Sumw + Dexp(Wt(Ichoi))
         Enddo
            
         Weight = Weight*Sumw
            
Ccccccccccccccccccccccccccc
C     Check For Overlap   C
Ccccccccccccccccccccccccccc

         If(Weight.Lt.1.0d-250) Then
            Lovrlap = .True.
            Weight  = 0.0d0
            If(.Not.Lold) Return
         Endif

Cccccccccccccccccccccccccccccccccccccccccccc
C     Choose A Trial Position At Random    C
C     The First For The Old Config         C
Cccccccccccccccccccccccccccccccccccccccccccc

         If(Lold) Then
            Ichoi = 1
         Else
            Call Select(Wt,Ichoi,Nchoi(Ibead))
         Endif
            
         Etot = Etot + Ent(Ichoi)
         
         Xn(Ibead) = Xt(Ichoi)
         Yn(Ibead) = Yt(Ichoi)
         Zn(Ibead) = Zt(Ichoi)
      Enddo

      Return
      End
