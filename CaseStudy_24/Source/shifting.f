      Subroutine Shifting
      Implicit None

      Include 'system.inc'
     
Ccccccccccccccccccccccccccccccccccccccccc
C     Shifting Move For Path Ensemble   C
Ccccccccccccccccccccccccccccccccccccccccc

      Double Precision Du,Ran_Uniform,Order

      Integer J,Ishift,Kkk,Nleft,Nright
      Logical L_Ready,Lslice

Cccccccccccccccccccccccccccccccccccccccccccc
C     Choose The Shifting Move At Random   C
C     Select At Random To Have A Positive  C
C     Or Negative Shift                    C
Cccccccccccccccccccccccccccccccccccccccccccc

      Shift2  = Shift2 + 1.0d0
      L_Ready = .False.
      Ishift  = 1 + Idint(Ran_Uniform()*Dble(Nslice)*0.2d0)

      If(Ran_Uniform().Lt.0.5d0) Ishift = -Ishift
      
Cccccccccccccccccccccccccccccccccccc
C     Shift The Path               C
C     Copy Coordinates And Whether C
C     The Path Is In B At Time T   C
Cccccccccccccccccccccccccccccccccccc

      Do J=1,Nslice

         Kkk = J + Ishift
            
         If(Kkk.Ge.1.And.Kkk.Le.Nslice) Then
            La(Kkk)    = La_Old(J)
            Lb(Kkk)    = Lb_Old(J)
            Eetra(Kkk) = Eeold(J)
            Xxtra(Kkk) = Xxold(J)
            Yytra(Kkk) = Yyold(J)
            Vxtra(Kkk) = Vxold(J)
            Vytra(Kkk) = Vyold(J)
         Endif
      Enddo

Cccccccccccccccccccccccccccccccccccccc
C     Calculate New Safe Boundaries  C
Cccccccccccccccccccccccccccccccccccccc

      Nleft  = Max(1      , (Ishift + 1     ))
      Nright = Min(Nslice , (Ishift + Nslice))

      If(Nleft.Gt.1) Then

Cccccccccccccccccccccccccc
C     Backward Shifting  C
C     Turn Momenta       C
Cccccccccccccccccccccccccc

         Rxx =  Xxtra(Nleft)
         Ryy =  Yytra(Nleft)
         Vxx = -Vxtra(Nleft)
         Vyy = -Vytra(Nleft)
         
         Call Md(Nleft,-1,(Nleft-1),Du)

         Nleft  = 1
         Drift1 = Drift1 + Du
         Drift2 = Drift2 + 1.0d0
      Endif

Ccccccccccccccccccccccccccccccccccccccccccccccccccc
C     Check If Initial Point Is In A              C
C                                                 C
C     Check For The Energy. When The Energy       C
C     Drift Is Zero, No Paths Are Rejected Here   C
Ccccccccccccccccccccccccccccccccccccccccccccccccccc
      
      If(.Not.La(1)) Return

      If(Ran_Uniform().Gt.Dexp(-Beta*(Eetra(1)-Eeold(1)))) 
     &     Return

Cccccccccccccccccccccccccc
C     So Far, So Good    C
Cccccccccccccccccccccccccc

      If(Nright.Lt.Nslice) Then

Ccccccccccccccccccccccccccccccccc
C     Forward Shifting          C
Ccccccccccccccccccccccccccccccccc

         Rxx = Xxtra(Nright)
         Ryy = Yytra(Nright)
         Vxx = Vxtra(Nright)
         Vyy = Vytra(Nright)
         
         Call Md(Nright,1,(Nslice-Nright),Du)

         Nright = Nslice
         Drift1 = Drift1 + Du
         Drift2 = Drift2 + 1.0d0
      Endif

      If(Lumbrella) Then

Ccccccccccccccccccccccccccccccccccc
C     Check If Endpoint In Slice  C
Ccccccccccccccccccccccccccccccccccc

         Rxx = Xxtra(Nslice)
         Ryy = Yytra(Nslice)
         Vxx = Vxtra(Nslice)
         Vyy = Vytra(Nslice)
         
         Call In_Slice(Lslice,Order)

         If(.Not.Lslice) Then
            Return
         Else
            L_Ready = .True.
         Endif

      Else

Ccccccccccccccccccccccccccccc
C     Check If Once In B    C
Ccccccccccccccccccccccccccccc
         
         Do J=1,Nslice
            If(Lb(J)) L_Ready = .True.
         Enddo
      Endif

      If(L_Ready) Then

Ccccccccccccccccccccccccccccccccccccccccc
C     Accepted; Energy Is Not Changed   C
C     (Only Due To Drift...)            C
Ccccccccccccccccccccccccccccccccccccccccc

         Shift1 = Shift1 + 1.0d0
                 
         Do J=1,Nslice
            La_Old(J) = La(J)
            Lb_Old(J) = Lb(J)
            Eeold(J)  = Eetra(J)
            Xxold(J)  = Xxtra(J)
            Yyold(J)  = Yytra(J)
            Vxold(J)  = Vxtra(J)
            Vyold(J)  = Vytra(J)
         Enddo
      Endif
      
      Return
      End
