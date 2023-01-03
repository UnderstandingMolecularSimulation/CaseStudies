      Subroutine Shooting(Linit,Lsuccess)
      Implicit None

      Include 'system.inc'
      
Cccccccccccccccccccccccccccccccccccccccc
C     Shooting Move For Path Ensemble  C
Cccccccccccccccccccccccccccccccccccccccc

      Double Precision Vxforw,Vyforw,Du,Ran_Uniform,
     &                 Ran_Gauss,Order,Ukin,Factor

      Integer          J,Islice
      Logical          L_Ready,Linit,Lsuccess,Lslice

      Lsuccess = .False.
      L_Ready  = .False.
      
      If(Linit) Then

Cccccccccccccccccccccccccccccccccccccccccccccccccc
C     Generate An Initial Path Form A Gaussian   C
C     Scale To The Desired Temperature           C
Cccccccccccccccccccccccccccccccccccccccccccccccccc

         Islice = Nslice/2
         Vxx    = Ran_Gauss()
         Vyy    = Ran_Gauss()
         Ukin   = 0.5d0*(Vxx**2 + Vyy**2)
         Factor = Dsqrt(Temp/Ukin)
         Vxx    = Vxx*Factor
         Vyy    = Vyy*Factor
         Vxforw = -Vxx
         Vyforw = -Vyy
         Rxx    = Xxold(Islice)
         Ryy    = Yyold(Islice)
                          
      Else

         Islice = 1 + Idint(Ran_Uniform()*Dble(Nslice))
         Shootb = Shootb + 1.0d0
         Shoot2 = Shoot2 + 1.0d0

         If(Islice.Eq.1.Or.Islice.Eq.Nslice) Then
            Rxx    = Xxold(Islice)
            Ryy    = Yyold(Islice)
            Vxx    = Vxold(Islice)
            Vyy    = Vyold(Islice)
            Vxforw = -Vxx
            Vyforw = -Vyy
         Else
            Rxx    = Xxold(Islice)
            Ryy    = Yyold(Islice)
            Vxx    = -(Vxold(Islice) + Deltap*Ran_Gauss())
            Vyy    = -(Vyold(Islice) + Deltap*Ran_Gauss())
            Vxforw = -Vxx
            Vyforw = -Vyy
         Endif
      Endif

Ccccccccccccccccccccccccccccccccccccccc
C     Backward Integration First      C
C     Rejected If Initially Not In A  C
C                                     C
C     Reject/Accept Path Energy       C
C     Use The First Point Of The      C
C     Path For This                   C
Ccccccccccccccccccccccccccccccccccccccc

      Call Md(Islice,-1,(Islice-1),Du)

      Drift1 = Drift1 + Du
      Drift2 = Drift2 + 1.0d0
      
      If(.Not.La(1)) Return

Ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C     Accept/Reject On The Basis Of The First Point       C
C                                                         C
C     Pnew: Probability To Select The Selected Velocity   C
C     Pold: Probability To Select The Old Velocity From   C
C           The New                                       C
Ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      If(.Not.Linit) Then
         If(Ran_Uniform().Gt.Dexp(-Beta*(Eetra(1)-Eeold(1)))) Return
      Endif

Cccccccccccccccccccccccccccc
C     Forward Integration  C
C     Copy Coordinates     C
Cccccccccccccccccccccccccccc

      Rxx = Xxold(Islice)
      Ryy = Yyold(Islice)  
      Vxx = Vxforw
      Vyy = Vyforw
      
      Call Md(Islice,1,(Nslice-Islice),Du)

      Drift1 = Drift1 + Du
      Drift2 = Drift2 + 1.0d0
      
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

Ccccccccccccccccccccccccccc
C     Check If In B Once  C
Ccccccccccccccccccccccccccc

         Do J=1,Nslice
            If(Lb(J)) L_Ready = .True.
         Enddo
      Endif
      
      If (L_Ready) Then

Ccccccccccccccccccccccccccccc
C     Accepted              C
C     Copy Coordinates      C
Ccccccccccccccccccccccccccccc

         Shoot1   = Shoot1 + 1.0d0
         Shoota   = Shoota + 1.0d0
         Lsuccess = .True.
         
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
