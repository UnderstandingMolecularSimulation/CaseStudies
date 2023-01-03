      Subroutine Grow_Recoil(Lovrlap,Lold,Weight,Xn,Yn,Zn,Ichain)
      Implicit None

      Include 'system.inc'
      
Ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C     Grow A Chain Using Either Recoil Growth Or Normal Cbmc    C
C     The Potential Is An Arbitrary Continuous Potential That   C
C     Is Defined In Epair                                       C
Ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      
      Logical          Lold,Lovrlap,Lfixed(Ellmax),Lopen,
     &                 Lready1,Lready2
 
      Integer          J,Ibead,Ichain,Mxbead,Mybead,I,Nchoi(Ellmax),
     &                 Ip,Iparent,Whmain(Ellmax),Ndir(Ellmax)

      Double Precision Ran_Uniform,Weight,Bx,By,Bz,En,
     &                 Xn(Ellmax),Yn(Ellmax),Zn(Ellmax),P_Open,
     &                 Xt(Kmax),Yt(Kmax),Zt(Kmax),Pmain(Ellmax),
     &                 Xnn(Ellmax),Ynn(Ellmax),Znn(Ellmax)

Cccccccccccccccccccccccccc
C     Initialize Arrays  C
Cccccccccccccccccccccccccc

      Do I=1,Ellmax
         Xn(I)     = 0.0d0
         Yn(I)     = 0.0d0
         Zn(I)     = 0.0d0
         Ndir(I)   = 1
         Pmain(I)  = 0.0d0
         Lfixed(I) = .False.
      Enddo

      Lovrlap   = .False.
      Lfixed(1) = .True.

Cccccccccccccccccccccccccc
C     Copy Coordinates   C
Cccccccccccccccccccccccccc

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

      If(Lold) Then
         Xt(1) = Xn(1)
         Yt(1) = Yn(1)
         Zt(1) = Zn(1)  
      Else
         Xt(1) = Ran_Uniform()*Box
         Yt(1) = Ran_Uniform()*Box
         Zt(1) = Ran_Uniform()*Box
      Endif
         
Ccccccccccccccccccccccccc
C     Grow The Chains   C
Ccccccccccccccccccccccccc

      If(.Not.Lold) Then

Ccccccccccccccccccccc
C     First Bead    C
Ccccccccccccccccccccc
                  
         Xn(1) = Xt(1)
         Yn(1) = Yt(1)
         Zn(1) = Zt(1)

         Call Echoice(Ichain,1,Xn(1),Yn(1),Zn(1),
     &        Xn,Yn,Zn,En)
         Call Open_Dir(En,Lopen,P_Open)

         If(.Not.Lopen) Then
            Weight  = 0.0d0
            Lovrlap = .True.
            Return
         Endif

         Parent(1) = 0
         Pmain(1)  = P_Open
         Nchild(1) = 0
         Ipoint    = 1
         Iparent   = 1
         Ibead     = 2

         Do While(Ibead.Le.Ell)

Ccccccccccccccccccccccccccccccccccccccccccccccc
C     Check If We Have Used The Max. Number   C
C     Of Trial Directions                     C
Ccccccccccccccccccccccccccccccccccccccccccccccc
               
            If(Nchild(Iparent).Eq.Nchoi(Ibead)) Then

               Ibead   = Ibead - 1
               Iparent = Parent(Iparent)
               
               If(Lfixed(Ibead)) Then
                  Weight  = 0.0d0
                  Lovrlap = .True.
                  Return
               Endif
           
            Else

Cccccccccccccccccccccccccccccccccccccc
C     Generate A New Configuration   C
Cccccccccccccccccccccccccccccccccccccc

               If(Ibead.Eq.2) Then
                  Call Ran_Sphere(Bx,By,Bz)
               Else
                  Bx = Xn(Ibead-2) - Xn(Ibead-1) 
                  By = Yn(Ibead-2) - Yn(Ibead-1)
                  Bz = Zn(Ibead-2) - Zn(Ibead-1)

                  Call Ran_Cone(Bx,By,Bz)
               Endif
                     
               Xn(Ibead) = Xn(Ibead-1) + Bx
               Yn(Ibead) = Yn(Ibead-1) + By
               Zn(Ibead) = Zn(Ibead-1) + Bz
                  
               Call Echoice(Ichain,Ibead,Xn(Ibead),
     &              Yn(Ibead),Zn(Ibead),
     &              Xn,Yn,Zn,En)

               Call Open_Dir(En,Lopen,P_Open)

               Ipoint = Ipoint + 1

               If(Ipoint.Gt.Maxtree) Then
                  Write(6,*) 'Ipoint Too Large !!!'
                  Stop
               Endif

               Nchild(Iparent) = Nchild(Iparent) + 1   
               Parent(Ipoint)  = Iparent
               Nchild(Ipoint)  = 0
               Pmain(Ibead)    = P_Open
               
               If(Lopen) Then
                  
                  Ibead   = Ibead + 1
                  Iparent = Ipoint

Cccccccccccccccccccccccccccc
C     Try To Fix A Bead    C
Cccccccccccccccccccccccccccc

                  If(Ibead.Le.Ell) Then

                     J = Ibead - Nrecoil
                        
                     If(J.Gt.0) Then
                        Lfixed(J) = .True.
                     Endif
                  Endif
               Endif
            Endif
         Enddo
        
      Else

Cccccccccccccccccccccccccccc
C     Old Configuration    C
Cccccccccccccccccccccccccccc

         Call Echoice(Ichain,1,Xn(1),Yn(1),Zn(1),
     &        Xn,Yn,Zn,En)
         Call Open_Dir(En,Lopen,P_Open)

Ccccccccccccccccccccc
C     First Bead    C
Ccccccccccccccccccccc

         Pmain(1)     = P_Open
         Parent(1)    = 0
         Nchild(1)    = 0
         Ipoint       = 1
                  
         Do Ibead=2,Ell

            Call Echoice(Ichain,Ibead,Xn(Ibead),
     &           Yn(Ibead),Zn(Ibead),
     &           Xn,Yn,Zn,En)

            Call Open_Dir(En,Lopen,P_Open)

            Ipoint = Ipoint + 1

            If(Ipoint.Gt.Maxtree) Then
               Write(6,*) 'Ipoint Too Large !!!'
               Stop
            Endif

            Nchild(Ipoint)   = 0
            Parent(Ipoint)   = Ipoint - 1
            Nchild(Ipoint-1) = 1
            Pmain(Ibead)     = P_Open

         Enddo
      Endif
      
Ccccccccccccccccccccccccccccccccccccc
C     What Is The Main Chain  ???   C
Ccccccccccccccccccccccccccccccccccccc

      Ip    = Ipoint
      Ibead = Ell

      Do While(Ip.Ne.0)
         Whmain(Ibead) = Ip
         Ip            = Parent(Ip)
         Ibead         = Ibead - 1
      Enddo
      
      Do Mybead=2,Ell

         Lready1      = .False.
         Lready2      = .False.
         Ndir(Mybead) = 1
         
         Do I=1,Ellmax
            Xnn(I) = Xn(I)
            Ynn(I) = Yn(I)
            Znn(I) = Zn(I)
         Enddo

Cccccccccccccccccccccccccccccccccccc
C     Search The Open Directions   C
Cccccccccccccccccccccccccccccccccccc

         Do While(.Not.Lready1)

            Ibead   = Mybead
            Mxbead  = Min(Ell,Ibead-1+Nrecoil)
            Iparent = Parent(Whmain(Ibead))
            Lready2 = .False.
           
            Do While(.Not.Lready2)
                        
               If(Nchild(Iparent).Eq.Nchoi(Ibead)) Then

                  Ibead   = Ibead - 1
                  Iparent = Parent(Iparent)
               
                  If(Ibead.Lt.Mybead) Then
                     Lready1 = .True.
                     Lready2 = .True.
                  Endif
               
               Else

                  If(Ibead.Eq.2) Then
                     Call Ran_Sphere(Bx,By,Bz)
                  Else
                     Bx = Xnn(Ibead-2) - Xnn(Ibead-1) 
                     By = Ynn(Ibead-2) - Ynn(Ibead-1)
                     Bz = Znn(Ibead-2) - Znn(Ibead-1)
                     
                     Call Ran_Cone(Bx,By,Bz)
                  Endif
                     
                  Xnn(Ibead) = Xnn(Ibead-1) + Bx
                  Ynn(Ibead) = Ynn(Ibead-1) + By
                  Znn(Ibead) = Znn(Ibead-1) + Bz
                  
                  Call Echoice(Ichain,Ibead,
     &                 Xnn(Ibead),Ynn(Ibead),Znn(Ibead),
     &                 Xnn,Ynn,Znn,En)

                  Call Open_Dir(En,Lopen,P_Open)

                  Ipoint = Ipoint + 1

                  If(Ipoint.Gt.Maxtree) Then
                     Write(6,*) 'Ipoint Too Large !!!'
                     Stop
                  Endif

                  Nchild(Iparent) = Nchild(Iparent) + 1   
                  Parent(Ipoint)  = Iparent
                  Nchild(Ipoint)  = 0
                     
                  If(Lopen) Then
                                 
                     Ibead   = Ibead + 1
                     Iparent = Ipoint

Cccccccccccccccccccccccccccc
C     Try To Fix A Bead    C
Cccccccccccccccccccccccccccc

                     If(Ibead.Gt.Mxbead) Then
                        Lready2      = .True.
                        Ndir(Mybead) = Ndir(Mybead) + 1
                     Endif
                  Endif
               Endif
            Enddo
         Enddo
      Enddo

Ccccccccccccccccccccccccccccccccccc
C     Weight Calculation          C
Ccccccccccccccccccccccccccccccccccc

      Weight = 1.0d0/Pmain(1)

      Do Ibead=2,Ell
         Weight = Weight*Dble(Ndir(Ibead))/Pmain(Ibead)
      Enddo
      
      Return
      End
