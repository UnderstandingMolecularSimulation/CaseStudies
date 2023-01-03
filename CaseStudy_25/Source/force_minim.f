      Subroutine Force_Minim
      Implicit None
 
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C     Calculate The Conservative Force Only                  C
C     A Linked-Cellist Is Used To Compute The Interactions   C
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      Include 'maxarray.inc'
      Include 'selflist.inc'
      Include 'system.inc'
 
      Integer Icx1,Icy1,Icz1,Icx2,Icy2,Icz2,Icell1,
     &     Icell2,I,J,Idc,Icell,Ipart,Head(0:Maxcell),
     &     Link(Maxtotal),Ia,Ib,Ispring
 
      Double Precision Rsq,Dpx,Dpy,Dpz,Rsqi,
     &     Ffx,Ffy,Ffz,R2,Fac

Cccccccccccccccccccccccccccccccccccccccccc
C     Dirty Rng                          C
C     Initialize Using A Better Rng      C
Cccccccccccccccccccccccccccccccccccccccccc

      Upot = 0.0d0
      
      Do Icell = 0,Maxcell
         Head(Icell) = 0
      Enddo

Ccccccccccccccccccccccccccccc
C     Make Linked Cellist   C
Ccccccccccccccccccccccccccccc

      Do Ipart = 1,Npart
         Icell = Idint(Xx(Ipart)*Rlx)          + 
     &           Idint(Yy(Ipart)*Rly)*Ncx      + 
     &           Idint(Zz(Ipart)*Rlz)*Ncx*Ncy

         Link(Ipart) = Head(Icell)
         Head(Icell) = Ipart

         Fx(Ipart)   = 0.0d0
         Fy(Ipart)   = 0.0d0
         Fz(Ipart)   = 0.0d0
      Enddo
         
Ccccccccccccccccccccccccccccccc
C     Loop Over All Cells     C
Ccccccccccccccccccccccccccccccc

      Do Icz1 = 0,Ncz - 1
         Do Icy1 = 0,Ncy - 1
            Do Icx1 = 0,Ncx - 1
               
               Icell1 = Icx1 + Icy1*Ncx + Icz1*Ncx*Ncy
 
Ccccccccccccccccccccccccccccccccccccccccccccc
C     Run Through All Particles In Cell1    C
Ccccccccccccccccccccccccccccccccccccccccccccc

               I = Head(Icell1)

               Do While (I.Ne.0)
                   
Ccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C     Run Through Upper & Right Neighbouring Cells    C
Ccccccccccccccccccccccccccccccccccccccccccccccccccccccc

                  Do Idc = 1,14
                     Icx2 = Icx1 + Dcx(Idc)
                     Icy2 = Icy1 + Dcy(Idc)
                     Icz2 = Icz1 + Dcz(Idc)
 
Ccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C     Periodic Bc: Note That Icz2 Is Always >= Icz1   C
Ccccccccccccccccccccccccccccccccccccccccccccccccccccccc

                     If(Icx2.Lt.0) Then
                        Icx2 = Icx2 + Ncx
                     Elseif(Icx2.Ge.Ncx) Then
                        Icx2 = Icx2 - Ncx
                     Endif

                     If(Icy2.Lt.0) Then
                        Icy2 = Icy2 + Ncy
                     Elseif(Icy2.Ge.Ncy) Then
                        Icy2 = Icy2 - Ncy
                     Endif

                     If(Icz2.Ge.Ncz) Icz2 = Icz2 - Ncz
 
                     Icell2 = Icx2 + Icy2*Ncx + Icz2*Ncx*Ncy
 
Ccccccccccccccccccccccccccccccccccccccccccccc
C     Run Through All Particles In Cell2    C
Ccccccccccccccccccccccccccccccccccccccccccccc

                     If(Icell1.Eq.Icell2) Then
                        J = Link(I)
                     Else
                        J = Head(Icell2)
                     Endif
                       
                     Do While (J.Ne.0)
                        Dpx = Xx(I) - Xx(J)
                        Dpy = Yy(I) - Yy(J)
                        Dpz = Zz(I) - Zz(J)

                        If(Dpx.Ge.Hbx) Then
                           Dpx = Dpx - Boxlx
                        Elseif(Dpx.Lt.-Hbx) Then
                           Dpx = Dpx + Boxlx
                        Endif

                        If(Dpy.Ge.Hby) Then
                           Dpy = Dpy - Boxly
                        Elseif(Dpy.Lt.-Hby) Then
                           Dpy = Dpy + Boxly
                        Endif

                        If(Dpz.Ge.Hbz) Then
                           Dpz = Dpz - Boxlz
                        Elseif(Dpz.Lt.-Hbz) Then
                           Dpz = Dpz + Boxlz
                        Endif

Cccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C     Avoid Overfolow/Underflow In Force Calculation   C
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccc

                        R2 = Max(0.000001d0,
     &                       (Dpx**2 + Dpy**2 + Dpz**2))
 
                        If(R2.Lt.1.0d0) Then
                           Rsq  = Dsqrt(R2)
                           Rsqi = 1.0d0/Rsq
                           Fac  = Phrep(Type(I),Type(J))*(Rsqi-1.0d0)
                           Upot = Upot + 
     &                          Phrep(Type(I),Type(J))*
     &                          (Rsq*(0.5d0*Rsq - 1.0d0) + 0.5d0)

                           Ffx   = Dpx*Fac
                           Ffy   = Dpy*Fac
                           Ffz   = Dpz*Fac
 
                           Fx(I) = Fx(I) + Ffx
                           Fy(I) = Fy(I) + Ffy
                           Fz(I) = Fz(I) + Ffz

                           Fx(J) = Fx(J) - Ffx
                           Fy(J) = Fy(J) - Ffy
                           Fz(J) = Fz(J) - Ffz
                        Endif

                        J = Link(J)

                     Enddo
                  Enddo
 
                  I = Link(I)
 
               Enddo            
            Enddo       
         Enddo          
      Enddo           
 
Ccccccccccccccccccccc
C     Add Springs   C
Ccccccccccccccccccccc
 
      Do Ispring = 1,Nspring
 
         Ia = Aspring(Ispring)
         Ib = Ia - 1
 
         Dpx = Xx(Ib) - Xx(Ia)
         Dpy = Yy(Ib) - Yy(Ia)
         Dpz = Zz(Ib) - Zz(Ia)
 
         If(Dpx.Ge.Hbx) Then
            Dpx = Dpx - Boxlx
         Elseif(Dpx.Lt.-Hbx) Then
            Dpx = Dpx + Boxlx
         Endif
 
         If(Dpy.Ge.Hby) Then
            Dpy = Dpy - Boxly
         Elseif(Dpy.Lt.-Hby) Then
            Dpy = Dpy + Boxly
         Endif
 
         If(Dpz.Ge.Hbz) Then
            Dpz = Dpz - Boxlz
         Elseif(Dpz.Lt.-Hbz) Then
            Dpz = Dpz + Boxlz
         Endif

         Upot = Upot +  
     &        0.5d0*Bondkb*(Dpx*Dpx + 
     &                      Dpy*Dpy + 
     &                      Dpz*Dpz)

         Ffx = Bondkb*Dpx
         Ffy = Bondkb*Dpy
         Ffz = Bondkb*Dpz

         Fx(Ia)  = Fx(Ia)  + Ffx
         Fy(Ia)  = Fy(Ia)  + Ffy
         Fz(Ia)  = Fz(Ia)  + Ffz
 
         Fx(Ib)  = Fx(Ib)  - Ffx
         Fy(Ib)  = Fy(Ib)  - Ffy
         Fz(Ib)  = Fz(Ib)  - Ffz
      Enddo

      Return
      End
