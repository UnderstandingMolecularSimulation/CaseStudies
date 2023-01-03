      Subroutine Force
      Implicit None
 
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C     Calculate The Random And Conservative Force            C
C     The Dissipative Force Is Calculated In Dpd.F           C
C     A Linked-Cellist Is Used To Compute The Interactions   C
C     A Table Is Mad To Compute The Dragforce                C
C                                                            C
C     Pressure And Surface Tension Are Also Recorder         C
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      Include 'maxarray.inc'
      Include 'selflist.inc'
      Include 'system.inc'
 
      Integer Icx1,Icy1,Icz1,Icx2,Icy2,Icz2,Icell1,
     &     Icell2,I,J,Idc,Icell,Ipart,Head(0:Maxcell),
     &     Link(Maxtotal),Ia,Ib,Ispring,Jran
 
      Double Precision Rsq,Dpx,Dpy,Dpz,Rsqi,
     &     Ffx,Ffy,Ffz,R2,Prep,Fac,Dran,Rim,
     &     Ran_Uniform,Pressxx,Pressyy,Presszz

Cccccccccccccccccccccccccccccccccccccccccc
C     Dirty Rng                          C
C     Initialize Using A Better Rng      C
Cccccccccccccccccccccccccccccccccccccccccc

      Jran = 10 + Idint(Ran_Uniform()*100000.0d0)
      Rim  = 1.0d0/139968.0d0

      Do J=1,(10+Idint(Ran_Uniform()*100.0d0))
         Jran = Mod(Jran*205+29573,139968)
      Enddo

      Upot     = 0.0d0
      Pressxx  = 0.0d0
      Pressyy  = 0.0d0
      Presszz  = 0.0d0
      Selflist = 0
      
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

                        R2 = Dpx**2 + Dpy**2 + Dpz**2
 
                        If(R2.Lt.1.0d0) Then
                           Rsq      = Dsqrt(R2)
                           Rsqi     = 1.0d0/Rsq
                           Prep     = Phrep(Type(I),Type(J))
                           Jran     = Mod(Jran*205+29573,139968)
                           Dran     = Dble(Jran)*Rim - 0.5d0
                           Selflist = Selflist + 1

                           Spi(Selflist)  = I
                           Spj(Selflist)  = J
                           Sdpx(Selflist) = Dpx
                           Sdpy(Selflist) = Dpy
                           Sdpz(Selflist) = Dpz
                           Srsq(Selflist) = Gamma*(Rsqi - 1.0d0)**2
 
                           Fac  = (Sigmat*Dran + Prep)*(Rsqi-1.0d0)
                           Upot = Upot + 
     &                          Prep*(Rsq*(0.5d0*Rsq - 1.0d0) + 0.5d0)

                           Dran = Prep*(Rsqi-1.0d0)

                           Pressxx = Pressxx + Dpx*Dpx*Dran
                           Pressyy = Pressyy + Dpy*Dpy*Dran
                           Presszz = Presszz + Dpz*Dpz*Dran

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
 
      If(Selflist.Gt.Maxself) Then
         Write(6,*) 'Selflist Is Too Short !!'
         Call Exitt(2)
      Endif

      Mlll = Max(Mlll,Selflist)

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

         Pressxx = Pressxx - Ffx*Dpx
         Pressyy = Pressyy - Ffy*Dpy
         Presszz = Presszz - Ffz*Dpz
 
         Fx(Ia)  = Fx(Ia)  + Ffx
         Fy(Ia)  = Fy(Ia)  + Ffy
         Fz(Ia)  = Fz(Ia)  + Ffz
 
         Fx(Ib)  = Fx(Ib)  - Ffx
         Fy(Ib)  = Fy(Ib)  - Ffy
         Fz(Ib)  = Fz(Ib)  - Ffz
      Enddo

      Press = ((Dble(Npart)/Beta) + 
     &     ((1.0d0/3.0d0)*(Pressxx + Pressyy + Presszz)))/
     &     (Boxlx*Boxly*Boxlz)

      Surface = (Presszz - 0.5d0*(Pressxx + Pressyy))/(Boxlx*Boxly)

      Return
      End
