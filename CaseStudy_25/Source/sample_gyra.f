      Subroutine Sample_Gyra(Ichoise)
      Implicit None

      Include 'maxarray.inc'
      Include 'system.inc'

Cccccccccccccccccccccccccccccccccccccccccccccccccc
C     Samples The Radial Distribution Function   C
Cccccccccccccccccccccccccccccccccccccccccccccccccc

      Integer I,J,Maxx,Ichoise,A

      Parameter(Maxx = 2000)

      Double Precision Ggt,Gg(Maxx),
     &     Delta,R2,Dpx,Dpy,Dpz,Rmax

      Save Ggt,Gg,Delta,Rmax

      If(Ichoise.Eq.1) Then
         Do I=1,Maxx
            Gg(I) = 0.0d0
         Enddo
         
         Rmax  = (0.49999d0*Min(Boxlx,Boxly,Boxlz))**2
         Ggt   = 0.0d0
         Delta = Dble(Maxx-1)/(0.5d0*Min(Boxlx,Boxly,Boxlz))
      
      Elseif(Ichoise.Eq.2) Then

         Ggt = Ggt + 1.0d0

         Do I=1,Npart-1
            Do J=I+1,Npart

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

               If(R2.Lt.Rmax) Then
                  A     = 1     + Idint(Dsqrt(R2)*Delta)
                  Gg(A) = 2.0d0 + Gg(A)  
               Endif
            Enddo
         Enddo

      Else

         Ggt   = 1.0d0/(Ggt*Dble(Npart))
         Delta = 1.0d0/Delta

         Open(21,File="Gyra")

         Do I=1,Maxx-1
            R2 = (16.0d0*Datan(1.0d0)/3.0d0)*
     &           (Dble(Npart)/(Boxlx*Boxly*Boxlz))*(Delta**3)*
     &           ((Dble(I))**3 - (Dble(I-1)**3))
            
            Write(21,*) ((Dble(I)-0.5d0)*Delta),Gg(I)*Ggt/R2
         Enddo

         Close(21)
      Endif

      Return
      End
