      Subroutine Sample_Dens(Switch)
      Implicit None

      Include 'maxarray.inc'
      Include 'system.inc'

Ccccccccccccccccccccccccccccccccccccccccccccc
C     Sample Density Profile Along Z-Axis   C
C     Take The Head Of Each Molecule        C
Ccccccccccccccccccccccccccccccccccccccccccccc

      Integer Switch,I,Ii
      Double Precision Dnum,Deltx,Dens(Maxslab,Maxcom)

      Save Dens,Dnum,Deltx

      If(Switch.Eq.1) Then

         Dnum  = 0.0d0
         Deltx = Dble(Maxslab)/Boxlz

         Do I=1,Maxcom
            Do Ii=1,Maxslab
               Dens(Ii,I) = 0.0d0
            Enddo
         Enddo

      Elseif(Switch.Eq.2) Then

         Dnum = Dnum + 1.0d0

         Do I=1,Molecule
            Ii                    = 1 + Idint(Deltx*Zz(Smolecule(I)))
            Dens(Ii,Tmolecule(I)) = Dble(Nuall(Tmolecule(I))) + 
     &           Dens(Ii,Tmolecule(I))
         Enddo

      Else

         Dnum = Dble(Maxslab)/(Dnum*Boxlx*Boxly*Boxlz)

         Do I=1,Numcom
            Do Ii=1,Maxslab
               Dens(Ii,I) = Dens(Ii,I)*Dnum
            Enddo
         Enddo

         Open(21,File="Densprof")

         Do Ii=1,Maxslab
            Write(21,'(10E15.7)') (Dble(Ii)-0.5d0)*Boxlz/Dble(Maxslab),
     &           (Dens(Ii,I),I=1,Numcom)
         Enddo

         Close(21)

      Endif

      Return
      End
