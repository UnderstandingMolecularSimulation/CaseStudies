      Subroutine Readdat
      Implicit None
 
      Include 'system.inc'

      Integer I,J,K,Ij
 
      Read (21,*) Box,Npart,Nstep,Temp,Tstep,Ninit
      Read (21,*) Bondl,Bondkb,Nrespa,Lrespa,Lcons,Linit
      Read (21,*) Rcutsq,Rcut2,Lambda,Rver
 
      Idum = 1001
      Hbox = 0.5d0*Box
 
      Rcut2sq = Rcut2*Rcut2
      Rcutsq = Rcutsq*Rcutsq
      Rminlab = (Rcut2 - Lambda)**2
 
      Ecut = 4.0d0*((Rcutsq**(-6.0d0)) - 
     &     (Rcutsq**(-3.0d0)))

      Do I=-1,1
         Do J=-1,1
            Do K=-1,1
               Ij = 9*I + 3*J + K
               Tx(Ij) = Dble(I)*Box
               Ty(Ij) = Dble(J)*Box
               Tz(Ij) = Dble(K)*Box
            Enddo
         Enddo
      Enddo
 
      If (Lcons .And. Lrespa) Then
         Write (6,*) 'Respa Does Not Work With Constraints'
         Stop
      Endif
 
      If (Npart.Gt.Maxpart .Or. Mod(Npart,2).Ne.0) Then
         Write (6,*) 'Maximum Number Of Particles : ',Maxpart
         Write (6,*) 'Must Be An Even Number      : ',Npart
         Stop
      Endif
 
      If ((Dsqrt(Rcutsq).Ge.Hbox) .Or. (Rcut2sq.Gt.Rcutsq) .Or. 
     &     (Rminlab.Gt.Rcut2sq) .Or. 
     &     (Rcut2.Lt.0.0d0)) Then
         Write (6,*) 'Error In Cut-Off Radius !!!!'
         Write (6,*) 'Rcut   : ',Dsqrt(Rcutsq)
         Write (6,*) 'Rcut2  : ',Rcut2
         Write (6,*) 'Lambda : ',Lambda
         Stop
      Endif
 
      Nfree = 3*Npart - 3
 
C     Remove The Number Of Bond Constraints From The Number Of D.O.F.
 
      If (Lcons) Nfree = Nfree - (Npart/2)
 
      Write (6,*) 'Molecular Dynamics Program'
 
      If (Lrespa) Then
         Write (6,*) 'Respa Is Used'
      Else
         Write (6,*) 'Respa Is Not Used !!!'
         Nrespa = 1
      Endif
 
      Write (6,*)
      Write (6,*) 'Number Of Particles   : ',Npart
      Write (6,*) 'Degrees Of Freedom    : ',Nfree
      Write (6,*) 'Temperature           : ',Temp
      Write (6,*) 'Boxlength             : ',Box
      Write (6,*) 'Density               : ',Dble(Npart)/(Box*Box*Box)
      Write (6,*) 'Cut-Off Radius        : ',Dsqrt(Rcutsq)
      Write (6,*) 'Verlet Extra          : ',Rver
      Write (6,*) 'Second Cut-Off        : ',Rcut2
      Write (6,*) 'Lambda                : ',Lambda
      Write (6,*) 'Cut-Off Energy        : ',Ecut
      Write (6,*) 'Number Of Steps       : ',Nstep
      Write (6,*) 'Number Of Init Steps  : ',Ninit
      Write (6,*) 'Nrespa                : ',Nrespa
      Write (6,*) 'Timestep (Small)      : ',Tstep
      Write (6,*) 'Timestep (Large)      : ',Dble(Nrespa)*Tstep
      Write (6,*) 'Lo                    : ',Bondl
      Write (6,*) 'Kb                    : ',Bondkb
      
      Return
      End
