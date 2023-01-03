      Subroutine Md(Ntstart,Nsign,Nstep,Av1)
      Implicit None

      Include 'system.inc'
 
      Double Precision Ukin,Utot,Av1,Av2,Av0,Upot,Fx,Fy,
     &     Fxn,Fyn
 
      Integer          Iii,Iiii,Nstep,Nsign,Ntstart
      Logical          In_A,In_B

Ccccccccccccccccccccccccccccc              
C     Loop Over All Steps   C
C     Calc Initial Force    C
Ccccccccccccccccccccccccccccc
             
      Call Force(Rxx,Ryy,Upot,Fx,Fy)
      
      Ukin = 0.5d0*(Vxx**2 + Vyy**2)
      Utot = Upot + Ukin
      Av0  = Utot
      Av1  = 0.0d0
      Av2  = 0.0d0
      
Cccccccccccccccccccccccccccc
C     Initial Point Start  C
Cccccccccccccccccccccccccccc

      La(Ntstart)    = In_A()
      Lb(Ntstart)    = In_B()
      Eetra(Ntstart) = Utot

      If(Nsign.Eq.1) Then
         Xxtra(Ntstart) = Rxx
         Yytra(Ntstart) = Ryy
         Vxtra(Ntstart) = Vxx
         Vytra(Ntstart) = Vyy
      Else                   
         Xxtra(Ntstart) = Rxx
         Yytra(Ntstart) = Ryy
         Vxtra(Ntstart) = -Vxx
         Vytra(Ntstart) = -Vyy
      Endif
      
Ccccccccccccccccccccccccccccccccccccccccccc
C     Return If Nothing Has To Be Done    C
Ccccccccccccccccccccccccccccccccccccccccccc

      If(Nstep.Eq.0) Return

Cccccccccccccccccccccccccccccc
C     Loop Over All Cycles   C
C     Loop Over Nshort       C
Cccccccccccccccccccccccccccccc

      Do Iii = 1,Nstep
      
         Rxx = Rxx + Vxx*Tstep + Tstep2*Fx
         Ryy = Ryy + Vyy*Tstep + Tstep2*Fy

         Call Force(Rxx,Ryy,Upot,Fxn,Fyn)

         Vxx  = Vxx + Tstep3*(Fx+Fxn)
         Vyy  = Vyy + Tstep3*(Fy+Fyn)
         Fx   = Fxn
         Fy   = Fyn
         Ukin = 0.5d0*(Vxx**2 + Vyy**2)
         Utot = Ukin + Upot
         Av1  = Av1  + Dabs((Av0-Utot)/Av0)
         Av2  = Av2  + 1.0d0
      
Ccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C     Store Particle Positions En Velocities;           C
C     Check If The Particle Is In A Or B At Slice Iiii  C
Ccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

         Iiii = Ntstart + Iii*Nsign

         La(Iiii)    = In_A()
         Lb(Iiii)    = In_B()
         Eetra(Iiii) = Utot 

         If(Nsign.Eq.1) Then
            Xxtra(Iiii) = Rxx
            Yytra(Iiii) = Ryy
            Vxtra(Iiii) = Vxx
            Vytra(Iiii) = Vyy
         Else     
            Xxtra(Iiii) = Rxx
            Yytra(Iiii) = Ryy
            Vxtra(Iiii) = -Vxx
            Vytra(Iiii) = -Vyy
         Endif
      Enddo

Ccccccccccccccccccc
C     The End     C
Ccccccccccccccccccc

      If(Av2.Gt.0.5d0) Av1 = Av1/Av2
            
      Return
      End
