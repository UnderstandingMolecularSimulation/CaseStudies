      Subroutine Readdat
      Implicit None
 
      Include 'system.inc'
 
Cccccccccccccccccccccccccccccccccccccc
C     Read In System Information     C
Cccccccccccccccccccccccccccccccccccccc
      
      Integer I
      Double Precision U,F,Suma

      Read (21,*) Ncycle,Nstep,Tstep,Temp,Qstar
 
      Write (6,*) 'Barrier Crossing Of A Particle Over A Energy Barrier'
      Write (6,*)
      Write (6,*) 'Number Of Cycles      : ',Ncycle
      Write (6,*) 'Number Of Md Steps    : ',Nstep
      Write (6,*) 'Temperature           : ',Temp
      Write (6,*) 'Position Of Q(Star)   : ',Qstar
      Write (6,*) 'Timestep              : ',Tstep
      Write (6,*)

      Onepi = 4.0d0*Datan(1.0d0)

      If(Qstar.Lt.0.3d0.Or.Qstar.Gt.0.7d0) Then
         Write(6,*) 'Wrong Value Qstar'
         Write(6,*) '0.3<Qstar<0.7'
         Stop
      Endif

C     Suma = Integral Over A

      Suma = 0.0d0

      Do I=1,10001
         Xpos = Dble(I-1)*0.001d0 - 5.0d0
         Call Force(Xpos,U,F)
         If(Xpos.Le.Qstar) Suma = Suma + Dexp(-U/Temp)*0.001d0
      Enddo

      Call Force(Qstar,U,F)
      
      Write (6,*) 'P(Qstar)/P(A)         : ',Dexp(-U/Temp)/Suma
       
      Return
      End
