      Program Path
      Implicit None

      Include 'system.inc'
           
Ccccccccccccccccccccccccccccccccccccccc
C     Main Program.. Initialize Rng   C
Ccccccccccccccccccccccccccccccccccccccc
 
      Integer          I,J,Sstmm
      Double Precision M1,Ran_Uniform

      J = Sstmm()

      Write(6,*)
      Write(6,*)
      Write(6,*) 'Path Ensemble Sampling'
      Write(6,*) 'Written By Thijs J.H. Vlugt'
      Write(6,*)
      Write(6,*) 'Special Thanks To Christoph Dellago'
      Write(6,*)
      Write(6,*)
      Write(6,*) 'Initial Seed                           : ',J
      
Cccccccccccccccccccccccccc
C     Initialize Rng     C
Cccccccccccccccccccccccccc

      Onesixth = 1.0d0/6.0d0
      M1       = 0.001d0*Dble(Mod((10+10*J),1000))
      
      If(M1.Lt.0.001d0) M1 = 0.001d0
      If(M1.Gt.0.999d0) M1 = 0.999d0 
      
      Call Genrand(M1)

      J = 10 + Idint(Ran_Uniform()*1000.0d0)

      Do I=1,J
         M1 = Ran_Uniform()
      Enddo

      Write(6,*) 'Random Number                          : ',
     &     Ran_Uniform()
      Write(6,*) 'Random Number                          : ',
     &     Ran_Uniform()
      Write(6,*)
      Write(6,*)
      Write(6,*)

Cccccccccccccccccccc
C     Read Input   C
Cccccccccccccccccccc
      
      Call Pathensemble
      Call Exitt(1)

      Stop
      End
