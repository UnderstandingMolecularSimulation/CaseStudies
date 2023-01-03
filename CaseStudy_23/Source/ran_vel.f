      Function Ran_Vel()
      Implicit None

      Include 'system.inc'
 
      Double Precision Ran_Vel,Ran_Gauss
 
      Ran_Vel = Dsqrt(Temp)*Ran_Gauss()
 
      Return
      End
