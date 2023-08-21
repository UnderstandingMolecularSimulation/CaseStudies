      Function Ran_Gauss()
      Implicit None
 
Ccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C     Random Number From A Gaussian Distribution      C
C     Quite Rough, But Good Enough Here....           C
Ccccccccccccccccccccccccccccccccccccccccccccccccccccccc
 
      Integer          I
      Double Precision Ran_Gauss,Ran_Uniform

      Ran_Gauss = -6.0d0

      Do I=1,12
         Ran_Gauss = Ran_Gauss + RAND()
      Enddo
 
      Return
      End
