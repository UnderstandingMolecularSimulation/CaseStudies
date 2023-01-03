      Subroutine Mdloop
      Implicit None
 
      Include 'system.inc'

      Integer I,J
      Double Precision Ran_Vel,A,C1,C2

      Call Sample(1,0)

      C1 = 0.0d0
      C2 = 0.0d0

      Do I=1,Ncycle
         Do J=1,1000

Ccccccccccccccccccccccccccccccccccccc
C     Generate Initial Coordinates  C
C                                   C
C     Xpos = Starting Position      C
C     Vpos = Starting Velocity      C
Ccccccccccccccccccccccccccccccccccccc

            Xpos   = Qstar
            Vpos   = Ran_Vel()
            Thetan = Vpos

            Call Integrate(A)

            C1 = C1 + A
            C2 = C2 + 1.0d0
         Enddo
      Enddo

      Call Sample(3,0)

      Write(6,*) 'Av. Energy Drift      :',C1/C2

      Return
      End
