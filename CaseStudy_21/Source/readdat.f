      Subroutine Readdat
      Implicit None
 
      Include 'system.inc'
 
Cccccccccccccccccccccccccccccccccccccccc
C     Read In System Information       C
C                                      C
C     Nstep = Number Of Cycles         C
C     Ptemp = Prob. For Swapping       C
C     Ntemp = Numberof Temperatures    C
Cccccccccccccccccccccccccccccccccccccccc
 
      Double Precision Temp(9),U,Ran_Uniform
      Integer          I
      Logical          L

      Read(21,*)
      Read(21,*) Nstep,Ptemp,Ntemp

      If(Ntemp.Lt.1.Or.Ntemp.Gt.9) Stop

      Read(21,*)
      Read(21,*) (Temp(I),I=1,Ntemp)

      Do I=1,Ntemp
         Beta(I) = 1.0d0/Temp(I)
         Xold(I) = -1.25d0 + 0.01d0*(Ran_Uniform()-0.5d0)

         Call Force(Xold(I),U,L)

         Uold(I) = U
      Enddo

      Return
      End
