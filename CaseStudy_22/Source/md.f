      Program Md
      Implicit None

      Include 'system.inc'
 
Cccccccccccccccccccccccccccccccccccccccccccccccc
C     Molecular Dynamics Program Of Argon,     C
C     Written By Thijs Vlugt On 6-10-1998      C
Cccccccccccccccccccccccccccccccccccccccccccccccc
 
Cccccccccccccccccccccccccccccccccccccccccccccccc
C     Read Data From Disk                      C
Cccccccccccccccccccccccccccccccccccccccccccccccc
 
      Call Readdat
 
Cccccccccccccccccccccccccccccccccccccccccccccccc
C     Generate Initial Coordinates/Velocities  C
Cccccccccccccccccccccccccccccccccccccccccccccccc
 
      Call Init
 
Cccccccccccccccccccccccccccccccccccccccccccccccc
C     Finally, Perform An Md Simulation        C
Cccccccccccccccccccccccccccccccccccccccccccccccc
 
      Call Mdloop
 
Cccccccccccccccccccccccccccccccccccccccccccccccc
C     End Of The Program                       C
Cccccccccccccccccccccccccccccccccccccccccccccccc
 
      Stop
      End
