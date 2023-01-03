      Program Tempering
      Implicit None

      Include 'system.inc'

      Integer Sstmm
      Double Precision M1
 
Cccccccccccccccccccccccccccccccccccccccccccccccccc
C     Parallel Tempering Of A Single Particle    C
C     Written By Thijs Vlugt On 6-10-1998        C
Cccccccccccccccccccccccccccccccccccccccccccccccccc
 
      Twopi = 8.0d0*Datan(1.0d0)
      M1    = 0.001d0*Dble(Mod((10+10*Sstmm()),1000))

      If(M1.Lt.0.001d0) M1 = 0.001d0
      If(M1.Gt.0.999d0) M1 = 0.999d0

      Call Genrand(M1)

Cccccccccccccccccccccccccccccccccccccccccccccccc
C     Read Data From Disk                      C
Cccccccccccccccccccccccccccccccccccccccccccccccc
 
      Call Readdat
 
Cccccccccccccccccccccccccccccccccccccccccccccccc
C     Finally, Perform An Md Simulation        C
Cccccccccccccccccccccccccccccccccccccccccccccccc
 
      Call Mcloop
 
Cccccccccccccccccccccccccccccccccccccccccccccccc
C     End Of The Program                       C
Cccccccccccccccccccccccccccccccccccccccccccccccc
 
      Stop
      End
