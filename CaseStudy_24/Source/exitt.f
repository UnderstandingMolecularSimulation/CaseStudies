      Subroutine Exitt(Swt)
      Implicit None

Ccccccccccccccccccccccccccccc
C     End Of The Program    C
Ccccccccccccccccccccccccccccc 

      Integer Swt

      If (Swt.Ne.2) Then
         Write(6,*)
         Write(6,*) 'The Program Ended Normally'
         Write(6,*)
      Else
         Write(6,*)
         Write(6,*) 'The Program Ended With Errors !!!'
         Write(6,*)
         Write(6,*)
      Endif

      Stop
      End
