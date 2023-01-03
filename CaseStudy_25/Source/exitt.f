      Subroutine Exitt(Swt)
      Implicit None
 
      Integer Swt
 
      Write(6,*)
      Write(6,*)
 
      If (Swt.Ne.2) Then
         Write(6,*) 'Program Ended Normally'
      Else
         Write(6,*) '*************************************'
         Write(6,*) '*   Program Ended With Errors !!!   *'
         Write(6,*) '*************************************'
      Endif
 
      Write(6,*)
      Write(6,*)
       
      Stop
      End
