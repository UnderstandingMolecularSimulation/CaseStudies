      Subroutine Convert_Num(I,Number)
      Implicit None

Ccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C     Convert A Number Between 0 And 999 To A String    C
C     Add The Number Of The Temperature...              C
Ccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      Integer     I
      Character*3 Num
      Character*5 Number

      If(I.Lt.0) Then
         Write(6,*) 'I Cannot Be Negative !!!'
         Call Exitt(2)
      Elseif(I.Lt.10) Then
         Num = '00'//Char(48+I)
      Elseif(I.Lt.100) Then
         Num = '0'//Char(48+(I/10))//Char(48+I-((I/10)*10))
      Elseif(I.Lt.1000) Then
         Num = Char(48+(I/100))//Char(48+(I - (100*(I/100)))/10)//
     &            Char(48+(I - (I/10)*10))
      Else
         Num = 'XXX'
      Endif

      Number = 'TT'//Num
     
      Return
      End
