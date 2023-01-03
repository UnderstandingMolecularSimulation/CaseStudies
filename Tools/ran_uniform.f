        Subroutine Ran_Init(Ijkl)
        Implicit None
        Integer Ijkl

Ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C       This Is The Initialization Routine For Ran_Uniform()                  C
C       Note: The Seed Variable Should Be In The Range 0 <= 900 000 000       C
Ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

        Integer I,J,K,L,Ij,Kl
        Integer Ii,Jj,M
        Double Precision S,T

        Double Precision    U(97),C,Cd,Cm
        Integer I97,J97
        Logical Initialised
        Common  /Raset1/ U,C,Cd,Cm,I97,J97,Initialised

        Initialised = .False.

        If( Ijkl .Lt. 0  .Or.  Ijkl .Gt. 900 000 000) Then
            Write(6,*) 'The Random Number Seed Must Have A Value ',
     &                 'Between 0 And 900 000 000'
              Stop
        Endif

        Ij = Ijkl / 30082
        Kl = Ijkl - 30082 * Ij

        I = Mod(Ij/177,177) + 2
        J = Mod(Ij    ,177) + 2
        K = Mod(Kl/169,178) + 1
        L = Mod(Kl,   169)

        Do Ii = 1,97
           S = 0.0d0
           T = 0.5d0
           Do Jj = 1,24
              M = Mod(Mod(I*J,179)*K,179)
              I = J
              J = K
              K = M
              L = Mod(53*L+1,169)
              If (Mod(L*M,64) .Ge. 32) S = S + T
              T = 0.5d0 * T
           End Do
           U(Ii) = S
        End Do

        C           = 362436.0d0 / 16777216.0d0
        Cd          = 7654321.0d0 / 16777216.0d0
        Cm          = 16777213.0d0 /16777216.0d0
        I97         = 97
        J97         = 33
        Initialised = .True.

        Return
        End


        Function Ran_Uniform()
        Implicit None

Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C     Generate A Uniformly Distributed Randomnumber Between 0 And 1    C
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

        Double Precision U(97),C,Cd,Cm,Uni,Ran_Uniform
        Integer I97,J97
        Logical Initialised

        Common /Raset1/ U,C,Cd,Cm,I97,J97,Initialised

        If (.Not. Initialised) Then
          Write(6,*) 'Ran_Uniform: Initialise Ran_Uniform With Ran_Init'
          Stop
        Endif

        Uni = U(I97) - U(J97)
        If( Uni .Lt. 0.0d0 ) Uni = Uni + 1.0d0
        U(I97) = Uni
        I97 = I97 - 1
        If(I97 .Eq. 0) I97 = 97
        J97 = J97 - 1
        If(J97 .Eq. 0) J97 = 97
        C = C - Cd
        If( C .Lt. 0.0d0 ) C = C + Cm
        Uni = Uni - C
        If( Uni .Lt. 0.0d0 ) Uni = Uni + 1.0d0
        Ran_Uniform = Uni

        Return
        End
