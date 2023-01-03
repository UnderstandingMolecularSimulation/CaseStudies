FCOMPFLAGS    =	-O
CCOMPFLAGS    =	-O

FFLAGS        =	$(FCOMPFLAGS)
CFLAGS        = $(CCOMPFLAGS)
LDFLAGS       =	$(FCOMPFLAGS)

LD            =	f77
FC            =	f77
CC            =	cc

MAKEFILE      =	Makefile
PROG1         =	avhb
PROG2         =	umbrella
OBJS1	      =	avhb.o 
OBJS2         =	umbrella.o

all:		$(PROG1)

$(PROG1)::	$(OBJS1) $(OBJS2) $(MAKEFILE)
		@$(LD) $(LDFLAGS) $(OBJS1) -o $(PROG1)
		@$(LD) $(LDFLAGS) $(OBJS2) -o $(PROG2)

clean:;		@rm -f $(OBJS1) $(OBJS2) core
