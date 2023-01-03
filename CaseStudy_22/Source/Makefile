FCOMPFLAGS    =	-O

FFLAGS        =	$(FCOMPFLAGS)
LDFLAGS       =	$(FCOMPFLAGS)

LD            =	f77
FC            =	f77
CC            = cc

MAKEFILE      =	Makefile
PROGRAM       =	md

INCS	      =	system.inc

OBJS	      =	checkdis.o          \
		force.o             \
		init.o              \
		integrate.o         \
		md.o                \
		mdloop.o            \
		ranf.o              \
		rangauss.o          \
		ransphere.o         \
		readdat.o           \
		shake.o             \
		ttime.o

all:		$(PROGRAM)

$(PROGRAM)::	$(INCS)
		@/bin/rm -f $(OBJS) core

$(PROGRAM)::	$(OBJS) $(MAKEFILE)
		@$(LD) $(LDFLAGS) $(OBJS) -o $(PROGRAM)

clean:;		@rm -f $(OBJS) core
