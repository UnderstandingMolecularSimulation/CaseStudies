FCOMPFLAGS    =	-O
CCOMPFLAGS    =	-O

FFLAGS        =	$(FCOMPFLAGS)
CFLAGS        = $(CCOMPFLAGS)
LDFLAGS       =	$(FCOMPFLAGS)

LD            =	f77
FC            =	f77
CC            =	cc

MAKEFILE      =	Makefile
PROGRAM       =	path

INCS	      =	system.inc

OBJS	      =	convert_num.o      \
		exitt.o            \
		force.o            \
		in_A.o             \
		in_B.o             \
		in_Slice.o         \
		md.o               \
		movie.o            \
		order.o            \
		path.o             \
		pathensemble.o     \
		ran_gauss.o        \
		ran_uniform.o      \
		readpath.o         \
		sample_avhb.o      \
		sample_umbrella.o  \
		shifting.o         \
		shooting.o         \
		sstmm.o            \
		ttime.o            \
		writepath.o

all:		$(PROGRAM)

$(PROGRAM)::	$(INCS)
		@/bin/rm -f $(OBJS) core

$(PROGRAM)::	$(OBJS) $(MAKEFILE)
		@$(LD) $(LDFLAGS) $(OBJS) -o $(PROGRAM)

clean:;		@rm -f $(OBJS) core
