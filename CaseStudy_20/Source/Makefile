FCOMPFLAGS    =	-O
CCOMPFLAGS    =	-O

FFLAGS        =	$(FCOMPFLAGS)
CFLAGS        = $(CCOMPFLAGS)
LDFLAGS       =	$(FCOMPFLAGS)

LD            =	f77
FC            =	f77
CC            =	cc

MAKEFILE      =	Makefile
PROGRAM       =	recoil

INCS	      =	system.inc

OBJS	      =	backbox.o      \
		echain.o       \
		echoice.o      \
		grow_cbmc.o    \
		grow_recoil.o  \
		init.o         \
		initlat.o      \
		mccbmc.o       \
		mcmove.o       \
		mcrota.o       \
		open_dir.o     \
		ran_cone.o     \
		ran_sphere.o   \
		ran_uniform.o  \
		recoil.o       \
		sample_gyra.o  \
		sample_rad.o   \
		select.o       \
		store.o        \
		sstmm.o        \
		toterg.o       \
		ttime.o

all:		$(PROGRAM)

$(PROGRAM)::	$(INCS)
		@/bin/rm -f $(OBJS) core

$(PROGRAM)::	$(OBJS) $(MAKEFILE)
		@$(LD) $(LDFLAGS) $(OBJS) -o $(PROGRAM)

clean:;		@rm -f $(OBJS) core
