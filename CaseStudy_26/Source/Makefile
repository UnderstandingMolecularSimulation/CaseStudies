COMPFLAGS     = -O # -check_bounds  -g -trapeuv

CFLAGS	      = $(COMPFLAGS)
PFLAGS	      = $(COMPFLAGS)
FFLAGS	      = $(COMPFLAGS)
CCFLAGS       = $(COMPFLAGS)
CXXFLAGS      = $(COMPFLAGS)

DEST	      = .

# Uncomment this if you want system header files to be expanded
#
# SYSHDRS       =

EXTHDRS	      =

HDRS	      =

INSTALL	      = install

LD	      = f77

LDFLAGS	      = $(COMPFLAGS)

LIBS	      =

LINTLIBS      =

LINTFLAGS     = -u $(CFLAGS)

MAKEFILE      = Makefile

OBJS	      = adjust.o \
		cell.o \
		corp.o \
		coru.o \
		dist2.o \
		ener.o \
		eneri.o \
		lattice.o \
		mc_nvt.o \
		mcmove.o \
		neigcell.o \
		new_nlist.o \
		new_vlist.o \
		ranf.o \
		rantest.o \
		readdat.o \
		sample.o \
		store.o \
		toterg.o

PRINTFLAGS    =

LP	      = lp

LPFLAGS       = 

PROGRAM       = mc_nvtNL

SHELL	      = /bin/sh

SRCS	      = adjust.f \
		cell.f \
		corp.f \
		coru.f \
		dist2.f \
		ener.f \
		eneri.f \
		lattice.f \
		mc_nvt.f \
		mcmove.f \
		neigcell.f \
		new_nlist.f \
		new_vlist.f \
		ranf.f \
		rantest.f \
		readdat.f \
		sample.f \
		store.f \
		toterg.f

$(PROGRAM):     $(OBJS) $(LIBS) $(MAKEFILE)
		@echo "Linking $(PROGRAM) ..."
		@$(LD) $(LDFLAGS) $(OBJS) $(LIBS) -o $(PROGRAM)
		@echo "done"

clean:;		@rm -f *.o core

clobber:;	@rm -f $(OBJS) $(PROGRAM) core tags

depend:;	@mkmf -f $(MAKEFILE) ROOT=$(ROOT)

echo:;		@echo $(HDRS) $(SRCS)

-1:;		@ctags -wx $(HDRS) $(SRCS)

install:	$(PROGRAM)
		@echo Installing $(PROGRAM) in $(DEST)
		@-strip $(PROGRAM)
		@if [ $(DEST) != . ]; then \
		(rm -f $(DEST)/$(PROGRAM); $(INSTALL) -f $(DEST) $(PROGRAM)); fi

lint:		$(LINTLIBS) $(SRCS) $(HDRS) $(EXTHDRS)
		lint $(LINTFLAGS) $(LINTLIBS) $(SRCS)

print:;		@$(PRINT) $(PRINTFLAGS) $(HDRS) $(SRCS) | $(LP) $(LPFLAGS)

tags:           $(HDRS) $(SRCS); @ctags $(HDRS) $(SRCS)

update:		$(DEST)/$(PROGRAM)

touch:;		@touch $(SRCS) $(HDRS)
