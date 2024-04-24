#
# An automatic makefile to make a load module file or an archived
# library file on a heterogeneous platform.
#
#                                            Version February 8th, 1999
#
#     written by ckato (Chisachi Kato) of MERL HITACHI, June 19th, 1995.
#
#     Usage:
#         Place in the current directory, a file named "FILES" which
#	 defines the following macros
#
#		PROG = program file name    to make
#		LIBS = library file name(s) to make or needed to make
#		       $PROG
#		OBJS = object file name(s) needed to make $PROG or $LIBS
#
#        and type:
#
#            % make CPU=`uname`
#
#        if you are making a load module file, or type:
#
#            % make CPU=`uname` lib
#
#        if you are making a library file.
#
#         The above macros, "PROG", "LIBS", and/or "OBJS", can also be
#        defined in the shell environment or in the command line.  But,
#        be sure to at least place a file named "FILES", otherwise make
#        would fail. IF "CPU" has already been defined in the current
#        shell, the argument "CPU=`uname`" can be omitted.
#        
#     Notes:
#        (1) Since this makefile is intented for use in 
#        a multi-platformed unix system, it removes all the object files
#        (.o files) before quitting. If make fails, be sure to remove
#        all the object files manually by typing:
#
#            % make clean
#
#        (2) If make fails for some reasons, be sure to unlink the
#        include files manually by typing:
#
#            % make unlink
#        
#        (3) If you are in HI-UX/MPP system, set "CPU" to "HI-UXMPP",
#        because `uname` will result in an invalid directory name
#        "HI-UX/MPP".
#
#     Descriptions:
#         At the beginning of the make process, this makefile will read
#        a file named $(CPU) from directory $(HOME)/make, which contains
#        information pertinent to the system, such as locations of the
#        system libraries, command names to invoke compilers and
#        appropriate options for the compilers. Your target will be
#        made according to this information. If your target is a library
#        file, the source files will be compiled, archived and saved in
#        directory $(HOME)/lib/$(CPU). If your target is a load module
#        file, the compiled object files (.o files) will be linked with
#        the library files specified by $LIBS (which should have been
#        made in directory $(HOME)/lib/$(CPU) prior to the make), and
#        the executable (load module file) will be saved in directory 
#        $(HOME)/bin/$(CPU). 
#
#         If you wish to use this makefile in a new system, make
#        following five directories, if they do not exist, by typing:
#
#            % mkdir $(HOME)/make
#            % mkdir $(HOME)/lib
#            % mkdir $(HOME)/bin
#
#            % mkdir $(HOME)/lib/`uname`
#            % mkdir $(HOME)/bin/`uname`
#
#        and place a file named `uname`, in the directory $(HOME)/make,
#        which defines the following macros:
#
#              CPP  = command name to invoke C Pre-Processor
#              POPT = options for C pre-processor
#              CCOM = command name to invoke C-compiler
#              COPT = options for C compiler
#              FCOM = command name to invoke Fortran-compiler
#              FOPT = options for Fortran compiler
#
#              INCDIR = directories searched for system include files
#              LIBDIR = directories searched for system library files
#
#     Modification Histories:
#               February 8th, 1999: Modified such that, under the 
#                               assumption that "CPU" has already been
#                               defined, it will read all the
#                               system-dependent information from a file
#                               named $(CPU) in directory named
#                               $(HOME)/make. Switching by `uname`,
#                               namely, recursive call to this makefile,
#                               was ceased because it became no longer 
#                               necessary and due to this change, it
#                               became possible to use this makefile in
#                               a system where "make" command uses only
#                               "sh", such as "HI-UX/MPP" or "OSF1".
#
#               February 4th, 1999: "UNIX_System_V", "OSF1", and "SunOS"
#                               were added to the available systems.
#
#               February 4th, 1999: Command to invoke Fortran compiler
#                               on AIX changed from "mpxlf" to "f77",
#                               because parallel environment is not
#                               installed in RS/6000's in IIS. To link
#                               with "poe" libraries, change the command
#                               back to "mpxlf", or explicitly specify
#                               all the libraries needed for parallel
#                               environment.
#
#               August  27th, 1997: '-avoid_got_overflow' compiler
#                               option added for IRIX64 for avoiding
#                               'GP_Overflow' problem which would
#                               otherwise occur in the linking stage.
#
#               July    22nd, 1996: Fortran compiler on HI-UX changed
#                               from f77 to f90 (with appropriate option
#                               changes) to append an underscore
#                               to external symbol names, which 
#                               guarantees compatibility with other
#                               systems. Compiler option '+ppu' of f77
#                               appends an underscore to external symbol
#                               names. With this option enabled,
#                               however, external calls to fortran
#                               functions such as 'sin' or 'exp' cannot
#                               be resolved. There is another problem
#                               with FILE I/O with f77 compiler, which
#                               also made the above compiler change.
#
#               April  30nd,2003:Several modifications were done for releas of LES3D
#                                                                        by Y.Yamade. 
#                              (1)"CPU" was abolished for simplicity.
#                                 So the libraly files will be put on $(HOME)/lib,
#                                 and load module will be put on $(HOME)/bin. 
#                              (2)"link" and "unlink" were abolished for simplicity.
#                                 Note that $(HOME)/include will be alwayas included.
#
#               Frebrary 12th,2010:'loadhere' function was added By Y.Yamade.
#
#
# Set Environments
#LES3DHOME   = 
HOME       = ${LES3DHOME}

include $(HOME)/make/OPTION

FINC.DIR   = $(HOME)/include
FINC.FILE1 = gf.h 
FINC.FILE2 = gfdummy.h 
FINC.FILE3 = gn.h 

include FILES

load: printload  makeload

here: printload  makeloadhere  

lib:  printlib   makelib

makeload: $(OBJS)
	@echo
	@echo Creating load module file...
	$(LINK) $(LOPT) $(OBJS) -o $(HOME)/bin/$(PROG) -L$(HOME)/lib/ -L$(LIBDIR) $(LIBS)

makeloadhere: $(OBJS)
	@echo
	@echo Creating load module file here ...
	$(LINK) $(LOPT) $(OBJS) -o ./$(PROG) -L$(HOME)/lib/ -L$(LIBDIR) $(LIBS) 

makelib: $(OBJS)
	@echo
	@echo Archiving object files ...
	@ar -vru $(HOME)/lib/$(LIBS) $(OBJS)

printload:
	@echo
	@echo Making $(PROG) .

printlib:
	@echo
	@echo Making $(LIBS) .

link:
	@echo
	@echo Symbolic-linking include files ...
	@ln -s $(FINC.DIR)/$(FINC.FILE1) .
	@ln -s $(FINC.DIR)/$(FINC.FILE2) .
	@ln -s $(FINC.DIR)/$(FINC.FILE3) .
	@echo
	@echo Compiling source files ...

unlink:
	@echo
	@echo Unlinking include files ...
	@rm ./$(FINC.FILE1)
	@rm ./$(FINC.FILE2)
	@rm ./$(FINC.FILE3)

clean:
	@echo
	@echo Removing object files
	@rm -f *.o

.f.o:
	@echo
	@echo ----------------------------------------------
	@echo Making $@ from $<
	@echo ----------------------------------------------
# 	$(CPP) $(POPT) $*.f > tmp.f ; $(FCOM) -I$(HOME)/include -I$(INCDIR) $(FOPT) -c tmp.f ; mv tmp.o $*.o; rm tmp.f
#	$(CPP) $(POPT) $*.f > tmp.f ; $(FCOM) -I$(HOME)/include -I$(INCDIR) $(FOPT) -c tmp.f ; mv tmp.o $*.o; rm tmp.f; mv tmp.lst $*.lst
	$(FCOM) -I$(HOME)/include -I$(INCDIR) $(FOPT) -c $<

.c.o:
	@echo
	@echo ----------------------------------------------
	@echo Making $@ from $<
	@echo ----------------------------------------------
	$(CCOM) $(COPT) -c  $<

