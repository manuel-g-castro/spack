HOME       = ${LES3DHOME}

include $(HOME)/make/OPTION

FINC.DIR   = $(HOME)/include
FINC.FILE1 = gf.h 
FINC.FILE2 = gfdummy.h 
FINC.FILE3 = gn.h 

include FILES

here: printload  makeloadhere  

load: printload  makeload


lib:  printlib   makelib

makeload: $(OBJS)
	@echo
	@echo Creating load module file...
#	$(LINK) $(LOPT) $(OBJS) -o $(HOME)/bin/$(PROG) -L$(HOME)/lib/ -L$(LIBDIR) $(LIBS)
	$(LINK) $(LOPT) $(OBJS) -o $(HOME)/bin/$(PROG) -L$(HOME)/lib/             $(LIBS)

makeloadhere: $(OBJS)
	@echo
	@echo Creating load module file here ...
#	$(LINK) $(LOPT) $(OBJS) -o ./$(PROG) -L$(HOME)/lib/ -L$(LIBDIR) $(LIBS) 
	$(LINK) $(LOPT) $(OBJS) -o ./$(PROG) -L$(HOME)/lib/             $(LIBS) 

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

