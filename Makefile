NAME = gravs2
GRADJ = Gradj306
GRREDU = Grredu304

ifeq ($(OS),Windows_NT)
	FC = gfortran.exe
	GRADJEXE = $(GRADJ).exe
	GRREDUEXE = $(GRREDU).exe
else
	FC = gfortran
	GRADJEXE = $(GRADJ)
	GRREDUEXE = $(GRREDU)
endif

SRC = ./source/
BUILD = ./build/
INCLUDE = $(SRC)include/
FFLAGS = -std=legacy

all: grredu gradj
	rm -rf *.$(OBJ) *.mod *.o

grredu: $(SRC)Grredu304.f
	$(FC) -g -std=legacy -o $(BUILD)$(GRREDUEXE) $(SRC)Grredu304.f -I$(INCLUDE)

gradj: $(SRC)Gradj306.f
	$(FC) -g -std=legacy -o $(BUILD)$(GRADJEXE) $(SRC)Gradj306.f -I$(INCLUDE)

debug: FFLAGS := $(FLAGS) -g
debug: all

clean:
	rm -rf *.$(OBJ) *.mod $(BUILD)$(GRADJEXE) $(BUILD)$(GRREDUEXE)

install:
	cp $(BUILD)$(GRADJEXE) $(BUILD)$(GRREDU) /opt/bin
