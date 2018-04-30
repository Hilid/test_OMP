# Faire make ... pour exectuer les commandes associées
# typiquement: make all + make traitement execute le programme puis le traitement 
# enchainer avec make movie crée l'animation

 SHELL = /bin/bash
.PHONY: movie visu clean checks_valgrind


# Compilation avec GFORTRAN
#FC = gfortran
#FCFLAGS = -g -O3


# COMPILATION AVEC IFORT
FC = ifort
FCFLAGS = -g -O3 -traceback -heap-arrays  

EXE = programme
SRC = test.f90

all: compil simu


omp : FCFLAGS += -fopenmp
omp : compil

ompval : FCFLAGS += -fopenmp
ompval : FCFLAGS += -g 
ompval : compil
	valgrind ./$(EXE) --leak-check=full  |& tee log_valgrind.txt 

compil :
	@#Création du répertoire ou stocker les sources compilées
	$(FC) $(SRC) -o $(EXE) $(FCFLAGS)

simu : 
	./programme
