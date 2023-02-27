#=================================================================================
#=================================================================================
# Compiler?
#Possible values: (Empty: gfortran)
#                gfortran (version: 9.0 linux and osx)
# F90 = mpifort
 FC = gfortran
#
# Optimize? Empty: default No optimization; 0: No Optimization; 1 Optimzation
OPT = 0
## OpenMP? Empty: default with OpenMP; 0: No OpenMP; 1 with OpenMP
OMP = 1
## Lapack/blas/mkl? Empty: default with Lapack; 0: without Lapack; 1 with Lapack
LAPACK = 1
## force the default integer (without kind) during the compillation.
## default 4: , INT=8 (for kind=8)
INT = 4
#
## how to get external libraries;  "loc" (default): from local zip file, Empty or something else (v0.5): from github
EXTLIB_TYPE = loc
#=================================================================================
#=================================================================================
ifeq ($(FC),)
  FFC      := gfortran
else
  FFC      := $(FC)
endif
ifeq ($(OPT),)
  OOPT      := 1
else
  OOPT      := $(OPT)
endif
ifeq ($(OMP),)
  OOMP      := 1
else
  OOMP      := $(OMP)
endif
ifeq ($(LAPACK),)
  LLAPACK      := 1
else
  LLAPACK      := $(LAPACK)
endif
#===============================================================================
# setup for mpifort
ifeq ($(FFC),mpifort)
  ## MPI compiled with: gfortran or ifort
  MPICORE := $(shell ompi_info | grep 'Fort compiler:' | awk '{print $3}')
  OOMP = 0
endif
#===============================================================================
#
# Operating system, OS? automatic using uname:
OS :=$(shell uname)

# about EVRT, path, versions ...:
LOC_path:= $(shell pwd)
TNUM_ver:=$(shell awk '/Tnum/ {print $$3}' $(LOC_path)/version-EVR-T)
TANA_ver:=$(shell awk '/Tana/ {print $$3}' $(LOC_path)/version-EVR-T)
EVR_ver:=$(shell awk '/EVR/ {print $$3}' $(LOC_path)/version-EVR-T)

# Extension for the object directory and the library
ifeq ($(FFC),mpifort)
  extlibwi_obj:=_$(FFC)_$(MPICORE)_opt$(OOPT)_omp$(OOMP)_lapack$(LLAPACK)_int$(INT)
else
  extlibwi_obj:=_$(FFC)_opt$(OOPT)_omp$(OOMP)_lapack$(LLAPACK)_int$(INT)
endif
extlib_obj:=_$(FFC)_opt$(OOPT)_omp$(OOMP)_lapack$(LLAPACK)



OBJ_DIR = obj/obj$(extlibwi_obj)
$(info ***********OBJ_DIR:            $(OBJ_DIR))
$(shell [ -d $(OBJ_DIR) ] || mkdir -p $(OBJ_DIR))
MOD_DIR=$(OBJ_DIR)
#
# library name
LIBA=libFOR_EVRT$(extlibwi_obj).a
#=================================================================================
# cpp preprocessing
CPPSHELL = -D__COMPILE_DATE="\"$(shell date +"%a %e %b %Y - %H:%M:%S")\"" \
           -D__COMPILE_HOST="\"$(shell hostname -s)\"" \
           -D__COMPILER="'$(FFC)'" \
           -D__COMPILER_VER="'$(FC_VER)'" \
           -D__EVRTPATH="'$(LOC_path)'" \
           -D__EVR_VER="'$(EVR_ver)'" \
           -D__TNUM_VER="'$(TNUM_ver)'" \
           -D__TANA_VER="'$(TANA_ver)'"
CPPSHELL_LAPACK  = -D__LAPACK="$(LLAPACK)"

#===============================================================================
#
#===============================================================================
# external lib (QML, AD_dnSVM ...)
ifeq ($(ExtLibDIR),)
  ExtLibDIR := $(LOC_path)/Ext_Lib
endif

QML_DIR    = $(ExtLibDIR)/QuantumModelLib
QMLMOD_DIR = $(QML_DIR)/OBJ/obj$(extlib_obj)
QMLLIBA    = $(QML_DIR)/libQMLib$(extlib_obj).a

AD_DIR    = $(ExtLibDIR)/AD_dnSVM
ADMOD_DIR = $(AD_DIR)/OBJ/obj$(extlib_obj)
ADLIBA    = $(AD_DIR)/libAD_dnSVM$(extlib_obj).a

QD_DIR    = $(ExtLibDIR)/QDUtilLib
QDMOD_DIR = $(QD_DIR)/OBJ/obj$(extlib_obj)
QDLIBA    = $(QD_DIR)/libQD$(extlib_obj).a

EXTLib     = $(QMLLIBA) $(ADLIBA) $(QDLIBA)
#===============================================================================
#
#===============================================================================
# gfortran (osx and linux)
#ifeq ($(F90),gfortran)
#===============================================================================
ifeq ($(F90),$(filter $(F90),gfortran gfortran-8))

  # opt management
  ifeq ($(OOPT),1)
    FFLAGS = -O5 -g -fbacktrace -funroll-loops -ftree-vectorize -falign-loops=16
  else
    FFLAGS = -Og -g -fbacktrace -fcheck=all -fwhole-file -fcheck=pointer -Wuninitialized -finit-real=nan -finit-integer=nan
  endif

  # integer kind management
  ifeq ($(INT),8)
    FFLAGS += -fdefault-integer-8 -Dint8=1
  endif

  # omp management
  ifeq ($(OOMP),1)
    FFLAGS += -fopenmp
  endif
  FFLAGS0 := $(FFLAGS)


  # where to store the .mod files
  FFLAGS +=-J$(MOD_DIR)

  # where to look the .mod files
  FFLAGS += -I$(QMLMOD_DIR) -I$(ADMOD_DIR) -I$(QDMOD_DIR)

  # some cpreprocessing
  FFLAGS += -cpp $(CPPSHELL)
  ifeq ($(OMP),1)
    FFLAGS += -Drun_openMP=1
  endif

  FLIB   = $(EXTLib)
  # OS management
  ifeq ($(LLAPACK),1)
    ifeq ($(OS),Darwin)    # OSX
      # OSX libs (included lapack+blas)
      FLIB += -framework Accelerate
    else                   # Linux
      # linux libs
      FLIB += -llapack -lblas
      #
      # linux libs with mkl and with openmp
      #FLIB = -L$(MKLROOT)/lib/intel64 -lmkl_intel_lp64 -lmkl_core -lmkl_gnu_thread
      # linux libs with mkl and without openmp
      #FLIB = -L$(MKLROOT)/lib/intel64 -lmkl_intel_lp64 -lmkl_core -lmkl_sequential
    endif
  endif

  FC_VER = $(shell $(FFC) --version | head -1 )

endif
#=================================================================================

#===============================================================================
#===============================================================================
$(info ************************************************************************)
$(info ***********OS:               $(OS))
$(info ***********COMPILER:         $(FFC))
$(info ***********OPTIMIZATION:     $(OOPT))
$(info ***********COMPILER VERSION: $(FC_VER))
ifeq ($(FFC),mpifort)
$(info ***********COMPILED with:    $(MPICORE))
endif
$(info ***********OpenMP:           $(OOMP))
$(info ***********Lapack:           $(LLAPACK))
$(info ***********FFLAGS0:          $(FFLAGS0))
$(info ***********FLIB:             $(FLIB))
$(info ************************************************************************)
$(info ************************************************************************)
$(info ***************** TNUM_ver: $(TNUM_ver))
$(info ***************** TANA_ver: $(TANA_ver))
$(info ****************** EVR_ver: $(EVR_ver))
$(info ************************************************************************)
$(info ************************************************************************)
#==========================================
VPATH = SRC/sub_system SRC/sub_nDindex SRC/sub_dnSVM SRC/sub_module SRC/sub_communf90/sub_math TESTS

Primlib_SRCFILES  = sub_module_MPI.f90  sub_module_system.f90 sub_module_MPI_aux.f90 sub_module_cart.f90

math_SRCFILES = sub_integration.f90 sub_polyortho.f90 sub_function.f90 sub_fft.f90

dnSVM_SRCFILES = \
  sub_module_dnS.f90 sub_module_VecOFdnS.f90 sub_module_MatOFdnS.f90 \
  sub_module_dnV.f90 sub_module_dnM.f90 sub_module_IntVM.f90 \
  sub_module_dnSVM.f90

FiniteDiff_SRCFILES = mod_FiniteDiff.f90

nDindex_SRCFILES  = sub_module_DInd.f90 sub_module_nDindex.f90

nDfit_SRCFILES    = sub_module_nDfit.f90
#============================================================================

SRCFILES= $(Primlib_SRCFILES) $(math_SRCFILES) $(io_SRCFILES) $(dnSVM_SRCFILES) $(FiniteDiff_SRCFILES)  \
          $(nDindex_SRCFILES) $(nDfit_SRCFILES)

OBJ0=${SRCFILES:.f90=.o}
OBJ=$(addprefix $(OBJ_DIR)/, $(OBJ0))
$(info ************ OBJ: $(OBJ))
#
#===============================================
#============= tests ===========================
#===============================================
.PHONY: ut
ut: Test_FOR_EVRT.exe
	@echo "---------------------------------------"
	@echo "Tests FOR_EVRT"
	./Test_FOR_EVRT.exe > tests.log
	@echo "---------------------------------------"
#
Test_FOR_EVRT.exe: $(OBJ_DIR)/Test_FOR_EVRT.o $(LIBA) $(EXTLib)
	$(FFC) $(FFLAGS) -o Test_FOR_EVRT.exe $(OBJ_DIR)/Test_FOR_EVRT.o $(LIBA) $(FLIB)
	@echo "  done Library: Test_FOR_EVRT.exe"
#
$(OBJ_DIR)/Test_FOR_EVRT.o: $(LIBA) $(EXTLib)
#===============================================
#============= Library: FOR_EVRT....a  ========
#===============================================
.PHONY: lib
lib: $(LIBA)

$(LIBA): $(OBJ)
	ar -cr $(LIBA) $(OBJ)
	@echo "  done Library: "$(LIBA)
#
#===============================================
#============= compilation =====================
#===============================================
$(OBJ_DIR)/%.o: %.f90
	@echo "  compile: " $<
	$(FFC) $(FFLAGS) -o $@ -c $<
#===============================================
#================ cleaning =====================
.PHONY: clean cleanall
clean:
	rm -f  $(OBJ_DIR)/*.o
	rm -f *.log 
	rm -f TEST*.x
	@echo "  done cleaning"

cleanall : clean clean_extlib
	rm -fr obj/* build
	rm -f *.a
	rm -f *.exe
	rm -f TESTS/res* TESTS/*log
	@echo "  done all cleaning"
#===============================================
#================ zip and copy the directory ===
ExtLibSAVEDIR := /Users/lauvergn/git/Ext_Lib
BaseName := FOR_EVRT
.PHONY: zip
zip: cleanall
	test -d $(ExtLibSAVEDIR) || (echo $(ExtLibDIR) "does not exist" ; exit 1)
	cd $(ExtLibSAVEDIR) ; rm -rf $(BaseName)_devloc
	mkdir $(ExtLibSAVEDIR)/$(BaseName)_devloc
	cp -r * $(ExtLibSAVEDIR)/$(BaseName)_devloc
	cd $(ExtLibSAVEDIR) ; zip -r Save_$(BaseName)_devloc.zip $(BaseName)_devloc
	cd $(ExtLibSAVEDIR) ; rm -rf $(BaseName)_devloc
	cd $(ExtLibSAVEDIR) ; ./cp_FOR_EVRT.sh
	@echo "  done zip"
#===============================================
#=== external libraries ========================
# AD_dnSVM + QML Lib
#===============================================
#
$(QMLLIBA):
	@test -d $(ExtLibDIR) || (echo $(ExtLibDIR) "does not exist" ; exit 1)
	@test -d $(QML_DIR) || (cd $(ExtLibDIR) ; ./get_QML.sh $(EXTLIB_TYPE))
	@test -d $(QML_DIR) || (echo $(QML_DIR) "does not exist" ; exit 1)
	cd $(QML_DIR) ; make lib FC=$(FFC) OPT=$(OOPT) OMP=$(OOMP) LAPACK=$(LLAPACK) ExtLibDIR=$(ExtLibDIR)
	@echo "  done " $(QDLIBA) " in "$(BaseName)
#
$(ADLIBA):
	@test -d $(ExtLibDIR) || (echo $(ExtLibDIR) "does not exist" ; exit 1)
	@test -d $(AD_DIR) || (cd $(ExtLibDIR) ; ./get_dnSVM.sh  $(EXTLIB_TYPE))
	@test -d $(AD_DIR) || (echo $(AD_DIR) "does not exist" ; exit 1)
	cd $(AD_DIR) ; make lib FC=$(FFC) OPT=$(OOPT) OMP=$(OOMP) LAPACK=$(LLAPACK) ExtLibDIR=$(ExtLibDIR)
	@echo "  done " $(AD_DIR) " in "$(BaseName)
#
$(QDLIBA):
	@test -d $(ExtLibDIR) || (echo $(ExtLibDIR) "does not exist" ; exit 1)
	@test -d $(QD_DIR) || (cd $(ExtLibDIR) ; ./get_QDUtilLib.sh $(EXTLIB_TYPE))
	@test -d $(QD_DIR) || (echo $(QD_DIR) "does not exist" ; exit 1)
	cd $(QD_DIR) ; make lib FC=$(FFC) OPT=$(OOPT) OMP=$(OOMP) LAPACK=$(LLAPACK) ExtLibDIR=$(ExtLibDIR)
	@echo "  done " $(QDLIBA) " in "$(BaseName)
##
.PHONY: clean_extlib
clean_extlib:
	cd $(ExtLibDIR) ; ./cleanlib
#=======================================================================================
#=======================================================================================
#add dependence for parallelization
$(OBJ): $(QMLLIBA) $(ADLIBA) $(QDLIBA)

$(OBJ_DIR)/sub_module_system.o:       $(OBJ_DIR)/sub_module_MPI.o
$(OBJ_DIR)/sub_module_MPI_aux.o:      $(OBJ_DIR)/sub_module_MPI.o $(OBJ_DIR)/sub_module_system.o

$(OBJ_DIR)/sub_integration.o:         $(OBJ_DIR)/sub_module_system.o
$(OBJ_DIR)/sub_polyortho.o:           $(OBJ_DIR)/sub_module_system.o
$(OBJ_DIR)/sub_function.o:            $(OBJ_DIR)/sub_module_system.o
$(OBJ_DIR)/sub_fft.o:                 $(OBJ_DIR)/sub_module_system.o

$(OBJ_DIR)/sub_module_IntVM.o:        $(OBJ_DIR)/sub_module_system.o
$(OBJ_DIR)/sub_module_dnS.o:          $(OBJ_DIR)/sub_module_system.o
$(OBJ_DIR)/sub_module_VecOFdnS.o:     $(OBJ_DIR)/sub_module_dnS.o $(OBJ_DIR)/sub_module_system.o
$(OBJ_DIR)/sub_module_MatOFdnS.o:     $(OBJ_DIR)/sub_module_dnS.o $(OBJ_DIR)/sub_module_system.o
$(OBJ_DIR)/sub_module_dnV.o:          $(OBJ_DIR)/sub_module_dnS.o $(OBJ_DIR)/sub_module_system.o
$(OBJ_DIR)/sub_module_dnM.o:          $(OBJ_DIR)/sub_module_dnV.o $(OBJ_DIR)/sub_module_dnS.o $(OBJ_DIR)/sub_module_system.o
$(OBJ_DIR)/sub_module_dnSVM.o:        $(OBJ_DIR)/sub_module_dnS.o $(OBJ_DIR)/sub_module_VecOFdnS.o $(OBJ_DIR)/sub_module_MatOFdnS.o \
                                      $(OBJ_DIR)/sub_module_dnV.o $(OBJ_DIR)/sub_module_dnM.o \
                                      $(OBJ_DIR)/sub_module_IntVM.o $(OBJ_DIR)/sub_module_system.o


$(OBJ_DIR)/mod_FiniteDiff.o:          $(OBJ_DIR)/sub_module_dnSVM.o $(OBJ_DIR)/sub_module_system.o
$(OBJ_DIR)/sub_module_cart.o:         $(OBJ_DIR)/sub_module_dnSVM.o $(OBJ_DIR)/sub_module_system.o


$(OBJ_DIR)/sub_module_DInd.o:         $(OBJ_DIR)/sub_module_system.o
$(OBJ_DIR)/sub_module_nDindex.o:      $(OBJ_DIR)/sub_module_DInd.o $(OBJ_DIR)/sub_module_system.o


$(OBJ_DIR)/sub_module_nDfit.o:        $(OBJ_DIR)/sub_module_nDindex.o $(OBJ_DIR)/sub_module_system.o


