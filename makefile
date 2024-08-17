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
MAIN_path:= $(shell pwd)

# Extension for the object directory and the library
ifeq ($(FFC),mpifort)
  extlibwi_obj:=_$(FFC)_$(MPICORE)_opt$(OOPT)_omp$(OOMP)_lapack$(LLAPACK)_int$(INT)
else
  extlibwi_obj:=_$(FFC)_opt$(OOPT)_omp$(OOMP)_lapack$(LLAPACK)_int$(INT)
endif
extlib_obj:=_$(FFC)_opt$(OOPT)_omp$(OOMP)_lapack$(LLAPACK)_int$(INT)



OBJ_DIR = obj/obj$(extlibwi_obj)
$(info ***********OBJ_DIR:            $(OBJ_DIR))
$(shell [ -d $(OBJ_DIR) ] || mkdir -p $(OBJ_DIR))
MOD_DIR=$(OBJ_DIR)
#
# library name
LIBA=libFOR_EVRT$(extlibwi_obj).a
#
#===============================================================================
#
#===============================================================================
# external lib (QDUtil, AD_dnSVM ...)
ifeq ($(ExtLibDIR),)
  ExtLibDIR := $(MAIN_path)/Ext_Lib
endif

nDindex_DIR    = $(ExtLibDIR)/nDindex
nDindexMOD_DIR = $(nDindex_DIR)/obj/obj$(extlib_obj)
nDindexLIBA    = $(nDindex_DIR)/libnDindex$(extlib_obj).a

EVRTdnSVM_DIR    = $(ExtLibDIR)/EVRT_dnSVM
EVRTdnSVMMOD_DIR = $(EVRTdnSVM_DIR)/obj/obj$(extlib_obj)
EVRTdnSVMLIBA    = $(EVRTdnSVM_DIR)/libEVRT_dnSVM$(extlib_obj).a

AD_DIR    = $(ExtLibDIR)/AD_dnSVM
ADMOD_DIR = $(AD_DIR)/OBJ/obj$(extlib_obj)
ADLIBA    = $(AD_DIR)/libAD_dnSVM$(extlib_obj).a

QD_DIR    = $(ExtLibDIR)/QDUtilLib
QDMOD_DIR = $(QD_DIR)/OBJ/obj$(extlib_obj)
QDLIBA    = $(QD_DIR)/libQD$(extlib_obj).a

EXTLib     = $(nDindexLIBA) $(EVRTdnSVMLIBA) $(ADLIBA)  $(QDLIBA)
EXTMod     = -I$(nDindexMOD_DIR) -I$(EVRTdnSVMMOD_DIR) -I$(ADMOD_DIR) -I$(QDMOD_DIR)
#===============================================================================
#
#=================================================================================
# To deal with external compilers.mk file
CompilersDIR = $(MAIN_path)
ifeq ($(CompilersDIR),)
  include compilers.mk
else
  include $(CompilersDIR)/compilers.mk
endif
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
$(info ************************************************************************)
$(info ************************************************************************)
#==========================================
VPATH = SRC/sub_system SRC/sub_module SRC/sub_communf90/sub_math TESTS

Primlib_SRCFILES  = sub_module_MPI.f90  FOR_EVRT_system_m.f90 sub_module_MPI_aux.f90 sub_module_cart.f90

math_SRCFILES = sub_integration.f90 sub_polyortho.f90 sub_function.f90 sub_fft.f90

nDfit_SRCFILES    = sub_module_nDfit.f90
#============================================================================

SRCFILES= $(Primlib_SRCFILES) $(math_SRCFILES) $(nDfit_SRCFILES)

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
	$(FFC) $(FFLAGS) -o Test_FOR_EVRT.exe $(OBJ_DIR)/Test_FOR_EVRT.o $(LIBA) $(EXTLib) $(FLIB)
	@echo "  done Library: Test_FOR_EVRT.exe"
#
$(OBJ_DIR)/Test_FOR_EVRT.o: $(LIBA) $(EXTLib)
#===============================================
#============= Library: FOR_EVRT....a  =========
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
	$(ExtLibSAVEDIR)/makezip.sh $(BaseName)
	cd $(ExtLibSAVEDIR) ; ./cp_FOR_EVRT.sh
	@echo "  done zip"
#===============================================
#=== external libraries ========================
# nDindex EVRT_dnSVM AD_dnSVM + QDUtil
#===============================================
#
$(nDindexLIBA):
	@test -d $(ExtLibDIR)   || (echo $(ExtLibDIR) "does not exist" ; exit 1)
	@test -d $(nDindex_DIR) || (cd $(ExtLibDIR) ; ./get_nDindex.sh  $(EXTLIB_TYPE))
	@test -d $(nDindex_DIR) || (echo $(nDindex_DIR) "does not exist" ; exit 1)
	cd $(nDindex_DIR) ; make lib FC=$(FFC) OPT=$(OOPT) OMP=$(OOMP) LAPACK=$(LLAPACK) INT=$(INT) ExtLibDIR=$(ExtLibDIR) CompilersDIR=$(CompilersDIR)
	@echo "  done " $(nDindex_DIR) " in "$(BaseName)
#
$(EVRTdnSVMLIBA):
	@test -d $(ExtLibDIR)     || (echo $(ExtLibDIR) "does not exist" ; exit 1)
	@test -d $(EVRTdnSVM_DIR) || (cd $(ExtLibDIR) ; ./get_EVRT_dnSVM.sh  $(EXTLIB_TYPE))
	@test -d $(EVRTdnSVM_DIR) || (echo $(EVRTdnSVM_DIR) "does not exist" ; exit 1)
	cd $(EVRTdnSVM_DIR) ; make lib FC=$(FFC) OPT=$(OOPT) OMP=$(OOMP) LAPACK=$(LLAPACK) INT=$(INT) ExtLibDIR=$(ExtLibDIR) CompilersDIR=$(CompilersDIR)
	@echo "  done " $(EVRTdnSVM_DIR) " in "$(BaseName)
#
$(ADLIBA):
	@test -d $(ExtLibDIR) || (echo $(ExtLibDIR) "does not exist" ; exit 1)
	@test -d $(AD_DIR) || (cd $(ExtLibDIR) ; ./get_AD_dnSVM.sh  $(EXTLIB_TYPE))
	@test -d $(AD_DIR) || (echo $(AD_DIR) "does not exist" ; exit 1)
	cd $(AD_DIR) ; make lib FC=$(FFC) OPT=$(OOPT) OMP=$(OOMP) LAPACK=$(LLAPACK) INT=$(INT) ExtLibDIR=$(ExtLibDIR) CompilersDIR=$(CompilersDIR)
	@echo "  done " $(AD_DIR) " in "$(BaseName)
#
$(QDLIBA):
	@test -d $(ExtLibDIR) || (echo $(ExtLibDIR) "does not exist" ; exit 1)
	@test -d $(QD_DIR) || (cd $(ExtLibDIR) ; ./get_QDUtilLib.sh $(EXTLIB_TYPE))
	@test -d $(QD_DIR) || (echo $(QD_DIR) "does not exist" ; exit 1)
	cd $(QD_DIR) ; make lib FC=$(FFC) OPT=$(OOPT) OMP=$(OOMP) LAPACK=$(LLAPACK) INT=$(INT) ExtLibDIR=$(ExtLibDIR) CompilersDIR=$(CompilersDIR)
	@echo "  done " $(QDLIBA) " in "$(BaseName)
##
.PHONY: clean_extlib
clean_extlib:
	echo cleanlib, DIR=$(ExtLibDIR)
	cd $(ExtLibDIR) ; ./cleanlib
#=======================================================================================
#=======================================================================================
#add dependence for parallelization
$(OBJ): $(QDLIBA) $(ADLIBA)  $(EVRTdnSVMLIBA) $(nDindexLIBA)

$(OBJ_DIR)/FOR_EVRT_system_m.o:       $(OBJ_DIR)/sub_module_MPI.o
$(OBJ_DIR)/sub_module_MPI_aux.o:      $(OBJ_DIR)/sub_module_MPI.o $(OBJ_DIR)/FOR_EVRT_system_m.o

$(OBJ_DIR)/sub_integration.o:         $(OBJ_DIR)/FOR_EVRT_system_m.o
$(OBJ_DIR)/sub_polyortho.o:           $(OBJ_DIR)/FOR_EVRT_system_m.o
$(OBJ_DIR)/sub_function.o:            $(OBJ_DIR)/FOR_EVRT_system_m.o
$(OBJ_DIR)/sub_fft.o:                 $(OBJ_DIR)/FOR_EVRT_system_m.o

$(OBJ_DIR)/sub_module_cart.o:         $(OBJ_DIR)/FOR_EVRT_system_m.o

$(OBJ_DIR)/sub_module_nDfit.o:        $(OBJ_DIR)/FOR_EVRT_system_m.o