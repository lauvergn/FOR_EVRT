!===============================================================================
!===============================================================================
!This file is part of FOR_EVRT library.
!
!===============================================================================
! MIT License
!
! Copyright (c) 2022 David Lauvergnat
!
! Permission is hereby granted, free of charge, to any person obtaining a copy
! of this software and associated documentation files (the "Software"), to deal
! in the Software without restriction, including without limitation the rights
! to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
! copies of the Software, and to permit persons to whom the Software is
! furnished to do so, subject to the following conditions:
!
! The above copyright notice and this permission notice shall be included in all
! copies or substantial portions of the Software.
!
! THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
! IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
! FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
! AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
! LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
! OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
! SOFTWARE.
!===============================================================================
!===============================================================================
MODULE mod_system
  USE QDUtil_m, out_unitp => out_unit, in_unitp => in_unit
  USE mod_MPI
  IMPLICIT NONE

#if defined(__TNUM_VER)
  character (len=Name_len) :: Tnum_version = __TNUM_VER
#else
  character (len=Name_len) :: Tnum_version = "unknown: -D__TNUM_VER=?"
#endif

#if defined(__TANA_VER)
  character (len=Name_len) :: Tana_version = __TANA_VER
#else
  character (len=Name_len) :: Tana_version = "unknown: -D__TANA_VER=?"
#endif

#if defined(__EVR_VER)
  character (len=Name_len) :: EVR_version = __EVR_VER
#else
  character (len=Name_len) :: EVR_version = "unknown: -D__EVR_VER=?"
#endif

#if defined(__EVRTPATH)
  character (len=Line_len) :: EVRT_path   =                         &
       __EVRTPATH
#else
  character (len=Line_len) :: EVRT_path   = '~/ElVibRot'
#endif

#if defined(__COMPILE_DATE)
  character (len=Line_len) :: compile_date = __COMPILE_DATE
#else
  character (len=Line_len) :: compile_date = "unknown: -D__COMPILE_DATE=?"
#endif

#if defined(__COMPILE_HOST)
  character (len=Line_len) :: compile_host = __COMPILE_HOST
#else
  character (len=Line_len) :: compile_host = "unknown: -D__COMPILE_HOST=?"
#endif
#if defined(__COMPILER)
  character (len=Line_len) :: compiler = __COMPILER
#else
  character (len=Line_len) :: compiler = "unknown: -D__COMPILER=?"
#endif
#if defined(__COMPILER_VER)
  character (len=Line_len) :: compiler_ver = __COMPILER_VER
#else
  character (len=Line_len) :: compiler_ver = "unknown: -D__COMPILER_VER=?"
#endif
#if defined(__COMPILER_OPT)
  character (len=Line_len) :: compiler_opt = &
      __COMPILER_OPT
#else
  character (len=Line_len) :: compiler_opt = "unknown: -D__COMPILER_OPT=?"
#endif
#if defined(__COMPILER_LIBS)
character (len=Line_len) :: compiler_libs = &
       __COMPILER_LIBS
#else
  character (len=Line_len) :: compiler_libs = "unknown: -D__COMPILER_LIBS=?"
#endif
#if defined(__GIT)
  character (len=Line_len) :: git_branch = __GIT
#else
  character (len=Line_len) :: git_branch = "unknown: -D__GIT=?"
#endif


  logical :: openmp = .FALSE.
  logical :: openmpi= .FALSE.
 
  integer :: MatOp_omp,MatOp_maxth,MatOp_maxth_init
  integer :: OpPsi_omp,OpPsi_maxth,OpPsi_maxth_init
  integer :: BasisTOGrid_omp,BasisTOGrid_maxth,BasisTOGrid_maxth_init
  integer :: Grid_omp,Grid_maxth,Grid_maxth_init
  integer :: SG4_omp,SG4_maxth,SG4_maxth_init
  integer :: CRP_omp,CRP_maxth,CRP_maxth_init

  logical :: Tune_SG4_omp  = .FALSE.
  logical :: Tune_Grid_omp = .FALSE.

  integer (kind=ILkind) :: nb_mult_BTOG  = 0
  integer (kind=ILkind) :: nb_mult_GTOB  = 0
  integer (kind=ILkind) :: nb_mult_OpPsi = 0

  integer, parameter :: max_HADA = 5000
  integer, parameter :: max_nb_G_FOR_print = 2000
  !integer, parameter :: max_nb_G_FOR_print = 20000

  integer :: SGtype               = -1
  integer :: FilePsiVersion       = 0
  logical :: NewBasisEl           = .FALSE.
  logical :: print_CoordType_done = .FALSE.! if T, the CoordType has been already print

  character (len=:), allocatable :: Current_Path


  character (len=Name_longlen) :: EneIO_format = "f20.5"

  TYPE param_FOR_optimization
    integer                        :: nb_OptParam    = 0
    integer                        :: i_OptParam     = 0
    real (kind=Rkind), allocatable :: Val_RVec(:)
    integer, allocatable           :: opt_RVec(:)
    character (len=Name_len) :: Optimization_param  = 'geometry'
  END TYPE param_FOR_optimization

  TYPE param_EVRT_calc
    integer :: optimization     = 0
    logical :: EVR              = .TRUE.   ! ElVibRot (default)
    logical :: analysis_only    = .FALSE.
    logical :: intensity_only   = .FALSE.
    logical :: Grid_only        = .FALSE.
    logical :: cart             = .FALSE.
    logical :: GridTOBasis_test = .FALSE.
    logical :: OpPsi_test       = .FALSE.
    logical :: nDfit            = .FALSE.
    logical :: nDGrid           = .FALSE.
    logical :: main_test        = .FALSE.
    logical :: Opt_CAP_Basis    = .FALSE.
    END TYPE param_EVRT_calc

  TYPE (param_FOR_optimization), save :: para_FOR_optimization
  TYPE (param_EVRT_calc),        save :: para_EVRT_calc

CONTAINS

!=======================================================================================
!> @brief subroutine for recording time
!> @param time_sum should be initialized before calling this function
!=======================================================================================
      SUBROUTINE time_record(time_sum,time1,time2,point)
        USE mod_MPI
        IMPLICIT NONE

        Integer,                        intent(inout) :: time_sum
        Integer,                        intent(inout) :: time1
        Integer,                        intent(inout) :: time2
        Integer,                           intent(in) :: point

        IF(point==1) THEN
          CALL system_clock(time1,time_rate,time_max)
        ELSEIF(point==2) THEN
          CALL system_clock(time2,time_rate,time_max)
          time_sum=time_sum+merge(time2-time1,time2-time1+time_max,time2>=time1)
        ELSE
          STOP 'error when calling time_record'
        ENDIF
  END SUBROUTINE

  FUNCTION nom_i(nom1,i1)
    IMPLICIT NONE
  
    character (len=14) :: nom_i
    character (len=10) :: nom1
    character (len=14) :: nom2
    integer            :: j,i1
  
    write(out_unitp,*) nom1,i1
    IF (i1 .GT. 100 ) STOP ' in nom_i: i1 too big'
  
    write(nom2,'(a10,i2)') nom1,i1
    DO j=1,12  ! it has to be 12 and not 14
      IF (nom2(j:j) .EQ. ' ') nom2(j:j)='_'
    END DO
    nom_i=nom2
  
  END FUNCTION nom_i
  
  FUNCTION nom_ii(nom1,i1,i2)
    IMPLICIT NONE
  
    character (len=14) :: nom_ii
  
    character (len=10) :: nom1
    character (len=14) :: nom2
    integer            :: j,i1,i2
  
    !write(out_unitp,*) nom1,i1,i2
    IF (i1 .GT. 100 .OR. i2 .GT. 100) STOP ' in nom_ii: i1 or i2 too big'
  
    write(nom2,'(a10,2i2)') nom1,i1,i2
    DO j=1,14
      IF (nom2(j:j) .EQ. ' ') nom2(j:j)='_'
    END DO
    nom_ii=nom2
  
  END FUNCTION nom_ii

  FUNCTION make_EVRTInternalFileName(FileName,FPath) RESULT(make_FileName)
    USE QDUtil_m, ONLY : err_FileName
    IMPLICIT NONE

    character (len=:), allocatable          :: make_FileName

    character(len=*), intent(in)            :: FileName
    character(len=*), intent(in), optional  :: FPath

    character (len=:), allocatable          :: FPath_loc


    integer :: ilast_char,err

    IF (present(FPath)) THEN
      FPath_loc = FPath
    ELSE
      FPath_loc = EVRT_path
    END IF

    err = err_FileName(FileName,name_sub='make_EVRTInternalFileName')
    IF (err /= 0) STOP 'ERROR in make_EVRTInternalFileName: problem with the FileName'

    ilast_char = len_trim(FPath_loc)

    IF (FileName(1:1) == "/" .OR. FileName(1:1) == "~" .OR. ilast_char == 0) THEN
      make_FileName = trim(adjustl(FileName))
    ELSE
      IF (FPath_loc(ilast_char:ilast_char) == "/") THEN
        make_FileName = trim(adjustl(FPath_loc)) // trim(adjustl(FileName))
      ELSE
        make_FileName = trim(adjustl(FPath_loc)) // '/' // trim(adjustl(FileName))
      END IF
    END IF

    IF (allocated(FPath_loc)) deallocate(FPath_loc)

  END FUNCTION make_EVRTInternalFileName
  FUNCTION make_EVRTFileName(FileName,FPath) RESULT(make_FileName)
    USE QDUtil_m, ONLY : err_FileName
    IMPLICIT NONE

    character (len=:), allocatable          :: make_FileName

    character(len=*), intent(in)            :: FileName
    character(len=*), intent(in), optional  :: FPath

    character (len=:), allocatable          :: FPath_loc


    integer :: ilast_char,err

    IF (present(FPath)) THEN
      FPath_loc = FPath
    ELSE IF (allocated(Current_Path)) THEN
      FPath_loc = Current_Path
    ELSE
      FPath_loc = ''
    END IF

    err = err_FileName(FileName,name_sub='make_EVRTFileName')
    IF (err /= 0) STOP 'ERROR in make_EVRTFileName: problem with the FileName'

    ilast_char = len_trim(FPath_loc)

    IF (FileName(1:1) == "/" .OR. FileName(1:1) == "~" .OR. ilast_char == 0) THEN
      make_FileName = trim(adjustl(FileName))
    ELSE
      IF (FPath_loc(ilast_char:ilast_char) == "/") THEN
        make_FileName = trim(adjustl(FPath_loc)) // trim(adjustl(FileName))
      ELSE
        make_FileName = trim(adjustl(FPath_loc)) // '/' // trim(adjustl(FileName))
      END IF
    END IF

    IF (allocated(FPath_loc)) deallocate(FPath_loc)

  END FUNCTION make_EVRTFileName
  SUBROUTINE versionEVRT(write_version)
    IMPLICIT NONE
  
        logical :: write_version
  
        character (len=*), parameter :: EVR_name='ElVibRot'
        character (len=*), parameter :: Tnum_name='Tnum'
        character (len=*), parameter :: Tana_name='Tana'
  
  
  
        IF (write_version .AND. MPI_id==0) THEN
          write(out_unitp,*) '==============================================='
          write(out_unitp,*) '==============================================='
          write(out_unitp,*) 'Working with ',                             &
                     EVR_name,trim(adjustl(EVR_version)),'-',             &
                     Tnum_name,trim(adjustl(Tnum_version)),'-',           &
                     Tana_name,trim(adjustl(Tana_version))
  
          write(out_unitp,*) 'Compiled on "',trim(compile_host), '" the ',trim(compile_date)
          write(out_unitp,*) 'Compiler version: ',trim(compiler_ver)
          write(out_unitp,*) 'Compiler options: ',trim(compiler_opt)
          write(out_unitp,*) 'Compiler libs: ',trim(compiler_libs)
  
          write(out_unitp,*) 'EVRT_path: ',trim(EVRT_path)
          write(out_unitp,*) 'git ',trim(git_branch)
  
          write(out_unitp,*) '-----------------------------------------------'
  
          write(out_unitp,*) EVR_name,' is written by David Lauvergnat [1] '
          write(out_unitp,*) '  with contributions of'
          write(out_unitp,*) '     Josep Maria Luis (optimization) [2]'
          write(out_unitp,*) '     Ahai Chen (MPI) [1,4]'
          write(out_unitp,*) '     Lucien Dupuy (CRP) [5]'
  
          write(out_unitp,*) EVR_name,' is under GNU LGPL3 license.'
          write(out_unitp,*)
  
          write(out_unitp,*) Tnum_name,' is written David Lauvergnat [1]'
          write(out_unitp,*) Tana_name,' is written by Mamadou Ndong [1] and David Lauvergnat [1]'
          write(out_unitp,*) '  with contributions'
          write(out_unitp,*) '      Emil Lund klinting (coupling with MidasCpp) [3]'
  
          write(out_unitp,*) Tnum_name,' and ',Tana_name,' are under GNU LGPL3 license.'
          write(out_unitp,*)
          write(out_unitp,*) '[1]: Institut de Chimie Physique, UMR 8000, CNRS-Université Paris-Saclay, France'
          write(out_unitp,*) '[2]: Institut de Química Computacional and Departament de Química',&
                                     ' Universitat de Girona, Catalonia, Spain'
          write(out_unitp,*) '[3]: Department of Chemistry, Aarhus University, DK-8000 Aarhus C, Denmark'
          write(out_unitp,*) '[4]: Maison de la Simulation USR 3441, CEA Saclay, France'
          write(out_unitp,*) '[5]: Laboratoire Univers et Particule de Montpellier, UMR 5299,', &
                                     ' Université de Montpellier, France'
          write(out_unitp,*) '==============================================='
          write(out_unitp,*) '==============================================='
        END IF
  END SUBROUTINE versionEVRT

END MODULE mod_system
