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
MODULE FOR_EVRT_system_m
  USE QDUtil_m
  USE mod_MPI
  IMPLICIT NONE

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

  character (len=:), allocatable :: Current_Path


  character (len=Name_longlen) :: EneIO_format = "f20.5"

  TYPE param_FOR_optimization
    integer                        :: nb_OptParam    = 0
    integer                        :: i_OptParam     = 0
    real (kind=Rkind), allocatable :: Val_RVec(:)
    integer, allocatable           :: opt_RVec(:)
    character (len=Name_len) :: Optimization_param  = 'geometry'
  END TYPE param_FOR_optimization

END MODULE FOR_EVRT_system_m
