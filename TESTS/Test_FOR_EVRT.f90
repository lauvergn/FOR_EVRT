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
PROGRAM test
  USE mod_system
  USE mod_dnSVM
  IMPLICIT NONE


  integer                          :: io,ioerr
  real(kind=Rkind),    allocatable :: R1Mat(:,:),R1Vec(:),R11Mat(:,:)
  complex(kind=Rkind), allocatable :: C1Mat(:,:),C1Vec(:),C11Mat(:,:)
  real(kind=Rkind),    allocatable :: R2Mat(:,:),R2Vec(:)

  complex(kind=Rkind), allocatable :: C2Mat(:,:),C2Vec(:)
  real(kind=Rkind),    allocatable :: R3Mat(:,:),R3Vec(:)
  complex(kind=Rkind), allocatable :: C3Mat(:,:),C3Vec(:)

  real (kind=Rkind),   parameter   :: ZeroTresh    = ONETENTH**10
  integer               :: transfo_1D
  TYPE(Type_dnS)        :: dnS1,dnS2,dnS3
  real (kind=Rkind)     :: cte(20)


  CALL versionEVRT(write_version=.TRUE.)
    !====================================================================
    ! Tests for the identity matrix
    !
    ! define the matrices
  R1Mat = reshape([ONE,ZERO,ZERO,                              &
                   ZERO,ONE,ZERO,                              &
                   ZERO,ZERO,ONE],shape=[3,3])
  R2Mat = Identity_Mat(3)

  C1Mat = reshape([CONE,CZERO,CZERO,                              &
                   CZERO,CONE,CZERO,                              &
                   CZERO,CZERO,CONE],shape=[3,3])
  C2Mat = Identity_Mat(3)

  write(out_unitp,*) 'identity RMat, test: ',all(abs(R1Mat-R2Mat) < ZeroTresh)
  write(out_unitp,*) 'identity CMat, test: ',all(abs(C1Mat-C2Mat) < ZeroTresh)

  !====================================================================
  ! test for the determinant
  !
  R1Mat = reshape([ONE,HALF,ZERO,                             &
                   HALF,ONE,HALF,                             &
                   ZERO,HALF,ONE],shape=[3,3])

  C1Mat = EYE * R1Mat

  write(out_unitp,*) 'det RMat, test: ',(abs(Det_OF(R1Mat)-HALF) < ZeroTresh)
  write(out_unitp,*) 'det CMat, test: ',(abs(Det_OF(C1Mat)-(-EYE*HALF)) < ZeroTresh)
  !====================================================================

  !====================================================================
  ! test for the inversion
  !
  R1Mat = reshape([ONE,HALF,ZERO,                             &
                   HALF,ONE,HALF,                             &
                   ZERO,HALF,ONE],shape=[3,3])
  R2Mat = reshape([THREE,-TWO,ONE,                            &
                   -TWO,FOUR,-TWO,                            &
                   ONE,-TWO,THREE],shape=[3,3]) * HALF

  C1Mat =  EYE * R1Mat
  C2Mat = -EYE * R2Mat

  write(out_unitp,*) 'inversion RMat, test: ',all(abs(inv_OF_Mat_TO(R1Mat)-R2Mat) < ZeroTresh)
  !CALL Write_Mat(inv_OF_Mat_TO(R1Mat),out_unitp,5,info='R1Mat^-1')
  !CALL Write_Mat(R2Mat,out_unitp,5,info='R2Mat')

  CALL inv_OF_Mat_TO_Mat_inv(R1Mat,R11Mat,0,ZERO)
  write(out_unitp,*) 'inversion RMat (sub), test: ',all(abs(R11Mat-R2Mat) < ZeroTresh)
  !CALL Write_Mat(R11Mat,out_unitp,5,info='R11Mat')

  !====================================================================
  ! test dnS 1D-transformation
  dnS1%nb_var_deriv = 1
  dnS1%nderiv       = 3
  CALL alloc_dnS(dnS1)
  CALL Set_ZERO_TO_dnSVM(dnS1)
  CALL sub_dnS1_TO_dnS2(dnS1,dnS2)

  dnS1%d0 = HALF
  dnS1%d1 = ONE
  dnS1%d2 = TWO
  dnS1%d3 = ONE
  !CALL Write_dnS(dnS1)

  cte(:) = ZERO
  cte(1:2) = [THREE,FIVE]
  transfo_1D = 100
  CALL sub_dnS1_TO_dntR2(dnS1,dnS2,transfo_1D= transfo_1D,nderiv=3,cte=cte)
  CALL sub_dnS1_TO_dntR2(dnS2,dnS3,transfo_1D=-transfo_1D,nderiv=3,cte=cte)
  !CALL Write_dnS(dnS3)

  CALL sub_dnS1_MINUS_dnS2_TO_dnS3(dnS1,dnS3,dnS2)
  write(out_unitp,*) 'transfo ',transfo_1D,', ok: ',check_dnS_IsZERO(dnS2)

  transfo_1D = 171
  CALL sub_dnS1_TO_dntR2(dnS1,dnS2,transfo_1D= transfo_1D,nderiv=3,cte=cte)
  CALL sub_dnS1_TO_dntR2(dnS2,dnS3,transfo_1D=-transfo_1D,nderiv=3,cte=cte)
  !CALL Write_dnS(dnS3)

  CALL sub_dnS1_MINUS_dnS2_TO_dnS3(dnS1,dnS3,dnS2)
  write(out_unitp,*) 'transfo ',transfo_1D,', ok: ',check_dnS_IsZERO(dnS2)

  transfo_1D = 1171
  CALL sub_dnS1_TO_dntR2(dnS1,dnS2,transfo_1D= transfo_1D,nderiv=3,cte=cte)
  CALL sub_dnS1_TO_dntR2(dnS2,dnS3,transfo_1D=-transfo_1D,nderiv=3,cte=cte)
  !CALL Write_dnS(dnS3)

  CALL sub_dnS1_MINUS_dnS2_TO_dnS3(dnS1,dnS3,dnS2)
  write(out_unitp,*) 'transfo ',transfo_1D,', ok: ',check_dnS_IsZERO(dnS2)

END PROGRAM test