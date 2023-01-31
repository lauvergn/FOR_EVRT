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
MODULE mod_dnSVM
      USE mod_dnS
      USE mod_VecOFdnS
      USE mod_MatOFdnS
      USE mod_dnV
      USE mod_dnM
      USE mod_IntVM

      IMPLICIT NONE


      INTERFACE Write_dnSVM
        MODULE PROCEDURE Write_dnS,Write_dnVec,Write_dnMat,Write_IntVec,Write_dnCplxMat
      END INTERFACE
      INTERFACE alloc_dnSVM
        MODULE PROCEDURE alloc_dnS,alloc_dnVec,alloc_dnMat,alloc_IntVec,alloc_dnCplxMat
      END INTERFACE
      INTERFACE dealloc_dnSVM
        MODULE PROCEDURE dealloc_dnS,dealloc_dnVec,dealloc_dnMat,       &
                         dealloc_IntVec,dealloc_dnCplxMat
      END INTERFACE
      INTERFACE Set_ZERO_TO_dnSVM
        MODULE PROCEDURE sub_ZERO_TO_dnS,sub_ZERO_TO_dnVec,             &
                         sub_ZERO_TO_dnMat,sub_ZERO_TO_IntVec,sub_ZERO_TO_dnCplxMat
      END INTERFACE

END MODULE mod_dnSVM

