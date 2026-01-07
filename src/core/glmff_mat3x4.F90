module glmff_mat3x4
    use, intrinsic :: iso_fortran_env, only: real32, real64
    use glmff_vec4, only: vec4, dvec4

#include "glmff_macros.h"

    implicit none
    private

    ! --------------
    ! --- Data types
    ! --------------

    public :: mat3x4
    type :: mat3x4
        real(kind=real32), dimension(4,3) :: data = reshape( &
            [1.0, 0.0, 0.0, 0.0, &
             0.0, 1.0, 0.0, 0.0, &
             0.0, 0.0, 1.0, 0.0], [4,3])
    end type mat3x4

    public :: dmat3x4
    type :: dmat3x4
        real(kind=real64), dimension(4,3) :: data = reshape( &
            [1.0d0, 0.0d0, 0.0d0, 0.0d0, &
             0.0d0, 1.0d0, 0.0d0, 0.0d0, &
             0.0d0, 0.0d0, 1.0d0, 0.0d0], [4,3])
    end type dmat3x4

    ! ----------------
    ! --- Constructors
    ! ----------------

    interface mat3x4
        module procedure mat3x4_diag
        module procedure mat3x4_scalar
        module procedure mat3x4_col
    end interface mat3x4

    interface dmat3x4
        module procedure dmat3x4_diag
        module procedure dmat3x4_scalar
        module procedure dmat3x4_col
    end interface dmat3x4

    contains

#define MATRIX_TYPE_NCOL 3
#define MATRIX_TYPE_NROW 4
#include "detail/mat_type_impl.inc"

end module glmff_mat3x4

