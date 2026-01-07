module glmff_mat4x4
    use, intrinsic :: iso_fortran_env, only: real32, real64
    use glmff_vec4, only: vec4, dvec4

#include "glmff_macros.h"

    implicit none
    private

    ! --------------
    ! --- Data types
    ! --------------

    public :: mat4x4
    type :: mat4x4
        real(kind=real32), dimension(4,4) :: data = reshape( &
            [1.0, 0.0, 0.0, 0.0, &
             0.0, 1.0, 0.0, 0.0, &
             0.0, 0.0, 1.0, 0.0, &
             0.0, 0.0, 0.0, 1.0], [4,4])
    end type mat4x4

    public :: dmat4x4
    type :: dmat4x4
        real(kind=real64), dimension(4,4) :: data = reshape( &
            [1.0d0, 0.0d0, 0.0d0, 0.0d0, &
             0.0d0, 1.0d0, 0.0d0, 0.0d0, &
             0.0d0, 0.0d0, 1.0d0, 0.0d0, &
             0.0d0, 0.0d0, 0.0d0, 1.0d0], [4,4])
    end type dmat4x4

    ! ----------------
    ! --- Constructors
    ! ----------------

    interface mat4x4
        module procedure mat4x4_diag
        module procedure mat4x4_scalar
        module procedure mat4x4_col
    end interface mat4x4

    interface dmat4x4
        module procedure dmat4x4_diag
        module procedure dmat4x4_scalar
        module procedure dmat4x4_col
    end interface dmat4x4

    contains

#define MATRIX_TYPE_NCOL 4
#define MATRIX_TYPE_NROW 4
#include "detail/mat_type_impl.inc"

end module glmff_mat4x4

