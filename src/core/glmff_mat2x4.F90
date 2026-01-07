module glmff_mat2x4
    use, intrinsic :: iso_fortran_env, only: real32, real64
    use glmff_vec4, only: vec4, dvec4

#include "glmff_macros.h"

    implicit none
    private

    ! --------------
    ! --- Data types
    ! --------------

    public :: mat2x4
    type :: mat2x4
        real(kind=real32), dimension(4,2) :: data = reshape( &
            [1.0, 0.0, 0.0, 0.0, &
             0.0, 1.0, 0.0, 0.0], [4,2])
    end type mat2x4

    public :: dmat2x4
    type :: dmat2x4
        real(kind=real64), dimension(4,2) :: data = reshape( &
            [1.0d0, 0.0d0, 0.0d0, 0.0d0, &
             0.0d0, 1.0d0, 0.0d0, 0.0d0], [4,2])
    end type dmat2x4

    ! ----------------
    ! --- Constructors
    ! ----------------

    interface mat2x4
        module procedure mat2x4_diag
        module procedure mat2x4_scalar
        module procedure mat2x4_col
    end interface mat2x4

    interface dmat2x4
        module procedure dmat2x4_diag
        module procedure dmat2x4_scalar
        module procedure dmat2x4_col
    end interface dmat2x4

    contains

#define MATRIX_TYPE_NCOL 2
#define MATRIX_TYPE_NROW 4
#include "detail/mat_type_impl.inc"

end module glmff_mat2x4

