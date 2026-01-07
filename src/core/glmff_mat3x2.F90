module glmff_mat3x2
    use, intrinsic :: iso_fortran_env, only: real32, real64
    use glmff_vec2, only: vec2, dvec2

#include "glmff_macros.h"

    implicit none
    private

    ! --------------
    ! --- Data types
    ! --------------

    public :: mat3x2
    type :: mat3x2
        real(kind=real32), dimension(2,3) :: data = reshape( &
            [1.0, 0.0, &
             0.0, 1.0, &
             0.0, 0.0], [2,3])
    end type mat3x2

    public :: dmat3x2
    type :: dmat3x2
        real(kind=real64), dimension(2,3) :: data = reshape( &
            [1.0d0, 0.0d0, &
             0.0d0, 1.0d0, &
             0.0d0, 0.0d0], [2,3])
    end type dmat3x2

    ! ----------------
    ! --- Constructors
    ! ----------------

    interface mat3x2
        module procedure mat3x2_diag
        module procedure mat3x2_scalar
        module procedure mat3x2_col
    end interface mat3x2

    interface dmat3x2
        module procedure dmat3x2_diag
        module procedure dmat3x2_scalar
        module procedure dmat3x2_col
    end interface dmat3x2

    contains

#define MATRIX_TYPE_NCOL 3
#define MATRIX_TYPE_NROW 2
#include "detail/mat_type_impl.inc"

end module glmff_mat3x2

