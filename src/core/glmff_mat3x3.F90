module glmff_mat3x3
    use, intrinsic :: iso_fortran_env, only: real32, real64
    use glmff_vec3, only: vec3, dvec3

#include "glmff_macros.h"

    implicit none
    private

    ! --------------
    ! --- Data types
    ! --------------

    public :: mat3x3
    type :: mat3x3
        real(kind=real32), dimension(3,3) :: data = reshape( &
            [1.0, 0.0, 0.0, &
             0.0, 1.0, 0.0, &
             0.0, 0.0, 1.0], [3,3])
    end type mat3x3

    public :: dmat3x3
    type :: dmat3x3
        real(kind=real64), dimension(3,3) :: data = reshape( &
            [1.0d0, 0.0d0, 0.0d0, &
             0.0d0, 1.0d0, 0.0d0, &
             0.0d0, 0.0d0, 1.0d0], [3,3])
    end type dmat3x3

    ! ----------------
    ! --- Constructors
    ! ----------------

    interface mat3x3
        module procedure mat3x3_diag
        module procedure mat3x3_scalar
        module procedure mat3x3_col
    end interface mat3x3

    interface dmat3x3
        module procedure dmat3x3_diag
        module procedure dmat3x3_scalar
        module procedure dmat3x3_col
    end interface dmat3x3

    contains

#define MATRIX_TYPE_NCOL 3
#define MATRIX_TYPE_NROW 3
#include "detail/mat_type_impl.inc"

end module glmff_mat3x3

