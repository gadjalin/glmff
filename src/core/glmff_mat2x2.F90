module glmff_mat2x2
    use, intrinsic :: iso_fortran_env, only: real32, real64
    use glmff_vec2, only: vec2, dvec2

#include "glmff_macros.h"

    implicit none
    private

    ! --------------
    ! --- Data types
    ! --------------

    public :: mat2x2
    type :: mat2x2
        real(kind=real32), dimension(2,2) :: data = reshape( &
            [1.0, 0.0, &
             0.0, 1.0], [2,2])
    end type mat2x2

    public :: dmat2x2
    type :: dmat2x2
        real(kind=real64), dimension(2,2) :: data = reshape( &
            [1.0d0, 0.0d0, &
             0.0d0, 1.0d0], [2,2])
    end type dmat2x2

    ! ----------------
    ! --- Constructors
    ! ----------------

    interface mat2x2
        module procedure mat2x2_diag
        module procedure mat2x2_scalar
        module procedure mat2x2_col
    end interface mat2x2

    interface dmat2x2
        module procedure dmat2x2_diag
        module procedure dmat2x2_scalar
        module procedure dmat2x2_col
    end interface dmat2x2

    contains

#define MATRIX_TYPE_NCOL 2
#define MATRIX_TYPE_NROW 2
#include "detail/mat_type_impl.inc"

end module glmff_mat2x2

