module glmff_mat4x2
    use, intrinsic :: iso_fortran_env, only: real32, real64
    use glmff_vec2, only: vec2, dvec2

#include "glmff_macros.h"

    implicit none
    private

    ! --------------
    ! --- Data types
    ! --------------

    public :: mat4x2
    type :: mat4x2
        real(kind=real32), dimension(2,4) :: data
    end type mat4x2

    public :: dmat4x2
    type :: dmat4x2
        real(kind=real64), dimension(2,4) :: data
    end type dmat4x2

    ! ----------------
    ! --- Constructors
    ! ----------------

    interface mat4x2
        module procedure mat4x2_diag
        module procedure mat4x2_scalar
        module procedure mat4x2_col
    end interface mat4x2

    interface dmat4x2
        module procedure dmat4x2_diag
        module procedure dmat4x2_scalar
        module procedure dmat4x2_col
    end interface dmat4x2

    contains

#define MATRIX_TYPE_NCOL 4
#define MATRIX_TYPE_NROW 2
#include "detail/mat_type_impl.inc"

end module glmff_mat4x2

