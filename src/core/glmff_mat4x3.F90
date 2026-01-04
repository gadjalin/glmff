module glmff_mat4x3
    use, intrinsic :: iso_fortran_env, only: real32, real64
    use glmff_vec3, only: vec3, dvec3

#include "glmff_macros.h"

    implicit none
    private

    ! --------------
    ! --- Data types
    ! --------------

    public :: mat4x3
    type :: mat4x3
        real(kind=real32), dimension(3,4) :: data
    end type mat4x3

    public :: dmat4x3
    type :: dmat4x3
        real(kind=real64), dimension(3,4) :: data
    end type dmat4x3

    ! ----------------
    ! --- Constructors
    ! ----------------

    interface mat4x3
        module procedure mat4x3_diag
        module procedure mat4x3_scalar
        module procedure mat4x3_col
    end interface mat4x3

    interface dmat4x3
        module procedure dmat4x3_diag
        module procedure dmat4x3_scalar
        module procedure dmat4x3_col
    end interface dmat4x3

    contains

#define MATRIX_TYPE_NCOL 4
#define MATRIX_TYPE_NROW 3
#include "detail/mat_type_impl.inc"

end module glmff_mat4x3

