module glmff_mat2x3
    use, intrinsic :: iso_fortran_env, only: real32, real64
    use glmff_vec3, only: vec3, dvec3

#include "glmff_macros.h"

    implicit none
    private

    ! --------------
    ! --- Data types
    ! --------------

    public :: mat2x3
    type :: mat2x3
        real(kind=real32), dimension(3,2) :: data
    end type mat2x3

    public :: dmat2x3
    type :: dmat2x3
        real(kind=real64), dimension(3,2) :: data
    end type dmat2x3

    ! ----------------
    ! --- Constructors
    ! ----------------

    interface mat2x3
        module procedure mat2x3_diag
        module procedure mat2x3_scalar
        module procedure mat2x3_col
    end interface mat2x3

    interface dmat2x3
        module procedure dmat2x3_diag
        module procedure dmat2x3_scalar
        module procedure dmat2x3_col
    end interface dmat2x3

    contains

#define MATRIX_TYPE_NCOL 2
#define MATRIX_TYPE_NROW 3
#include "detail/mat_type_impl.inc"

end module glmff_mat2x3

