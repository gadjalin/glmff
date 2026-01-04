module glmff_vec2
    use, intrinsic :: iso_fortran_env, only: int32, real32, real64

#include "glmff_macros.h"

    implicit none
    private

    ! --------------
    ! --- Data types
    ! --------------

    public :: bvec2
    type :: bvec2
        logical, dimension(2) :: data
    end type bvec2

    public :: ivec2
    type :: ivec2
        integer(kind=int32), dimension(2) :: data
    end type ivec2

    public :: vec2
    type :: vec2
        real(kind=real32), dimension(2) :: data
    end type vec2

    public :: dvec2
    type :: dvec2
        real(kind=real64), dimension(2) :: data
    end type dvec2

    ! ----------------
    ! --- Constructors
    ! ----------------

    interface bvec2
        module procedure bvec2_scalar
        module procedure bvec2_array
    end interface bvec2

    interface ivec2
        module procedure ivec2_scalar
        module procedure ivec2_array
    end interface ivec2

    interface vec2
        module procedure vec2_scalar
        module procedure vec2_array
    end interface vec2

    interface dvec2
        module procedure dvec2_scalar
        module procedure dvec2_array
    end interface dvec2

    contains

#define VECTOR_TYPE_SIZE 2
#include "detail/vec_type_impl.inc"

end module glmff_vec2

