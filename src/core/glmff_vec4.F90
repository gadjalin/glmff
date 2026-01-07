module glmff_vec4
    use, intrinsic :: iso_fortran_env, only: int32, real32, real64

#include "glmff_macros.h"

    implicit none
    private

    ! --------------
    ! --- Data types
    ! --------------

    public :: bvec4
    type :: bvec4
        logical, dimension(4) :: data = .false.
    end type bvec4

    public :: ivec4
    type :: ivec4
        integer(kind=int32), dimension(4) :: data = 0
    end type ivec4

    public :: vec4
    type :: vec4
        real(kind=real32), dimension(4) :: data = 0.0
    end type vec4

    public :: dvec4
    type :: dvec4
        real(kind=real64), dimension(4) :: data = 0.0d0
    end type dvec4

    ! ----------------
    ! --- Constructors
    ! ----------------

    interface bvec4
        module procedure bvec4_scalar
        module procedure bvec4_array
    end interface bvec4

    interface ivec4
        module procedure ivec4_scalar
        module procedure ivec4_array
    end interface ivec4

    interface vec4
        module procedure vec4_scalar
        module procedure vec4_array
    end interface vec4

    interface dvec4
        module procedure dvec4_scalar
        module procedure dvec4_array
    end interface dvec4

    contains

#define VECTOR_TYPE_SIZE 4
#include "detail/vec_type_impl.inc"

end module glmff_vec4

