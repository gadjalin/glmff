module glmff_vec3
    use, intrinsic :: iso_fortran_env, only: int32, real32, real64

#include "glmff_macros.h"

    implicit none
    private

    ! --------------
    ! --- Data types
    ! --------------

    public :: bvec3
    type :: bvec3
        logical, dimension(3) :: data
    end type bvec3

    public :: ivec3
    type :: ivec3
        integer(kind=int32), dimension(3) :: data
    end type ivec3

    public :: vec3
    type :: vec3
        real(kind=real32), dimension(3) :: data
    end type vec3

    public :: dvec3
    type :: dvec3
        real(kind=real64), dimension(3) :: data
    end type dvec3

    ! ----------------
    ! --- Constructors
    ! ----------------

    interface bvec3
        module procedure bvec3_scalar
        module procedure bvec3_array
    end interface bvec3

    interface ivec3
        module procedure ivec3_scalar
        module procedure ivec3_array
    end interface ivec3

    interface vec3
        module procedure vec3_scalar
        module procedure vec3_array
    end interface vec3

    interface dvec3
        module procedure dvec3_scalar
        module procedure dvec3_array
    end interface dvec3

    contains

#define VECTOR_TYPE_SIZE 3
#include "detail/vec_type_impl.inc"

end module glmff_vec3

