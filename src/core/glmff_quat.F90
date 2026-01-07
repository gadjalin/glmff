module glmff_quat
    use, intrinsic :: iso_fortran_env, only: real32, real64
    use glmff_scalar, only: mix
    use glmff_vec, only: vec3, dvec3, bvec4, vec4, dvec4, dot, cross, length
    use glmff_mat, only: mat3x3, mat4x4, dmat3x3, dmat4x4

#include "glmff_macros.h"

    implicit none
    private

    ! --------------
    ! --- Data types
    ! --------------

    public :: quat
    type :: quat
        real(kind=real32) :: w = 1.0
        real(kind=real32), dimension(3) :: v = 0.0
    end type quat

    public :: dquat
    type :: dquat
        real(kind=real64) :: w = 1.0
        real(kind=real64), dimension(3) :: v = 0.0d0
    end type dquat

    ! ----------------
    ! --- Constructors
    ! ----------------

    interface quat
        module procedure quat_scalar
        module procedure quat_scalar_vec
        module procedure quat_vec_vec
        module procedure quat_mat3
        module procedure quat_mat4
        module procedure quat_euler
    end interface quat

    interface dquat
        module procedure dquat_scalar
        module procedure dquat_scalar_vec
        module procedure dquat_vec_vec
        module procedure dquat_mat3
        module procedure dquat_mat4
        module procedure dquat_euler
    end interface dquat

    ! -------------
    ! --- Operators
    ! -------------

    public :: operator(+)
    interface operator(+)
        module procedure pos_quat
        module procedure pos_dquat
        module procedure add_quat
        module procedure add_dquat
    end interface operator(+)

    public :: operator(-)
    interface operator(-)
        module procedure neg_quat
        module procedure neg_dquat
        module procedure sub_quat
        module procedure sub_dquat
    end interface operator(-)

    public :: operator(*)
    interface operator(*)
        module procedure mul_quat
        module procedure mul_dquat
        module procedure mul_quat_vec3
        module procedure mul_dquat_vec3
        module procedure mul_vec3_quat
        module procedure mul_vec3_dquat
        module procedure mul_quat_vec4
        module procedure mul_dquat_vec4
        module procedure mul_vec4_quat
        module procedure mul_vec4_dquat
        module procedure mul_quat_scalar
        module procedure mul_dquat_scalar
        module procedure mul_scalar_quat
        module procedure mul_scalar_dquat
    end interface operator(*)

    public :: operator(/)
    interface operator(/)
        module procedure div_quat_scalar
        module procedure div_dquat_scalar
    end interface operator(/)

    public :: operator(==)
    interface operator(==)
        module procedure eq_quat
        module procedure eq_dquat
    end interface operator(==)

    public :: operator(/=)
    interface operator(/=)
        module procedure ne_quat
        module procedure ne_dquat
    end interface operator(/=)

    public :: operator(.dot.)
    interface operator(.dot.)
        module procedure dot_quat
        module procedure dot_dquat
    end interface operator(.dot.)

    public :: dot
    interface dot
        module procedure dot_quat
        module procedure dot_dquat
    end interface dot

    public :: operator(.cross.)
    interface operator(.cross.)
        module procedure cross_quat
        module procedure cross_dquat
    end interface operator(.cross.)

    public :: cross
    interface cross
        module procedure cross_quat
        module procedure cross_dquat
    end interface cross

    public :: rotate
    interface rotate
        module procedure rotate_quat
        module procedure rotate_dquat
    end interface rotate

    public :: length
    interface length
        module procedure length_quat
        module procedure length_dquat
    end interface length

    public :: normalize
    interface normalize
        module procedure normalize_quat
        module procedure normalize_dquat
    end interface normalize

    public :: angle
    interface angle
        module procedure angle_quat
        module procedure angle_dquat
    end interface angle

    public :: axis
    interface axis
        module procedure axis_quat
        module procedure axis_dquat
    end interface axis

    public :: angleAxis
    interface angleAxis
        module procedure angleAxis_quat
        module procedure angleAxis_dquat
    end interface angleAxis

    public :: mix
    interface mix
        module procedure mix_quat
        module procedure mix_dquat
    end interface mix

    public :: lerp
    interface lerp
        module procedure lerp_quat
        module procedure lerp_dquat
    end interface lerp

    public :: slerp
    interface slerp
        module procedure slerp_quat
        module procedure slerp_dquat
        module procedure slerp_k_quat
        module procedure slerp_k_dquat
    end interface slerp

    public :: conjugate
    interface conjugate
        module procedure conjugate_quat
        module procedure conjugate_dquat
    end interface conjugate

    public :: inverse
    interface inverse
        module procedure inverse_quat
        module procedure inverse_dquat
    end interface inverse

    public :: isnan
    interface isnan
        module procedure isnan_quat
        module procedure isnan_dquat
    end interface isnan

    public :: isinf
    interface isinf
        module procedure isinf_quat
        module procedure isinf_dquat
    end interface isinf

    public :: equal
    interface equal
        module procedure equal_quat
        module procedure equal_eps_quat
        module procedure equal_dquat
        module procedure equal_eps_dquat
    end interface equal

    public :: notEqual
    interface notEqual
        module procedure notEqual_quat
        module procedure notEqual_eps_quat
        module procedure notEqual_dquat
        module procedure notEqual_eps_dquat
    end interface notEqual

    contains

#include "detail/quat_type_impl.inc"
#include "detail/quat_impl.inc"

end module glmff_quat

