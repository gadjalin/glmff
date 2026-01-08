module glmff_vec
    use, intrinsic :: iso_fortran_env, only: int32, real32, real64

    use glmff_vec2, only: bvec2, ivec2, vec2, dvec2
    use glmff_vec3, only: bvec3, ivec3, vec3, dvec3
    use glmff_vec4, only: bvec4, ivec4, vec4, dvec4

#include "glmff_macros.h"

    implicit none
    private

    public :: bvec2, ivec2, vec2, dvec2
    public :: bvec3, ivec3, vec3, dvec3
    public :: bvec4, ivec4, vec4, dvec4

    ! --------------
    ! --- Conversion
    ! --------------

    public :: to_vec2
    interface to_vec2
        module procedure bvec3_to_vec2
        module procedure bvec4_to_vec2
        module procedure ivec3_to_vec2
        module procedure ivec4_to_vec2
        module procedure vec3_to_vec2
        module procedure vec4_to_vec2
        module procedure dvec3_to_vec2
        module procedure dvec4_to_vec2
    end interface to_vec2

    public :: to_vec3
    interface to_vec3
        module procedure bvec2_to_vec3
        module procedure bvec4_to_vec3
        module procedure ivec2_to_vec3
        module procedure ivec4_to_vec3
        module procedure vec2_to_vec3
        module procedure vec4_to_vec3
        module procedure dvec2_to_vec3
        module procedure dvec4_to_vec3
    end interface to_vec3

    public :: to_vec4
    interface to_vec4
        module procedure bvec2_to_vec4
        module procedure bvec3_to_vec4
        module procedure ivec2_to_vec4
        module procedure ivec3_to_vec4
        module procedure vec2_to_vec4
        module procedure vec3_to_vec4
        module procedure dvec2_to_vec4
        module procedure dvec3_to_vec4
    end interface to_vec4

    ! -------------
    ! --- Operators
    ! -------------

    public :: operator(+)
    interface operator(+)
        module procedure pos_ivec2
        module procedure pos_vec2
        module procedure pos_dvec2
        module procedure add_ivec2
        module procedure add_vec2
        module procedure add_dvec2
        module procedure add_scalar_ivec2
        module procedure add_scalar_vec2
        module procedure add_scalar_dvec2
        module procedure add_ivec2_scalar
        module procedure add_vec2_scalar
        module procedure add_dvec2_scalar

        module procedure pos_ivec3
        module procedure pos_vec3
        module procedure pos_dvec3
        module procedure add_ivec3
        module procedure add_vec3
        module procedure add_dvec3
        module procedure add_scalar_ivec3
        module procedure add_scalar_vec3
        module procedure add_scalar_dvec3
        module procedure add_ivec3_scalar
        module procedure add_vec3_scalar
        module procedure add_dvec3_scalar

        module procedure pos_ivec4
        module procedure pos_vec4
        module procedure pos_dvec4
        module procedure add_ivec4
        module procedure add_vec4
        module procedure add_dvec4
        module procedure add_scalar_ivec4
        module procedure add_scalar_vec4
        module procedure add_scalar_dvec4
        module procedure add_ivec4_scalar
        module procedure add_vec4_scalar
        module procedure add_dvec4_scalar
    end interface operator(+)

    public :: operator(-)
    interface operator(-)
        module procedure neg_ivec2
        module procedure neg_vec2
        module procedure neg_dvec2
        module procedure sub_ivec2
        module procedure sub_vec2
        module procedure sub_dvec2
        module procedure sub_scalar_ivec2
        module procedure sub_scalar_vec2
        module procedure sub_scalar_dvec2
        module procedure sub_ivec2_scalar
        module procedure sub_vec2_scalar
        module procedure sub_dvec2_scalar

        module procedure neg_ivec3
        module procedure neg_vec3
        module procedure neg_dvec3
        module procedure sub_ivec3
        module procedure sub_vec3
        module procedure sub_dvec3
        module procedure sub_scalar_ivec3
        module procedure sub_scalar_vec3
        module procedure sub_scalar_dvec3
        module procedure sub_ivec3_scalar
        module procedure sub_vec3_scalar
        module procedure sub_dvec3_scalar

        module procedure neg_ivec4
        module procedure neg_vec4
        module procedure neg_dvec4
        module procedure sub_ivec4
        module procedure sub_vec4
        module procedure sub_dvec4
        module procedure sub_scalar_ivec4
        module procedure sub_scalar_vec4
        module procedure sub_scalar_dvec4
        module procedure sub_ivec4_scalar
        module procedure sub_vec4_scalar
        module procedure sub_dvec4_scalar
    end interface operator(-)

    public :: operator(*)
    interface operator(*)
        module procedure mul_ivec2
        module procedure mul_vec2
        module procedure mul_dvec2
        module procedure mul_scalar_ivec2
        module procedure mul_scalar_vec2
        module procedure mul_scalar_dvec2
        module procedure mul_ivec2_scalar
        module procedure mul_vec2_scalar
        module procedure mul_dvec2_scalar

        module procedure mul_ivec3
        module procedure mul_vec3
        module procedure mul_dvec3
        module procedure mul_scalar_ivec3
        module procedure mul_scalar_vec3
        module procedure mul_scalar_dvec3
        module procedure mul_ivec3_scalar
        module procedure mul_vec3_scalar
        module procedure mul_dvec3_scalar

        module procedure mul_ivec4
        module procedure mul_vec4
        module procedure mul_dvec4
        module procedure mul_scalar_ivec4
        module procedure mul_scalar_vec4
        module procedure mul_scalar_dvec4
        module procedure mul_ivec4_scalar
        module procedure mul_vec4_scalar
        module procedure mul_dvec4_scalar
    end interface operator(*)

    public :: operator(/)
    interface operator(/)
        module procedure div_ivec2
        module procedure div_vec2
        module procedure div_dvec2
        module procedure div_scalar_ivec2
        module procedure div_scalar_vec2
        module procedure div_scalar_dvec2
        module procedure div_ivec2_scalar
        module procedure div_vec2_scalar
        module procedure div_dvec2_scalar

        module procedure div_ivec3
        module procedure div_vec3
        module procedure div_dvec3
        module procedure div_scalar_ivec3
        module procedure div_scalar_vec3
        module procedure div_scalar_dvec3
        module procedure div_ivec3_scalar
        module procedure div_vec3_scalar
        module procedure div_dvec3_scalar

        module procedure div_ivec4
        module procedure div_vec4
        module procedure div_dvec4
        module procedure div_scalar_ivec4
        module procedure div_scalar_vec4
        module procedure div_scalar_dvec4
        module procedure div_ivec4_scalar
        module procedure div_vec4_scalar
        module procedure div_dvec4_scalar
    end interface operator(/)

    public :: operator(.dot.)
    interface operator(.dot.)
        module procedure dot_vec2
        module procedure dot_dvec2

        module procedure dot_vec3
        module procedure dot_dvec3

        module procedure dot_vec4
        module procedure dot_dvec4
    end interface operator(.dot.)

    public :: dot
    interface dot
        module procedure dot_vec2
        module procedure dot_dvec2

        module procedure dot_vec3
        module procedure dot_dvec3

        module procedure dot_vec4
        module procedure dot_dvec4
    end interface dot

    public :: operator(.cross.)
    interface operator(.cross.)
        module procedure cross_vec3
        module procedure cross_dvec3
    end interface operator(.cross.)

    public :: cross
    interface cross
        module procedure cross_vec3
        module procedure cross_dvec3
    end interface cross

    public :: operator(==)
    interface operator(==)
        module procedure eq_bvec2
        module procedure eq_ivec2
        module procedure eq_vec2
        module procedure eq_dvec2

        module procedure eq_bvec3
        module procedure eq_ivec3
        module procedure eq_vec3
        module procedure eq_dvec3

        module procedure eq_bvec4
        module procedure eq_ivec4
        module procedure eq_vec4
        module procedure eq_dvec4
    end interface operator(==)

    public :: operator(/=)
    interface operator(/=)
        module procedure ne_bvec2
        module procedure ne_ivec2
        module procedure ne_vec2
        module procedure ne_dvec2

        module procedure ne_bvec3
        module procedure ne_ivec3
        module procedure ne_vec3
        module procedure ne_dvec3

        module procedure ne_bvec4
        module procedure ne_ivec4
        module procedure ne_vec4
        module procedure ne_dvec4
    end interface operator(/=)

    public :: operator(.and.)
    interface operator(.and.)
        module procedure and_bvec2
        module procedure and_bvec3
        module procedure and_bvec4
    end interface operator(.and.)

    public :: operator(.or.)
    interface operator(.or.)
        module procedure or_bvec2
        module procedure or_bvec3
        module procedure or_bvec4
    end interface operator(.or.)

    public :: any
    interface any
        module procedure any_bvec2
        module procedure any_bvec3
        module procedure any_bvec4
    end interface any

    public :: all
    interface all
        module procedure all_bvec2
        module procedure all_bvec3
        module procedure all_bvec4
    end interface all

    public :: operator(.not.)
    interface operator(.not.)
        module procedure not_bvec2
        module procedure not_bvec3
        module procedure not_bvec4
    end interface operator(.not.)

    ! -------------
    ! --- Utilities
    ! -------------

    public :: length
    interface length
        module procedure length_vec2
        module procedure length_dvec2

        module procedure length_vec3
        module procedure length_dvec3

        module procedure length_vec4
        module procedure length_dvec4
    end interface length

    public :: distance
    interface distance
        module procedure distance_vec2
        module procedure distance_dvec2

        module procedure distance_vec3
        module procedure distance_dvec3

        module procedure distance_vec4
        module procedure distance_dvec4
    end interface distance

    public :: normalize
    interface normalize
        module procedure normalize_vec2
        module procedure normalize_dvec2

        module procedure normalize_vec3
        module procedure normalize_dvec3

        module procedure normalize_vec4
        module procedure normalize_dvec4
    end interface normalize

    public :: faceforward
    interface faceforward
        module procedure faceforward_vec2
        module procedure faceforward_dvec2

        module procedure faceforward_vec3
        module procedure faceforward_dvec3

        module procedure faceforward_vec4
        module procedure faceforward_dvec4
    end interface faceforward

    public :: reflect
    interface reflect
        module procedure reflect_vec2
        module procedure reflect_dvec2

        module procedure reflect_vec3
        module procedure reflect_dvec3

        module procedure reflect_vec4
        module procedure reflect_dvec4
    end interface reflect

    public :: refract
    interface refract
        module procedure refract_vec2
        module procedure refract_dvec2

        module procedure refract_vec3
        module procedure refract_dvec3

        module procedure refract_vec4
        module procedure refract_dvec4
    end interface refract

    public :: lessThan
    interface lessThan
        module procedure lessThan_ivec2
        module procedure lessThan_vec2
        module procedure lessThan_dvec2

        module procedure lessThan_ivec3
        module procedure lessThan_vec3
        module procedure lessThan_dvec3

        module procedure lessThan_ivec4
        module procedure lessThan_vec4
        module procedure lessThan_dvec4
    end interface lessThan

    public :: lessThanEqual
    interface lessThanEqual
        module procedure lessThanEqual_ivec2
        module procedure lessThanEqual_vec2
        module procedure lessThanEqual_dvec2

        module procedure lessThanEqual_ivec3
        module procedure lessThanEqual_vec3
        module procedure lessThanEqual_dvec3

        module procedure lessThanEqual_ivec4
        module procedure lessThanEqual_vec4
        module procedure lessThanEqual_dvec4
    end interface lessThanEqual

    public :: greaterThan
    interface greaterThan
        module procedure greaterThan_ivec2
        module procedure greaterThan_vec2
        module procedure greaterThan_dvec2

        module procedure greaterThan_ivec3
        module procedure greaterThan_vec3
        module procedure greaterThan_dvec3

        module procedure greaterThan_ivec4
        module procedure greaterThan_vec4
        module procedure greaterThan_dvec4
    end interface greaterThan

    public :: greaterThanEqual
    interface greaterThanEqual
        module procedure greaterThanEqual_ivec2
        module procedure greaterThanEqual_vec2
        module procedure greaterThanEqual_dvec2

        module procedure greaterThanEqual_ivec3
        module procedure greaterThanEqual_vec3
        module procedure greaterThanEqual_dvec3

        module procedure greaterThanEqual_ivec4
        module procedure greaterThanEqual_vec4
        module procedure greaterThanEqual_dvec4
    end interface greaterThanEqual

    public :: equal
    interface equal
        module procedure equal_ivec2
        module procedure equal_vec2
        module procedure equal_dvec2

        module procedure equal_ivec3
        module procedure equal_vec3
        module procedure equal_dvec3

        module procedure equal_ivec4
        module procedure equal_vec4
        module procedure equal_dvec4
    end interface equal

    public :: notEqual
    interface notEqual
        module procedure notEqual_ivec2
        module procedure notEqual_vec2
        module procedure notEqual_dvec2

        module procedure notEqual_ivec3
        module procedure notEqual_vec3
        module procedure notEqual_dvec3

        module procedure notEqual_ivec4
        module procedure notEqual_vec4
        module procedure notEqual_dvec4
    end interface notEqual

    public :: abs
    interface abs
        module procedure abs_ivec2
        module procedure abs_vec2
        module procedure abs_dvec2

        module procedure abs_ivec3
        module procedure abs_vec3
        module procedure abs_dvec3

        module procedure abs_ivec4
        module procedure abs_vec4
        module procedure abs_dvec4
    end interface abs

    public :: sign
    interface sign
        module procedure sign_ivec2
        module procedure sign_vec2
        module procedure sign_dvec2

        module procedure sign_ivec3
        module procedure sign_vec3
        module procedure sign_dvec3

        module procedure sign_ivec4
        module procedure sign_vec4
        module procedure sign_dvec4
    end interface sign

    public :: mod
    interface mod
        module procedure mod_ivec2
        module procedure mod_vec2
        module procedure mod_dvec2

        module procedure mod_ivec3
        module procedure mod_vec3
        module procedure mod_dvec3

        module procedure mod_ivec4
        module procedure mod_vec4
        module procedure mod_dvec4
    end interface mod

    public :: floor
    interface floor
        module procedure floor_vec2
        module procedure floor_dvec2

        module procedure floor_vec3
        module procedure floor_dvec3

        module procedure floor_vec4
        module procedure floor_dvec4
    end interface floor

    public :: ceil
    interface ceil
        module procedure ceil_vec2
        module procedure ceil_dvec2

        module procedure ceil_vec3
        module procedure ceil_dvec3

        module procedure ceil_vec4
        module procedure ceil_dvec4
    end interface ceil

    public :: fract
    interface fract
        module procedure fract_vec2
        module procedure fract_dvec2

        module procedure fract_vec3
        module procedure fract_dvec3

        module procedure fract_vec4
        module procedure fract_dvec4
    end interface fract

    public :: trunc
    interface trunc
        module procedure trunc_vec2
        module procedure trunc_dvec2

        module procedure trunc_vec3
        module procedure trunc_dvec3

        module procedure trunc_vec4
        module procedure trunc_dvec4
    end interface trunc

    public :: round
    interface round
        module procedure round_vec2
        module procedure round_dvec2

        module procedure round_vec3
        module procedure round_dvec3

        module procedure round_vec4
        module procedure round_dvec4
    end interface round

    public :: clamp
    interface clamp
        module procedure clamp_ivec2
        module procedure clamp_vec2
        module procedure clamp_dvec2
        module procedure clamp_ivec2_scalar
        module procedure clamp_vec2_scalar
        module procedure clamp_dvec2_scalar
        module procedure clamp_ivec2_scalar_low
        module procedure clamp_vec2_scalar_low
        module procedure clamp_dvec2_scalar_low
        module procedure clamp_ivec2_scalar_high
        module procedure clamp_vec2_scalar_high
        module procedure clamp_dvec2_scalar_high

        module procedure clamp_ivec3
        module procedure clamp_vec3
        module procedure clamp_dvec3
        module procedure clamp_ivec3_scalar
        module procedure clamp_vec3_scalar
        module procedure clamp_dvec3_scalar
        module procedure clamp_ivec3_scalar_low
        module procedure clamp_vec3_scalar_low
        module procedure clamp_dvec3_scalar_low
        module procedure clamp_ivec3_scalar_high
        module procedure clamp_vec3_scalar_high
        module procedure clamp_dvec3_scalar_high

        module procedure clamp_ivec4
        module procedure clamp_vec4
        module procedure clamp_dvec4
        module procedure clamp_ivec4_scalar
        module procedure clamp_vec4_scalar
        module procedure clamp_dvec4_scalar
        module procedure clamp_ivec4_scalar_low
        module procedure clamp_vec4_scalar_low
        module procedure clamp_dvec4_scalar_low
        module procedure clamp_ivec4_scalar_high
        module procedure clamp_vec4_scalar_high
        module procedure clamp_dvec4_scalar_high
    end interface clamp

    public :: min
    interface min
        module procedure min_ivec2
        module procedure min_vec2
        module procedure min_dvec2
        module procedure min_ivec2_scalar
        module procedure min_vec2_scalar
        module procedure min_dvec2_scalar

        module procedure min_ivec3
        module procedure min_vec3
        module procedure min_dvec3
        module procedure min_ivec3_scalar
        module procedure min_vec3_scalar
        module procedure min_dvec3_scalar

        module procedure min_ivec4
        module procedure min_vec4
        module procedure min_dvec4
        module procedure min_ivec4_scalar
        module procedure min_vec4_scalar
        module procedure min_dvec4_scalar
    end interface min

    public :: max
    interface max
        module procedure max_ivec2
        module procedure max_vec2
        module procedure max_dvec2
        module procedure max_ivec2_scalar
        module procedure max_vec2_scalar
        module procedure max_dvec2_scalar

        module procedure max_ivec3
        module procedure max_vec3
        module procedure max_dvec3
        module procedure max_ivec3_scalar
        module procedure max_vec3_scalar
        module procedure max_dvec3_scalar

        module procedure max_ivec4
        module procedure max_vec4
        module procedure max_dvec4
        module procedure max_ivec4_scalar
        module procedure max_vec4_scalar
        module procedure max_dvec4_scalar
    end interface max

    public :: mix
    interface mix
        module procedure mix_bvec2
        module procedure mix_bvec2_scalar
        module procedure mix_ivec2_bool
        module procedure mix_ivec2_bvec
        module procedure mix_vec2
        module procedure mix_vec2_scalar
        module procedure mix_vec2_bool
        module procedure mix_vec2_bvec
        module procedure mix_dvec2
        module procedure mix_dvec2_scalar
        module procedure mix_dvec2_bool
        module procedure mix_dvec2_bvec

        module procedure mix_bvec3
        module procedure mix_bvec3_scalar
        module procedure mix_ivec3_bool
        module procedure mix_ivec3_bvec
        module procedure mix_vec3
        module procedure mix_vec3_scalar
        module procedure mix_vec3_bool
        module procedure mix_vec3_bvec
        module procedure mix_dvec3
        module procedure mix_dvec3_scalar
        module procedure mix_dvec3_bool
        module procedure mix_dvec3_bvec

        module procedure mix_bvec4
        module procedure mix_bvec4_scalar
        module procedure mix_ivec4_bool
        module procedure mix_ivec4_bvec
        module procedure mix_vec4
        module procedure mix_vec4_scalar
        module procedure mix_vec4_bool
        module procedure mix_vec4_bvec
        module procedure mix_dvec4
        module procedure mix_dvec4_scalar
        module procedure mix_dvec4_bool
        module procedure mix_dvec4_bvec
    end interface mix

    public :: step
    interface step
        module procedure step_vec2
        module procedure step_vec2_scalar
        module procedure step_dvec2
        module procedure step_dvec2_scalar

        module procedure step_vec3
        module procedure step_vec3_scalar
        module procedure step_dvec3
        module procedure step_dvec3_scalar

        module procedure step_vec4
        module procedure step_vec4_scalar
        module procedure step_dvec4
        module procedure step_dvec4_scalar
    end interface step

    public :: smoothstep
    interface smoothstep
        module procedure smoothstep_vec2
        module procedure smoothstep_vec2_scalar
        module procedure smoothstep_dvec2
        module procedure smoothstep_dvec2_scalar

        module procedure smoothstep_vec3
        module procedure smoothstep_vec3_scalar
        module procedure smoothstep_dvec3
        module procedure smoothstep_dvec3_scalar

        module procedure smoothstep_vec4
        module procedure smoothstep_vec4_scalar
        module procedure smoothstep_dvec4
        module procedure smoothstep_dvec4_scalar
    end interface smoothstep

    public :: fma
    interface fma
        module procedure fma_vec2
        module procedure fma_dvec2

        module procedure fma_vec3
        module procedure fma_dvec3

        module procedure fma_vec4
        module procedure fma_dvec4
    end interface fma

    public :: isinf
    interface isinf
        module procedure isinf_vec2
        module procedure isinf_dvec2

        module procedure isinf_vec3
        module procedure isinf_dvec3

        module procedure isinf_vec4
        module procedure isinf_dvec4
    end interface isinf

    public :: isnan
    interface isnan
        module procedure isnan_vec2
        module procedure isnan_dvec2

        module procedure isnan_vec3
        module procedure isnan_dvec3

        module procedure isnan_vec4
        module procedure isnan_dvec4
    end interface isnan

    ! ----------------
    ! --- Trigonometry
    ! ----------------

    public :: radians
    interface radians
        module procedure radians_vec2
        module procedure radians_dvec2

        module procedure radians_vec3
        module procedure radians_dvec3

        module procedure radians_vec4
        module procedure radians_dvec4
    end interface radians

    public :: degrees
    interface degrees
        module procedure degrees_vec2
        module procedure degrees_dvec2

        module procedure degrees_vec3
        module procedure degrees_dvec3

        module procedure degrees_vec4
        module procedure degrees_dvec4
    end interface degrees

    public :: sin
    interface sin
        module procedure sin_vec2
        module procedure sin_dvec2

        module procedure sin_vec3
        module procedure sin_dvec3

        module procedure sin_vec4
        module procedure sin_dvec4
    end interface sin

    public :: cos
    interface cos
        module procedure cos_vec2
        module procedure cos_dvec2

        module procedure cos_vec3
        module procedure cos_dvec3

        module procedure cos_vec4
        module procedure cos_dvec4
    end interface cos

    public :: tan
    interface tan
        module procedure tan_vec2
        module procedure tan_dvec2

        module procedure tan_vec3
        module procedure tan_dvec3

        module procedure tan_vec4
        module procedure tan_dvec4
    end interface tan

    public :: asin
    interface asin
        module procedure asin_vec2
        module procedure asin_dvec2

        module procedure asin_vec3
        module procedure asin_dvec3

        module procedure asin_vec4
        module procedure asin_dvec4
    end interface asin

    public :: acos
    interface acos
        module procedure acos_vec2
        module procedure acos_dvec2

        module procedure acos_vec3
        module procedure acos_dvec3

        module procedure acos_vec4
        module procedure acos_dvec4
    end interface acos

    public :: atan
    interface atan
        module procedure atan_vec2
        module procedure atan_dvec2
        module procedure atan2_vec2
        module procedure atan2_dvec2

        module procedure atan_vec3
        module procedure atan_dvec3
        module procedure atan2_vec3
        module procedure atan2_dvec3

        module procedure atan_vec4
        module procedure atan_dvec4
        module procedure atan2_vec4
        module procedure atan2_dvec4
    end interface atan

    public :: sinh
    interface sinh
        module procedure sinh_vec2
        module procedure sinh_dvec2

        module procedure sinh_vec3
        module procedure sinh_dvec3

        module procedure sinh_vec4
        module procedure sinh_dvec4
    end interface sinh

    public :: cosh
    interface cosh
        module procedure cosh_vec2
        module procedure cosh_dvec2

        module procedure cosh_vec3
        module procedure cosh_dvec3

        module procedure cosh_vec4
        module procedure cosh_dvec4
    end interface cosh

    public :: tanh
    interface tanh
        module procedure tanh_vec2
        module procedure tanh_dvec2

        module procedure tanh_vec3
        module procedure tanh_dvec3

        module procedure tanh_vec4
        module procedure tanh_dvec4
    end interface tanh

    public :: asinh
    interface asinh
        module procedure asinh_vec2
        module procedure asinh_dvec2

        module procedure asinh_vec3
        module procedure asinh_dvec3

        module procedure asinh_vec4
        module procedure asinh_dvec4
    end interface asinh

    public :: acosh
    interface acosh
        module procedure acosh_vec2
        module procedure acosh_dvec2

        module procedure acosh_vec3
        module procedure acosh_dvec3

        module procedure acosh_vec4
        module procedure acosh_dvec4
    end interface acosh

    public :: atanh
    interface atanh
        module procedure atanh_vec2
        module procedure atanh_dvec2

        module procedure atanh_vec3
        module procedure atanh_dvec3

        module procedure atanh_vec4
        module procedure atanh_dvec4
    end interface atanh

    contains

#undef VECTOR_TYPE_SIZE
#define VECTOR_TYPE_SIZE 2
#include "detail/vec_impl.inc"

#undef VECTOR_TYPE_SIZE
#define VECTOR_TYPE_SIZE 3
#include "detail/vec_impl.inc"

#undef VECTOR_TYPE_SIZE
#define VECTOR_TYPE_SIZE 4
#include "detail/vec_impl.inc"

end module glmff_vec

