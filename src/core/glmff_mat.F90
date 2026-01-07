module glmff_mat
    use, intrinsic :: iso_fortran_env, only: real32, real64

    use glmff_mat2x2, only: mat2x2, dmat2x2, mat2 => mat2x2, dmat2 => dmat2x2
    use glmff_mat2x3, only: mat2x3, dmat2x3
    use glmff_mat2x4, only: mat2x4, dmat2x4
    use glmff_mat3x2, only: mat3x2, dmat3x2
    use glmff_mat3x3, only: mat3x3, dmat3x3, mat3 => mat3x3, dmat3 => dmat3x3
    use glmff_mat3x4, only: mat3x4, dmat3x4
    use glmff_mat4x2, only: mat4x2, dmat4x2
    use glmff_mat4x3, only: mat4x3, dmat4x3
    use glmff_mat4x4, only: mat4x4, dmat4x4, mat4 => mat4x4, dmat4 => dmat4x4

    use glmff_vec, only: vec2, dvec2, vec3, dvec3, vec4, dvec4, normalize, cross, dot, &
        operator(-), operator(*)

#include "glmff_macros.h"

    implicit none
    private

    public :: mat2x2, dmat2x2, mat2, dmat2
    public :: mat2x3, dmat2x3
    public :: mat2x4, dmat2x4
    public :: mat3x2, dmat3x2
    public :: mat3x3, dmat3x3, mat3, dmat3
    public :: mat3x4, dmat3x4
    public :: mat4x2, dmat4x2
    public :: mat4x3, dmat4x3
    public :: mat4x4, dmat4x4, mat4, dmat4

    ! -------------
    ! --- Operators
    ! -------------

    public :: operator(+)
    interface operator(+)
        module procedure pos_mat2x2
        module procedure pos_dmat2x2
        module procedure add_mat2x2
        module procedure add_dmat2x2
        module procedure add_mat2x2_scalar
        module procedure add_dmat2x2_scalar

        module procedure pos_mat2x3
        module procedure pos_dmat2x3
        module procedure add_mat2x3
        module procedure add_dmat2x3
        module procedure add_mat2x3_scalar
        module procedure add_dmat2x3_scalar

        module procedure pos_mat2x4
        module procedure pos_dmat2x4
        module procedure add_mat2x4
        module procedure add_dmat2x4
        module procedure add_mat2x4_scalar
        module procedure add_dmat2x4_scalar

        module procedure pos_mat3x2
        module procedure pos_dmat3x2
        module procedure add_mat3x2
        module procedure add_dmat3x2
        module procedure add_mat3x2_scalar
        module procedure add_dmat3x2_scalar

        module procedure pos_mat3x3
        module procedure pos_dmat3x3
        module procedure add_mat3x3
        module procedure add_dmat3x3
        module procedure add_mat3x3_scalar
        module procedure add_dmat3x3_scalar

        module procedure pos_mat3x4
        module procedure pos_dmat3x4
        module procedure add_mat3x4
        module procedure add_dmat3x4
        module procedure add_mat3x4_scalar
        module procedure add_dmat3x4_scalar

        module procedure pos_mat4x2
        module procedure pos_dmat4x2
        module procedure add_mat4x2
        module procedure add_dmat4x2
        module procedure add_mat4x2_scalar
        module procedure add_dmat4x2_scalar

        module procedure pos_mat4x3
        module procedure pos_dmat4x3
        module procedure add_mat4x3
        module procedure add_dmat4x3
        module procedure add_mat4x3_scalar
        module procedure add_dmat4x3_scalar

        module procedure pos_mat4x4
        module procedure pos_dmat4x4
        module procedure add_mat4x4
        module procedure add_dmat4x4
        module procedure add_mat4x4_scalar
        module procedure add_dmat4x4_scalar
    end interface operator(+)

    public :: operator(-)
    interface operator(-)
        module procedure neg_mat2x2
        module procedure neg_dmat2x2
        module procedure sub_mat2x2
        module procedure sub_dmat2x2
        module procedure sub_mat2x2_scalar
        module procedure sub_dmat2x2_scalar

        module procedure neg_mat2x3
        module procedure neg_dmat2x3
        module procedure sub_mat2x3
        module procedure sub_dmat2x3
        module procedure sub_mat2x3_scalar
        module procedure sub_dmat2x3_scalar

        module procedure neg_mat2x4
        module procedure neg_dmat2x4
        module procedure sub_mat2x4
        module procedure sub_dmat2x4
        module procedure sub_mat2x4_scalar
        module procedure sub_dmat2x4_scalar

        module procedure neg_mat3x2
        module procedure neg_dmat3x2
        module procedure sub_mat3x2
        module procedure sub_dmat3x2
        module procedure sub_mat3x2_scalar
        module procedure sub_dmat3x2_scalar

        module procedure neg_mat3x3
        module procedure neg_dmat3x3
        module procedure sub_mat3x3
        module procedure sub_dmat3x3
        module procedure sub_mat3x3_scalar
        module procedure sub_dmat3x3_scalar

        module procedure neg_mat3x4
        module procedure neg_dmat3x4
        module procedure sub_mat3x4
        module procedure sub_dmat3x4
        module procedure sub_mat3x4_scalar
        module procedure sub_dmat3x4_scalar

        module procedure neg_mat4x2
        module procedure neg_dmat4x2
        module procedure sub_mat4x2
        module procedure sub_dmat4x2
        module procedure sub_mat4x2_scalar
        module procedure sub_dmat4x2_scalar

        module procedure neg_mat4x3
        module procedure neg_dmat4x3
        module procedure sub_mat4x3
        module procedure sub_dmat4x3
        module procedure sub_mat4x3_scalar
        module procedure sub_dmat4x3_scalar

        module procedure neg_mat4x4
        module procedure neg_dmat4x4
        module procedure sub_mat4x4
        module procedure sub_dmat4x4
        module procedure sub_mat4x4_scalar
        module procedure sub_dmat4x4_scalar
    end interface operator(-)

    public :: operator(*)
    interface operator(*)
        module procedure mul_mat2x2_mat2
        module procedure mul_dmat2x2_mat2
        module procedure mul_mat2x2_mat3
        module procedure mul_dmat2x2_mat3
        module procedure mul_mat2x2_mat4
        module procedure mul_dmat2x2_mat4
        module procedure mul_mat2x2_scalar
        module procedure mul_dmat2x2_scalar
        module procedure mul_scalar_mat2x2
        module procedure mul_scalar_dmat2x2
        module procedure mul_mat2x2_vec
        module procedure mul_dmat2x2_vec
        module procedure mul_vec_mat2x2
        module procedure mul_vec_dmat2x2

        module procedure mul_mat2x3_mat2
        module procedure mul_dmat2x3_mat2
        module procedure mul_mat2x3_mat3
        module procedure mul_dmat2x3_mat3
        module procedure mul_mat2x3_mat4
        module procedure mul_dmat2x3_mat4
        module procedure mul_mat2x3_scalar
        module procedure mul_dmat2x3_scalar
        module procedure mul_scalar_mat2x3
        module procedure mul_scalar_dmat2x3
        module procedure mul_mat2x3_vec
        module procedure mul_dmat2x3_vec
        module procedure mul_vec_mat2x3
        module procedure mul_vec_dmat2x3

        module procedure mul_mat2x4_mat2
        module procedure mul_dmat2x4_mat2
        module procedure mul_mat2x4_mat3
        module procedure mul_dmat2x4_mat3
        module procedure mul_mat2x4_mat4
        module procedure mul_dmat2x4_mat4
        module procedure mul_mat2x4_scalar
        module procedure mul_dmat2x4_scalar
        module procedure mul_scalar_mat2x4
        module procedure mul_scalar_dmat2x4
        module procedure mul_mat2x4_vec
        module procedure mul_dmat2x4_vec
        module procedure mul_vec_mat2x4
        module procedure mul_vec_dmat2x4

        module procedure mul_mat3x2_mat2
        module procedure mul_dmat3x2_mat2
        module procedure mul_mat3x2_mat3
        module procedure mul_dmat3x2_mat3
        module procedure mul_mat3x2_mat4
        module procedure mul_dmat3x2_mat4
        module procedure mul_mat3x2_scalar
        module procedure mul_dmat3x2_scalar
        module procedure mul_scalar_mat3x2
        module procedure mul_scalar_dmat3x2
        module procedure mul_mat3x2_vec
        module procedure mul_dmat3x2_vec
        module procedure mul_vec_mat3x2
        module procedure mul_vec_dmat3x2

        module procedure mul_mat3x3_mat2
        module procedure mul_dmat3x3_mat2
        module procedure mul_mat3x3_mat3
        module procedure mul_dmat3x3_mat3
        module procedure mul_mat3x3_mat4
        module procedure mul_dmat3x3_mat4
        module procedure mul_mat3x3_scalar
        module procedure mul_dmat3x3_scalar
        module procedure mul_scalar_mat3x3
        module procedure mul_scalar_dmat3x3
        module procedure mul_mat3x3_vec
        module procedure mul_dmat3x3_vec
        module procedure mul_vec_mat3x3
        module procedure mul_vec_dmat3x3

        module procedure mul_mat3x4_mat2
        module procedure mul_dmat3x4_mat2
        module procedure mul_mat3x4_mat3
        module procedure mul_dmat3x4_mat3
        module procedure mul_mat3x4_mat4
        module procedure mul_dmat3x4_mat4
        module procedure mul_mat3x4_scalar
        module procedure mul_dmat3x4_scalar
        module procedure mul_scalar_mat3x4
        module procedure mul_scalar_dmat3x4
        module procedure mul_mat3x4_vec
        module procedure mul_dmat3x4_vec
        module procedure mul_vec_mat3x4
        module procedure mul_vec_dmat3x4

        module procedure mul_mat4x2_mat2
        module procedure mul_dmat4x2_mat2
        module procedure mul_mat4x2_mat3
        module procedure mul_dmat4x2_mat3
        module procedure mul_mat4x2_mat4
        module procedure mul_dmat4x2_mat4
        module procedure mul_mat4x2_scalar
        module procedure mul_dmat4x2_scalar
        module procedure mul_scalar_mat4x2
        module procedure mul_scalar_dmat4x2
        module procedure mul_mat4x2_vec
        module procedure mul_dmat4x2_vec
        module procedure mul_vec_mat4x2
        module procedure mul_vec_dmat4x2

        module procedure mul_mat4x3_mat2
        module procedure mul_dmat4x3_mat2
        module procedure mul_mat4x3_mat3
        module procedure mul_dmat4x3_mat3
        module procedure mul_mat4x3_mat4
        module procedure mul_dmat4x3_mat4
        module procedure mul_mat4x3_scalar
        module procedure mul_dmat4x3_scalar
        module procedure mul_scalar_mat4x3
        module procedure mul_scalar_dmat4x3
        module procedure mul_mat4x3_vec
        module procedure mul_dmat4x3_vec
        module procedure mul_vec_mat4x3
        module procedure mul_vec_dmat4x3

        module procedure mul_mat4x4_mat2
        module procedure mul_dmat4x4_mat2
        module procedure mul_mat4x4_mat3
        module procedure mul_dmat4x4_mat3
        module procedure mul_mat4x4_mat4
        module procedure mul_dmat4x4_mat4
        module procedure mul_mat4x4_scalar
        module procedure mul_dmat4x4_scalar
        module procedure mul_scalar_mat4x4
        module procedure mul_scalar_dmat4x4
        module procedure mul_mat4x4_vec
        module procedure mul_dmat4x4_vec
        module procedure mul_vec_mat4x4
        module procedure mul_vec_dmat4x4
    end interface operator(*)

    public :: operator(/)
    interface operator(/)
        module procedure div_mat2x2_scalar
        module procedure div_dmat2x2_scalar
        module procedure div_scalar_mat2x2
        module procedure div_scalar_dmat2x2

        module procedure div_mat2x3_scalar
        module procedure div_dmat2x3_scalar
        module procedure div_scalar_mat2x3
        module procedure div_scalar_dmat2x3

        module procedure div_mat2x4_scalar
        module procedure div_dmat2x4_scalar
        module procedure div_scalar_mat2x4
        module procedure div_scalar_dmat2x4

        module procedure div_mat3x2_scalar
        module procedure div_dmat3x2_scalar
        module procedure div_scalar_mat3x2
        module procedure div_scalar_dmat3x2

        module procedure div_mat3x3_scalar
        module procedure div_dmat3x3_scalar
        module procedure div_scalar_mat3x3
        module procedure div_scalar_dmat3x3

        module procedure div_mat3x4_scalar
        module procedure div_dmat3x4_scalar
        module procedure div_scalar_mat3x4
        module procedure div_scalar_dmat3x4

        module procedure div_mat4x2_scalar
        module procedure div_dmat4x2_scalar
        module procedure div_scalar_mat4x2
        module procedure div_scalar_dmat4x2

        module procedure div_mat4x3_scalar
        module procedure div_dmat4x3_scalar
        module procedure div_scalar_mat4x3
        module procedure div_scalar_dmat4x3

        module procedure div_mat4x4_scalar
        module procedure div_dmat4x4_scalar
        module procedure div_scalar_mat4x4
        module procedure div_scalar_dmat4x4
    end interface operator(/)

    public :: operator(==)
    interface operator(==)
        module procedure eq_mat2x2
        module procedure eq_dmat2x2

        module procedure eq_mat2x3
        module procedure eq_dmat2x3

        module procedure eq_mat2x4
        module procedure eq_dmat2x4

        module procedure eq_mat3x2
        module procedure eq_dmat3x2

        module procedure eq_mat3x3
        module procedure eq_dmat3x3

        module procedure eq_mat3x4
        module procedure eq_dmat3x4

        module procedure eq_mat4x2
        module procedure eq_dmat4x2

        module procedure eq_mat4x3
        module procedure eq_dmat4x3

        module procedure eq_mat4x4
        module procedure eq_dmat4x4
    end interface operator(==)

    public :: operator(/=)
    interface operator(/=)
        module procedure ne_mat2x2
        module procedure ne_dmat2x2

        module procedure ne_mat2x3
        module procedure ne_dmat2x3

        module procedure ne_mat2x4
        module procedure ne_dmat2x4

        module procedure ne_mat3x2
        module procedure ne_dmat3x2

        module procedure ne_mat3x3
        module procedure ne_dmat3x3

        module procedure ne_mat3x4
        module procedure ne_dmat3x4

        module procedure ne_mat4x2
        module procedure ne_dmat4x2

        module procedure ne_mat4x3
        module procedure ne_dmat4x3

        module procedure ne_mat4x4
        module procedure ne_dmat4x4
    end interface operator(/=)

    public :: abs
    interface abs
        module procedure abs_mat2x2
        module procedure abs_dmat2x2

        module procedure abs_mat2x3
        module procedure abs_dmat2x3

        module procedure abs_mat2x4
        module procedure abs_dmat2x4

        module procedure abs_mat3x2
        module procedure abs_dmat3x2

        module procedure abs_mat3x3
        module procedure abs_dmat3x3

        module procedure abs_mat3x4
        module procedure abs_dmat3x4

        module procedure abs_mat4x2
        module procedure abs_dmat4x2

        module procedure abs_mat4x3
        module procedure abs_dmat4x3

        module procedure abs_mat4x4
        module procedure abs_dmat4x4
    end interface abs

    public :: mix
    interface mix
        module procedure mix_mat2x2
        module procedure mix_dmat2x2
        module procedure mix_mat2x2_scalar
        module procedure mix_dmat2x2_scalar

        module procedure mix_mat2x3
        module procedure mix_dmat2x3
        module procedure mix_mat2x3_scalar
        module procedure mix_dmat2x3_scalar

        module procedure mix_mat2x4
        module procedure mix_dmat2x4
        module procedure mix_mat2x4_scalar
        module procedure mix_dmat2x4_scalar

        module procedure mix_mat3x2
        module procedure mix_dmat3x2
        module procedure mix_mat3x2_scalar
        module procedure mix_dmat3x2_scalar

        module procedure mix_mat3x3
        module procedure mix_dmat3x3
        module procedure mix_mat3x3_scalar
        module procedure mix_dmat3x3_scalar

        module procedure mix_mat3x4
        module procedure mix_dmat3x4
        module procedure mix_mat3x4_scalar
        module procedure mix_dmat3x4_scalar

        module procedure mix_mat4x2
        module procedure mix_dmat4x2
        module procedure mix_mat4x2_scalar
        module procedure mix_dmat4x2_scalar

        module procedure mix_mat4x3
        module procedure mix_dmat4x3
        module procedure mix_mat4x3_scalar
        module procedure mix_dmat4x3_scalar

        module procedure mix_mat4x4
        module procedure mix_dmat4x4
        module procedure mix_mat4x4_scalar
        module procedure mix_dmat4x4_scalar
    end interface mix

    public :: matrixCompMult
    interface matrixCompMult
        module procedure matrixCompMult_mat2x2
        module procedure matrixCompMult_dmat2x2

        module procedure matrixCompMult_mat2x3
        module procedure matrixCompMult_dmat2x3

        module procedure matrixCompMult_mat2x4
        module procedure matrixCompMult_dmat2x4

        module procedure matrixCompMult_mat3x2
        module procedure matrixCompMult_dmat3x2

        module procedure matrixCompMult_mat3x3
        module procedure matrixCompMult_dmat3x3

        module procedure matrixCompMult_mat3x4
        module procedure matrixCompMult_dmat3x4

        module procedure matrixCompMult_mat4x2
        module procedure matrixCompMult_dmat4x2

        module procedure matrixCompMult_mat4x3
        module procedure matrixCompMult_dmat4x3

        module procedure matrixCompMult_mat4x4
        module procedure matrixCompMult_dmat4x4
    end interface matrixCompMult

    public :: outerProduct
    interface outerProduct
        module procedure outerProduct_vec2_vec2
        module procedure outerProduct_vec2_vec3
        module procedure outerProduct_vec2_vec4
        module procedure outerProduct_dvec2_dvec2
        module procedure outerProduct_dvec2_dvec3
        module procedure outerProduct_dvec2_dvec4

        module procedure outerProduct_vec3_vec2
        module procedure outerProduct_vec3_vec3
        module procedure outerProduct_vec3_vec4
        module procedure outerProduct_dvec3_dvec2
        module procedure outerProduct_dvec3_dvec3
        module procedure outerProduct_dvec3_dvec4

        module procedure outerProduct_vec4_vec2
        module procedure outerProduct_vec4_vec3
        module procedure outerProduct_vec4_vec4
        module procedure outerProduct_dvec4_dvec2
        module procedure outerProduct_dvec4_dvec3
        module procedure outerProduct_dvec4_dvec4
    end interface outerProduct

    public :: transpose
    interface transpose
        module procedure transpose_mat2x2
        module procedure transpose_dmat2x2

        module procedure transpose_mat2x3
        module procedure transpose_dmat2x3

        module procedure transpose_mat2x4
        module procedure transpose_dmat2x4

        module procedure transpose_mat3x2
        module procedure transpose_dmat3x2

        module procedure transpose_mat3x3
        module procedure transpose_dmat3x3

        module procedure transpose_mat3x4
        module procedure transpose_dmat3x4

        module procedure transpose_mat4x2
        module procedure transpose_dmat4x2

        module procedure transpose_mat4x3
        module procedure transpose_dmat4x3

        module procedure transpose_mat4x4
        module procedure transpose_dmat4x4
    end interface transpose

    public :: determinant
    interface determinant
        module procedure determinant_mat2x2
        module procedure determinant_dmat2x2

        module procedure determinant_mat3x3
        module procedure determinant_dmat3x3

        module procedure determinant_mat4x4
        module procedure determinant_dmat4x4
    end interface determinant

    public :: inverse
    interface inverse
        module procedure inverse_mat2x2
        module procedure inverse_dmat2x2

        module procedure inverse_mat3x3
        module procedure inverse_dmat3x3

        module procedure inverse_mat4x4
        module procedure inverse_dmat4x4
    end interface inverse

    public :: ortho
    interface ortho
        module procedure ortho_mat
        module procedure ortho_dmat
        module procedure ortho_full_mat
        module procedure ortho_full_dmat
    end interface ortho

    public :: orthoLH_ZO
    interface orthoLH_ZO
        module procedure orthoLH_ZO_mat
        module procedure orthoLH_ZO_dmat
    end interface orthoLH_ZO

    public :: orthoLH_NO
    interface orthoLH_NO
        module procedure orthoLH_NO_mat
        module procedure orthoLH_NO_dmat
    end interface orthoLH_NO

    public :: orthoRH_ZO
    interface orthoRH_ZO
        module procedure orthoRH_ZO_mat
        module procedure orthoRH_ZO_dmat
    end interface orthoRH_ZO

    public :: orthoRH_NO
    interface orthoRH_NO
        module procedure orthoRH_NO_mat
        module procedure orthoRH_NO_dmat
    end interface orthoRH_NO

    public :: frustum
    interface frustum
        module procedure frustum_full_mat
        module procedure frustum_full_dmat
    end interface frustum

    public :: frustumLH_ZO
    interface frustumLH_ZO
        module procedure frustumLH_ZO_mat
        module procedure frustumLH_ZO_dmat
    end interface frustumLH_ZO

    public :: frustumLH_NO
    interface frustumLH_NO
        module procedure frustumLH_NO_mat
        module procedure frustumLH_NO_dmat
    end interface frustumLH_NO

    public :: frustumRH_ZO
    interface frustumRH_ZO
        module procedure frustumRH_ZO_mat
        module procedure frustumRH_ZO_dmat
    end interface frustumRH_ZO

    public :: frustumRH_NO
    interface frustumRH_NO
        module procedure frustumRH_NO_mat
        module procedure frustumRH_NO_dmat
    end interface frustumRH_NO

    public :: perspective
    interface perspective
        module procedure perspective_full_mat
        module procedure perspective_full_dmat
        module procedure perspectiveFov_full_mat
        module procedure perspectiveFov_full_dmat
    end interface perspective

    public :: perspectiveLH_ZO
    interface perspectiveLH_ZO
        module procedure perspectiveLH_ZO_mat
        module procedure perspectiveLH_ZO_dmat
        module procedure perspectiveFovLH_ZO_mat
        module procedure perspectiveFovLH_ZO_dmat
    end interface perspectiveLH_ZO

    public :: perspectiveLH_NO
    interface perspectiveLH_NO
        module procedure perspectiveLH_NO_mat
        module procedure perspectiveLH_NO_dmat
        module procedure perspectiveFovLH_NO_mat
        module procedure perspectiveFovLH_NO_dmat
    end interface perspectiveLH_NO

    public :: perspectiveRH_ZO
    interface perspectiveRH_ZO
        module procedure perspectiveRH_ZO_mat
        module procedure perspectiveRH_ZO_dmat
        module procedure perspectiveFovRH_ZO_mat
        module procedure perspectiveFovRH_ZO_dmat
    end interface perspectiveRH_ZO

    public :: perspectiveRH_NO
    interface perspectiveRH_NO
        module procedure perspectiveRH_NO_mat
        module procedure perspectiveRH_NO_dmat
        module procedure perspectiveFovRH_NO_mat
        module procedure perspectiveFovRH_NO_dmat
    end interface perspectiveRH_NO

    public :: infinitePerspective
    interface infinitePerspective
        module procedure infinitePerspective_full_mat
        module procedure infinitePerspective_full_dmat
    end interface infinitePerspective

    public :: infinitePerspectiveLH_ZO
    interface infinitePerspectiveLH_ZO
        module procedure infinitePerspectiveLH_ZO_mat
        module procedure infinitePerspectiveLH_ZO_dmat
    end interface infinitePerspectiveLH_ZO

    public :: infinitePerspectiveLH_NO
    interface infinitePerspectiveLH_NO
        module procedure infinitePerspectiveLH_NO_mat
        module procedure infinitePerspectiveLH_NO_dmat
    end interface infinitePerspectiveLH_NO

    public :: infinitePerspectiveRH_ZO
    interface infinitePerspectiveRH_ZO
        module procedure infinitePerspectiveRH_ZO_mat
        module procedure infinitePerspectiveRH_ZO_dmat
    end interface infinitePerspectiveRH_ZO

    public :: infinitePerspectiveRH_NO
    interface infinitePerspectiveRH_NO
        module procedure infinitePerspectiveRH_NO_mat
        module procedure infinitePerspectiveRH_NO_dmat
    end interface infinitePerspectiveRH_NO

    public :: tweakedInfinitePerspective
    interface tweakedInfinitePerspective
        module procedure tweakedInfinitePerspective_mat
        module procedure tweakedInfinitePerspective_dmat
        module procedure tweakedInfinitePerspective_eps_mat
        module procedure tweakedInfinitePerspective_eps_dmat
    end interface tweakedInfinitePerspective

    public :: projectNO
    interface projectNO
        module procedure projectNO_mat
        module procedure projectNO_dmat
    end interface projectNO

    public :: projectZO
    interface projectZO
        module procedure projectZO_mat
        module procedure projectZO_dmat
    end interface projectZO

    public :: project
    interface project
        module procedure project_mat
        module procedure project_dmat
    end interface project

    public :: unprojectNO
    interface unprojectNO
        module procedure unprojectNO_mat
        module procedure unprojectNO_dmat
    end interface unprojectNO

    public :: unprojectZO
    interface unprojectZO
        module procedure unprojectZO_mat
        module procedure unprojectZO_dmat
    end interface unprojectZO

    public :: unproject
    interface unproject
        module procedure unproject_mat
        module procedure unproject_dmat
    end interface unproject

    public :: translate
    interface translate
        module procedure translate_mat
        module procedure translate_dmat
    end interface translate

    public :: rotate
    interface rotate
        module procedure rotate_mat
        module procedure rotate_dmat
    end interface rotate

    public :: scale
    interface scale
        module procedure scale_mat
        module procedure scale_dmat
    end interface scale

    public :: shear
    interface shear
        module procedure shear_mat
        module procedure shear_dmat
    end interface shear

    public :: lookAtRH
    interface lookAtRH
        module procedure lookAtRH_mat
        module procedure lookAtRH_dmat
    end interface lookAtRH

    public :: lookAtLH
    interface lookAtLH
        module procedure lookAtLH_mat
        module procedure lookAtLH_dmat
    end interface lookAtLH

    public :: lookAt
    interface lookAt
        module procedure lookAt_mat
        module procedure lookAt_dmat
    end interface lookAt

    contains

#undef MATRIX_TYPE_NCOL
#undef MATRIX_TYPE_NROW
#define MATRIX_TYPE_NCOL 2
#define MATRIX_TYPE_NROW 2
#include "detail/mat_impl.inc"

#undef MATRIX_TYPE_NCOL
#undef MATRIX_TYPE_NROW
#define MATRIX_TYPE_NCOL 2
#define MATRIX_TYPE_NROW 3
#include "detail/mat_impl.inc"

#undef MATRIX_TYPE_NCOL
#undef MATRIX_TYPE_NROW
#define MATRIX_TYPE_NCOL 2
#define MATRIX_TYPE_NROW 4
#include "detail/mat_impl.inc"

#undef MATRIX_TYPE_NCOL
#undef MATRIX_TYPE_NROW
#define MATRIX_TYPE_NCOL 3
#define MATRIX_TYPE_NROW 2
#include "detail/mat_impl.inc"

#undef MATRIX_TYPE_NCOL
#undef MATRIX_TYPE_NROW
#define MATRIX_TYPE_NCOL 3
#define MATRIX_TYPE_NROW 3
#include "detail/mat_impl.inc"

#undef MATRIX_TYPE_NCOL
#undef MATRIX_TYPE_NROW
#define MATRIX_TYPE_NCOL 3
#define MATRIX_TYPE_NROW 4
#include "detail/mat_impl.inc"

#undef MATRIX_TYPE_NCOL
#undef MATRIX_TYPE_NROW
#define MATRIX_TYPE_NCOL 4
#define MATRIX_TYPE_NROW 2
#include "detail/mat_impl.inc"

#undef MATRIX_TYPE_NCOL
#undef MATRIX_TYPE_NROW
#define MATRIX_TYPE_NCOL 4
#define MATRIX_TYPE_NROW 3
#include "detail/mat_impl.inc"

#undef MATRIX_TYPE_NCOL
#undef MATRIX_TYPE_NROW
#define MATRIX_TYPE_NCOL 4
#define MATRIX_TYPE_NROW 4
#include "detail/mat_impl.inc"

#undef VECTOR_TYPE_NAME
#undef MATRIX_TYPE_NAME
#undef SCALAR_TYPE_KIND
#define VECTOR_TYPE_NAME vec
#define MATRIX_TYPE_NAME mat
#define SCALAR_TYPE_KIND real32
#include "detail/mat_square.inc"
#include "detail/mat_clip_space.inc"
#include "detail/mat_projection.inc"
#include "detail/mat_transform.inc"

#undef VECTOR_TYPE_NAME
#undef MATRIX_TYPE_NAME
#undef SCALAR_TYPE_KIND
#define VECTOR_TYPE_NAME dvec
#define MATRIX_TYPE_NAME dmat
#define SCALAR_TYPE_KIND real64
#include "detail/mat_square.inc"
#include "detail/mat_clip_space.inc"
#include "detail/mat_projection.inc"
#include "detail/mat_transform.inc"

end module glmff_mat

