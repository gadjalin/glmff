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

    use glmff_vec2, only: vec2, dvec2
    use glmff_vec3, only: vec3, dvec3
    use glmff_vec4, only: vec4, dvec4

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
        module procedure add_mat2x2
        module procedure add_dmat2x2
        module procedure add_mat2x2_scalar
        module procedure add_dmat2x2_scalar

        module procedure add_mat2x3
        module procedure add_dmat2x3
        module procedure add_mat2x3_scalar
        module procedure add_dmat2x3_scalar

        module procedure add_mat2x4
        module procedure add_dmat2x4
        module procedure add_mat2x4_scalar
        module procedure add_dmat2x4_scalar

        module procedure add_mat3x2
        module procedure add_dmat3x2
        module procedure add_mat3x2_scalar
        module procedure add_dmat3x2_scalar

        module procedure add_mat3x3
        module procedure add_dmat3x3
        module procedure add_mat3x3_scalar
        module procedure add_dmat3x3_scalar

        module procedure add_mat3x4
        module procedure add_dmat3x4
        module procedure add_mat3x4_scalar
        module procedure add_dmat3x4_scalar

        module procedure add_mat4x2
        module procedure add_dmat4x2
        module procedure add_mat4x2_scalar
        module procedure add_dmat4x2_scalar

        module procedure add_mat4x3
        module procedure add_dmat4x3
        module procedure add_mat4x3_scalar
        module procedure add_dmat4x3_scalar

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

#undef MATRIX_TYPE_NAME
#undef SCALAR_TYPE_KIND
#define MATRIX_TYPE_NAME mat
#define SCALAR_TYPE_KIND real32
#include "detail/mat_square.inc"

#undef MATRIX_TYPE_NAME
#undef SCALAR_TYPE_KIND
#define MATRIX_TYPE_NAME dmat
#define SCALAR_TYPE_KIND real64
#include "detail/mat_square.inc"

end module glmff_mat

