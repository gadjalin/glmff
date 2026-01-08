module glmff

    use glmff_vec
    use glmff_mat
    use glmff_quat

    implicit none
    private

    ! -----------------------------
    ! --- Data types & Constructors
    ! -----------------------------

    public :: bvec2, ivec2, vec2, dvec2
    public :: bvec3, ivec3, vec3, dvec3
    public :: bvec4, ivec4, vec4, dvec4

    public :: mat2x2, dmat2x2, mat2 , dmat2
    public :: mat2x3, dmat2x3
    public :: mat2x4, dmat2x4
    public :: mat3x2, dmat3x2
    public :: mat3x3, dmat3x3, mat3, dmat3
    public :: mat3x4, dmat3x4
    public :: mat4x2, dmat4x2
    public :: mat4x3, dmat4x3
    public :: mat4x4, dmat4x4, mat4, dmat4

    public :: quat, dquat

    ! --------------
    ! --- Conversion
    ! --------------

    public :: to_vec2, to_vec3, to_vec4
    public :: to_mat2, to_mat2x3, to_mat2x4
    public :: to_mat3, to_mat3x2, to_mat3x4
    public :: to_mat4, to_mat4x2, to_mat4x3

    ! -------------------------
    ! --- Operators & Functions
    ! -------------------------

    public :: operator(+), operator(-), operator(*), operator(/), operator(**)
    public :: operator(.dot.), dot, operator(.cross.), cross
    public :: operator(==), operator(/=), operator(.not.), any, all
    public :: lessThan, lessThanEqual, greaterThan, greaterThanEqual, equal, notEqual

    public :: min, max, abs, sign, mod, exp, log, sqrt
    public :: floor, ceil, fract, trunc, round, clamp

    public :: length, distance, normalize, faceforward, reflect, refract
    public :: mix, step, smoothstep, fma
    public :: angle, axis, angleAxis, lerp, slerp, conjugate

    public :: matrixCompMult, outerProduct, transpose, determinant, inverse
    public :: translate, rotate, scale, shear
    public :: project, projectZO, projectNO, unproject, unprojectZO, unprojectNO, pickMatrix
    public :: ortho, orthoRH_NO, orthoRH_ZO, orthoLH_NO, orthoLH_ZO
    public :: frustum, frustumRH_NO, frustumRH_ZO, frustumLH_NO, frustumLH_ZO
    public :: perspective, perspectiveRH_NO, perspectiveRH_ZO, perspectiveLH_NO, perspectiveLH_ZO
    public :: infinitePerspective, infinitePerspectiveRH_NO, infinitePerspectiveRH_ZO, &
              infinitePerspectiveLH_NO, infinitePerspectiveLH_ZO, tweakedInfinitePerspective
    public :: lookAt, lookAtLH, lookAtRH

end module glmff

