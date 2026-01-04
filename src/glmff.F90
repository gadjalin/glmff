module glmff

    use glmff_vec
    use glmff_mat

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

    ! -------------
    ! --- Operators
    ! -------------

    public :: operator(+), operator(-), operator(*), operator(/)
    public :: operator(.dot.), operator(.cross.)
    public :: operator(==), operator(/=)
    public :: dot, cross, length, distance, normalize, faceforward, reflect, refract
    public :: any, all, operator(.not.)
    public :: lessThan, lessThanEqual, greaterThan, greaterThanEqual, equal, notEqual
    public :: min, max, abs, sign, mod
    public :: floor, ceil, fract, trunc, round, clamp
    public :: mix, step, smoothstep, fma
    public :: matrixCompMult, outerProduct, transpose, determinant, inverse

end module glmff

