module glmff_scalar
    use, intrinsic :: iso_fortran_env, only: int32, int64, real32, real64

#include "glmff_macros.h"

    implicit none
    private

    public :: distance
    interface distance
        module procedure distance_real32
        module procedure distance_real64
    end interface distance

    public :: faceforward
    interface faceforward
        module procedure faceforward_real32
        module procedure faceforward_real64
    end interface faceforward

    public :: reflect
    interface reflect
        module procedure reflect_real32
        module procedure reflect_real64
    end interface reflect

    public :: refract
    interface refract
        module procedure refract_real32
        module procedure refract_real64
    end interface refract

    ! -------------
    ! --- Utilities
    ! -------------

    public :: fmin
    interface fmin
        module procedure fmin2_real32
        module procedure fmin3_real32
        module procedure fmin4_real32
        module procedure fmin2_real64
        module procedure fmin3_real64
        module procedure fmin4_real64
    end interface fmin

    public :: fmax
    interface fmax
        module procedure fmax2_real32
        module procedure fmax3_real32
        module procedure fmax4_real32
        module procedure fmax2_real64
        module procedure fmax3_real64
        module procedure fmax4_real64
    end interface fmax

    public :: sign
    interface sign
        module procedure sign_int32
        module procedure sign_int64
        module procedure sign_real32
        module procedure sign_real64
    end interface sign

    public :: clamp
    interface clamp
        module procedure clamp_int32
        module procedure clamp_int64
        module procedure clamp_real32
        module procedure clamp_real64
        module procedure clamp_tex_real32
        module procedure clamp_tex_real64
    end interface clamp

    public :: fclamp
    interface fclamp
        module procedure fclamp_real32
        module procedure fclamp_real64
    end interface fclamp

    public :: repeat
    interface repeat
        module procedure repeat_tex_real32
        module procedure repeat_tex_real64
    end interface repeat

    public :: mirrorClamp
    interface mirrorClamp
        module procedure mirrorClamp_tex_real32
        module procedure mirrorClamp_tex_real64
    end interface mirrorClamp

    public :: mirrorRepeat
    interface mirrorRepeat
        module procedure mirrorRepeat_tex_real32
        module procedure mirrorRepeat_tex_real64
    end interface mirrorRepeat

    public :: mix
    interface mix
        module procedure mix_int32_bool
        module procedure mix_int64_bool
        module procedure mix_real32
        module procedure mix_real64
        module procedure mix_real32_bool
        module procedure mix_real64_bool
    end interface mix

    public :: fma
    interface fma
        module procedure fma_int32
        module procedure fma_int64
        module procedure fma_real32
        module procedure fma_real64
    end interface fma

    public :: step
    interface step
        module procedure step_real32
        module procedure step_real64
    end interface step

    public :: smoothstep
    interface smoothstep
        module procedure smoothstep_real32
        module procedure smoothstep_real64
    end interface smoothstep

    public :: fract
    interface fract
        module procedure fract_real32
        module procedure fract_real64
    end interface fract

    public :: floatBitsToInt
    interface floatBitsToInt
        module procedure floatBitsToInt_real32
        module procedure floatBitsToInt_real64
    end interface floatBitsToInt

    public :: intBitsToFloat
    interface intBitsToFloat
        module procedure intBitsToFloat_int32
        module procedure intBitsToFloat_int64
    end interface intBitsToFloat

    public :: isinf
    interface isinf
        module procedure isinf_real32
        module procedure isinf_real64
    end interface isinf

    public :: isnan
    interface isnan
        module procedure isnan_real32
        module procedure isnan_real64
    end interface isnan

    public :: frexp
    interface frexp
        module procedure frexp_real32
        module procedure frexp_real64
    end interface frexp

    public :: ldexp
    interface ldexp
        module procedure ldexp_real32
        module procedure ldexp_real64
    end interface ldexp

    ! ----------------
    ! --- Trigonometry
    ! ----------------

    public :: radians
    interface radians
        module procedure radians_real32
        module procedure radians_real64
    end interface radians

    public :: degrees
    interface degrees
        module procedure degrees_real32
        module procedure degrees_real64
    end interface degrees

    public :: sec
    interface sec
        module procedure sec_real32
        module procedure sec_real64
    end interface sec

    public :: csc
    interface csc
        module procedure csc_real32
        module procedure csc_real64
    end interface csc

    public :: cot
    interface cot
        module procedure cot_real32
        module procedure cot_real64
    end interface cot

    public :: asec
    interface asec
        module procedure asec_real32
        module procedure asec_real64
    end interface asec

    public :: acsc
    interface acsc
        module procedure acsc_real32
        module procedure acsc_real64
    end interface acsc

    public :: acot
    interface acot
        module procedure acot_real32
        module procedure acot_real64
    end interface acot

    public :: sech
    interface sech
        module procedure sech_real32
        module procedure sech_real64
    end interface sech

    public :: csch
    interface csch
        module procedure csch_real32
        module procedure csch_real64
    end interface csch

    public :: coth
    interface coth
        module procedure coth_real32
        module procedure coth_real64
    end interface coth

    public :: asech
    interface asech
        module procedure asech_real32
        module procedure asech_real64
    end interface asech

    public :: acsch
    interface acsch
        module procedure acsch_real32
        module procedure acsch_real64
    end interface acsch

    public :: acoth
    interface acoth
        module procedure acoth_real32
        module procedure acoth_real64
    end interface acoth

contains

#include "detail/scalar_impl.inc"

end module glmff_scalar

