module glmff_scalar
    use, intrinsic :: iso_fortran_env, only: int32, int64, real32, real64

#include "glmff_macros.h"

    implicit none
    private

    public :: length
    interface length
        module procedure length_real32
        module procedure length_real64
    end interface length

    public :: distance
    interface distance
        module procedure distance_real32
        module procedure distance_real64
    end interface distance

    public :: operator(.dot.)
    interface operator(.dot.)
        module procedure dot_real32
        module procedure dot_real64
    end interface operator(.dot.)

    public :: dot
    interface dot
        module procedure dot_real32
        module procedure dot_real64
    end interface dot

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
    end interface clamp

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

contains

#include "detail/scalar_impl.inc"

end module glmff_scalar

