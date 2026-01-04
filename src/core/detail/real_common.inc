pure function CAT(mix_,SCALAR_TYPE_KIND)(x, y, a) result(res)
    implicit none
    SCALAR_TYPE_FULL, intent(in) :: x, y, a
    SCALAR_TYPE_FULL :: res

    SCALAR_TYPE_FULL, parameter :: one = CAT(1_,SCALAR_TYPE_KIND)

    res = x*(one - a) + y*a
end function

pure function CAT(step_,SCALAR_TYPE_KIND)(edge, x) result(res)
    implicit none
    SCALAR_TYPE_FULL, intent(in) :: edge, x
    SCALAR_TYPE_FULL :: res

    SCALAR_TYPE_FULL, parameter :: zero = CAT(0_,SCALAR_TYPE_KIND)
    SCALAR_TYPE_FULL, parameter :: one = CAT(1_,SCALAR_TYPE_KIND)

    res = merge(zero, one, x < edge)
end function

pure function CAT(smoothstep_,SCALAR_TYPE_KIND)(edge0, edge1, x) result(res)
    implicit none
    SCALAR_TYPE_FULL, intent(in) :: edge0, edge1, x
    SCALAR_TYPE_FULL :: res

    SCALAR_TYPE_FULL, parameter :: zero = CAT(0_,SCALAR_TYPE_KIND)
    SCALAR_TYPE_FULL, parameter :: one = CAT(1_,SCALAR_TYPE_KIND)
    SCALAR_TYPE_FULL, parameter :: two = CAT(2_,SCALAR_TYPE_KIND)
    SCALAR_TYPE_FULL, parameter :: three = CAT(3_,SCALAR_TYPE_KIND)
    SCALAR_TYPE_FULL :: t

    t = (x - edge0) / (edge1 - edge0)
    t = min(max(t, zero), one)
    res = t*t*(three - two*t)
end function

pure function CAT(fract_,SCALAR_TYPE_KIND)(x) result(res)
    implicit none
    SCALAR_TYPE_FULL, intent(in) :: x
    SCALAR_TYPE_FULL :: res

    res = x - floor(x, kind=SCALAR_TYPE_KIND)
end function

pure function CAT(floatBitsToInt_,SCALAR_TYPE_KIND)(v) result(res)
    implicit none
    SCALAR_TYPE_FULL, intent(in) :: v
    integer(kind=CAT(int,SCALAR_TYPE_BIT_SIZE)) :: res

    res = transfer(v, res)
end function

pure logical function CAT(isnan_,SCALAR_TYPE_KIND)(x)
    use, intrinsic :: ieee_arithmetic, only: ieee_is_nan

    implicit none
    SCALAR_TYPE_FULL, intent(in) :: x

    CAT(isnan_,SCALAR_TYPE_KIND) = ieee_is_nan(x)
end function

pure logical function CAT(isinf_,SCALAR_TYPE_KIND)(x)
    use, intrinsic :: ieee_arithmetic, only: ieee_is_finite, ieee_is_nan

    implicit none
    SCALAR_TYPE_FULL, intent(in) :: x

    CAT(isinf_,SCALAR_TYPE_KIND) = .not. ieee_is_finite(x) .and. .not. ieee_is_nan(x)
end function

pure subroutine CAT(frexp_,SCALAR_TYPE_KIND)(x, f, e)
    implicit none
    SCALAR_TYPE_FULL, intent(in) :: x
    SCALAR_TYPE_FULL, intent(out) :: f, e

    f = fraction(x)
    e = exponent(x)
end subroutine

pure function CAT(ldexp_,SCALAR_TYPE_KIND)(x, e) result(res)
    implicit none
    SCALAR_TYPE_FULL, intent(in) :: x
    integer(kind=CAT(int,SCALAR_TYPE_BIT_SIZE)), intent(in) :: e
    SCALAR_TYPE_FULL :: res

    res = scale(x, e)
end function

