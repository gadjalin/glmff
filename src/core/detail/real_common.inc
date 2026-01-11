elemental function CAT(fmin2_,SCALAR_TYPE_KIND)(a, b) result(res)
    implicit none
    SCALAR_TYPE_FULL, intent(in) :: a, b
    SCALAR_TYPE_FULL :: res

    if (isnan(a)) then
        res = b
    else if (isnan(b)) then
        res = a
    else
        res = min(a, b)
    end if
end function

elemental function CAT(fmin3_,SCALAR_TYPE_KIND)(a, b, c) result(res)
    implicit none
    SCALAR_TYPE_FULL, intent(in) :: a, b, c
    SCALAR_TYPE_FULL :: res

    if (isnan(a)) then
        res = fmin(b, c)
    else if (isnan(b)) then
        res = fmin(a, c)
    else if (isnan(c)) then
        res = min(a, b)
    else
        res = min(a, b, c)
    end if
end function

elemental function CAT(fmin4_,SCALAR_TYPE_KIND)(a, b, c, d) result(res)
    implicit none
    SCALAR_TYPE_FULL, intent(in) :: a, b, c, d
    SCALAR_TYPE_FULL :: res

    if (isnan(a)) then
        res = fmin(b, c, d)
    else if (isnan(b)) then
        res = min(a, fmin(c, d))
    else if (isnan(c)) then
        res = fmin(min(a, b), d)
    else if (isnan(d)) then
        res = min(a, b, c)
    else
        res = min(a, b, c, d)
    end if
end function

elemental function CAT(fmax2_,SCALAR_TYPE_KIND)(a, b) result(res)
    implicit none
    SCALAR_TYPE_FULL, intent(in) :: a, b
    SCALAR_TYPE_FULL :: res

    if (isnan(a)) then
        res = b
    else if (isnan(b)) then
        res = a
    else
        res = max(a, b)
    end if
end function

elemental function CAT(fmax3_,SCALAR_TYPE_KIND)(a, b, c) result(res)
    implicit none
    SCALAR_TYPE_FULL, intent(in) :: a, b, c
    SCALAR_TYPE_FULL :: res

    if (isnan(a)) then
        res = fmax(b, c)
    else if (isnan(b)) then
        res = fmax(a, c)
    else if (isnan(c)) then
        res = max(a, b)
    else
        res = max(a, b, c)
    end if
end function

elemental function CAT(fmax4_,SCALAR_TYPE_KIND)(a, b, c, d) result(res)
    implicit none
    SCALAR_TYPE_FULL, intent(in) :: a, b, c, d
    SCALAR_TYPE_FULL :: res

    if (isnan(a)) then
        res = fmax(b, c, d)
    else if (isnan(b)) then
        res = max(a, fmax(c, d))
    else if (isnan(c)) then
        res = fmax(max(a, b), d)
    else if (isnan(d)) then
        res = max(a, b, c)
    else
        res = max(a, b, c, d)
    end if
end function

elemental function CAT(fclamp_,SCALAR_TYPE_KIND)(x, min_val, max_val) result(res)
    implicit none
    SCALAR_TYPE_FULL, intent(in) :: x, min_val, max_val
    SCALAR_TYPE_FULL :: res

    res = fmin(fmax(x, min_val), max_val)
end function

elemental function CAT(clamp_tex_,SCALAR_TYPE_KIND)(coords) result(res)
    implicit none
    SCALAR_TYPE_FULL, intent(in) :: coords
    SCALAR_TYPE_FULL :: res

    SCALAR_TYPE_FULL, parameter :: zero = CAT(0_,SCALAR_TYPE_KIND)
    SCALAR_TYPE_FULL, parameter :: one = CAT(1_,SCALAR_TYPE_KIND)

    res = fmin(fmax(coords, zero), one)
end function

elemental function CAT(repeat_tex_,SCALAR_TYPE_KIND)(coords) result(res)
    implicit none
    SCALAR_TYPE_FULL, intent(in) :: coords
    SCALAR_TYPE_FULL :: res

    res = fract(coords)
end function

elemental function CAT(mirrorClamp_tex_,SCALAR_TYPE_KIND)(coords) result(res)
    implicit none
    SCALAR_TYPE_FULL, intent(in) :: coords
    SCALAR_TYPE_FULL :: res

    res = fract(abs(coords))
end function

elemental function CAT(mirrorRepeat_tex_,SCALAR_TYPE_KIND)(coords) result(res)
    implicit none
    SCALAR_TYPE_FULL, intent(in) :: coords
    SCALAR_TYPE_FULL :: res

    SCALAR_TYPE_FULL :: absx, clampx, floorx, restx, mirrorx
    SCALAR_TYPE_FULL, parameter :: one = CAT(1_,SCALAR_TYPE_KIND)
    SCALAR_TYPE_FULL, parameter :: two = CAT(2_,SCALAR_TYPE_KIND)

    absx = abs(coords)
    floorx = floor(absx)
    clampx = modulo(floorx, two)
    restx = absx - floorx
    mirrorx = clampx + restx

    res = mix(restx, one - restx, mirrorx >= one)
end function

elemental function CAT(mix_,SCALAR_TYPE_KIND)(x, y, a) result(res)
    implicit none
    SCALAR_TYPE_FULL, intent(in) :: x, y, a
    SCALAR_TYPE_FULL :: res

    SCALAR_TYPE_FULL, parameter :: one = CAT(1_,SCALAR_TYPE_KIND)

    res = x*(one - a) + y*a
end function

elemental function CAT(step_,SCALAR_TYPE_KIND)(edge, x) result(res)
    implicit none
    SCALAR_TYPE_FULL, intent(in) :: edge, x
    SCALAR_TYPE_FULL :: res

    SCALAR_TYPE_FULL, parameter :: zero = CAT(0_,SCALAR_TYPE_KIND)
    SCALAR_TYPE_FULL, parameter :: one = CAT(1_,SCALAR_TYPE_KIND)

    res = merge(zero, one, x < edge)
end function

elemental function CAT(smoothstep_,SCALAR_TYPE_KIND)(edge0, edge1, x) result(res)
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

elemental function CAT(fract_,SCALAR_TYPE_KIND)(x) result(res)
    implicit none
    SCALAR_TYPE_FULL, intent(in) :: x
    SCALAR_TYPE_FULL :: res

    res = x - floor(x, kind=SCALAR_TYPE_KIND)
end function

elemental function CAT(floatBitsToInt_,SCALAR_TYPE_KIND)(x) result(res)
    implicit none
    SCALAR_TYPE_FULL, intent(in) :: x
    integer(kind=CAT(int,SCALAR_TYPE_BIT_SIZE)) :: res

    res = transfer(x, res)
end function

elemental logical function CAT(isnan_,SCALAR_TYPE_KIND)(x)
    use, intrinsic :: ieee_arithmetic, only: ieee_is_nan

    implicit none
    SCALAR_TYPE_FULL, intent(in) :: x

    CAT(isnan_,SCALAR_TYPE_KIND) = ieee_is_nan(x)
end function

elemental logical function CAT(isinf_,SCALAR_TYPE_KIND)(x)
    use, intrinsic :: ieee_arithmetic, only: ieee_is_finite, ieee_is_nan

    implicit none
    SCALAR_TYPE_FULL, intent(in) :: x

    CAT(isinf_,SCALAR_TYPE_KIND) = .not. ieee_is_finite(x) .and. .not. ieee_is_nan(x)
end function

elemental subroutine CAT(frexp_,SCALAR_TYPE_KIND)(x, f, e)
    implicit none
    SCALAR_TYPE_FULL, intent(in) :: x
    SCALAR_TYPE_FULL, intent(out) :: f, e

    f = fraction(x)
    e = exponent(x)
end subroutine

elemental function CAT(ldexp_,SCALAR_TYPE_KIND)(x, e) result(res)
    implicit none
    SCALAR_TYPE_FULL, intent(in) :: x
    integer(kind=CAT(int,SCALAR_TYPE_BIT_SIZE)), intent(in) :: e
    SCALAR_TYPE_FULL :: res

    res = scale(x, e)
end function

