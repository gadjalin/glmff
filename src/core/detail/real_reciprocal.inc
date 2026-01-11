elemental function CAT(sec_,SCALAR_TYPE_KIND)(x) result(res)
    implicit none
    SCALAR_TYPE_FULL, intent(in) :: x
    SCALAR_TYPE_FULL :: res

    SCALAR_TYPE_FULL, parameter :: one = CAT(1_,SCALAR_TYPE_KIND)

    res = one / cos(x)
end function

elemental function CAT(csc_,SCALAR_TYPE_KIND)(x) result(res)
    implicit none
    SCALAR_TYPE_FULL, intent(in) :: x
    SCALAR_TYPE_FULL :: res

    SCALAR_TYPE_FULL, parameter :: one = CAT(1_,SCALAR_TYPE_KIND)

    res = one / sin(x)
end function

elemental function CAT(cot_,SCALAR_TYPE_KIND)(x) result(res)
    implicit none
    SCALAR_TYPE_FULL, intent(in) :: x
    SCALAR_TYPE_FULL :: res

    SCALAR_TYPE_FULL, parameter :: half_pi = acos(CAT(-1.0_,SCALAR_TYPE_KIND)) / CAT(2_,SCALAR_TYPE_KIND)

    res = tan(half_pi - x)
end function

elemental function CAT(asec_,SCALAR_TYPE_KIND)(x) result(res)
    implicit none
    SCALAR_TYPE_FULL, intent(in) :: x
    SCALAR_TYPE_FULL :: res

    SCALAR_TYPE_FULL, parameter :: one = CAT(1_,SCALAR_TYPE_KIND)

    res = acos(one / x)
end function

elemental function CAT(acsc_,SCALAR_TYPE_KIND)(x) result(res)
    implicit none
    SCALAR_TYPE_FULL, intent(in) :: x
    SCALAR_TYPE_FULL :: res

    SCALAR_TYPE_FULL, parameter :: one = CAT(1_,SCALAR_TYPE_KIND)

    res = asin(one / x)
end function

elemental function CAT(acot_,SCALAR_TYPE_KIND)(x) result(res)
    implicit none
    SCALAR_TYPE_FULL, intent(in) :: x
    SCALAR_TYPE_FULL :: res

    SCALAR_TYPE_FULL, parameter :: half_pi = acos(CAT(-1.0_,SCALAR_TYPE_KIND)) / CAT(2_,SCALAR_TYPE_KIND)

    res = half_pi - atan(x)
end function

elemental function CAT(sech_,SCALAR_TYPE_KIND)(x) result(res)
    implicit none
    SCALAR_TYPE_FULL, intent(in) :: x
    SCALAR_TYPE_FULL :: res

    SCALAR_TYPE_FULL, parameter :: one = CAT(1_,SCALAR_TYPE_KIND)

    res = one / cosh(x)
end function

elemental function CAT(csch_,SCALAR_TYPE_KIND)(x) result(res)
    implicit none
    SCALAR_TYPE_FULL, intent(in) :: x
    SCALAR_TYPE_FULL :: res

    SCALAR_TYPE_FULL, parameter :: one = CAT(1_,SCALAR_TYPE_KIND)

    res = one / sinh(x)
end function

elemental function CAT(coth_,SCALAR_TYPE_KIND)(x) result(res)
    implicit none
    SCALAR_TYPE_FULL, intent(in) :: x
    SCALAR_TYPE_FULL :: res

    res = cosh(x) / sinh(x)
end function

elemental function CAT(asech_,SCALAR_TYPE_KIND)(x) result(res)
    implicit none
    SCALAR_TYPE_FULL, intent(in) :: x
    SCALAR_TYPE_FULL :: res

    SCALAR_TYPE_FULL, parameter :: one = CAT(1_,SCALAR_TYPE_KIND)

    res = acosh(one / x)
end function

elemental function CAT(acsch_,SCALAR_TYPE_KIND)(x) result(res)
    implicit none
    SCALAR_TYPE_FULL, intent(in) :: x
    SCALAR_TYPE_FULL :: res

    SCALAR_TYPE_FULL, parameter :: one = CAT(1_,SCALAR_TYPE_KIND)

    res = asinh(one / x)
end function

elemental function CAT(acoth_,SCALAR_TYPE_KIND)(x) result(res)
    implicit none
    SCALAR_TYPE_FULL, intent(in) :: x
    SCALAR_TYPE_FULL :: res

    SCALAR_TYPE_FULL, parameter :: one = CAT(1_,SCALAR_TYPE_KIND)

    res = atanh(one / x)
end function

