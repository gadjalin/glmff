elemental function CAT(sec_,VECTOR_TYPE_FULL)(x) result(res)
    implicit none
    type(VECTOR_TYPE_FULL), intent(in) :: x
    type(VECTOR_TYPE_FULL) :: res

    SCALAR_TYPE_FULL, parameter :: one = CAT(1_,SCALAR_TYPE_KIND)

    res%data = one / cos(x%data)
end function

elemental function CAT(csc_,VECTOR_TYPE_FULL)(x) result(res)
    implicit none
    type(VECTOR_TYPE_FULL), intent(in) :: x
    type(VECTOR_TYPE_FULL) :: res

    SCALAR_TYPE_FULL, parameter :: one = CAT(1_,SCALAR_TYPE_KIND)

    res%data = one / sin(x%data)
end function

elemental function CAT(cot_,VECTOR_TYPE_FULL)(x) result(res)
    implicit none
    type(VECTOR_TYPE_FULL), intent(in) :: x
    type(VECTOR_TYPE_FULL) :: res

    SCALAR_TYPE_FULL, parameter :: half_pi = acos(CAT(-1.0_,SCALAR_TYPE_KIND)) / CAT(2_,SCALAR_TYPE_KIND)

    res%data = tan(half_pi - x%data)
end function

elemental function CAT(asec_,VECTOR_TYPE_FULL)(x) result(res)
    implicit none
    type(VECTOR_TYPE_FULL), intent(in) :: x
    type(VECTOR_TYPE_FULL) :: res

    SCALAR_TYPE_FULL, parameter :: one = CAT(1_,SCALAR_TYPE_KIND)

    res%data = acos(one / x%data)
end function

elemental function CAT(acsc_,VECTOR_TYPE_FULL)(x) result(res)
    implicit none
    type(VECTOR_TYPE_FULL), intent(in) :: x
    type(VECTOR_TYPE_FULL) :: res

    SCALAR_TYPE_FULL, parameter :: one = CAT(1_,SCALAR_TYPE_KIND)

    res%data = asin(one / x%data)
end function

elemental function CAT(acot_,VECTOR_TYPE_FULL)(x) result(res)
    implicit none
    type(VECTOR_TYPE_FULL), intent(in) :: x
    type(VECTOR_TYPE_FULL) :: res

    SCALAR_TYPE_FULL, parameter :: half_pi = acos(CAT(-1.0_,SCALAR_TYPE_KIND)) / CAT(2_,SCALAR_TYPE_KIND)

    res%data = half_pi - atan(x%data)
end function

elemental function CAT(sech_,VECTOR_TYPE_FULL)(x) result(res)
    implicit none
    type(VECTOR_TYPE_FULL), intent(in) :: x
    type(VECTOR_TYPE_FULL) :: res

    SCALAR_TYPE_FULL, parameter :: one = CAT(1_,SCALAR_TYPE_KIND)

    res%data = one / cosh(x%data)
end function

elemental function CAT(csch_,VECTOR_TYPE_FULL)(x) result(res)
    implicit none
    type(VECTOR_TYPE_FULL), intent(in) :: x
    type(VECTOR_TYPE_FULL) :: res

    SCALAR_TYPE_FULL, parameter :: one = CAT(1_,SCALAR_TYPE_KIND)

    res%data = one / sinh(x%data)
end function

elemental function CAT(coth_,VECTOR_TYPE_FULL)(x) result(res)
    implicit none
    type(VECTOR_TYPE_FULL), intent(in) :: x
    type(VECTOR_TYPE_FULL) :: res

    res%data = cosh(x%data) / sinh(x%data)
end function

elemental function CAT(asech_,VECTOR_TYPE_FULL)(x) result(res)
    implicit none
    type(VECTOR_TYPE_FULL), intent(in) :: x
    type(VECTOR_TYPE_FULL) :: res

    SCALAR_TYPE_FULL, parameter :: one = CAT(1_,SCALAR_TYPE_KIND)

    res%data = acosh(one / x%data)
end function

elemental function CAT(acsch_,VECTOR_TYPE_FULL)(x) result(res)
    implicit none
    type(VECTOR_TYPE_FULL), intent(in) :: x
    type(VECTOR_TYPE_FULL) :: res

    SCALAR_TYPE_FULL, parameter :: one = CAT(1_,SCALAR_TYPE_KIND)

    res%data = asinh(one / x%data)
end function

elemental function CAT(acoth_,VECTOR_TYPE_FULL)(x) result(res)
    implicit none
    type(VECTOR_TYPE_FULL), intent(in) :: x
    type(VECTOR_TYPE_FULL) :: res

    SCALAR_TYPE_FULL, parameter :: one = CAT(1_,SCALAR_TYPE_KIND)

    res%data = atanh(one / x%data)
end function

