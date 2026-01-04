pure function CAT(radians_,VECTOR_TYPE_FULL)(deg) result(rad)
    implicit none
    type(VECTOR_TYPE_FULL), intent(in) :: deg
    type(VECTOR_TYPE_FULL) :: rad
    SCALAR_TYPE_FULL, parameter :: pi = acos(CAT(-1.0_,SCALAR_TYPE_KIND))
    SCALAR_TYPE_FULL, parameter :: pi_over_180 = pi / CAT(180_,SCALAR_TYPE_KIND)

    rad%data = deg%data * pi_over_180
end function

pure function CAT(degrees_,VECTOR_TYPE_FULL)(rad) result(deg)
    implicit none
    type(VECTOR_TYPE_FULL), intent(in) :: rad
    type(VECTOR_TYPE_FULL) :: deg
    SCALAR_TYPE_FULL, parameter :: pi = acos(CAT(-1.0_,SCALAR_TYPE_KIND))
    SCALAR_TYPE_FULL, parameter :: ipi_over_180 = CAT(180_,SCALAR_TYPE_KIND) / pi

    deg%data = rad%data * ipi_over_180
end function

pure function CAT(sin_,VECTOR_TYPE_FULL)(x) result(res)
    implicit none
    type(VECTOR_TYPE_FULL), intent(in) :: x
    type(VECTOR_TYPE_FULL) :: res

    res%data = sin(x%data)
end function

pure function CAT(cos_,VECTOR_TYPE_FULL)(x) result(res)
    implicit none
    type(VECTOR_TYPE_FULL), intent(in) :: x
    type(VECTOR_TYPE_FULL) :: res

    res%data = cos(x%data)
end function

pure function CAT(tan_,VECTOR_TYPE_FULL)(x) result(res)
    implicit none
    type(VECTOR_TYPE_FULL), intent(in) :: x
    type(VECTOR_TYPE_FULL) :: res

    res%data = tan(x%data)
end function

pure function CAT(asin_,VECTOR_TYPE_FULL)(x) result(res)
    implicit none
    type(VECTOR_TYPE_FULL), intent(in) :: x
    type(VECTOR_TYPE_FULL) :: res

    res%data = asin(x%data)
end function

pure function CAT(acos_,VECTOR_TYPE_FULL)(x) result(res)
    implicit none
    type(VECTOR_TYPE_FULL), intent(in) :: x
    type(VECTOR_TYPE_FULL) :: res

    res%data = acos(x%data)
end function

pure function CAT(atan_,VECTOR_TYPE_FULL)(x) result(res)
    implicit none
    type(VECTOR_TYPE_FULL), intent(in) :: x
    type(VECTOR_TYPE_FULL) :: res

    res%data = atan(x%data)
end function

pure function CAT(atan2_,VECTOR_TYPE_FULL)(x, y) result(res)
    implicit none
    type(VECTOR_TYPE_FULL), intent(in) :: x, y
    type(VECTOR_TYPE_FULL) :: res

    res%data = atan2(x%data, y%data)
end function

pure function CAT(sinh_,VECTOR_TYPE_FULL)(x) result(res)
    implicit none
    type(VECTOR_TYPE_FULL), intent(in) :: x
    type(VECTOR_TYPE_FULL) :: res

    res%data = sinh(x%data)
end function

pure function CAT(cosh_,VECTOR_TYPE_FULL)(x) result(res)
    implicit none
    type(VECTOR_TYPE_FULL), intent(in) :: x
    type(VECTOR_TYPE_FULL) :: res

    res%data = cosh(x%data)
end function

pure function CAT(tanh_,VECTOR_TYPE_FULL)(x) result(res)
    implicit none
    type(VECTOR_TYPE_FULL), intent(in) :: x
    type(VECTOR_TYPE_FULL) :: res

    res%data = tanh(x%data)
end function

pure function CAT(asinh_,VECTOR_TYPE_FULL)(x) result(res)
    implicit none
    type(VECTOR_TYPE_FULL), intent(in) :: x
    type(VECTOR_TYPE_FULL) :: res

    res%data = asinh(x%data)
end function

pure function CAT(acosh_,VECTOR_TYPE_FULL)(x) result(res)
    implicit none
    type(VECTOR_TYPE_FULL), intent(in) :: x
    type(VECTOR_TYPE_FULL) :: res

    res%data = acosh(x%data)
end function

pure function CAT(atanh_,VECTOR_TYPE_FULL)(x) result(res)
    implicit none
    type(VECTOR_TYPE_FULL), intent(in) :: x
    type(VECTOR_TYPE_FULL) :: res

    res%data = atanh(x%data)
end function

