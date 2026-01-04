pure function CAT(floor_,VECTOR_TYPE_FULL)(x) result(res)
    implicit none
    type(VECTOR_TYPE_FULL), intent(in) :: x
    type(VECTOR_TYPE_FULL) :: res

    res%data = floor(x%data, kind=SCALAR_TYPE_KIND)
end function

pure function CAT(ceil_,VECTOR_TYPE_FULL)(x) result(res)
    implicit none
    type(VECTOR_TYPE_FULL), intent(in) :: x
    type(VECTOR_TYPE_FULL) :: res

    res%data = ceiling(x%data, kind=SCALAR_TYPE_KIND)
end function

pure function CAT(fract_,VECTOR_TYPE_FULL)(x) result(res)
    implicit none
    type(VECTOR_TYPE_FULL), intent(in) :: x
    type(VECTOR_TYPE_FULL) :: res

    res%data = x%data - floor(x%data, kind=SCALAR_TYPE_KIND)
end function

pure function CAT(trunc_,VECTOR_TYPE_FULL)(x) result(res)
    implicit none
    type(VECTOR_TYPE_FULL), intent(in) :: x
    type(VECTOR_TYPE_FULL) :: res

    res%data = aint(x%data, kind=SCALAR_TYPE_KIND)
end function

pure function CAT(round_,VECTOR_TYPE_FULL)(x) result(res)
    implicit none
    type(VECTOR_TYPE_FULL), intent(in) :: x
    type(VECTOR_TYPE_FULL) :: res

    res%data = nint(x%data, kind=SCALAR_TYPE_KIND)
end function

pure function CAT(mix_,VECTOR_TYPE_FULL)(x, y, a) result(res)
    implicit none
    type(VECTOR_TYPE_FULL), intent(in) :: x, y, a
    type(VECTOR_TYPE_FULL) :: res

    SCALAR_TYPE_FULL, parameter :: one = CAT(1_,SCALAR_TYPE_KIND)

    res%data = x%data*(one - a%data) + y%data*a%data
end function

pure function CAT3(mix_,VECTOR_TYPE_FULL,_scalar)(x, y, a) result(res)
    implicit none
    type(VECTOR_TYPE_FULL), intent(in) :: x, y
    SCALAR_TYPE_FULL, intent(in) :: a
    type(VECTOR_TYPE_FULL) :: res

    SCALAR_TYPE_FULL, parameter :: one = CAT(1_,SCALAR_TYPE_KIND)

    res%data = x%data*(one - a) + y%data*a
end function

pure function CAT(step_,VECTOR_TYPE_FULL)(edge, x) result(res)
    implicit none
    type(VECTOR_TYPE_FULL), intent(in) :: edge, x
    type(VECTOR_TYPE_FULL) :: res

    SCALAR_TYPE_FULL, parameter :: zero = CAT(0_,SCALAR_TYPE_KIND)
    SCALAR_TYPE_FULL, parameter :: one = CAT(1_,SCALAR_TYPE_KIND)

    res%data = merge(zero, one, x%data < edge%data)
end function

pure function CAT3(step_,VECTOR_TYPE_FULL,_scalar)(edge, x) result(res)
    implicit none
    SCALAR_TYPE_FULL, intent(in) :: edge
    type(VECTOR_TYPE_FULL), intent(in) :: x
    type(VECTOR_TYPE_FULL) :: res

    SCALAR_TYPE_FULL, parameter :: zero = CAT(0_,SCALAR_TYPE_KIND)
    SCALAR_TYPE_FULL, parameter :: one = CAT(1_,SCALAR_TYPE_KIND)

    res%data = merge(zero, one, x%data < edge)
end function

pure function CAT(smoothstep_,VECTOR_TYPE_FULL)(edge0, edge1, x) result(res)
    implicit none
    type(VECTOR_TYPE_FULL), intent(in) :: edge0, edge1, x
    type(VECTOR_TYPE_FULL) :: res

    SCALAR_TYPE_FULL, parameter :: zero = CAT(0_,SCALAR_TYPE_KIND)
    SCALAR_TYPE_FULL, parameter :: one = CAT(1_,SCALAR_TYPE_KIND)
    SCALAR_TYPE_FULL, parameter :: two = CAT(2_,SCALAR_TYPE_KIND)
    SCALAR_TYPE_FULL, parameter :: three = CAT(3_,SCALAR_TYPE_KIND)
    SCALAR_TYPE_FULL, dimension(VECTOR_TYPE_SIZE) :: t

    t = (x%data - edge0%data) / (edge1%data - edge0%data)
    t = min(max(t, zero), one)
    res%data = t*t*(three - two*t)
end function

pure function CAT3(smoothstep_,VECTOR_TYPE_FULL,_scalar)(edge0, edge1, x) result(res)
    implicit none
    SCALAR_TYPE_FULL, intent(in) :: edge0, edge1
    type(VECTOR_TYPE_FULL), intent(in) :: x
    type(VECTOR_TYPE_FULL) :: res

    SCALAR_TYPE_FULL, parameter :: zero = CAT(0_,SCALAR_TYPE_KIND)
    SCALAR_TYPE_FULL, parameter :: one = CAT(1_,SCALAR_TYPE_KIND)
    SCALAR_TYPE_FULL, parameter :: two = CAT(2_,SCALAR_TYPE_KIND)
    SCALAR_TYPE_FULL, parameter :: three = CAT(3_,SCALAR_TYPE_KIND)
    SCALAR_TYPE_FULL, dimension(VECTOR_TYPE_SIZE) :: t

    t = (x%data - edge0) / (edge1 - edge0)
    t = min(max(t, zero), one)
    res%data = t*t*(three - two*t)
end function

pure function CAT(fma_,VECTOR_TYPE_FULL)(a, b, c) result(res)
    implicit none
    type(VECTOR_TYPE_FULL), intent(in) :: a, b, c
    type(VECTOR_TYPE_FULL) :: res

    res%data = a%data * b%data + c%data
end function

pure function CAT(isnan_,VECTOR_TYPE_FULL)(x) result(res)
    use, intrinsic :: ieee_arithmetic, only: ieee_is_nan

    implicit none
    type(VECTOR_TYPE_FULL), intent(in) :: x
    type(CAT(bvec,VECTOR_TYPE_SIZE)) :: res

    res%data = ieee_is_nan(x%data)
end function

pure function CAT(isinf_,VECTOR_TYPE_FULL)(x) result(res)
    use, intrinsic :: ieee_arithmetic, only: ieee_is_finite, ieee_is_nan

    implicit none
    type(VECTOR_TYPE_FULL), intent(in) :: x
    type(CAT(bvec,VECTOR_TYPE_SIZE)) :: res

    res%data = .not. ieee_is_finite(x%data) .and. .not. ieee_is_nan(x%data)
end function

