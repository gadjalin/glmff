pure function CAT(angle_,QUAT_TYPE_FULL)(x) result(res)
    implicit none
    type(QUAT_TYPE_FULL), intent(in) :: x
    SCALAR_TYPE_FULL :: res

    SCALAR_TYPE_FULL :: a
    SCALAR_TYPE_FULL, parameter :: zero = CAT(0_,SCALAR_TYPE_KIND)
    SCALAR_TYPE_FULL, parameter :: two = CAT(2_,SCALAR_TYPE_KIND)
    SCALAR_TYPE_FULL, parameter :: pi = acos(CAT(-1.0_,SCALAR_TYPE_KIND))
    SCALAR_TYPE_FULL, parameter :: cos_half = cos(CAT(0.5_,SCALAR_TYPE_KIND))

    if (abs(x%w) > cos_half) then
        a = asin(sqrt(dot_product(x%v, x%v))) * two
        if (x%w < zero) then
            res = pi * two - a;
        else
            res = a
        end if
    else
        res = acos(x%w) * two
    end if
end function

pure function CAT(axis_,QUAT_TYPE_FULL)(x) result(res)
    implicit none
    type(QUAT_TYPE_FULL), intent(in) :: x
    type(CAT(VECTOR_TYPE_NAME,3)) :: res

    SCALAR_TYPE_FULL :: tmp1, tmp2
    SCALAR_TYPE_FULL, parameter :: zero = CAT(0_,SCALAR_TYPE_KIND)
    SCALAR_TYPE_FULL, parameter :: one = CAT(1_,SCALAR_TYPE_KIND)

    tmp1 = one - x%w * x%w
    if(tmp1 <= zero) then
        res%data = [zero, zero, one]
    else
        tmp2 = one / sqrt(tmp1)
        res%data = [ x%v(1) * tmp2, x%v(2) * tmp2, x%v(3) * tmp2]
    end if
end function

pure function CAT(angleAxis_,QUAT_TYPE_FULL)(angle, axis) result(res)
    implicit none
    SCALAR_TYPE_FULL, intent(in) :: angle
    type(CAT(VECTOR_TYPE_NAME,3)), intent(in) :: axis
    type(QUAT_TYPE_FULL) :: res

    SCALAR_TYPE_FULL, parameter :: half = CAT(0.5_,SCALAR_TYPE_KIND)

    res%w = cos(angle * half)
    res%v = axis%data * sin(angle * half)
end function

