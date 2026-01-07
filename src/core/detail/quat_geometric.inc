pure function CAT(dot_,QUAT_TYPE_FULL)(x, y) result(res)
    implicit none
    type(QUAT_TYPE_FULL), intent(in) :: x, y
    SCALAR_TYPE_FULL :: res

    res = x%w * y%w + dot_product(x%v, y%v)
end function

pure function CAT(length_,QUAT_TYPE_FULL)(q) result(res)
    implicit none
    type(QUAT_TYPE_FULL), intent(in) :: q
    SCALAR_TYPE_FULL :: res

    res = sqrt(q%w*q%w + dot_product(q%v, q%v))
end function

pure function CAT(normalize_,QUAT_TYPE_FULL)(q) result(res)
    implicit none
    type(QUAT_TYPE_FULL), intent(in) :: q
    type(QUAT_TYPE_FULL) :: res


    SCALAR_TYPE_FULL :: lenq, ilenq
    SCALAR_TYPE_FULL, parameter :: zero = CAT(0_,SCALAR_TYPE_KIND)
    SCALAR_TYPE_FULL, parameter :: one = CAT(1_,SCALAR_TYPE_KIND)

    lenq = length(q)
    if (lenq <= zero) then
        res%w = one
        res%v = zero
    end if

    ilenq = one / lenq
    res%w = q%w * ilenq
    res%v = q%v * ilenq
end function

pure function CAT(cross_,QUAT_TYPE_FULL)(q1, q2) result(res)
    implicit none
    type(QUAT_TYPE_FULL), intent(in) :: q1, q2
    type(QUAT_TYPE_FULL) :: res

    res%w    = q1%w * q2%w - q1%v(1) * q2%v(1) - q1%v(2) * q2%v(2) - q1%v(3) * q2%v(3)
    res%v(1) = q1%w * q2%v(1) + q1%v(1) * q2%w + q1%v(2) * q2%v(3) - q1%v(3) * q2%v(2)
    res%v(2) = q1%w * q2%v(2) + q1%v(2) * q2%w + q1%v(3) * q2%v(1) - q1%v(1) * q2%v(3)
    res%v(3) = q1%w * q2%v(3) + q1%v(3) * q2%w + q1%v(1) * q2%v(2) - q1%v(2) * q2%v(1)
end function

pure function CAT(rotate_,QUAT_TYPE_FULL)(q, angle, axis) result(res)
    implicit none
    type(QUAT_TYPE_FULL), intent(in) :: q
    SCALAR_TYPE_FULL, intent(in) :: angle
    type(CAT(VECTOR_TYPE_NAME,3)), intent(in) :: axis
    type(QUAT_TYPE_FULL) :: res

    type(CAT(VECTOR_TYPE_NAME,3)) :: tmp
    SCALAR_TYPE_FULL :: len_axis, ilen, sin_angle
    SCALAR_TYPE_FULL, parameter :: one = CAT(1_,SCALAR_TYPE_KIND)
    SCALAR_TYPE_FULL, parameter :: half = CAT(0.5_,SCALAR_TYPE_KIND)
    SCALAR_TYPE_FULL, parameter :: small = CAT(0.001_,SCALAR_TYPE_KIND)

    tmp = axis

    len_axis = length(tmp)
    if (abs(len_axis - one) > small) then
        ilen = one / len_axis
        tmp%data = tmp%data * ilen
    end if

    sin_angle = sin(angle * half)
    res%w = cos(angle * half)
    res%v = tmp%data * sin_angle
    res = q * res
end function

