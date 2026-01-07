pure function CAT(mix_,QUAT_TYPE_FULL)(x, y, a) result(res)
    implicit none
    type(QUAT_TYPE_FULL), intent(in) :: x, y
    SCALAR_TYPE_FULL, intent(in) :: a
    type(QUAT_TYPE_FULL) :: res

    SCALAR_TYPE_FULL :: cos_theta, angle
    SCALAR_TYPE_FULL, parameter :: one = CAT(1_,SCALAR_TYPE_KIND)

    cos_theta = dot(x, y);

    if(cos_theta > one - epsilon(one)) then
        res%w = mix(x%w, y%w, a)
        res%v = [ mix(x%v(1), y%v(1), a), &
                  mix(x%v(2), y%v(2), a), &
                  mix(x%v(3), y%v(3), a) ]
    else
        angle = acos(cos_theta)
        res = (sin((one - a) * angle) * x + sin(a * angle) * y) / sin(angle)
    end if
end function

pure function CAT(lerp_,QUAT_TYPE_FULL)(x, y, a) result(res)
    implicit none
    type(QUAT_TYPE_FULL), intent(in) :: x, y
    SCALAR_TYPE_FULL, intent(in) :: a
    type(QUAT_TYPE_FULL) :: res

    SCALAR_TYPE_FULL, parameter :: one = CAT(1_,SCALAR_TYPE_KIND)

    ! TODO assert a in [0, 1]
    res = x * (one - a) + (y * a)
end function

pure function CAT(slerp_,QUAT_TYPE_FULL)(x, y, a) result(res)
    implicit none
    type(QUAT_TYPE_FULL), intent(in) :: x, y
    SCALAR_TYPE_FULL, intent(in) :: a
    type(QUAT_TYPE_FULL) :: res

    type(QUAT_TYPE_FULL) :: z
    SCALAR_TYPE_FULL :: cos_theta, angle
    SCALAR_TYPE_FULL, parameter :: zero = CAT(0_,SCALAR_TYPE_KIND)
    SCALAR_TYPE_FULL, parameter :: one = CAT(1_,SCALAR_TYPE_KIND)

    z = y

    cos_theta = dot(x, y)

    if(cos_theta < zero) then
        z = -y
        cos_theta = -cos_theta
    end if

    if(cos_theta > one - epsilon(one)) then
        res%w = mix(x%w, z%w, a)
        res%v = [ mix(x%v(1), z%v(1), a), &
                  mix(x%v(2), z%v(2), a), &
                  mix(x%v(3), z%v(3), a) ]
    else
        angle = acos(cos_theta)
        res = (sin((one - a) * angle) * x + sin(a * angle) * z) / sin(angle)
    end if
end function

pure function CAT(slerp_k_,QUAT_TYPE_FULL)(x, y, a, k) result(res)
    implicit none
    type(QUAT_TYPE_FULL), intent(in) :: x, y
    SCALAR_TYPE_FULL, intent(in) :: a, k
    type(QUAT_TYPE_FULL) :: res

    type(QUAT_TYPE_FULL) :: z
    SCALAR_TYPE_FULL :: cos_theta, angle, phi
    SCALAR_TYPE_FULL, parameter :: zero = CAT(0_,SCALAR_TYPE_KIND)
    SCALAR_TYPE_FULL, parameter :: one = CAT(1_,SCALAR_TYPE_KIND)
    SCALAR_TYPE_FULL, parameter :: pi = acos(CAT(-1.0_,SCALAR_TYPE_KIND))

    z = y

    cos_theta = dot(x, y)

    if(cos_theta < zero) then
        z = -y
        cos_theta = -cos_theta
    end if

    if(cos_theta > one - epsilon(one)) then
        res%w = mix(x%w, z%w, a)
        res%v = [ mix(x%v(1), z%v(1), a), &
                  mix(x%v(2), z%v(2), a), &
                  mix(x%v(3), z%v(3), a) ]
    else
        angle = acos(cos_theta)
        phi = angle + k*pi
        res = (sin(angle - a * phi) * x + sin(a * phi) * z) / sin(angle)
    end if
end function

pure function CAT(conjugate_,QUAT_TYPE_FULL)(q) result(res)
    implicit none
    type(QUAT_TYPE_FULL), intent(in) :: q
    type(QUAT_TYPE_FULL) :: res

    res%w = q%w
    res%v = -q%v
end function

pure function CAT(inverse_,QUAT_TYPE_FULL)(q) result(res)
    implicit none
    type(QUAT_TYPE_FULL), intent(in) :: q
    type(QUAT_TYPE_FULL) :: res

    res = conjugate(q) / dot(q, q)
end function

pure function CAT(isnan_,QUAT_TYPE_FULL)(x) result(res)
    use, intrinsic :: ieee_arithmetic, only: ieee_is_nan

    implicit none
    type(QUAT_TYPE_FULL), intent(in) :: x
    type(bvec4) :: res

    res%data = [ieee_is_nan(x%w), ieee_is_nan(x%v)]
end function

pure function CAT(isinf_,QUAT_TYPE_FULL)(x) result(res)
    use, intrinsic :: ieee_arithmetic, only: ieee_is_finite, ieee_is_nan

    implicit none
    type(QUAT_TYPE_FULL), intent(in) :: x
    type(bvec4) :: res

    res%data = [.not. ieee_is_finite(x%w) .and. .not. ieee_is_nan(x%w), &
                .not. ieee_is_finite(x%v) .and. .not. ieee_is_nan(x%v)]
end function

