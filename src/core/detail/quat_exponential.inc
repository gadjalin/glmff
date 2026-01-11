pure function CAT(exp_,QUAT_TYPE_FULL)(q) result(res)
    implicit none
    type(QUAT_TYPE_FULL), intent(in) :: q
    type(QUAT_TYPE_FULL) :: res

    SCALAR_TYPE_FULL :: angle

    angle = sqrt(dot_product(q%v, q%v))
    if (angle < epsilon(angle)) return

    res%w = cos(angle)
    res%v = sin(angle) * (q%v / angle)
end function

pure function CAT(log_,QUAT_TYPE_FULL)(q) result(res)
    use, intrinsic :: ieee_arithmetic, only: ieee_value, ieee_positive_inf

    implicit none
    type(QUAT_TYPE_FULL), intent(in) :: q
    type(QUAT_TYPE_FULL) :: res

    SCALAR_TYPE_FULL :: lenv, t, qlen2
    SCALAR_TYPE_FULL, parameter :: zero = CAT(0_,SCALAR_TYPE_KIND)
    SCALAR_TYPE_FULL, parameter :: half = CAT(0.5_,SCALAR_TYPE_KIND)
    SCALAR_TYPE_FULL, parameter :: one = CAT(1_,SCALAR_TYPE_KIND)
    SCALAR_TYPE_FULL, parameter :: pi = acos(CAT(-1.0_,SCALAR_TYPE_KIND))

    lenv = sqrt(dot_product(q%v, q%v))

    if (lenv < epsilon(lenv)) then
        if(q%w > zero) then
            res%w = log(q%w)
            res%v = zero
        else if(q%w < zero) then
            res%w = log(-q%w)
            res%v = [ pi, zero, zero ]
        else
            res%w = ieee_value(one, ieee_positive_inf)
            res%v = ieee_value(one, ieee_positive_inf)
        end if
    else
        t = atan(lenv, q%w) / lenv
        qlen2 = lenv * lenv + q%w * q%w;
        res%w = half * log(qlen2)
        res%v = t * q%v
    end if
end function

pure function CAT(pow_,QUAT_TYPE_FULL)(x, y) result(res)
    implicit none
    type(QUAT_TYPE_FULL), intent(in) :: x
    SCALAR_TYPE_FULL, intent(in) :: y
    type(QUAT_TYPE_FULL) :: res

    SCALAR_TYPE_FULL :: magq, magv, angle, new_angle, div, mag
    SCALAR_TYPE_FULL, parameter :: zero = CAT(0_,SCALAR_TYPE_KIND)
    SCALAR_TYPE_FULL, parameter :: one = CAT(1_,SCALAR_TYPE_KIND)
    SCALAR_TYPE_FULL, parameter :: cos_half = cos(CAT(0.5_,SCALAR_TYPE_KIND))

    if(abs(y) < epsilon(y)) return

    magq = sqrt(x%w * x%w + dot_product(x%v, x%v))

    if(abs(x%w / magq) > cos_half) then
        magv = dot_product(x%v, x%v)
        if (magv < tiny(magv)) then
            res%w = x%w**y
            res%v = zero
            return
        end if

        angle = asin(sqrt(magv) / magq)
    else
        angle = acos(x%w / magq)
    end if

    new_angle = angle * y
    div = sin(new_angle) / sin(angle)
    mag = magq**(y - one)
    res%w = cos(new_angle) * magq * mag
    res%v = x%v * div * mag
end function

pure function CAT(sqrt_,QUAT_TYPE_FULL)(x) result(res)
    implicit none
    type(QUAT_TYPE_FULL), intent(in) :: x
    type(QUAT_TYPE_FULL) :: res

    SCALAR_TYPE_FULL, parameter :: half = CAT(0.5_,SCALAR_TYPE_KIND)

    res = CAT(pow_,QUAT_TYPE_FULL)(x, half)
end function

