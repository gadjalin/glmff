pure function CAT(pos_,QUAT_TYPE_FULL)(qua) result(res)
    implicit none
    type(QUAT_TYPE_FULL), intent(in) :: qua
    type(QUAT_TYPE_FULL) :: res

    res = qua
end function

pure function CAT(add_,QUAT_TYPE_FULL)(lhs, rhs) result(res)
    implicit none
    type(QUAT_TYPE_FULL), intent(in) :: lhs, rhs
    type(QUAT_TYPE_FULL) :: res

    res%w = lhs%w + rhs%w
    res%v = lhs%v + rhs%v
end function

pure function CAT(neg_,QUAT_TYPE_FULL)(qua) result(res)
    implicit none
    type(QUAT_TYPE_FULL), intent(in) :: qua
    type(QUAT_TYPE_FULL) :: res

    res%w = -qua%w
    res%v = -qua%v
end function

pure function CAT(sub_,QUAT_TYPE_FULL)(lhs, rhs) result(res)
    implicit none
    type(QUAT_TYPE_FULL), intent(in) :: lhs, rhs
    type(QUAT_TYPE_FULL) :: res

    res%w = lhs%w - rhs%w
    res%v = lhs%v - rhs%v
end function

pure function CAT(mul_,QUAT_TYPE_FULL)(lhs, rhs) result(res)
    implicit none
    type(QUAT_TYPE_FULL), intent(in) :: lhs, rhs
    type(QUAT_TYPE_FULL) :: res

    res%w    = lhs%w * rhs%w - lhs%v(1) * rhs%v(1) - lhs%v(2) * rhs%v(2) - lhs%v(3) * rhs%v(3)
    res%v(1) = lhs%w * rhs%v(1) + lhs%v(1) * rhs%w + lhs%v(2) * rhs%v(3) - lhs%v(3) * rhs%v(2)
    res%v(2) = lhs%w * rhs%v(2) + lhs%v(2) * rhs%w + lhs%v(3) * rhs%v(1) - lhs%v(1) * rhs%v(3)
    res%v(3) = lhs%w * rhs%v(3) + lhs%v(3) * rhs%w + lhs%v(1) * rhs%v(2) - lhs%v(2) * rhs%v(1)
end function

pure function CAT3(mul_,QUAT_TYPE_FULL,_vec3)(lhs, vec) result(res)
    implicit none
    type(QUAT_TYPE_FULL), intent(in) :: lhs
    type(CAT(VECTOR_TYPE_NAME,3)), intent(in) :: vec
    type(CAT(VECTOR_TYPE_NAME,3)) :: res
    type(CAT(VECTOR_TYPE_NAME,3)) :: qv, uv, uuv

    SCALAR_TYPE_FULL, parameter :: two = CAT(2_,SCALAR_TYPE_KIND)

    qv%data = lhs%v
    uv = cross(qv, vec)
    uuv = cross(qv, uv)
    res%data = vec%data + ((uv%data * lhs%w) + uuv%data) * two
end function

pure function CAT(mul_vec3_,QUAT_TYPE_FULL)(vec, rhs) result(res)
    implicit none
    type(CAT(VECTOR_TYPE_NAME,3)), intent(in) :: vec
    type(QUAT_TYPE_FULL), intent(in) :: rhs
    type(CAT(VECTOR_TYPE_NAME,3)) :: res

    res = inverse(rhs) * vec
end function

pure function CAT3(mul_,QUAT_TYPE_FULL,_vec4)(lhs, vec) result(res)
    implicit none
    type(QUAT_TYPE_FULL), intent(in) :: lhs
    type(CAT(VECTOR_TYPE_NAME,4)), intent(in) :: vec
    type(CAT(VECTOR_TYPE_NAME,4)) :: res
    type(CAT(VECTOR_TYPE_NAME,3)) :: v3

    v3%data = vec%data(1:3)
    v3 = lhs * v3
    res%data = [v3%data, vec%data(4)]
end function

pure function CAT(mul_vec4_,QUAT_TYPE_FULL)(vec, rhs) result(res)
    implicit none
    type(CAT(VECTOR_TYPE_NAME,4)), intent(in) :: vec
    type(QUAT_TYPE_FULL), intent(in) :: rhs
    type(CAT(VECTOR_TYPE_NAME,4)) :: res

    res = inverse(rhs) * vec
end function

pure function CAT3(mul_,QUAT_TYPE_FULL,_scalar)(lhs, scalar) result(res)
    implicit none
    type(QUAT_TYPE_FULL), intent(in) :: lhs
    SCALAR_TYPE_FULL, intent(in) :: scalar
    type(QUAT_TYPE_FULL) :: res

    res%w = lhs%w * scalar
    res%v = lhs%v * scalar
end function

pure function CAT(mul_scalar_,QUAT_TYPE_FULL)(scalar, rhs) result(res)
    implicit none
    SCALAR_TYPE_FULL, intent(in) :: scalar
    type(QUAT_TYPE_FULL), intent(in) :: rhs
    type(QUAT_TYPE_FULL) :: res

    res%w = rhs%w * scalar
    res%v = rhs%v * scalar
end function

pure function CAT3(div_,QUAT_TYPE_FULL,_scalar)(lhs, scalar) result(res)
    implicit none
    type(QUAT_TYPE_FULL), intent(in) :: lhs
    SCALAR_TYPE_FULL, intent(in) :: scalar
    type(QUAT_TYPE_FULL) :: res

    res%w = lhs%w / scalar
    res%v = lhs%v / scalar
end function

