pure function CAT(QUAT_TYPE_FULL,_to_mat3)(qua) result(res)
    implicit none
    type(QUAT_TYPE_FULL), intent(in) :: qua
    type(CAT(MATRIX_TYPE_NAME,3x3)) :: res

    SCALAR_TYPE_FULL :: qxx, qyy, qzz, qxy, qxz, qyz, qwx, qwy, qwz
    SCALAR_TYPE_FULL, parameter :: one = CAT(1_,SCALAR_TYPE_KIND)
    SCALAR_TYPE_FULL, parameter :: two = CAT(2_,SCALAR_TYPE_KIND)

    qxx = qua%v(1) * qua%v(1)
    qyy = qua%v(2) * qua%v(2)
    qzz = qua%v(3) * qua%v(3)
    qxz = qua%v(1) * qua%v(3)
    qxy = qua%v(1) * qua%v(2)
    qyz = qua%v(2) * qua%v(3)
    qwx = qua%w    * qua%v(1)
    qwy = qua%w    * qua%v(2)
    qwz = qua%w    * qua%v(3)

    res%data(1,1) = one - two * (qyy +  qzz)
    res%data(2,1) = two * (qxy + qwz)
    res%data(3,1) = two * (qxz - qwy)

    res%data(1,2) = two * (qxy - qwz)
    res%data(2,2) = one - two * (qxx +  qzz)
    res%data(3,2) = two * (qyz + qwx)

    res%data(1,3) = two * (qxz + qwy)
    res%data(2,3) = two * (qyz - qwx)
    res%data(3,3) = one - two * (qxx +  qyy)
end function

pure function CAT(QUAT_TYPE_FULL,_to_mat4)(qua) result(res)
    implicit none
    type(QUAT_TYPE_FULL), intent(in) :: qua
    type(CAT(MATRIX_TYPE_NAME,4x4)) :: res

    type(CAT(MATRIX_TYPE_NAME,3x3)) :: tmp

    tmp = CAT(QUAT_TYPE_FULL,_to_mat3)(qua)
    res%data(1:3,1:3) = tmp%data
end function

