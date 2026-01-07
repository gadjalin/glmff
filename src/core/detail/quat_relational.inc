pure function CAT(equal_,QUAT_TYPE_FULL)(x, y) result(res)
    implicit none
    type(QUAT_TYPE_FULL), intent(in) :: x, y
    type(bvec4) :: res

    res%data = [x%w == y%w, x%v == y%v]
end function

pure function CAT(equal_eps_,QUAT_TYPE_FULL)(x, y, eps) result(res)
    implicit none
    type(QUAT_TYPE_FULL), intent(in) :: x, y
    SCALAR_TYPE_FULL, intent(in) :: eps
    type(bvec4) :: res

    res%data = abs([x%w - y%w, x%v - y%v]) < eps
end function

pure function CAT(notEqual_,QUAT_TYPE_FULL)(x, y) result(res)
    implicit none
    type(QUAT_TYPE_FULL), intent(in) :: x, y
    type(bvec4) :: res

    res%data = [x%w /= y%w, x%v /= y%v]
end function

pure function CAT(notEqual_eps_,QUAT_TYPE_FULL)(x, y, eps) result(res)
    implicit none
    type(QUAT_TYPE_FULL), intent(in) :: x, y
    SCALAR_TYPE_FULL, intent(in) :: eps
    type(bvec4) :: res

    res%data = abs([x%w - y%w, x%v - y%v]) >= eps
end function

