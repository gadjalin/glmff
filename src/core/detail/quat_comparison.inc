pure logical function CAT(eq_,QUAT_TYPE_FULL)(lhs, rhs)
    implicit none
    type(QUAT_TYPE_FULL), intent(in) :: lhs, rhs

    CAT(eq_,QUAT_TYPE_FULL) = lhs%w == rhs%w .and. all(lhs%v == rhs%v)
end function

pure logical function CAT(ne_,QUAT_TYPE_FULL)(lhs, rhs)
    implicit none
    type(QUAT_TYPE_FULL), intent(in) :: lhs, rhs

    CAT(ne_,QUAT_TYPE_FULL) = lhs%w /= rhs%w .or. any(lhs%v /= rhs%v)
end function

