pure logical function CAT(any_,VECTOR_TYPE_FULL)(x)
    implicit none
    type(VECTOR_TYPE_FULL), intent(in) :: x

    CAT(any_,VECTOR_TYPE_FULL) = any(x%data)
end function

pure logical function CAT(all_,VECTOR_TYPE_FULL)(x)
    implicit none
    type(VECTOR_TYPE_FULL), intent(in) :: x

    CAT(all_,VECTOR_TYPE_FULL) = all(x%data)
end function

pure function CAT(not_,VECTOR_TYPE_FULL)(x) result(res)
    implicit none
    type(VECTOR_TYPE_FULL), intent(in) :: x
    type(VECTOR_TYPE_FULL) :: res

    res%data = .not. x%data
end function

pure function CAT(and_,VECTOR_TYPE_FULL)(lhs, rhs) result(res)
    implicit none
    type(VECTOR_TYPE_FULL), intent(in) :: lhs, rhs
    type(VECTOR_TYPE_FULL) :: res

    res%data = lhs%data .and. rhs%data
end function

pure function CAT(or_,VECTOR_TYPE_FULL)(lhs, rhs) result(res)
    implicit none
    type(VECTOR_TYPE_FULL), intent(in) :: lhs, rhs
    type(VECTOR_TYPE_FULL) :: res

    res%data = lhs%data .or. rhs%data
end function

