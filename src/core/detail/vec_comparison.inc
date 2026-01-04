pure logical function CAT(eq_,VECTOR_TYPE_FULL)(lhs, rhs)
    implicit none
    type(VECTOR_TYPE_FULL), intent(in) :: lhs, rhs

    CAT(eq_,VECTOR_TYPE_FULL) = all(lhs%data == rhs%data)
end function

pure logical function CAT(ne_,VECTOR_TYPE_FULL)(lhs, rhs)
    implicit none
    type(VECTOR_TYPE_FULL), intent(in) :: lhs, rhs

    CAT(ne_,VECTOR_TYPE_FULL) = any(lhs%data /= rhs%data)
end function

