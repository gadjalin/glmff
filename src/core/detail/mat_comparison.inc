pure logical function CAT(eq_,MATRIX_TYPE_FULL)(lhs, rhs)
    implicit none
    type(MATRIX_TYPE_FULL), intent(in) :: lhs, rhs

    CAT(eq_,MATRIX_TYPE_FULL) = all(lhs%data == rhs%data)
end function

pure logical function CAT(ne_,MATRIX_TYPE_FULL)(lhs, rhs)
    implicit none
    type(MATRIX_TYPE_FULL), intent(in) :: lhs, rhs

    CAT(ne_,MATRIX_TYPE_FULL) = any(lhs%data /= rhs%data)
end function

