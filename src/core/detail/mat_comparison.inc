pure logical function CAT(eq_,MATRIX_TYPE_FULL)(a, b) result(res)
    implicit none
    type(MATRIX_TYPE_FULL), intent(in) :: a, b

    res = all(a%data == b%data)
end function

pure logical function CAT(ne_,MATRIX_TYPE_FULL)(a, b) result(res)
    implicit none
    type(MATRIX_TYPE_FULL), intent(in) :: a, b

    res = any(a%data /= b%data)
end function

