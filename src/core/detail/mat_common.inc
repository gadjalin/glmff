pure function CAT(mix_,MATRIX_TYPE_FULL)(x, y, a) result(res)
    implicit none
    type(MATRIX_TYPE_FULL), intent(in) :: x, y, a
    type(MATRIX_TYPE_FULL) :: res

    SCALAR_TYPE_FULL, parameter :: one = CAT(1_,SCALAR_TYPE_KIND)

    res%data = x%data*(one - a%data) + y%data*a%data
end function

pure function CAT3(mix_,MATRIX_TYPE_FULL,_scalar)(x, y, a) result(res)
    implicit none
    type(MATRIX_TYPE_FULL), intent(in) :: x, y
    SCALAR_TYPE_FULL, intent(in) :: a
    type(MATRIX_TYPE_FULL) :: res

    SCALAR_TYPE_FULL, parameter :: one = CAT(1_,SCALAR_TYPE_KIND)

    res%data = x%data*(one - a) + y%data*a
end function

pure function CAT(abs_,MATRIX_TYPE_FULL)(x) result(res)
    implicit none
    type(MATRIX_TYPE_FULL), intent(in) :: x
    type(MATRIX_TYPE_FULL) :: res

    res%data = abs(x%data)
end function

