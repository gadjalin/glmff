elemental function CAT(pos_,MATRIX_TYPE_FULL)(x) result(res)
    implicit none
    type(MATRIX_TYPE_FULL), intent(in) :: x
    type(MATRIX_TYPE_FULL) :: res

    res = x
end function

elemental function CAT(add_,MATRIX_TYPE_FULL)(a, b) result(res)
    implicit none
    type(MATRIX_TYPE_FULL), intent(in) :: a, b
    type(MATRIX_TYPE_FULL) :: res

    res%data = a%data + b%data
end function

elemental function CAT3(add_,MATRIX_TYPE_FULL,_scalar)(a, s) result(res)
    implicit none
    type(MATRIX_TYPE_FULL), intent(in) :: a
    SCALAR_TYPE_FULL, intent(in) :: s
    type(MATRIX_TYPE_FULL) :: res

    res%data = a%data + s
end function

elemental function CAT(neg_,MATRIX_TYPE_FULL)(x) result(res)
    implicit none
    type(MATRIX_TYPE_FULL), intent(in) :: x
    type(MATRIX_TYPE_FULL) :: res

    res%data = -x%data
end function

elemental function CAT(sub_,MATRIX_TYPE_FULL)(a, b) result(res)
    implicit none
    type(MATRIX_TYPE_FULL), intent(in) :: a, b
    type(MATRIX_TYPE_FULL) :: res

    res%data = a%data - b%data
end function

elemental function CAT3(sub_,MATRIX_TYPE_FULL,_scalar)(a, s) result(res)
    implicit none
    type(MATRIX_TYPE_FULL), intent(in) :: a
    SCALAR_TYPE_FULL, intent(in) :: s
    type(MATRIX_TYPE_FULL) :: res

    res%data = a%data - s
end function

elemental function CAT3(mul_,MATRIX_TYPE_FULL,_mat2)(a, b) result(res)
    implicit none
    type(MATRIX_TYPE_FULL), intent(in) :: a
    type(CAT3(MATRIX_TYPE_NAME,2x,MATRIX_TYPE_NCOL)), intent(in) :: b
    type(CAT3(MATRIX_TYPE_NAME,2x,MATRIX_TYPE_NROW)) :: res

    res%data = matmul(a%data, b%data)
end function

elemental function CAT3(mul_,MATRIX_TYPE_FULL,_mat3)(a, b) result(res)
    implicit none
    type(MATRIX_TYPE_FULL), intent(in) :: a
    type(CAT3(MATRIX_TYPE_NAME,3x,MATRIX_TYPE_NCOL)), intent(in) :: b
    type(CAT3(MATRIX_TYPE_NAME,3x,MATRIX_TYPE_NROW)) :: res

    res%data = matmul(a%data, b%data)
end function

elemental function CAT3(mul_,MATRIX_TYPE_FULL,_mat4)(a, b) result(res)
    implicit none
    type(MATRIX_TYPE_FULL), intent(in) :: a
    type(CAT3(MATRIX_TYPE_NAME,4x,MATRIX_TYPE_NCOL)), intent(in) :: b
    type(CAT3(MATRIX_TYPE_NAME,4x,MATRIX_TYPE_NROW)) :: res

    res%data = matmul(a%data, b%data)
end function

elemental function CAT3(mul_,MATRIX_TYPE_FULL,_scalar)(a, s) result(res)
    implicit none
    type(MATRIX_TYPE_FULL), intent(in) :: a
    SCALAR_TYPE_FULL, intent(in) :: s
    type(MATRIX_TYPE_FULL) :: res

    res%data = a%data * s
end function

elemental function CAT(mul_scalar_,MATRIX_TYPE_FULL)(s, a) result(res)
    implicit none
    SCALAR_TYPE_FULL, intent(in) :: s
    type(MATRIX_TYPE_FULL), intent(in) :: a
    type(MATRIX_TYPE_FULL) :: res

    res%data = s * a%data
end function

elemental function CAT3(mul_,MATRIX_TYPE_FULL,_vec)(a, v) result(res)
    implicit none
    type(MATRIX_TYPE_FULL), intent(in) :: a
    type(ROW_TYPE_FULL), intent(in) :: v
    type(COL_TYPE_FULL) :: res

    res%data = matmul(a%data, v%data)
end function

elemental function CAT(mul_vec_,MATRIX_TYPE_FULL)(v, a) result(res)
    implicit none
    type(COL_TYPE_FULL), intent(in) :: v
    type(MATRIX_TYPE_FULL), intent(in) :: a
    type(ROW_TYPE_FULL) :: res

    res%data = matmul(v%data, a%data)
end function

elemental function CAT3(div_,MATRIX_TYPE_FULL,_scalar)(a, s) result(res)
    implicit none
    type(MATRIX_TYPE_FULL), intent(in) :: a
    SCALAR_TYPE_FULL, intent(in) :: s
    type(MATRIX_TYPE_FULL) :: res

    res%data = a%data / s
end function

elemental function CAT(div_scalar_,MATRIX_TYPE_FULL)(s, a) result(res)
    implicit none
    type(MATRIX_TYPE_FULL), intent(in) :: a
    SCALAR_TYPE_FULL, intent(in) :: s
    type(MATRIX_TYPE_FULL) :: res

    res%data = s / a%data
end function

