pure function CAT(pos_,MATRIX_TYPE_FULL)(mat) result(res)
    implicit none
    type(MATRIX_TYPE_FULL), intent(in) :: mat
    type(MATRIX_TYPE_FULL) :: res

    res = mat
end function

pure function CAT(add_,MATRIX_TYPE_FULL)(lhs, rhs) result(res)
    implicit none
    type(MATRIX_TYPE_FULL), intent(in) :: lhs, rhs
    type(MATRIX_TYPE_FULL) :: res

    res%data = lhs%data + rhs%data
end function

pure function CAT3(add_,MATRIX_TYPE_FULL,_scalar)(mat, scalar) result(res)
    implicit none
    type(MATRIX_TYPE_FULL), intent(in) :: mat
    SCALAR_TYPE_FULL, intent(in) :: scalar
    type(MATRIX_TYPE_FULL) :: res

    res%data = mat%data + scalar
end function

pure function CAT(neg_,MATRIX_TYPE_FULL)(mat) result(res)
    implicit none
    type(MATRIX_TYPE_FULL), intent(in) :: mat
    type(MATRIX_TYPE_FULL) :: res

    res%data = -mat%data
end function

pure function CAT(sub_,MATRIX_TYPE_FULL)(lhs, rhs) result(res)
    implicit none
    type(MATRIX_TYPE_FULL), intent(in) :: lhs, rhs
    type(MATRIX_TYPE_FULL) :: res

    res%data = lhs%data - rhs%data
end function

pure function CAT3(sub_,MATRIX_TYPE_FULL,_scalar)(mat, scalar) result(res)
    implicit none
    type(MATRIX_TYPE_FULL), intent(in) :: mat
    SCALAR_TYPE_FULL, intent(in) :: scalar
    type(MATRIX_TYPE_FULL) :: res

    res%data = mat%data - scalar
end function

pure function CAT3(mul_,MATRIX_TYPE_FULL,_mat2)(lhs, rhs) result(res)
    implicit none
    type(MATRIX_TYPE_FULL), intent(in) :: lhs
    type(CAT3(MATRIX_TYPE_NAME,2x,MATRIX_TYPE_NCOL)), intent(in) :: rhs
    type(CAT3(MATRIX_TYPE_NAME,2x,MATRIX_TYPE_NROW)) :: res

    res%data = matmul(lhs%data, rhs%data)
end function

pure function CAT3(mul_,MATRIX_TYPE_FULL,_mat3)(lhs, rhs) result(res)
    implicit none
    type(MATRIX_TYPE_FULL), intent(in) :: lhs
    type(CAT3(MATRIX_TYPE_NAME,3x,MATRIX_TYPE_NCOL)), intent(in) :: rhs
    type(CAT3(MATRIX_TYPE_NAME,3x,MATRIX_TYPE_NROW)) :: res

    res%data = matmul(lhs%data, rhs%data)
end function

pure function CAT3(mul_,MATRIX_TYPE_FULL,_mat4)(lhs, rhs) result(res)
    implicit none
    type(MATRIX_TYPE_FULL), intent(in) :: lhs
    type(CAT3(MATRIX_TYPE_NAME,4x,MATRIX_TYPE_NCOL)), intent(in) :: rhs
    type(CAT3(MATRIX_TYPE_NAME,4x,MATRIX_TYPE_NROW)) :: res

    res%data = matmul(lhs%data, rhs%data)
end function

pure function CAT3(mul_,MATRIX_TYPE_FULL,_scalar)(mat, scalar) result(res)
    implicit none
    type(MATRIX_TYPE_FULL), intent(in) :: mat
    SCALAR_TYPE_FULL, intent(in) :: scalar
    type(MATRIX_TYPE_FULL) :: res

    res%data = mat%data * scalar
end function

pure function CAT(mul_scalar_,MATRIX_TYPE_FULL)(scalar, mat) result(res)
    implicit none
    SCALAR_TYPE_FULL, intent(in) :: scalar
    type(MATRIX_TYPE_FULL), intent(in) :: mat
    type(MATRIX_TYPE_FULL) :: res

    res%data = scalar * mat%data
end function

pure function CAT3(mul_,MATRIX_TYPE_FULL,_vec)(mat, vec) result(res)
    implicit none
    type(MATRIX_TYPE_FULL), intent(in) :: mat
    type(ROW_TYPE_FULL), intent(in) :: vec
    type(COL_TYPE_FULL) :: res

    res%data = matmul(mat%data, vec%data)
end function

pure function CAT(mul_vec_,MATRIX_TYPE_FULL)(vec, mat) result(res)
    implicit none
    type(COL_TYPE_FULL), intent(in) :: vec
    type(MATRIX_TYPE_FULL), intent(in) :: mat
    type(ROW_TYPE_FULL) :: res

    res%data = matmul(vec%data, mat%data)
end function

pure function CAT3(div_,MATRIX_TYPE_FULL,_scalar)(mat, scalar) result(res)
    implicit none
    type(MATRIX_TYPE_FULL), intent(in) :: mat
    SCALAR_TYPE_FULL, intent(in) :: scalar
    type(MATRIX_TYPE_FULL) :: res

    res%data = mat%data / scalar
end function

pure function CAT(div_scalar_,MATRIX_TYPE_FULL)(scalar, mat) result(res)
    implicit none
    type(MATRIX_TYPE_FULL), intent(in) :: mat
    SCALAR_TYPE_FULL, intent(in) :: scalar
    type(MATRIX_TYPE_FULL) :: res

    res%data = scalar / mat%data
end function

