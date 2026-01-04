pure function CAT(matrixCompMult_,MATRIX_TYPE_FULL)(x, y) result(res)
    implicit none
    type(MATRIX_TYPE_FULL), intent(in) :: x, y
    type(MATRIX_TYPE_FULL) :: res

    res%data = x%data * y%data
end function

pure function CAT4(outerProduct_,COL_TYPE_FULL,_,ROW_TYPE_FULL)(col, row) result(mat)
    implicit none
    type(COL_TYPE_FULL), intent(in) :: col
    type(ROW_TYPE_FULL), intent(in) :: row
    type(MATRIX_TYPE_FULL) :: mat
    integer :: j

    do j = 1,MATRIX_TYPE_NCOL
        mat%data(:,j) = col%data * row%data(j)
    end do
end function

pure function CAT(transpose_,MATRIX_TYPE_FULL)(mat) result(res)
    implicit none
    type(MATRIX_TYPE_FULL), intent(in) :: mat
    type(CAT4(MATRIX_TYPE_NAME,MATRIX_TYPE_NROW,x,MATRIX_TYPE_NCOL)) :: res

    res%data = transpose(mat%data)
end function

