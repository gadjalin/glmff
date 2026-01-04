pure function CAT(MATRIX_TYPE_FULL,_diag)(s) result(mat)
    implicit none
    SCALAR_TYPE_FULL, intent(in) :: s
    type(MATRIX_TYPE_FULL) :: mat

    SCALAR_TYPE_FULL, parameter :: zero = CAT(0_,SCALAR_TYPE_KIND)

    mat%data = zero
    mat%data(1,1) = s
    mat%data(2,2) = s
#if MATRIX_TYPE_NCOL > 2
#if MATRIX_TYPE_NROW > 2
    mat%data(3,3) = s
#endif
#endif
#if MATRIX_TYPE_NCOL > 3
#if MATRIX_TYPE_NROW > 3
    mat%data(4,4) = s
#endif
#endif
end function

pure function CAT(MATRIX_TYPE_FULL,_scalar)( &
#if MATRIX_TYPE_NCOL == 2
#if MATRIX_TYPE_NROW == 2
            x0, y0, &
            x1, y1  &
        ) result(mat)
    implicit none
    SCALAR_TYPE_FULL, intent(in) :: x0, y0
    SCALAR_TYPE_FULL, intent(in) :: x1, y1
#elif MATRIX_TYPE_NROW == 3
            x0, y0, z0, &
            x1, y1, z1  &
        ) result(mat)
    implicit none
    SCALAR_TYPE_FULL, intent(in) :: x0, y0, z0
    SCALAR_TYPE_FULL, intent(in) :: x1, y1, z1
#elif MATRIX_TYPE_NROW == 4
            x0, y0, z0, w0, &
            x1, y1, z1, w1  &
        ) result(mat)
    implicit none
    SCALAR_TYPE_FULL, intent(in) :: x0, y0, z0, w0
    SCALAR_TYPE_FULL, intent(in) :: x1, y1, z1, w1
#endif
#elif MATRIX_TYPE_NCOL == 3
#if MATRIX_TYPE_NROW == 2
            x0, y0, &
            x1, y1, &
            x2, y2  &
        ) result(mat)
    implicit none
    SCALAR_TYPE_FULL, intent(in) :: x0, y0
    SCALAR_TYPE_FULL, intent(in) :: x1, y1
    SCALAR_TYPE_FULL, intent(in) :: x2, y2
#elif MATRIX_TYPE_NROW == 3
            x0, y0, z0, &
            x1, y1, z1, &
            x2, y2, z2  &
        ) result(mat)
    implicit none
    SCALAR_TYPE_FULL, intent(in) :: x0, y0, z0
    SCALAR_TYPE_FULL, intent(in) :: x1, y1, z1
    SCALAR_TYPE_FULL, intent(in) :: x2, y2, z2
#elif MATRIX_TYPE_NROW == 4
            x0, y0, z0, w0, &
            x1, y1, z1, w1, &
            x2, y2, z2, w2  &
        ) result(mat)
    implicit none
    SCALAR_TYPE_FULL, intent(in) :: x0, y0, z0, w0
    SCALAR_TYPE_FULL, intent(in) :: x1, y1, z1, w1
    SCALAR_TYPE_FULL, intent(in) :: x2, y2, z2, w2
#endif
#elif MATRIX_TYPE_NCOL == 4
#if MATRIX_TYPE_NROW == 2
            x0, y0, &
            x1, y1, &
            x2, y2, &
            x3, y3  &
        ) result(mat)
    implicit none
    SCALAR_TYPE_FULL, intent(in) :: x0, y0
    SCALAR_TYPE_FULL, intent(in) :: x1, y1
    SCALAR_TYPE_FULL, intent(in) :: x2, y2
    SCALAR_TYPE_FULL, intent(in) :: x3, y3
#elif MATRIX_TYPE_NROW == 3
            x0, y0, z0, &
            x1, y1, z1, &
            x2, y2, z2, &
            x3, y3, z3  &
        ) result(mat)
    implicit none
    SCALAR_TYPE_FULL, intent(in) :: x0, y0, z0
    SCALAR_TYPE_FULL, intent(in) :: x1, y1, z1
    SCALAR_TYPE_FULL, intent(in) :: x2, y2, z2
    SCALAR_TYPE_FULL, intent(in) :: x3, y3, z3
#elif MATRIX_TYPE_NROW == 4
            x0, y0, z0, w0, &
            x1, y1, z1, w1, &
            x2, y2, z2, w2, &
            x3, y3, z3, w3  &
        ) result(mat)
    implicit none
    SCALAR_TYPE_FULL, intent(in) :: x0, y0, z0, w0
    SCALAR_TYPE_FULL, intent(in) :: x1, y1, z1, w1
    SCALAR_TYPE_FULL, intent(in) :: x2, y2, z2, w2
    SCALAR_TYPE_FULL, intent(in) :: x3, y3, z3, w3
#endif
#endif
    type(MATRIX_TYPE_FULL) :: mat

#if MATRIX_TYPE_NROW == 2
    mat%data(:,1) = [x0, y0]
    mat%data(:,2) = [x1, y1]
#elif MATRIX_TYPE_NROW == 3
    mat%data(:,1) = [x0, y0, z0]
    mat%data(:,2) = [x1, y1, z1]
#elif MATRIX_TYPE_NROW == 4
    mat%data(:,1) = [x0, y0, z0, w0]
    mat%data(:,2) = [x1, y1, z1, w1]
#endif
#if MATRIX_TYPE_NCOL > 2
#if MATRIX_TYPE_NROW == 2
    mat%data(:,3) = [x2, y2]
#elif MATRIX_TYPE_NROW == 3
    mat%data(:,3) = [x2, y2, z2]
#elif MATRIX_TYPE_NROW == 4
    mat%data(:,3) = [x2, y2, z2, w2]
#endif
#endif
#if MATRIX_TYPE_NCOL > 3
#if MATRIX_TYPE_NROW == 2
    mat%data(:,4) = [x3, y3]
#elif MATRIX_TYPE_NROW == 3
    mat%data(:,4) = [x3, y3, z3]
#elif MATRIX_TYPE_NROW == 4
    mat%data(:,4) = [x3, y3, z3, w3]
#endif
#endif
end function

pure function CAT(MATRIX_TYPE_FULL,_col)( &
#if MATRIX_TYPE_NCOL == 2
            v0, v1 &
        ) result(mat)
    implicit none
    type(COL_TYPE_FULL), intent(in) :: v0, v1
#elif MATRIX_TYPE_NCOL == 3
            v0, v1, v2 &
        ) result(mat)
    implicit none
    type(COL_TYPE_FULL), intent(in) :: v0, v1, v2
#elif MATRIX_TYPE_NCOL == 4
            v0, v1, v2, v3 &
        ) result(mat)
    implicit none
    type(COL_TYPE_FULL), intent(in) :: v0, v1, v2, v3
#endif
    type(MATRIX_TYPE_FULL) :: mat

    mat%data(:,1) = v0%data
    mat%data(:,2) = v1%data
#if MATRIX_TYPE_NCOL > 2
    mat%data(:,3) = v2%data
#endif
#if MATRIX_TYPE_NCOL > 3
    mat%data(:,4) = v3%data
#endif
end function

