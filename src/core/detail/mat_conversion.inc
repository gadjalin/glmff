#if (MATRIX_TYPE_NCOL != 2) || (MATRIX_TYPE_NROW != 2)
pure function CAT(MATRIX_TYPE_FULL,_to_mat2x2)(mat) result(res)
    implicit none
    type(MATRIX_TYPE_FULL), intent(in) :: mat
    type(CAT(MATRIX_TYPE_NAME,2x2)) :: res

#if (MATRIX_TYPE_NCOL < 2) && (MATRIX_TYPE_NROW < 2)
    res%data(1:MATRIX_TYPE_NROW,1:MATRIX_TYPE_NCOL) = mat%data(:,:)
#elif MATRIX_TYPE_NCOL < 2
    res%data(:,1:MATRIX_TYPE_NCOL) = mat%data(1:2,:)
#elif MATRIX_TYPE_NROW < 2
    res%data(1:MATRIX_TYPE_NROW,:) = mat%data(:,1:2)
#else
    res%data(:,:) = mat%data(1:2,1:2)
#endif
end function
#endif

#if (MATRIX_TYPE_NCOL != 2) || (MATRIX_TYPE_NROW != 3)
pure function CAT(MATRIX_TYPE_FULL,_to_mat2x3)(mat) result(res)
    implicit none
    type(MATRIX_TYPE_FULL), intent(in) :: mat
    type(CAT(MATRIX_TYPE_NAME,2x3)) :: res

#if (MATRIX_TYPE_NCOL < 2) && (MATRIX_TYPE_NROW < 3)
    res%data(1:MATRIX_TYPE_NROW,1:MATRIX_TYPE_NCOL) = mat%data(:,:)
#elif MATRIX_TYPE_NCOL < 2
    res%data(:,1:MATRIX_TYPE_NCOL) = mat%data(1:3,:)
#elif MATRIX_TYPE_NROW < 3
    res%data(1:MATRIX_TYPE_NROW,:) = mat%data(:,1:2)
#else
    res%data(:,:) = mat%data(1:3,1:2)
#endif
end function
#endif

#if (MATRIX_TYPE_NCOL != 2) || (MATRIX_TYPE_NROW != 4)
pure function CAT(MATRIX_TYPE_FULL,_to_mat2x4)(mat) result(res)
    implicit none
    type(MATRIX_TYPE_FULL), intent(in) :: mat
    type(CAT(MATRIX_TYPE_NAME,2x4)) :: res

#if (MATRIX_TYPE_NCOL < 2) && (MATRIX_TYPE_NROW < 4)
    res%data(1:MATRIX_TYPE_NROW,1:MATRIX_TYPE_NCOL) = mat%data(:,:)
#elif MATRIX_TYPE_NCOL < 2
    res%data(:,1:MATRIX_TYPE_NCOL) = mat%data(1:4,:)
#elif MATRIX_TYPE_NROW < 4
    res%data(1:MATRIX_TYPE_NROW,:) = mat%data(:,1:2)
#else
    res%data(:,:) = mat%data(1:4,1:2)
#endif
end function
#endif

#if (MATRIX_TYPE_NCOL != 3) || (MATRIX_TYPE_NROW != 2)
pure function CAT(MATRIX_TYPE_FULL,_to_mat3x2)(mat) result(res)
    implicit none
    type(MATRIX_TYPE_FULL), intent(in) :: mat
    type(CAT(MATRIX_TYPE_NAME,3x2)) :: res

#if (MATRIX_TYPE_NCOL < 3) && (MATRIX_TYPE_NROW < 2)
    res%data(1:MATRIX_TYPE_NROW,1:MATRIX_TYPE_NCOL) = mat%data(:,:)
#elif MATRIX_TYPE_NCOL < 3
    res%data(:,1:MATRIX_TYPE_NCOL) = mat%data(1:2,:)
#elif MATRIX_TYPE_NROW < 2
    res%data(1:MATRIX_TYPE_NROW,:) = mat%data(:,1:3)
#else
    res%data(:,:) = mat%data(1:2,1:3)
#endif
end function
#endif

#if (MATRIX_TYPE_NCOL != 3) || (MATRIX_TYPE_NROW != 3)
pure function CAT(MATRIX_TYPE_FULL,_to_mat3x3)(mat) result(res)
    implicit none
    type(MATRIX_TYPE_FULL), intent(in) :: mat
    type(CAT(MATRIX_TYPE_NAME,3x3)) :: res

#if (MATRIX_TYPE_NCOL < 3) && (MATRIX_TYPE_NROW < 3)
    res%data(1:MATRIX_TYPE_NROW,1:MATRIX_TYPE_NCOL) = mat%data(:,:)
#elif MATRIX_TYPE_NCOL < 3
    res%data(:,1:MATRIX_TYPE_NCOL) = mat%data(1:3,:)
#elif MATRIX_TYPE_NROW < 3
    res%data(1:MATRIX_TYPE_NROW,:) = mat%data(:,1:3)
#else
    res%data(:,:) = mat%data(1:3,1:3)
#endif
end function
#endif

#if (MATRIX_TYPE_NCOL != 3) || (MATRIX_TYPE_NROW != 4)
pure function CAT(MATRIX_TYPE_FULL,_to_mat3x4)(mat) result(res)
    implicit none
    type(MATRIX_TYPE_FULL), intent(in) :: mat
    type(CAT(MATRIX_TYPE_NAME,3x4)) :: res

#if (MATRIX_TYPE_NCOL < 3) && (MATRIX_TYPE_NROW < 4)
    res%data(1:MATRIX_TYPE_NROW,1:MATRIX_TYPE_NCOL) = mat%data(:,:)
#elif MATRIX_TYPE_NCOL < 3
    res%data(:,1:MATRIX_TYPE_NCOL) = mat%data(1:4,:)
#elif MATRIX_TYPE_NROW < 4
    res%data(1:MATRIX_TYPE_NROW,:) = mat%data(:,1:3)
#else
    res%data(:,:) = mat%data(1:4,1:3)
#endif
end function
#endif

#if (MATRIX_TYPE_NCOL != 4) || (MATRIX_TYPE_NROW != 2)
pure function CAT(MATRIX_TYPE_FULL,_to_mat4x2)(mat) result(res)
    implicit none
    type(MATRIX_TYPE_FULL), intent(in) :: mat
    type(CAT(MATRIX_TYPE_NAME,4x2)) :: res

#if (MATRIX_TYPE_NCOL < 4) && (MATRIX_TYPE_NROW < 2)
    res%data(1:MATRIX_TYPE_NROW,1:MATRIX_TYPE_NCOL) = mat%data(:,:)
#elif MATRIX_TYPE_NCOL < 4
    res%data(:,1:MATRIX_TYPE_NCOL) = mat%data(1:2,:)
#elif MATRIX_TYPE_NROW < 2
    res%data(1:MATRIX_TYPE_NROW,:) = mat%data(:,1:4)
#else
    res%data(:,:) = mat%data(1:2,1:4)
#endif
end function
#endif

#if (MATRIX_TYPE_NCOL != 4) || (MATRIX_TYPE_NROW != 3)
pure function CAT(MATRIX_TYPE_FULL,_to_mat4x3)(mat) result(res)
    implicit none
    type(MATRIX_TYPE_FULL), intent(in) :: mat
    type(CAT(MATRIX_TYPE_NAME,4x3)) :: res

#if (MATRIX_TYPE_NCOL < 4) && (MATRIX_TYPE_NROW < 3)
    res%data(1:MATRIX_TYPE_NROW,1:MATRIX_TYPE_NCOL) = mat%data(:,:)
#elif MATRIX_TYPE_NCOL < 4
    res%data(:,1:MATRIX_TYPE_NCOL) = mat%data(1:3,:)
#elif MATRIX_TYPE_NROW < 3
    res%data(1:MATRIX_TYPE_NROW,:) = mat%data(:,1:4)
#else
    res%data(:,:) = mat%data(1:3,1:4)
#endif
end function
#endif

#if (MATRIX_TYPE_NCOL != 4) || (MATRIX_TYPE_NROW != 4)
pure function CAT(MATRIX_TYPE_FULL,_to_mat4x4)(mat) result(res)
    implicit none
    type(MATRIX_TYPE_FULL), intent(in) :: mat
    type(CAT(MATRIX_TYPE_NAME,4x4)) :: res

#if (MATRIX_TYPE_NCOL < 4) && (MATRIX_TYPE_NROW < 4)
    res%data(1:MATRIX_TYPE_NROW,1:MATRIX_TYPE_NCOL) = mat%data(:,:)
#elif MATRIX_TYPE_NCOL < 4
    res%data(:,1:MATRIX_TYPE_NCOL) = mat%data(1:4,:)
#elif MATRIX_TYPE_NROW < 4
    res%data(1:MATRIX_TYPE_NROW,:) = mat%data(:,1:4)
#else
    res%data(:,:) = mat%data(1:4,1:4)
#endif
end function
#endif

