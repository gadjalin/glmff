#if VECTOR_TYPE_SIZE == 2
pure function CAT(VECTOR_TYPE_FULL,_scalar)(x, y) result(vec)
    implicit none
    SCALAR_TYPE_FULL, intent(in) :: x, y
    type(VECTOR_TYPE_FULL) :: vec

    vec%data = [x, y]
end function
#elif VECTOR_TYPE_SIZE == 3
pure function CAT(VECTOR_TYPE_FULL,_scalar)(x, y, z) result(vec)
    implicit none
    SCALAR_TYPE_FULL, intent(in) :: x, y, z
    type(VECTOR_TYPE_FULL) :: vec

    vec%data = [x, y, z]
end function
#elif VECTOR_TYPE_SIZE == 4
pure function CAT(VECTOR_TYPE_FULL,_scalar)(x, y, z, w) result(vec)
    implicit none
    SCALAR_TYPE_FULL, intent(in) :: x, y, z, w
    type(VECTOR_TYPE_FULL) :: vec

    vec%data = [x, y, z, w]
end function
#endif

pure function CAT(VECTOR_TYPE_FULL,_array)(data) result(vec)
    implicit none
    SCALAR_TYPE_FULL, dimension(VECTOR_TYPE_SIZE), intent(in) :: data
    type(VECTOR_TYPE_FULL) :: vec

    vec%data = data
end function

