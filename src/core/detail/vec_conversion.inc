#if VECTOR_TYPE_SIZE != 2
pure function CAT(VECTOR_TYPE_FULL,_to_vec2)(vec) result(res)
    implicit none
    type(VECTOR_TYPE_FULL), intent(in) :: vec
    type(CAT(VECTOR_TYPE_NAME,2)) :: res

#if VECTOR_TYPE_SIZE < 2
    res%data(1:VECTOR_TYPE_SIZE) = vec%data
#elif VECTOR_TYPE_SIZE > 2
    res%data = vec%data(1:2)
#endif
end function
#endif

#if VECTOR_TYPE_SIZE != 3
pure function CAT(VECTOR_TYPE_FULL,_to_vec3)(vec) result(res)
    implicit none
    type(VECTOR_TYPE_FULL), intent(in) :: vec
    type(CAT(VECTOR_TYPE_NAME,3)) :: res

#if VECTOR_TYPE_SIZE < 3
    res%data(1:VECTOR_TYPE_SIZE) = vec%data
#elif VECTOR_TYPE_SIZE > 3
    res%data = vec%data(1:3)
#endif
end function
#endif

#if VECTOR_TYPE_SIZE != 4
pure function CAT(VECTOR_TYPE_FULL,_to_vec4)(vec) result(res)
    implicit none
    type(VECTOR_TYPE_FULL), intent(in) :: vec
    type(CAT(VECTOR_TYPE_NAME,4)) :: res

#if VECTOR_TYPE_SIZE < 4
    res%data(1:VECTOR_TYPE_SIZE) = vec%data
#elif VECTOR_TYPE_SIZE > 4
    res%data = vec%data(1:4)
#endif
end function
#endif

