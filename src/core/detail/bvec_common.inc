pure function CAT(mix_,VECTOR_TYPE_FULL)(v1, v2, a) result(res)
    implicit none
    type(VECTOR_TYPE_FULL), intent(in) :: v1, v2, a
    type(VECTOR_TYPE_FULL) :: res

    res%data = merge(v2%data, v1%data, a%data)
end function

pure function CAT3(mix_,VECTOR_TYPE_FULL,_scalar)(v1, v2, a) result(res)
    implicit none
    type(VECTOR_TYPE_FULL), intent(in) :: v1, v2
    logical, intent(in) :: a
    type(VECTOR_TYPE_FULL) :: res

    res%data = merge(v2%data, v1%data, a)
end function

