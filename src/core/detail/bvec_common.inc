elemental function CAT(mix_,VECTOR_TYPE_FULL)(x, y, a) result(res)
    implicit none
    type(VECTOR_TYPE_FULL), intent(in) :: x, y, a
    type(VECTOR_TYPE_FULL) :: res

    res%data = merge(y%data, x%data, a%data)
end function

elemental function CAT3(mix_,VECTOR_TYPE_FULL,_scalar)(x, y, a) result(res)
    implicit none
    type(VECTOR_TYPE_FULL), intent(in) :: x, y
    logical, intent(in) :: a
    type(VECTOR_TYPE_FULL) :: res

    res%data = merge(y%data, x%data, a)
end function

