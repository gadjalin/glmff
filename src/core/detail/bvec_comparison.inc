elemental logical function CAT(eq_,VECTOR_TYPE_FULL)(a, b) result(res)
    implicit none
    type(VECTOR_TYPE_FULL), intent(in) :: a, b

    res = all(a%data .eqv. b%data)
end function

elemental logical function CAT(ne_,VECTOR_TYPE_FULL)(a, b) result(res)
    implicit none
    type(VECTOR_TYPE_FULL), intent(in) :: a, b

    res = any(a%data .neqv. b%data)
end function

