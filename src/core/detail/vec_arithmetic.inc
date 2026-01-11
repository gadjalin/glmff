elemental function CAT(pos_,VECTOR_TYPE_FULL)(x) result(res)
    implicit none
    type(VECTOR_TYPE_FULL), intent(in) :: x
    type(VECTOR_TYPE_FULL) :: res

    res = x
end function

elemental function CAT(add_,VECTOR_TYPE_FULL)(a, b) result(res)
    implicit none
    type(VECTOR_TYPE_FULL), intent(in) :: a, b
    type(VECTOR_TYPE_FULL) :: res

    res%data = a%data + b%data
end function

elemental function CAT(add_scalar_,VECTOR_TYPE_FULL)(s, b) result(res)
    implicit none
    SCALAR_TYPE_FULL, intent(in) :: s
    type(VECTOR_TYPE_FULL), intent(in) :: b
    type(VECTOR_TYPE_FULL) :: res

    res%data = s + b%data
end function

elemental function CAT3(add_,VECTOR_TYPE_FULL,_scalar)(a, s) result(res)
    implicit none
    type(VECTOR_TYPE_FULL), intent(in) :: a
    SCALAR_TYPE_FULL, intent(in) :: s
    type(VECTOR_TYPE_FULL) :: res

    res%data = a%data + s
end function

elemental function CAT(neg_,VECTOR_TYPE_FULL)(x) result(res)
    implicit none
    type(VECTOR_TYPE_FULL), intent(in) :: x
    type(VECTOR_TYPE_FULL) :: res

    res%data = -x%data
end function

elemental function CAT(sub_,VECTOR_TYPE_FULL)(a, b) result(res)
    implicit none
    type(VECTOR_TYPE_FULL), intent(in) :: a, b
    type(VECTOR_TYPE_FULL) :: res

    res%data = a%data - b%data
end function

elemental function CAT(sub_scalar_,VECTOR_TYPE_FULL)(s, b) result(res)
    implicit none
    SCALAR_TYPE_FULL, intent(in) :: s
    type(VECTOR_TYPE_FULL), intent(in) :: b
    type(VECTOR_TYPE_FULL) :: res

    res%data = s - b%data
end function

elemental function CAT3(sub_,VECTOR_TYPE_FULL,_scalar)(a, s) result(res)
    implicit none
    type(VECTOR_TYPE_FULL), intent(in) :: a
    SCALAR_TYPE_FULL, intent(in) :: s
    type(VECTOR_TYPE_FULL) :: res

    res%data = a%data - s
end function

elemental function CAT(mul_,VECTOR_TYPE_FULL)(a, b) result(res)
    implicit none
    type(VECTOR_TYPE_FULL), intent(in) :: a, b
    type(VECTOR_TYPE_FULL) :: res

    res%data = a%data * b%data
end function

elemental function CAT(mul_scalar_,VECTOR_TYPE_FULL)(s, b) result(res)
    implicit none
    SCALAR_TYPE_FULL, intent(in) :: s
    type(VECTOR_TYPE_FULL), intent(in) :: b
    type(VECTOR_TYPE_FULL) :: res

    res%data = s * b%data
end function

elemental function CAT3(mul_,VECTOR_TYPE_FULL,_scalar)(a, s) result(res)
    implicit none
    type(VECTOR_TYPE_FULL), intent(in) :: a
    SCALAR_TYPE_FULL, intent(in) :: s
    type(VECTOR_TYPE_FULL) :: res

    res%data = a%data * s
end function

elemental function CAT(div_,VECTOR_TYPE_FULL)(a, b) result(res)
    implicit none
    type(VECTOR_TYPE_FULL), intent(in) :: a, b
    type(VECTOR_TYPE_FULL) :: res

    res%data = a%data / b%data
end function

elemental function CAT(div_scalar_,VECTOR_TYPE_FULL)(s, b) result(res)
    implicit none
    SCALAR_TYPE_FULL, intent(in) :: s
    type(VECTOR_TYPE_FULL), intent(in) :: b
    type(VECTOR_TYPE_FULL) :: res

    res%data = s / b%data
end function

elemental function CAT3(div_,VECTOR_TYPE_FULL,_scalar)(a, s) result(res)
    implicit none
    type(VECTOR_TYPE_FULL), intent(in) :: a
    SCALAR_TYPE_FULL, intent(in) :: s
    type(VECTOR_TYPE_FULL) :: res

    res%data = a%data / s
end function

