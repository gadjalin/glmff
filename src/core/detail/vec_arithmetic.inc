pure function CAT(pos_,VECTOR_TYPE_FULL)(vec) result(res)
    implicit none
    type(VECTOR_TYPE_FULL), intent(in) :: vec
    type(VECTOR_TYPE_FULL) :: res

    res = vec
end function

pure function CAT(add_,VECTOR_TYPE_FULL)(lhs, rhs) result(res)
    implicit none
    type(VECTOR_TYPE_FULL), intent(in) :: lhs, rhs
    type(VECTOR_TYPE_FULL) :: res

    res%data = lhs%data + rhs%data
end function

pure function CAT(add_scalar_,VECTOR_TYPE_FULL)(scalar, rhs) result(res)
    implicit none
    SCALAR_TYPE_FULL, intent(in) :: scalar
    type(VECTOR_TYPE_FULL), intent(in) :: rhs
    type(VECTOR_TYPE_FULL) :: res

    res%data = scalar + rhs%data
end function

pure function CAT3(add_,VECTOR_TYPE_FULL,_scalar)(lhs, scalar) result(res)
    implicit none
    type(VECTOR_TYPE_FULL), intent(in) :: lhs
    SCALAR_TYPE_FULL, intent(in) :: scalar
    type(VECTOR_TYPE_FULL) :: res

    res%data = lhs%data + scalar
end function

pure function CAT(neg_,VECTOR_TYPE_FULL)(vec) result(res)
    implicit none
    type(VECTOR_TYPE_FULL), intent(in) :: vec
    type(VECTOR_TYPE_FULL) :: res

    res%data = -vec%data
end function

pure function CAT(sub_,VECTOR_TYPE_FULL)(lhs, rhs) result(res)
    implicit none
    type(VECTOR_TYPE_FULL), intent(in) :: lhs, rhs
    type(VECTOR_TYPE_FULL) :: res

    res%data = lhs%data - rhs%data
end function

pure function CAT(sub_scalar_,VECTOR_TYPE_FULL)(scalar, rhs) result(res)
    implicit none
    SCALAR_TYPE_FULL, intent(in) :: scalar
    type(VECTOR_TYPE_FULL), intent(in) :: rhs
    type(VECTOR_TYPE_FULL) :: res

    res%data = scalar - rhs%data
end function

pure function CAT3(sub_,VECTOR_TYPE_FULL,_scalar)(lhs, scalar) result(res)
    implicit none
    type(VECTOR_TYPE_FULL), intent(in) :: lhs
    SCALAR_TYPE_FULL, intent(in) :: scalar
    type(VECTOR_TYPE_FULL) :: res

    res%data = lhs%data - scalar
end function

pure function CAT(mul_,VECTOR_TYPE_FULL)(lhs, rhs) result(res)
    implicit none
    type(VECTOR_TYPE_FULL), intent(in) :: lhs, rhs
    type(VECTOR_TYPE_FULL) :: res

    res%data = lhs%data * rhs%data
end function

pure function CAT(mul_scalar_,VECTOR_TYPE_FULL)(scalar, rhs) result(res)
    implicit none
    SCALAR_TYPE_FULL, intent(in) :: scalar
    type(VECTOR_TYPE_FULL), intent(in) :: rhs
    type(VECTOR_TYPE_FULL) :: res

    res%data = scalar * rhs%data
end function

pure function CAT3(mul_,VECTOR_TYPE_FULL,_scalar)(lhs, scalar) result(res)
    implicit none
    type(VECTOR_TYPE_FULL), intent(in) :: lhs
    SCALAR_TYPE_FULL, intent(in) :: scalar
    type(VECTOR_TYPE_FULL) :: res

    res%data = lhs%data * scalar
end function

pure function CAT(div_,VECTOR_TYPE_FULL)(lhs, rhs) result(res)
    implicit none
    type(VECTOR_TYPE_FULL), intent(in) :: lhs, rhs
    type(VECTOR_TYPE_FULL) :: res

    res%data = lhs%data / rhs%data
end function

pure function CAT(div_scalar_,VECTOR_TYPE_FULL)(scalar, rhs) result(res)
    implicit none
    SCALAR_TYPE_FULL, intent(in) :: scalar
    type(VECTOR_TYPE_FULL), intent(in) :: rhs
    type(VECTOR_TYPE_FULL) :: res

    res%data = scalar / rhs%data
end function

pure function CAT3(div_,VECTOR_TYPE_FULL,_scalar)(lhs, scalar) result(res)
    implicit none
    type(VECTOR_TYPE_FULL), intent(in) :: lhs
    SCALAR_TYPE_FULL, intent(in) :: scalar
    type(VECTOR_TYPE_FULL) :: res

    res%data = lhs%data / scalar
end function

