elemental function CAT(min_,VECTOR_TYPE_FULL)(x, y) result(res)
    implicit none
    type(VECTOR_TYPE_FULL), intent(in) :: x, y
    type(VECTOR_TYPE_FULL) :: res

    res%data = min(x%data, y%data)
end function

elemental function CAT3(min_,VECTOR_TYPE_FULL,_scalar)(x, y) result(res)
    implicit none
    type(VECTOR_TYPE_FULL), intent(in) :: x
    SCALAR_TYPE_FULL, intent(in) :: y
    type(VECTOR_TYPE_FULL) :: res

    res%data = min(x%data, y)
end function

elemental function CAT(max_,VECTOR_TYPE_FULL)(x, y) result(res)
    implicit none
    type(VECTOR_TYPE_FULL), intent(in) :: x, y
    type(VECTOR_TYPE_FULL) :: res

    res%data = max(x%data, y%data)
end function

elemental function CAT3(max_,VECTOR_TYPE_FULL,_scalar)(x, y) result(res)
    implicit none
    type(VECTOR_TYPE_FULL), intent(in) :: x
    SCALAR_TYPE_FULL, intent(in) :: y
    type(VECTOR_TYPE_FULL) :: res

    res%data = max(x%data, y)
end function

elemental function CAT(abs_,VECTOR_TYPE_FULL)(x) result(res)
    implicit none
    type(VECTOR_TYPE_FULL), intent(in) :: x
    type(VECTOR_TYPE_FULL) :: res

    res%data = abs(x%data)
end function

elemental function CAT(sign_,VECTOR_TYPE_FULL)(x) result(res)
    implicit none
    type(VECTOR_TYPE_FULL), intent(in) :: x
    type(VECTOR_TYPE_FULL) :: res

    SCALAR_TYPE_FULL, parameter :: one = CAT(1_,SCALAR_TYPE_KIND)

    res%data = sign(one, x%data)
end function

elemental function CAT(mod_,VECTOR_TYPE_FULL)(x, y) result(res)
    implicit none
    type(VECTOR_TYPE_FULL), intent(in) :: x, y
    type(VECTOR_TYPE_FULL) :: res

    res%data = modulo(x%data, y%data)
end function

elemental function CAT(clamp_,VECTOR_TYPE_FULL)(x, min_val, max_val) result(res)
    implicit none
    type(VECTOR_TYPE_FULL), intent(in) :: x, min_val, max_val
    type(VECTOR_TYPE_FULL) :: res

    res%data = min(max(x%data, min_val%data), max_val%data)
end function

elemental function CAT3(clamp_,VECTOR_TYPE_FULL,_scalar_max)(x, min_val, max_val) result(res)
    implicit none
    type(VECTOR_TYPE_FULL), intent(in) :: x, min_val
    SCALAR_TYPE_FULL, intent(in) :: max_val
    type(VECTOR_TYPE_FULL) :: res

    res%data = min(max(x%data, min_val%data), max_val)
end function

elemental function CAT3(clamp_,VECTOR_TYPE_FULL,_scalar_min)(x, min_val, max_val) result(res)
    implicit none
    type(VECTOR_TYPE_FULL), intent(in) :: x, max_val
    SCALAR_TYPE_FULL, intent(in) :: min_val
    type(VECTOR_TYPE_FULL) :: res

    res%data = min(max(x%data, min_val), max_val%data)
end function

elemental function CAT3(clamp_,VECTOR_TYPE_FULL,_scalar)(x, min_val, max_val) result(res)
    implicit none
    type(VECTOR_TYPE_FULL), intent(in) :: x
    SCALAR_TYPE_FULL, intent(in) :: min_val, max_val
    type(VECTOR_TYPE_FULL) :: res

    res%data = min(max(x%data, min_val), max_val)
end function

elemental function CAT3(mix_,VECTOR_TYPE_FULL,_bool)(x, y, a) result(res)
    implicit none
    type(VECTOR_TYPE_FULL), intent(in) :: x, y
    logical, intent(in) :: a
    type(VECTOR_TYPE_FULL) :: res

    res%data = merge(y%data, x%data, a)
end function

elemental function CAT3(mix_,VECTOR_TYPE_FULL,_bvec)(x, y, a) result(res)
    implicit none
    type(VECTOR_TYPE_FULL), intent(in) :: x, y
    type(CAT(bvec,VECTOR_TYPE_SIZE)), intent(in) :: a
    type(VECTOR_TYPE_FULL) :: res

    res%data = merge(y%data, x%data, a%data)
end function

