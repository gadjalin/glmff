elemental function CAT(sign_,SCALAR_TYPE_KIND)(x) result(res)
    implicit none
    SCALAR_TYPE_FULL, intent(in) :: x
    SCALAR_TYPE_FULL :: res
    SCALAR_TYPE_FULL, parameter :: one = CAT(1_,SCALAR_TYPE_KIND)

    res = sign(one, x)
end function

elemental function CAT(clamp_,SCALAR_TYPE_KIND)(x, min_val, max_val) result(res)
    implicit none
    SCALAR_TYPE_FULL, intent(in) :: x, min_val, max_val
    SCALAR_TYPE_FULL :: res

    res = min(max(x, min_val), max_val)
end function

elemental function CAT3(mix_,SCALAR_TYPE_KIND,_bool)(x, y, a) result(res)
    implicit none
    SCALAR_TYPE_FULL, intent(in) :: x, y
    logical, intent(in) :: a
    SCALAR_TYPE_FULL :: res

    res = merge(y, x, a)
end function

elemental function CAT(fma_,SCALAR_TYPE_KIND)(a, b, c) result(res)
    implicit none
    SCALAR_TYPE_FULL, intent(in) :: a, b, c
    SCALAR_TYPE_FULL :: res

    res = a*b + c
end function

