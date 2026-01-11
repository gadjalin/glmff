pure function CAT(intBitsToFloat_,SCALAR_TYPE_KIND)(x) result(res)
    implicit none
    SCALAR_TYPE_FULL, intent(in) :: x
    real(kind=CAT(real,SCALAR_TYPE_BIT_SIZE)) :: res

    res = transfer(x, res)
end function

