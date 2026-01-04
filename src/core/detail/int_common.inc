pure function CAT(intBitsToFloat_,SCALAR_TYPE_KIND)(v) result(res)
    implicit none
    SCALAR_TYPE_FULL, intent(in) :: v
    real(kind=CAT(real,SCALAR_TYPE_BIT_SIZE)) :: res

    res = transfer(v, res)
end function

