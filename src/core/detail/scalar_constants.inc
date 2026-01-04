pure function pi()
    implicit none
    real(kind=real32) :: pi

    pi = acos(-1.0_real32)
end function

pure function dpi()
    implicit none
    real(kind=real64) :: dpi

    dpi = acos(-1.0_real64)
end function

