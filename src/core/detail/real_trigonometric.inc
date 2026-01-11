elemental function CAT(radians_,SCALAR_TYPE_KIND)(deg) result(rad)
    implicit none
    SCALAR_TYPE_FULL, intent(in) :: deg
    SCALAR_TYPE_FULL :: rad

    SCALAR_TYPE_FULL, parameter :: pi = acos(CAT(-1.0_,SCALAR_TYPE_KIND))
    SCALAR_TYPE_FULL, parameter :: pi_over_180 = pi / CAT(180_,SCALAR_TYPE_KIND)

    rad = deg * pi_over_180
end function

elemental function CAT(degrees_,SCALAR_TYPE_KIND)(rad) result(deg)
    implicit none
    SCALAR_TYPE_FULL, intent(in) :: rad
    SCALAR_TYPE_FULL :: deg

    SCALAR_TYPE_FULL, parameter :: pi = acos(CAT(-1.0_,SCALAR_TYPE_KIND))
    SCALAR_TYPE_FULL, parameter :: ipi_over_180 = CAT(180_,SCALAR_TYPE_KIND) / pi

    deg = rad * ipi_over_180
end function

