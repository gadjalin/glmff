pure function CAT(length_,SCALAR_TYPE_KIND)(x) result(res)
    implicit none
    SCALAR_TYPE_FULL, intent(in) :: x
    SCALAR_TYPE_FULL :: res

    res = abs(x)
end function

pure function CAT(distance_,SCALAR_TYPE_KIND)(p0, p1) result(res)
    implicit none
    SCALAR_TYPE_FULL, intent(in) :: p0, p1
    SCALAR_TYPE_FULL :: res

    res = abs(p1 - p0)
end function

pure function CAT(dot_,SCALAR_TYPE_KIND)(x, y) result(res)
    implicit none
    SCALAR_TYPE_FULL, intent(in) :: x, y
    SCALAR_TYPE_FULL :: res

    res = x * y
end function

pure function CAT(faceforward_,SCALAR_TYPE_KIND)(N, I, Nref) result(res)
    implicit none
    SCALAR_TYPE_FULL, intent(in) :: N, I, Nref
    SCALAR_TYPE_FULL :: res

    SCALAR_TYPE_FULL, parameter :: zero = CAT(0_,SCALAR_TYPE_KIND)

    res = merge(N, -N, (Nref * I) < zero)
end function

pure function CAT(reflect_,SCALAR_TYPE_KIND)(I, N) result(res)
    implicit none
    SCALAR_TYPE_FULL, intent(in) :: I, N
    SCALAR_TYPE_FULL :: res

    SCALAR_TYPE_FULL, parameter :: two = CAT(2_,SCALAR_TYPE_KIND)

    res = I - N * (N * I) * two
end function

pure function CAT(refract_,SCALAR_TYPE_KIND)(I, N, eta) result(res)
    implicit none
    SCALAR_TYPE_FULL, intent(in) :: I, N
    SCALAR_TYPE_FULL, intent(in) :: eta
    SCALAR_TYPE_FULL :: res

    SCALAR_TYPE_FULL :: dot_value, k
    SCALAR_TYPE_FULL, parameter :: zero = CAT(0_,SCALAR_TYPE_KIND)
    SCALAR_TYPE_FULL, parameter :: one = CAT(1_,SCALAR_TYPE_KIND)

    dot_value = N * I
    k = one - eta*eta*(one - dot_value*dot_value)
    res = merge(eta*I - (eta*dot_value + sqrt(k))*N, zero, k >= zero)
end function

