pure function CAT(length_,VECTOR_TYPE_FULL)(x) result(length)
    implicit none
    type(VECTOR_TYPE_FULL), intent(in) :: x
    real(kind=CAT(real,SCALAR_TYPE_BIT_SIZE)):: length

    length = sqrt(dot_product(x%data, x%data))
end function

pure function CAT(distance_,VECTOR_TYPE_FULL)(v0, v1) result(distance)
    implicit none
    type(VECTOR_TYPE_FULL), intent(in) :: v0, v1
    real(kind=CAT(real,SCALAR_TYPE_BIT_SIZE)) :: distance

    distance = length(v1 - v0)
end function

pure function CAT(dot_,VECTOR_TYPE_FULL)(x, y) result(res)
    implicit none
    type(VECTOR_TYPE_FULL), intent(in) :: x, y
    SCALAR_TYPE_FULL :: res

    res = dot_product(x%data, y%data)
end function

#if VECTOR_TYPE_SIZE == 3
pure function CAT(cross_,VECTOR_TYPE_FULL)(x, y) result(res)
    implicit none
    type(VECTOR_TYPE_FULL), intent(in) :: x, y
    type(VECTOR_TYPE_FULL) :: res

    res%data = [ &
        x%data(2)*y%data(3) - x%data(3)*y%data(2), &
        x%data(3)*y%data(1) - x%data(1)*y%data(3), &
        x%data(1)*y%data(2) - x%data(2)*y%data(1) ]
end function
#endif

pure function CAT(normalize_,VECTOR_TYPE_FULL)(v) result(res)
    implicit none
    type(VECTOR_TYPE_FULL), intent(in) :: v
    type(VECTOR_TYPE_FULL) :: res

    res%data = v%data / length(v)
end function

pure function CAT(faceforward_,VECTOR_TYPE_FULL)(N, I, Nref) result(res)
    implicit none
    type(VECTOR_TYPE_FULL), intent(in) :: N, I, Nref
    type(VECTOR_TYPE_FULL) :: res

    SCALAR_TYPE_FULL, parameter :: zero = CAT(0_,SCALAR_TYPE_KIND)

    res = merge(N, -N, dot_product(Nref%data, I%data) < zero)
end function

pure function CAT(reflect_,VECTOR_TYPE_FULL)(I, N) result(res)
    implicit none
    type(VECTOR_TYPE_FULL), intent(in) :: I, N
    type(VECTOR_TYPE_FULL) :: res

    SCALAR_TYPE_FULL, parameter :: two = CAT(2_,SCALAR_TYPE_KIND)

    res = I - N * dot_product(N%data, I%data) * two
end function

pure function CAT(refract_,VECTOR_TYPE_FULL)(I, N, eta) result(res)
    implicit none
    type(VECTOR_TYPE_FULL), intent(in) :: I, N
    SCALAR_TYPE_FULL, intent(in) :: eta
    type(VECTOR_TYPE_FULL) :: res

    SCALAR_TYPE_FULL :: dot_value, k
    SCALAR_TYPE_FULL, parameter :: zero = CAT(0_,SCALAR_TYPE_KIND)
    SCALAR_TYPE_FULL, parameter :: one = CAT(1_,SCALAR_TYPE_KIND)

    dot_value = dot_product(N%data, I%data)
    k = one - eta*eta*(one - dot_value*dot_value)
    res%data = merge(eta*I%data - (eta*dot_value + sqrt(k))*N%data, zero, k >= zero)
end function

