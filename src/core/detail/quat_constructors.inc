pure function CAT(QUAT_TYPE_FULL,_scalar)(w, x, y, z) result(qua)
    implicit none
    SCALAR_TYPE_FULL, intent(in) :: w, x, y, z
    type(QUAT_TYPE_FULL) :: qua

    qua%w = w
    qua%v = [x, y, z]
end function

pure function CAT(QUAT_TYPE_FULL,_scalar_vec)(w, v) result(qua)
    implicit none
    SCALAR_TYPE_FULL, intent(in) :: w
    type(CAT(VECTOR_TYPE_NAME,3)), intent(in) :: v
    type(QUAT_TYPE_FULL) :: qua

    qua%w = w
    qua%v = v%data
end function

pure function CAT(QUAT_TYPE_FULL,_vec_vec)(u, v) result(qua)
    implicit none
    type(CAT(VECTOR_TYPE_NAME,3)), intent(in) :: u, v
    type(QUAT_TYPE_FULL) :: qua
    SCALAR_TYPE_FULL :: norm_u_norm_v, real_part
    type(CAT(VECTOR_TYPE_NAME,3)) :: t

    SCALAR_TYPE_FULL, parameter :: zero = CAT(0_,SCALAR_TYPE_KIND)

    norm_u_norm_v = sqrt(dot(u, u) * dot(v, v))
    real_part = norm_u_norm_v + dot(u, v);

    if (real_part < (1.0d-6 * norm_u_norm_v)) then
        real_part = zero
        if (abs(u%data(1)) > abs(u%data(3))) then
            t%data = [-u%data(2), u%data(1), zero]
        else
            t%data = [zero, -u%data(3), u%data(2)]
        end if
    else
        t = cross(u, v)
    end if

    qua%w = real_part
    qua%v = t%data
    qua = normalize(qua);
end function

pure function CAT(QUAT_TYPE_FULL,_euler)(angles) result(qua)
    implicit none
    type(CAT(VECTOR_TYPE_NAME,3)), intent(in) :: angles
    type(CAT(VECTOR_TYPE_NAME,3)) :: c, s
    type(QUAT_TYPE_FULL) :: qua

    SCALAR_TYPE_FULL, parameter :: half= CAT(0.5_,SCALAR_TYPE_KIND)

    c%data = cos(angles%data * half)
    s%data = sin(angles%data * half)

    qua%w = c%data(1) * c%data(2) * c%data(3) + s%data(1) * s%data(2) * s%data(3)
    qua%v(1) = s%data(1) * c%data(2) * c%data(3) - c%data(1) * s%data(2) * s%data(3)
    qua%v(2) = c%data(1) * s%data(2) * c%data(3) + s%data(1) * c%data(2) * s%data(3)
    qua%v(3) = c%data(1) * c%data(2) * s%data(3) - s%data(1) * s%data(2) * c%data(3)
end function

function CAT(QUAT_TYPE_FULL,_mat3)(mat) result(qua)
    implicit none
    type(CAT(MATRIX_TYPE_NAME,3x3)), intent(in) :: mat
    type(QUAT_TYPE_FULL) :: qua

    SCALAR_TYPE_FULL, dimension(4) :: four_square_minus_one
    SCALAR_TYPE_FULL :: max_val, mult
    integer :: max_loc

    SCALAR_TYPE_FULL, parameter :: one = CAT(1_,SCALAR_TYPE_KIND)
    SCALAR_TYPE_FULL, parameter :: half = CAT(0.5_,SCALAR_TYPE_KIND)
    SCALAR_TYPE_FULL, parameter :: quarter = CAT(0.25_,SCALAR_TYPE_KIND)

    four_square_minus_one(1) = mat%data(1,1) - mat%data(2,2) - mat%data(3,3);
    four_square_minus_one(2) = mat%data(2,2) - mat%data(1,1) - mat%data(3,3);
    four_square_minus_one(3) = mat%data(3,3) - mat%data(1,1) - mat%data(2,2);
    four_square_minus_one(4) = mat%data(1,1) + mat%data(2,2) + mat%data(3,3);

    max_loc = maxloc(four_square_minus_one, dim=1)
    max_val = sqrt(four_square_minus_one(max_loc) + one) * half
    mult = quarter / max_val

    select case (max_loc)
    case (1)
        qua%w = max_val
        qua%v = [ (mat%data(3,2) - mat%data(2,3)) * mult, &
                  (mat%data(1,3) - mat%data(3,1)) * mult, &
                  (mat%data(2,1) - mat%data(1,2)) * mult ]
    case (2)
        qua%w = (mat%data(3,2) - mat%data(2,3)) * mult
        qua%v = [  max_val, &
                  (mat%data(2,1) + mat%data(1,2)) * mult, &
                  (mat%data(1,3) + mat%data(3,1)) * mult ]
    case (3)
        qua%w = (mat%data(1,3) - mat%data(3,1)) * mult
        qua%v = [ (mat%data(2,1) + mat%data(1,2)) * mult, &
                   max_val, &
                  (mat%data(3,2) + mat%data(2,3)) * mult ]
    case (4)
        qua%w = (mat%data(2,1) - mat%data(1,2)) * mult
        qua%v = [ (mat%data(1,3) + mat%data(3,1)) * mult, &
                  (mat%data(3,2) + mat%data(2,3)) * mult, &
                   max_val ]
    case default
        error stop
    end select
end function

function CAT(QUAT_TYPE_FULL,_mat4)(mat) result(qua)
    implicit none
    type(CAT(MATRIX_TYPE_NAME,4x4)), intent(in) :: mat
    type(CAT(MATRIX_TYPE_NAME,3x3)) :: tmp
    type(QUAT_TYPE_FULL) :: qua

    tmp%data = mat%data(1:3,1:3)
    qua = CAT(QUAT_TYPE_FULL,_mat3)(tmp)
end function

