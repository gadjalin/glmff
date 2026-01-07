pure function CAT(translate_,MATRIX_TYPE_NAME)(mat, vec) result(res)
    implicit none
    type(CAT(MATRIX_TYPE_NAME,4x4)), intent(in) :: mat
    type(CAT(VECTOR_TYPE_NAME,3)), intent(in) :: vec
    type(CAT(MATRIX_TYPE_NAME,4x4)) :: res

    res = mat
    res%data(:,4) = mat%data(:,1)*vec%data(1) + mat%data(:,2)*vec%data(2) &
                  + mat%data(:,3)*vec%data(3) + mat%data(:,4)
end function

pure function CAT(rotate_,MATRIX_TYPE_NAME)(mat, angle, axis) result(res)
    implicit none
    type(CAT(MATRIX_TYPE_NAME,4x4)), intent(in) :: mat
    real(kind=SCALAR_TYPE_KIND), intent(in) :: angle
    type(CAT(VECTOR_TYPE_NAME,3)), intent(in) :: axis
    type(CAT(MATRIX_TYPE_NAME,4x4)) :: res

    real(kind=SCALAR_TYPE_KIND) :: c, s
    type(CAT(VECTOR_TYPE_NAME,3)) :: u, tmp
    type(CAT(MATRIX_TYPE_NAME,4x4)) :: rot
    real(kind=SCALAR_TYPE_KIND), parameter :: one = CAT(1_,SCALAR_TYPE_KIND)

    c = cos(angle)
    s = sin(angle)
    u = normalize(axis)
    tmp = ((one - c) * u)

    rot%data(1,1) = c + tmp%data(1) * u%data(1)
    rot%data(2,1) =     tmp%data(1) * u%data(2) + s * u%data(3)
    rot%data(3,1) =     tmp%data(1) * u%data(3) - s * u%data(2)

    rot%data(1,2) =     tmp%data(2) * u%data(1) - s * u%data(3)
    rot%data(2,2) = c + tmp%data(2) * u%data(2)
    rot%data(3,2) =     tmp%data(2) * u%data(3) + s * u%data(1)

    rot%data(1,3) =     tmp%data(3) * u%data(1) + s * u%data(2)
    rot%data(2,3) =     tmp%data(3) * u%data(2) - s * u%data(1)
    rot%data(3,3) = c + tmp%data(3) * u%data(3)

    res%data = matmul(mat%data, rot%data)
end function

pure function CAT(scale_,MATRIX_TYPE_NAME)(mat, vec) result(res)
    implicit none
    type(CAT(MATRIX_TYPE_NAME,4x4)), intent(in) :: mat
    type(CAT(VECTOR_TYPE_NAME,3)), intent(in) :: vec
    type(CAT(MATRIX_TYPE_NAME,4x4)) :: res

    res%data(:,1) = mat%data(:,1)*vec%data(1)
    res%data(:,2) = mat%data(:,2)*vec%data(2)
    res%data(:,3) = mat%data(:,3)*vec%data(3)
    res%data(:,4) = mat%data(:,4)
end function

pure function CAT(shear_,MATRIX_TYPE_NAME)(mat, vec, lx, ly, lz) result(res)
    implicit none
    type(CAT(MATRIX_TYPE_NAME,4x4)), intent(in) :: mat
    type(CAT(VECTOR_TYPE_NAME,3)), intent(in) :: vec
    type(CAT(VECTOR_TYPE_NAME,2)), intent(in) :: lx, ly, lz
    type(CAT(MATRIX_TYPE_NAME,4x4)) :: res

    real(kind=SCALAR_TYPE_KIND), dimension(3) :: p
    real(kind=SCALAR_TYPE_KIND), dimension(4,4) :: sh
    real(kind=SCALAR_TYPE_KIND), parameter :: zero = CAT(0_,SCALAR_TYPE_KIND)
    real(kind=SCALAR_TYPE_KIND), parameter :: one = CAT(1_,SCALAR_TYPE_KIND)

    p = [sum(lx%data), sum(ly%data), sum(lz%data)]
    sh = reshape( &
        [one,        lx%data(1), lx%data(2), -p(1) * vec%data(1), &
         ly%data(1), one,        ly%data(2), -p(2) * vec%data(2), &
         lz%data(1), lz%data(2), one,        -p(3) * vec%data(3), &
         zero,       zero,       zero,       one], [4,4])

    res%data = matmul(mat%data, sh)
end function

pure function CAT(lookAtRH_,MATRIX_TYPE_NAME)(eye, centre, up) result(res)
    implicit none
    type(CAT(VECTOR_TYPE_NAME,3)), intent(in) :: eye, centre, up
    type(CAT(MATRIX_TYPE_NAME,4x4)) :: res

    type(CAT(VECTOR_TYPE_NAME,3)) :: f, s, u

    f = normalize(centre - eye)
    s = normalize(cross(f, up))
    u = cross(s, f)

    res%data(1,1) =  s%data(1)
    res%data(2,1) =  u%data(1)
    res%data(3,1) = -f%data(1)
    res%data(1,2) =  s%data(2)
    res%data(2,2) =  u%data(2)
    res%data(3,2) = -f%data(2)
    res%data(1,3) =  s%data(3)
    res%data(2,3) =  u%data(3)
    res%data(3,3) = -f%data(3)
    res%data(1,4) = -dot(s, eye)
    res%data(2,4) = -dot(u, eye)
    res%data(3,4) =  dot(f, eye)
end function

pure function CAT(lookAtLH_,MATRIX_TYPE_NAME)(eye, centre, up) result(res)
    implicit none
    type(CAT(VECTOR_TYPE_NAME,3)), intent(in) :: eye, centre, up
    type(CAT(MATRIX_TYPE_NAME,4x4)) :: res

    type(CAT(VECTOR_TYPE_NAME,3)) :: f, s, u

    f = normalize(centre - eye)
    s = normalize(cross(up, f))
    u = cross(f, s)

    res%data(1,1) = s%data(1)
    res%data(2,1) = u%data(1)
    res%data(3,1) = f%data(1)
    res%data(1,2) = s%data(2)
    res%data(2,2) = u%data(2)
    res%data(3,2) = f%data(2)
    res%data(1,3) = s%data(3)
    res%data(2,3) = u%data(3)
    res%data(3,3) = f%data(3)
    res%data(1,4) = -dot(s, eye)
    res%data(2,4) = -dot(u, eye)
    res%data(3,4) = -dot(f, eye)
end function

pure function CAT(lookAt_,MATRIX_TYPE_NAME)(eye, centre, up) result(res)
    implicit none
    type(CAT(VECTOR_TYPE_NAME,3)), intent(in) :: eye, centre, up
    type(CAT(MATRIX_TYPE_NAME,4x4)) :: res

#if GLMFF_CONFIG_CLIP_CONTROL == GLMFF_CLIP_CONTROL_LH_ZO
    res = CAT(lookAtLH_,MATRIX_TYPE_NAME)(eye, centre, up)
#elif GLMFF_CONFIG_CLIP_CONTROL == GLMFF_CLIP_CONTROL_LH_NO
    res = CAT(lookAtLH_,MATRIX_TYPE_NAME)(eye, centre, up)
#elif GLMFF_CONFIG_CLIP_CONTROL == GLMFF_CLIP_CONTROL_RH_ZO
    res = CAT(lookAtRH_,MATRIX_TYPE_NAME)(eye, centre, up)
#elif GLMFF_CONFIG_CLIP_CONTROL == GLMFF_CLIP_CONTROL_RH_NO
    res = CAT(lookAtRH_,MATRIX_TYPE_NAME)(eye, centre, up)
#endif
end function

