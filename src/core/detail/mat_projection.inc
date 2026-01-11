elemental function CAT(projectZO_,MATRIX_TYPE_NAME)(obj, modelview, proj, viewport) result(res)
    implicit none
    type(CAT(VECTOR_TYPE_NAME,3)), intent(in) :: obj
    type(CAT(VECTOR_TYPE_NAME,4)), intent(in) :: viewport
    type(CAT(MATRIX_TYPE_NAME,4x4)), intent(in) :: modelview, proj
    type(CAT(VECTOR_TYPE_NAME,3)) :: res

    type(CAT(VECTOR_TYPE_NAME,4)) :: tmp
    real(kind=SCALAR_TYPE_KIND), parameter :: half = CAT(0.5_,SCALAR_TYPE_KIND)
    real(kind=SCALAR_TYPE_KIND), parameter :: one = CAT(1_,SCALAR_TYPE_KIND)

    tmp%data = [obj%data, one]
    tmp = modelview * tmp
    tmp = proj * tmp

    tmp%data = tmp%data / tmp%data(4)
    tmp%data(1) = tmp%data(1) * half + half
    tmp%data(2) = tmp%data(2) * half + half

    tmp%data(1) = tmp%data(1) * viewport%data(3) + viewport%data(1)
    tmp%data(2) = tmp%data(2) * viewport%data(4) + viewport%data(2)

    res%data = tmp%data(1:3)
end function

elemental function CAT(projectNO_,MATRIX_TYPE_NAME)(obj, modelview, proj, viewport) result(res)
    implicit none
    type(CAT(VECTOR_TYPE_NAME,3)), intent(in) :: obj
    type(CAT(VECTOR_TYPE_NAME,4)), intent(in) :: viewport
    type(CAT(MATRIX_TYPE_NAME,4x4)), intent(in) :: modelview, proj
    type(CAT(VECTOR_TYPE_NAME,3)) :: res

    type(CAT(VECTOR_TYPE_NAME,4)) :: tmp
    real(kind=SCALAR_TYPE_KIND), parameter :: half = CAT(0.5_,SCALAR_TYPE_KIND)
    real(kind=SCALAR_TYPE_KIND), parameter :: one = CAT(1_,SCALAR_TYPE_KIND)

    tmp%data = [obj%data, one]
    tmp = modelview * tmp
    tmp = proj * tmp

    tmp%data = tmp%data / tmp%data(4)
    tmp%data = tmp%data * half + half
    tmp%data(1) = tmp%data(1) * viewport%data(3) + viewport%data(1)
    tmp%data(2) = tmp%data(2) * viewport%data(4) + viewport%data(2)

    res%data = tmp%data(1:3)
end function

elemental function CAT(project_,MATRIX_TYPE_NAME)(obj, modelview, proj, viewport) result(res)
    implicit none
    type(CAT(VECTOR_TYPE_NAME,3)), intent(in) :: obj
    type(CAT(VECTOR_TYPE_NAME,4)), intent(in) :: viewport
    type(CAT(MATRIX_TYPE_NAME,4x4)), intent(in) :: modelview, proj
    type(CAT(VECTOR_TYPE_NAME,3)) :: res

#if GLMFF_CONFIG_CLIP_CONTROL == GLMFF_CLIP_CONTROL_LH_ZO
    res = CAT(projectZO_,MATRIX_TYPE_NAME)(obj, modelview, proj, viewport)
#elif GLMFF_CONFIG_CLIP_CONTROL == GLMFF_CLIP_CONTROL_RH_ZO
    res = CAT(projectZO_,MATRIX_TYPE_NAME)(obj, modelview, proj, viewport)
#elif GLMFF_CONFIG_CLIP_CONTROL == GLMFF_CLIP_CONTROL_LH_NO
    res = CAT(projectNO_,MATRIX_TYPE_NAME)(obj, modelview, proj, viewport)
#elif GLMFF_CONFIG_CLIP_CONTROL == GLMFF_CLIP_CONTROL_RH_NO
    res = CAT(projectNO_,MATRIX_TYPE_NAME)(obj, modelview, proj, viewport)
#endif
end function

elemental function CAT(unprojectZO_,MATRIX_TYPE_NAME)(win, modelview, proj, viewport) result(res)
    implicit none
    type(CAT(VECTOR_TYPE_NAME,3)), intent(in) :: win
    type(CAT(VECTOR_TYPE_NAME,4)), intent(in) :: viewport
    type(CAT(MATRIX_TYPE_NAME,4x4)), intent(in) :: modelview, proj
    type(CAT(VECTOR_TYPE_NAME,3)) :: res

    type(CAT(VECTOR_TYPE_NAME,4)) :: tmp, obj
    type(CAT(MATRIX_TYPE_NAME,4x4)) :: inv
    real(kind=SCALAR_TYPE_KIND), parameter :: one = CAT(1_,SCALAR_TYPE_KIND)
    real(kind=SCALAR_TYPE_KIND), parameter :: two = CAT(2_,SCALAR_TYPE_KIND)

    inv = inverse(proj * modelview)
    tmp%data = [win%data, one]

    tmp%data(1) = (tmp%data(1) - viewport%data(1)) / viewport%data(3)
    tmp%data(2) = (tmp%data(2) - viewport%data(2)) / viewport%data(4)
    tmp%data(1) =  tmp%data(1) * two - one
    tmp%data(2) =  tmp%data(2) * two - one

    obj = inv * tmp
    obj%data = obj%data / obj%data(4)

    res%data = obj%data(1:3)
end function

elemental function CAT(unprojectNO_,MATRIX_TYPE_NAME)(win, modelview, proj, viewport) result(res)
    implicit none
    type(CAT(VECTOR_TYPE_NAME,3)), intent(in) :: win
    type(CAT(VECTOR_TYPE_NAME,4)), intent(in) :: viewport
    type(CAT(MATRIX_TYPE_NAME,4x4)), intent(in) :: modelview, proj
    type(CAT(VECTOR_TYPE_NAME,3)) :: res

    type(CAT(VECTOR_TYPE_NAME,4)) :: tmp, obj
    type(CAT(MATRIX_TYPE_NAME,4x4)) :: inv
    real(kind=SCALAR_TYPE_KIND), parameter :: one = CAT(1_,SCALAR_TYPE_KIND)
    real(kind=SCALAR_TYPE_KIND), parameter :: two = CAT(2_,SCALAR_TYPE_KIND)

    inv = inverse(proj * modelview)
    tmp%data = [win%data, one]

    tmp%data(1) = (tmp%data(1) - viewport%data(1)) / viewport%data(3)
    tmp%data(2) = (tmp%data(2) - viewport%data(2)) / viewport%data(4)
    tmp%data =  tmp%data * two - one

    obj = inv * tmp
    obj%data = obj%data / obj%data(4)

    res%data = obj%data(1:3)
end function

elemental function CAT(unproject_,MATRIX_TYPE_NAME)(win, modelview, proj, viewport) result(res)
    implicit none
    type(CAT(VECTOR_TYPE_NAME,3)), intent(in) :: win
    type(CAT(VECTOR_TYPE_NAME,4)), intent(in) :: viewport
    type(CAT(MATRIX_TYPE_NAME,4x4)), intent(in) :: modelview, proj
    type(CAT(VECTOR_TYPE_NAME,3)) :: res

#if GLMFF_CONFIG_CLIP_CONTROL == GLMFF_CLIP_CONTROL_LH_ZO
    res = CAT(unprojectZO_,MATRIX_TYPE_NAME)(win, modelview, proj, viewport)
#elif GLMFF_CONFIG_CLIP_CONTROL == GLMFF_CLIP_CONTROL_RH_ZO
    res = CAT(unprojectZO_,MATRIX_TYPE_NAME)(win, modelview, proj, viewport)
#elif GLMFF_CONFIG_CLIP_CONTROL == GLMFF_CLIP_CONTROL_LH_NO
    res = CAT(unprojectNO_,MATRIX_TYPE_NAME)(win, modelview, proj, viewport)
#elif GLMFF_CONFIG_CLIP_CONTROL == GLMFF_CLIP_CONTROL_RH_NO
    res = CAT(unprojectNO_,MATRIX_TYPE_NAME)(win, modelview, proj, viewport)
#endif
end function

elemental function CAT(pickMatrix_,MATRIX_TYPE_NAME)(centre, delta, viewport) result(res)
    implicit none
    type(CAT(VECTOR_TYPE_NAME,2)), intent(in) :: centre, delta
    type(CAT(VECTOR_TYPE_NAME,4)), intent(in) :: viewport
    type(CAT(MATRIX_TYPE_NAME,4x4)) :: res

    type(CAT(VECTOR_TYPE_NAME,3)) :: tmp1, tmp2
    real(kind=SCALAR_TYPE_KIND), parameter :: zero = CAT(0_,SCALAR_TYPE_KIND)
    real(kind=SCALAR_TYPE_KIND), parameter :: one = CAT(1_,SCALAR_TYPE_KIND)
    real(kind=SCALAR_TYPE_KIND), parameter :: two = CAT(2_,SCALAR_TYPE_KIND)

    if (.not. (delta%data(1) > zero .and. delta%data(2) > zero)) return ! Error

    tmp1%data = [ &
        (viewport%data(3) - two * (centre%data(1) - viewport%data(1))) / delta%data(1), &
        (viewport%data(4) - two * (centre%data(2) - viewport%data(2))) / delta%data(2), &
        zero ]
    tmp2%data = [ &
        viewport%data(3) / delta%data(1), &
        viewport%data(4) / delta%data(2), &
        one ]

    res = translate(res, tmp1)
    res = scale(res, tmp2)
end function

