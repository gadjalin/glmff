pure function CAT(ortho_,MATRIX_TYPE_NAME)(left, right, bottom, top) result(res)
    implicit none
    real(kind=SCALAR_TYPE_KIND), intent(in) :: left, right, bottom, top
    type(CAT(MATRIX_TYPE_NAME,4x4)) :: res

    real(kind=SCALAR_TYPE_KIND), parameter :: one = CAT(1_,SCALAR_TYPE_KIND)
    real(kind=SCALAR_TYPE_KIND), parameter :: two = CAT(2_,SCALAR_TYPE_KIND)

    res%data(1,1) = two / (right - left)
    res%data(2,2) = two / (top - bottom)
    res%data(3,3) = -one
    res%data(1,4) = - (right + left) / (right - left)
    res%data(2,4) = - (top + bottom) / (top - bottom)
end function

pure function CAT(orthoLH_ZO_,MATRIX_TYPE_NAME)(left, right, bottom, top, near, far) result(res)
    implicit none
    real(kind=SCALAR_TYPE_KIND), intent(in) :: left, right, bottom, top, near, far
    type(CAT(MATRIX_TYPE_NAME,4x4)) :: res

    real(kind=SCALAR_TYPE_KIND), parameter :: one = CAT(1_,SCALAR_TYPE_KIND)
    real(kind=SCALAR_TYPE_KIND), parameter :: two = CAT(2_,SCALAR_TYPE_KIND)

    res%data(1,1) = two / (right - left)
    res%data(2,2) = two / (top - bottom)
    res%data(3,3) = one / (far - near)
    res%data(1,4) = - (right + left) / (right - left)
    res%data(2,4) = - (top + bottom) / (top - bottom)
    res%data(3,4) = - near / (far - near)
end function

pure function CAT(orthoLH_NO_,MATRIX_TYPE_NAME)(left, right, bottom, top, near, far) result(res)
    implicit none
    real(kind=SCALAR_TYPE_KIND), intent(in) :: left, right, bottom, top, near, far
    type(CAT(MATRIX_TYPE_NAME,4x4)) :: res

    real(kind=SCALAR_TYPE_KIND), parameter :: two = CAT(2_,SCALAR_TYPE_KIND)

    res%data(1,1) = two / (right - left)
    res%data(2,2) = two / (top - bottom)
    res%data(3,3) = two / (far - near)
    res%data(1,4) = - (right + left) / (right - left)
    res%data(2,4) = - (top + bottom) / (top - bottom)
    res%data(3,4) = - (far + near) / (far - near)
end function

pure function CAT(orthoRH_ZO_,MATRIX_TYPE_NAME)(left, right, bottom, top, near, far) result(res)
    implicit none
    real(kind=SCALAR_TYPE_KIND), intent(in) :: left, right, bottom, top, near, far
    type(CAT(MATRIX_TYPE_NAME,4x4)) :: res

    real(kind=SCALAR_TYPE_KIND), parameter :: one = CAT(1_,SCALAR_TYPE_KIND)
    real(kind=SCALAR_TYPE_KIND), parameter :: two = CAT(2_,SCALAR_TYPE_KIND)

    res%data(1,1) = two / (right - left)
    res%data(2,2) = two / (top - bottom)
    res%data(3,3) = -one / (far - near)
    res%data(1,4) = - (right + left) / (right - left)
    res%data(2,4) = - (top + bottom) / (top - bottom)
    res%data(3,4) = - near / (far - near)
end function

pure function CAT(orthoRH_NO_,MATRIX_TYPE_NAME)(left, right, bottom, top, near, far) result(res)
    implicit none
    real(kind=SCALAR_TYPE_KIND), intent(in) :: left, right, bottom, top, near, far
    type(CAT(MATRIX_TYPE_NAME,4x4)) :: res

    real(kind=SCALAR_TYPE_KIND), parameter :: two = CAT(2_,SCALAR_TYPE_KIND)

    res%data(1,1) = two / (right - left)
    res%data(2,2) = two / (top - bottom)
    res%data(3,3) = - two / (far - near)
    res%data(1,4) = - (right + left) / (right - left)
    res%data(2,4) = - (top + bottom) / (top - bottom)
    res%data(3,4) = - (far + near) / (far - near)
end function

pure function CAT(orthoZO_,MATRIX_TYPE_NAME)(left, right, bottom, top, near, far) result(res)
    implicit none
    real(kind=SCALAR_TYPE_KIND), intent(in) :: left, right, bottom, top, near, far
    type(CAT(MATRIX_TYPE_NAME,4x4)) :: res

#if GLM_CONFIG_CLIP_CONTROL & GLM_CLIP_CONTROL_LH_BIT
    res = CAT(orthoLH_ZO_,MATRIX_TYPE_NAME)(left, right, bottom, top, near, far)
#else
    res = CAT(orthoRH_ZO_,MATRIX_TYPE_NAME)(left, right, bottom, top, near, far)
#endif
end function

pure function CAT(orthoNO_,MATRIX_TYPE_NAME)(left, right, bottom, top, near, far) result(res)
    implicit none
    real(kind=SCALAR_TYPE_KIND), intent(in) :: left, right, bottom, top, near, far
    type(CAT(MATRIX_TYPE_NAME,4x4)) :: res

#if GLM_CONFIG_CLIP_CONTROL & GLM_CLIP_CONTROL_LH_BIT
    res = CAT(orthoLH_NO_,MATRIX_TYPE_NAME)(left, right, bottom, top, near, far)
#else
    res = CAT(orthoRH_NO_,MATRIX_TYPE_NAME)(left, right, bottom, top, near, far)
#endif
end function

pure function CAT(orthoLH_,MATRIX_TYPE_NAME)(left, right, bottom, top, near, far) result(res)
    implicit none
    real(kind=SCALAR_TYPE_KIND), intent(in) :: left, right, bottom, top, near, far
    type(CAT(MATRIX_TYPE_NAME,4x4)) :: res

#if GLM_CONFIG_CLIP_CONTROL & GLM_CLIP_CONTROL_ZO_BIT
    res = CAT(orthoLH_ZO_,MATRIX_TYPE_NAME)(left, right, bottom, top, near, far)
#else
    res = CAT(orthoLH_NO_,MATRIX_TYPE_NAME)(left, right, bottom, top, near, far)
#endif
end function

pure function CAT(orthoRH_,MATRIX_TYPE_NAME)(left, right, bottom, top, near, far) result(res)
    implicit none
    real(kind=SCALAR_TYPE_KIND), intent(in) :: left, right, bottom, top, near, far
    type(CAT(MATRIX_TYPE_NAME,4x4)) :: res

#if GLM_CONFIG_CLIP_CONTROL & GLM_CLIP_CONTROL_ZO_BIT
    res = CAT(orthoRH_ZO_,MATRIX_TYPE_NAME)(left, right, bottom, top, near, far)
#else
    res = CAT(orthoRH_NO_,MATRIX_TYPE_NAME)(left, right, bottom, top, near, far)
#endif
end function

pure function CAT(ortho_full_,MATRIX_TYPE_NAME)(left, right, bottom, top, near, far) result(res)
    implicit none
    real(kind=SCALAR_TYPE_KIND), intent(in) :: left, right, bottom, top, near, far
    type(CAT(MATRIX_TYPE_NAME,4x4)) :: res

#if GLMFF_CONFIG_CLIP_CONTROL == GLMFF_CLIP_CONTROL_LH_ZO
    res = CAT(orthoLH_ZO_,MATRIX_TYPE_NAME)(left, right, bottom, top, near, far)
#elif GLMFF_CONFIG_CLIP_CONTROL == GLMFF_CLIP_CONTROL_LH_NO
    res = CAT(orthoLH_NO_,MATRIX_TYPE_NAME)(left, right, bottom, top, near, far)
#elif GLMFF_CONFIG_CLIP_CONTROL == GLMFF_CLIP_CONTROL_RH_ZO
    res = CAT(orthoRH_ZO_,MATRIX_TYPE_NAME)(left, right, bottom, top, near, far)
#elif GLMFF_CONFIG_CLIP_CONTROL == GLMFF_CLIP_CONTROL_RH_NO
    res = CAT(orthoRH_NO_,MATRIX_TYPE_NAME)(left, right, bottom, top, near, far)
#endif
end function

pure function CAT(frustumLH_ZO_,MATRIX_TYPE_NAME)(left, right, bottom, top, near, far) result(res)
    implicit none
    real(kind=SCALAR_TYPE_KIND), intent(in) :: left, right, bottom, top, near, far
    type(CAT(MATRIX_TYPE_NAME,4x4)) :: res

    real(kind=SCALAR_TYPE_KIND), parameter :: zero = CAT(0_,SCALAR_TYPE_KIND)
    real(kind=SCALAR_TYPE_KIND), parameter :: one = CAT(1_,SCALAR_TYPE_KIND)
    real(kind=SCALAR_TYPE_KIND), parameter :: two = CAT(2_,SCALAR_TYPE_KIND)

    res%data = zero
    res%data(1,1) = (two * near) / (right - left)
    res%data(2,2) = (two * near) / (top - bottom)
    res%data(1,3) = -(right + left) / (right - left)
    res%data(2,3) = -(top + bottom) / (top - bottom)
    res%data(3,3) = far / (far - near)
    res%data(4,3) = one
    res%data(3,4) = -(far * near) / (far - near)
end function

pure function CAT(frustumLH_NO_,MATRIX_TYPE_NAME)(left, right, bottom, top, near, far) result(res)
    implicit none
    real(kind=SCALAR_TYPE_KIND), intent(in) :: left, right, bottom, top, near, far
    type(CAT(MATRIX_TYPE_NAME,4x4)) :: res

    real(kind=SCALAR_TYPE_KIND), parameter :: zero = CAT(0_,SCALAR_TYPE_KIND)
    real(kind=SCALAR_TYPE_KIND), parameter :: one = CAT(1_,SCALAR_TYPE_KIND)
    real(kind=SCALAR_TYPE_KIND), parameter :: two = CAT(2_,SCALAR_TYPE_KIND)

    res%data = zero
    res%data(1,1) = (two * near) / (right - left)
    res%data(2,2) = (two * near) / (top - bottom)
    res%data(1,3) = -(right + left) / (right - left)
    res%data(2,3) = -(top + bottom) / (top - bottom)
    res%data(3,3) = (far + near) / (far - near)
    res%data(4,3) = one
    res%data(3,4) = -(two * far * near) / (far - near)
end function

pure function CAT(frustumRH_ZO_,MATRIX_TYPE_NAME)(left, right, bottom, top, near, far) result(res)
    implicit none
    real(kind=SCALAR_TYPE_KIND), intent(in) :: left, right, bottom, top, near, far
    type(CAT(MATRIX_TYPE_NAME,4x4)) :: res

    real(kind=SCALAR_TYPE_KIND), parameter :: zero = CAT(0_,SCALAR_TYPE_KIND)
    real(kind=SCALAR_TYPE_KIND), parameter :: one = CAT(1_,SCALAR_TYPE_KIND)
    real(kind=SCALAR_TYPE_KIND), parameter :: two = CAT(2_,SCALAR_TYPE_KIND)

    res%data = zero
    res%data(1,1) = (two * near) / (right - left)
    res%data(2,2) = (two * near) / (top - bottom)
    res%data(1,3) = (right + left) / (right - left)
    res%data(2,3) = (top + bottom) / (top - bottom)
    res%data(3,3) = far / (near - far)
    res%data(4,3) = -one
    res%data(3,4) = -(far * near) / (far - near)
end function

pure function CAT(frustumRH_NO_,MATRIX_TYPE_NAME)(left, right, bottom, top, near, far) result(res)
    implicit none
    real(kind=SCALAR_TYPE_KIND), intent(in) :: left, right, bottom, top, near, far
    type(CAT(MATRIX_TYPE_NAME,4x4)) :: res

    real(kind=SCALAR_TYPE_KIND), parameter :: zero = CAT(0_,SCALAR_TYPE_KIND)
    real(kind=SCALAR_TYPE_KIND), parameter :: one = CAT(1_,SCALAR_TYPE_KIND)
    real(kind=SCALAR_TYPE_KIND), parameter :: two = CAT(2_,SCALAR_TYPE_KIND)

    res%data = zero
    res%data(1,1) = (two * near) / (right - left)
    res%data(2,2) = (two * near) / (top - bottom)
    res%data(1,3) = (right + left) / (right - left)
    res%data(2,3) = (top + bottom) / (top - bottom)
    res%data(3,3) = - (far + near) / (far - near)
    res%data(4,3) = -one
    res%data(3,4) = -(two * far * near) / (far - near)
end function

pure function CAT(frustumZO_,MATRIX_TYPE_NAME)(left, right, bottom, top, near, far) result(res)
    implicit none
    real(kind=SCALAR_TYPE_KIND), intent(in) :: left, right, bottom, top, near, far
    type(CAT(MATRIX_TYPE_NAME,4x4)) :: res

#if GLM_CONFIG_CLIP_CONTROL & GLM_CLIP_CONTROL_LH_BIT
    res = CAT(frustumLH_ZO_,MATRIX_TYPE_NAME)(left, right, bottom, top, near, far)
#else
    res = CAT(frustumRH_ZO_,MATRIX_TYPE_NAME)(left, right, bottom, top, near, far)
#endif
end function

pure function CAT(frustumNO_,MATRIX_TYPE_NAME)(left, right, bottom, top, near, far) result(res)
    implicit none
    real(kind=SCALAR_TYPE_KIND), intent(in) :: left, right, bottom, top, near, far
    type(CAT(MATRIX_TYPE_NAME,4x4)) :: res

#if GLM_CONFIG_CLIP_CONTROL & GLM_CLIP_CONTROL_LH_BIT
    res = CAT(frustumLH_NO_,MATRIX_TYPE_NAME)(left, right, bottom, top, near, far)
#else
    res = CAT(frustumRH_NO_,MATRIX_TYPE_NAME)(left, right, bottom, top, near, far)
#endif
end function

pure function CAT(frustumLH_,MATRIX_TYPE_NAME)(left, right, bottom, top, near, far) result(res)
    implicit none
    real(kind=SCALAR_TYPE_KIND), intent(in) :: left, right, bottom, top, near, far
    type(CAT(MATRIX_TYPE_NAME,4x4)) :: res

#if GLM_CONFIG_CLIP_CONTROL & GLM_CLIP_CONTROL_ZO_BIT
    res = CAT(frustumLH_ZO_,MATRIX_TYPE_NAME)(left, right, bottom, top, near, far)
#else
    res = CAT(frustumLH_NO_,MATRIX_TYPE_NAME)(left, right, bottom, top, near, far)
#endif
end function

pure function CAT(frustumRH_,MATRIX_TYPE_NAME)(left, right, bottom, top, near, far) result(res)
    implicit none
    real(kind=SCALAR_TYPE_KIND), intent(in) :: left, right, bottom, top, near, far
    type(CAT(MATRIX_TYPE_NAME,4x4)) :: res

#if GLM_CONFIG_CLIP_CONTROL & GLM_CLIP_CONTROL_ZO_BIT
    res = CAT(frustumRH_ZO_,MATRIX_TYPE_NAME)(left, right, bottom, top, near, far)
#else
    res = CAT(frustumRH_NO_,MATRIX_TYPE_NAME)(left, right, bottom, top, near, far)
#endif
end function

pure function CAT(frustum_,MATRIX_TYPE_NAME)(left, right, bottom, top, near, far) result(res)
    implicit none
    real(kind=SCALAR_TYPE_KIND), intent(in) :: left, right, bottom, top, near, far
    type(CAT(MATRIX_TYPE_NAME,4x4)) :: res

#if GLMFF_CONFIG_CLIP_CONTROL == GLMFF_CLIP_CONTROL_LH_ZO
    res = CAT(frustumLH_ZO_,MATRIX_TYPE_NAME)(left, right, bottom, top, near, far)
#elif GLMFF_CONFIG_CLIP_CONTROL == GLMFF_CLIP_CONTROL_LH_NO
    res = CAT(frustumLH_NO_,MATRIX_TYPE_NAME)(left, right, bottom, top, near, far)
#elif GLMFF_CONFIG_CLIP_CONTROL == GLMFF_CLIP_CONTROL_RH_ZO
    res = CAT(frustumRH_ZO_,MATRIX_TYPE_NAME)(left, right, bottom, top, near, far)
#elif GLMFF_CONFIG_CLIP_CONTROL == GLMFF_CLIP_CONTROL_RH_NO
    res = CAT(frustumRH_NO_,MATRIX_TYPE_NAME)(left, right, bottom, top, near, far)
#endif
end function

pure function CAT(perspectiveRH_ZO_,MATRIX_TYPE_NAME)(fovy, aspect, near, far) result(res)
    implicit none
    real(kind=SCALAR_TYPE_KIND), intent(in) :: fovy, aspect, near, far
    type(CAT(MATRIX_TYPE_NAME,4x4)) :: res

    real(kind=SCALAR_TYPE_KIND) :: tan_half_fovy
    real(kind=SCALAR_TYPE_KIND), parameter :: zero = CAT(0_,SCALAR_TYPE_KIND)
    real(kind=SCALAR_TYPE_KIND), parameter :: half = CAT(0.5_,SCALAR_TYPE_KIND)
    real(kind=SCALAR_TYPE_KIND), parameter :: one = CAT(1_,SCALAR_TYPE_KIND)

    tan_half_fovy = tan(fovy * half)

    res%data = zero
    res%data(1,1) = one / (aspect * tan_half_fovy)
    res%data(2,2) = one / (tan_half_fovy)
    res%data(3,3) = far / (near - far)
    res%data(4,3) = - one
    res%data(3,4) = - (far * near) / (far - near)
end function

pure function CAT(perspectiveRH_NO_,MATRIX_TYPE_NAME)(fovy, aspect, near, far) result(res)
    implicit none
    real(kind=SCALAR_TYPE_KIND), intent(in) :: fovy, aspect, near, far
    type(CAT(MATRIX_TYPE_NAME,4x4)) :: res

    real(kind=SCALAR_TYPE_KIND) :: tan_half_fovy
    real(kind=SCALAR_TYPE_KIND), parameter :: zero = CAT(0_,SCALAR_TYPE_KIND)
    real(kind=SCALAR_TYPE_KIND), parameter :: half = CAT(0.5_,SCALAR_TYPE_KIND)
    real(kind=SCALAR_TYPE_KIND), parameter :: one = CAT(1_,SCALAR_TYPE_KIND)
    real(kind=SCALAR_TYPE_KIND), parameter :: two = CAT(2_,SCALAR_TYPE_KIND)

    tan_half_fovy = tan(fovy * half)

    res%data = zero
    res%data(1,1) = one / (aspect * tan_half_fovy)
    res%data(2,2) = one / (tan_half_fovy)
    res%data(3,3) = - (far + near) / (far - near)
    res%data(4,3) = - one
    res%data(3,4) = - (two * far * near) / (far - near)
end function

pure function CAT(perspectiveLH_ZO_,MATRIX_TYPE_NAME)(fovy, aspect, near, far) result(res)
    implicit none
    real(kind=SCALAR_TYPE_KIND), intent(in) :: fovy, aspect, near, far
    type(CAT(MATRIX_TYPE_NAME,4x4)) :: res

    real(kind=SCALAR_TYPE_KIND) :: tan_half_fovy
    real(kind=SCALAR_TYPE_KIND), parameter :: zero = CAT(0_,SCALAR_TYPE_KIND)
    real(kind=SCALAR_TYPE_KIND), parameter :: half = CAT(0.5_,SCALAR_TYPE_KIND)
    real(kind=SCALAR_TYPE_KIND), parameter :: one = CAT(1_,SCALAR_TYPE_KIND)

    tan_half_fovy = tan(fovy * half)

    res%data = zero
    res%data(1,1) = one / (aspect * tan_half_fovy)
    res%data(2,2) = one / (tan_half_fovy)
    res%data(3,3) = far / (far - near)
    res%data(4,3) = one
    res%data(3,4) = - (far * near) / (far - near)
end function

pure function CAT(perspectiveLH_NO_,MATRIX_TYPE_NAME)(fovy, aspect, near, far) result(res)
    implicit none
    real(kind=SCALAR_TYPE_KIND), intent(in) :: fovy, aspect, near, far
    type(CAT(MATRIX_TYPE_NAME,4x4)) :: res

    real(kind=SCALAR_TYPE_KIND) :: tan_half_fovy
    real(kind=SCALAR_TYPE_KIND), parameter :: zero = CAT(0_,SCALAR_TYPE_KIND)
    real(kind=SCALAR_TYPE_KIND), parameter :: half = CAT(0.5_,SCALAR_TYPE_KIND)
    real(kind=SCALAR_TYPE_KIND), parameter :: one = CAT(1_,SCALAR_TYPE_KIND)
    real(kind=SCALAR_TYPE_KIND), parameter :: two = CAT(2_,SCALAR_TYPE_KIND)

    tan_half_fovy = tan(fovy * half)

    res%data = zero
    res%data(1,1) = one / (aspect * tan_half_fovy)
    res%data(2,2) = one / (tan_half_fovy)
    res%data(3,3) = (far + near) / (far - near)
    res%data(4,3) = one
    res%data(3,4) = - (two * far * near) / (far - near)
end function

pure function CAT(perspectiveZO_,MATRIX_TYPE_NAME)(fovy, aspect, near, far) result(res)
    implicit none
    real(kind=SCALAR_TYPE_KIND), intent(in) :: fovy, aspect, near, far
    type(CAT(MATRIX_TYPE_NAME,4x4)) :: res

#if GLM_CONFIG_CLIP_CONTROL & GLM_CLIP_CONTROL_LH_BIT
    res = CAT(perspectiveLH_ZO_,MATRIX_TYPE_NAME)(fovy, aspect, near, far)
#else
    res = CAT(perspectiveRH_ZO_,MATRIX_TYPE_NAME)(fovy, aspect, near, far)
#endif
end function

pure function CAT(perspectiveNO_,MATRIX_TYPE_NAME)(fovy, aspect, near, far) result(res)
    implicit none
    real(kind=SCALAR_TYPE_KIND), intent(in) :: fovy, aspect, near, far
    type(CAT(MATRIX_TYPE_NAME,4x4)) :: res

#if GLM_CONFIG_CLIP_CONTROL & GLM_CLIP_CONTROL_LH_BIT
    res = CAT(perspectiveLH_NO_,MATRIX_TYPE_NAME)(fovy, aspect, near, far)
#else
    res = CAT(perspectiveRH_NO_,MATRIX_TYPE_NAME)(fovy, aspect, near, far)
#endif
end function

pure function CAT(perspectiveLH_,MATRIX_TYPE_NAME)(fovy, aspect, near, far) result(res)
    implicit none
    real(kind=SCALAR_TYPE_KIND), intent(in) :: fovy, aspect, near, far
    type(CAT(MATRIX_TYPE_NAME,4x4)) :: res

#if GLM_CONFIG_CLIP_CONTROL & GLM_CLIP_CONTROL_ZO_BIT
    res = CAT(perspectiveLH_ZO_,MATRIX_TYPE_NAME)(fovy, aspect, near, far)
#else
    res = CAT(perspectiveLH_NO_,MATRIX_TYPE_NAME)(fovy, aspect, near, far)
#endif
end function

pure function CAT(perspectiveRH_,MATRIX_TYPE_NAME)(fovy, aspect, near, far) result(res)
    implicit none
    real(kind=SCALAR_TYPE_KIND), intent(in) :: fovy, aspect, near, far
    type(CAT(MATRIX_TYPE_NAME,4x4)) :: res

#if GLM_CONFIG_CLIP_CONTROL & GLM_CLIP_CONTROL_ZO_BIT
    res = CAT(perspectiveRH_ZO_,MATRIX_TYPE_NAME)(fovy, aspect, near, far)
#else
    res = CAT(perspectiveRH_NO_,MATRIX_TYPE_NAME)(fovy, aspect, near, far)
#endif
end function

pure function CAT(perspective_,MATRIX_TYPE_NAME)(fovy, aspect, near, far) result(res)
    implicit none
    real(kind=SCALAR_TYPE_KIND), intent(in) :: fovy, aspect, near, far
    type(CAT(MATRIX_TYPE_NAME,4x4)) :: res

#if GLMFF_CONFIG_CLIP_CONTROL == GLMFF_CLIP_CONTROL_LH_ZO
    res = CAT(perspectiveLH_ZO_,MATRIX_TYPE_NAME)(fovy, aspect, near, far)
#elif GLMFF_CONFIG_CLIP_CONTROL == GLMFF_CLIP_CONTROL_LH_NO
    res = CAT(perspectiveLH_NO_,MATRIX_TYPE_NAME)(fovy, aspect, near, far)
#elif GLMFF_CONFIG_CLIP_CONTROL == GLMFF_CLIP_CONTROL_RH_ZO
    res = CAT(perspectiveRH_ZO_,MATRIX_TYPE_NAME)(fovy, aspect, near, far)
#elif GLMFF_CONFIG_CLIP_CONTROL == GLMFF_CLIP_CONTROL_RH_NO
    res = CAT(perspectiveRH_NO_,MATRIX_TYPE_NAME)(fovy, aspect, near, far)
#endif
end function

pure function CAT(perspectiveFovRH_ZO_,MATRIX_TYPE_NAME)(fov, width, height, near, far) result(res)
    implicit none
    real(kind=SCALAR_TYPE_KIND), intent(in) :: fov, width, height, near, far
    type(CAT(MATRIX_TYPE_NAME,4x4)) :: res

    real(kind=SCALAR_TYPE_KIND) :: h, w
    real(kind=SCALAR_TYPE_KIND), parameter :: zero = CAT(0_,SCALAR_TYPE_KIND)
    real(kind=SCALAR_TYPE_KIND), parameter :: half = CAT(0.5_,SCALAR_TYPE_KIND)
    real(kind=SCALAR_TYPE_KIND), parameter :: one = CAT(1_,SCALAR_TYPE_KIND)

    h = cos(half * fov) / sin(half * fov)
    w = h * height / width

    res%data = zero
    res%data(1,1) = w
    res%data(2,2) = h
    res%data(3,3) = far / (near - far)
    res%data(4,3) = - one
    res%data(3,4) = - (far * near) / (far - near)
end function

pure function CAT(perspectiveFovRH_NO_,MATRIX_TYPE_NAME)(fov, width, height, near, far) result(res)
    implicit none
    real(kind=SCALAR_TYPE_KIND), intent(in) :: fov, width, height, near, far
    type(CAT(MATRIX_TYPE_NAME,4x4)) :: res

    real(kind=SCALAR_TYPE_KIND) :: h, w
    real(kind=SCALAR_TYPE_KIND), parameter :: zero = CAT(0_,SCALAR_TYPE_KIND)
    real(kind=SCALAR_TYPE_KIND), parameter :: half = CAT(0.5_,SCALAR_TYPE_KIND)
    real(kind=SCALAR_TYPE_KIND), parameter :: one = CAT(1_,SCALAR_TYPE_KIND)
    real(kind=SCALAR_TYPE_KIND), parameter :: two = CAT(2_,SCALAR_TYPE_KIND)

    h = cos(half * fov) / sin(half * fov)
    w = h * height / width

    res%data = zero
    res%data(1,1) = w
    res%data(2,2) = h
    res%data(3,3) = - (far + near) / (far - near)
    res%data(4,3) = - one
    res%data(3,4) = - (two * far * near) / (far - near)
end function

pure function CAT(perspectiveFovLH_ZO_,MATRIX_TYPE_NAME)(fov, width, height, near, far) result(res)
    implicit none
    real(kind=SCALAR_TYPE_KIND), intent(in) :: fov, width, height, near, far
    type(CAT(MATRIX_TYPE_NAME,4x4)) :: res

    real(kind=SCALAR_TYPE_KIND) :: h, w
    real(kind=SCALAR_TYPE_KIND), parameter :: zero = CAT(0_,SCALAR_TYPE_KIND)
    real(kind=SCALAR_TYPE_KIND), parameter :: half = CAT(0.5_,SCALAR_TYPE_KIND)
    real(kind=SCALAR_TYPE_KIND), parameter :: one = CAT(1_,SCALAR_TYPE_KIND)

    h = cos(half * fov) / sin(half * fov)
    w = h * height / width

    res%data = zero
    res%data(1,1) = w
    res%data(2,2) = h
    res%data(3,3) = far / (far - near)
    res%data(4,3) = one
    res%data(3,4) = - (far * near) / (far - near)
end function

pure function CAT(perspectiveFovLH_NO_,MATRIX_TYPE_NAME)(fov, width, height, near, far) result(res)
    implicit none
    real(kind=SCALAR_TYPE_KIND), intent(in) :: fov, width, height, near, far
    type(CAT(MATRIX_TYPE_NAME,4x4)) :: res

    real(kind=SCALAR_TYPE_KIND) :: h, w
    real(kind=SCALAR_TYPE_KIND), parameter :: zero = CAT(0_,SCALAR_TYPE_KIND)
    real(kind=SCALAR_TYPE_KIND), parameter :: half = CAT(0.5_,SCALAR_TYPE_KIND)
    real(kind=SCALAR_TYPE_KIND), parameter :: one = CAT(1_,SCALAR_TYPE_KIND)
    real(kind=SCALAR_TYPE_KIND), parameter :: two = CAT(2_,SCALAR_TYPE_KIND)

    h = cos(half * fov) / sin(half * fov)
    w = h * height / width

    res%data = zero
    res%data(1,1) = w
    res%data(2,2) = h
    res%data(3,3) = (far + near) / (far - near)
    res%data(4,3) = one
    res%data(3,4) = - (two * far * near) / (far - near)
end function

pure function CAT(perspectiveFovZO_,MATRIX_TYPE_NAME)(fov, width, height, near, far) result(res)
    implicit none
    real(kind=SCALAR_TYPE_KIND), intent(in) :: fov, width, height, near, far
    type(CAT(MATRIX_TYPE_NAME,4x4)) :: res

#if GLM_CONFIG_CLIP_CONTROL & GLM_CLIP_CONTROL_LH_BIT
    res = CAT(perspectiveFovLH_ZO_,MATRIX_TYPE_NAME)(fov, width, height, near, far)
#else
    res = CAT(perspectiveFovRH_ZO_,MATRIX_TYPE_NAME)(fov, width, height, near, far)
#endif
end function

pure function CAT(perspectiveFovNO_,MATRIX_TYPE_NAME)(fov, width, height, near, far) result(res)
    implicit none
    real(kind=SCALAR_TYPE_KIND), intent(in) :: fov, width, height, near, far
    type(CAT(MATRIX_TYPE_NAME,4x4)) :: res

#if GLM_CONFIG_CLIP_CONTROL & GLM_CLIP_CONTROL_LH_BIT
    res = CAT(perspectiveFovLH_NO_,MATRIX_TYPE_NAME)(fov, width, height, near, far)
#else
    res = CAT(perspectiveFovRH_NO_,MATRIX_TYPE_NAME)(fov, width, height, near, far)
#endif
end function

pure function CAT(perspectiveFovLH_,MATRIX_TYPE_NAME)(fov, width, height, near, far) result(res)
    implicit none
    real(kind=SCALAR_TYPE_KIND), intent(in) :: fov, width, height, near, far
    type(CAT(MATRIX_TYPE_NAME,4x4)) :: res

#if GLM_CONFIG_CLIP_CONTROL & GLM_CLIP_CONTROL_ZO_BIT
    res = CAT(perspectiveFovLH_ZO_,MATRIX_TYPE_NAME)(fov, width, height, near, far)
#else
    res = CAT(perspectiveFovLH_NO_,MATRIX_TYPE_NAME)(fov, width, height, near, far)
#endif
end function

pure function CAT(perspectiveFovRH_,MATRIX_TYPE_NAME)(fov, width, height, near, far) result(res)
    implicit none
    real(kind=SCALAR_TYPE_KIND), intent(in) :: fov, width, height, near, far
    type(CAT(MATRIX_TYPE_NAME,4x4)) :: res

#if GLM_CONFIG_CLIP_CONTROL & GLM_CLIP_CONTROL_ZO_BIT
    res = CAT(perspectiveFovRH_ZO_,MATRIX_TYPE_NAME)(fov, width, height, near, far)
#else
    res = CAT(perspectiveFovRH_NO_,MATRIX_TYPE_NAME)(fov, width, height, near, far)
#endif
end function

pure function CAT(perspectiveFov_,MATRIX_TYPE_NAME)(fov, width, height, near, far) result(res)
    implicit none
    real(kind=SCALAR_TYPE_KIND), intent(in) :: fov, width, height, near, far
    type(CAT(MATRIX_TYPE_NAME,4x4)) :: res

#if GLMFF_CONFIG_CLIP_CONTROL == GLMFF_CLIP_CONTROL_LH_ZO
    res = CAT(perspectiveFovLH_ZO_,MATRIX_TYPE_NAME)(fov, width, height, near, far)
#elif GLMFF_CONFIG_CLIP_CONTROL == GLMFF_CLIP_CONTROL_LH_NO
    res = CAT(perspectiveFovLH_NO_,MATRIX_TYPE_NAME)(fov, width, height, near, far)
#elif GLMFF_CONFIG_CLIP_CONTROL == GLMFF_CLIP_CONTROL_RH_ZO
    res = CAT(perspectiveFovRH_ZO_,MATRIX_TYPE_NAME)(fov, width , height, near, far)
#elif GLMFF_CONFIG_CLIP_CONTROL == GLMFF_CLIP_CONTROL_RH_NO
    res = CAT(perspectiveFovRH_NO_,MATRIX_TYPE_NAME)(fov, width, height, near, far)
#endif
end function

pure function CAT(infinitePerspectiveRH_ZO_,MATRIX_TYPE_NAME)(fovy, aspect, near) result(res)
    implicit none
    real(kind=SCALAR_TYPE_KIND), intent(in) :: fovy, aspect, near
    type(CAT(MATRIX_TYPE_NAME,4x4)) :: res

    real(kind=SCALAR_TYPE_KIND) :: rnge, left, right, bottom, top
    real(kind=SCALAR_TYPE_KIND), parameter :: zero = CAT(0_,SCALAR_TYPE_KIND)
    real(kind=SCALAR_TYPE_KIND), parameter :: half = CAT(0.5_,SCALAR_TYPE_KIND)
    real(kind=SCALAR_TYPE_KIND), parameter :: one = CAT(1_,SCALAR_TYPE_KIND)
    real(kind=SCALAR_TYPE_KIND), parameter :: two = CAT(2_,SCALAR_TYPE_KIND)

    rnge = tan(fovy * half) * near
    left = -rnge * aspect
    right = rnge * aspect
    bottom = -rnge
    top = rnge

    res%data = zero
    res%data(1,1) = (two * near) / (right - left)
    res%data(2,2) = (two * near) / (top - bottom)
    res%data(3,3) = - one
    res%data(4,3) = - one
    res%data(3,4) = - near
end function

pure function CAT(infinitePerspectiveRH_NO_,MATRIX_TYPE_NAME)(fovy, aspect, near) result(res)
    implicit none
    real(kind=SCALAR_TYPE_KIND), intent(in) :: fovy, aspect, near
    type(CAT(MATRIX_TYPE_NAME,4x4)) :: res

    real(kind=SCALAR_TYPE_KIND) :: rnge, left, right, bottom, top
    real(kind=SCALAR_TYPE_KIND), parameter :: zero = CAT(0_,SCALAR_TYPE_KIND)
    real(kind=SCALAR_TYPE_KIND), parameter :: half = CAT(0.5_,SCALAR_TYPE_KIND)
    real(kind=SCALAR_TYPE_KIND), parameter :: one = CAT(1_,SCALAR_TYPE_KIND)
    real(kind=SCALAR_TYPE_KIND), parameter :: two = CAT(2_,SCALAR_TYPE_KIND)

    rnge = tan(fovy * half) * near
    left = -rnge * aspect
    right = rnge * aspect
    bottom = -rnge
    top = rnge

    res%data = zero
    res%data(1,1) = (two * near) / (right - left)
    res%data(2,2) = (two * near) / (top - bottom)
    res%data(3,3) = - one
    res%data(4,3) = - one
    res%data(3,4) = - two * near
end function

pure function CAT(infinitePerspectiveLH_ZO_,MATRIX_TYPE_NAME)(fovy, aspect, near) result(res)
    implicit none
    real(kind=SCALAR_TYPE_KIND), intent(in) :: fovy, aspect, near
    type(CAT(MATRIX_TYPE_NAME,4x4)) :: res

    real(kind=SCALAR_TYPE_KIND) :: rnge, left, right, bottom, top
    real(kind=SCALAR_TYPE_KIND), parameter :: zero = CAT(0_,SCALAR_TYPE_KIND)
    real(kind=SCALAR_TYPE_KIND), parameter :: half = CAT(0.5_,SCALAR_TYPE_KIND)
    real(kind=SCALAR_TYPE_KIND), parameter :: one = CAT(1_,SCALAR_TYPE_KIND)
    real(kind=SCALAR_TYPE_KIND), parameter :: two = CAT(2_,SCALAR_TYPE_KIND)

    rnge = tan(fovy * half) * near
    left = -rnge * aspect
    right = rnge * aspect
    bottom = -rnge
    top = rnge

    res%data = zero
    res%data(1,1) = (two * near) / (right - left)
    res%data(2,2) = (two * near) / (top - bottom)
    res%data(3,3) = one
    res%data(4,3) = one
    res%data(3,4) = - near
end function

pure function CAT(infinitePerspectiveLH_NO_,MATRIX_TYPE_NAME)(fovy, aspect, near) result(res)
    implicit none
    real(kind=SCALAR_TYPE_KIND), intent(in) :: fovy, aspect, near
    type(CAT(MATRIX_TYPE_NAME,4x4)) :: res

    real(kind=SCALAR_TYPE_KIND) :: rnge, left, right, bottom, top
    real(kind=SCALAR_TYPE_KIND), parameter :: zero = CAT(0_,SCALAR_TYPE_KIND)
    real(kind=SCALAR_TYPE_KIND), parameter :: half = CAT(0.5_,SCALAR_TYPE_KIND)
    real(kind=SCALAR_TYPE_KIND), parameter :: one = CAT(1_,SCALAR_TYPE_KIND)
    real(kind=SCALAR_TYPE_KIND), parameter :: two = CAT(2_,SCALAR_TYPE_KIND)

    rnge = tan(fovy * half) * near
    left = -rnge * aspect
    right = rnge * aspect
    bottom = -rnge
    top = rnge

    res%data = zero
    res%data(1,1) = (two * near) / (right - left)
    res%data(2,2) = (two * near) / (top - bottom)
    res%data(3,3) = one
    res%data(4,3) = one
    res%data(3,4) = - two * near
end function

pure function CAT(infinitePerspectiveLH_,MATRIX_TYPE_NAME)(fovy, aspect, near) result(res)
    implicit none
    real(kind=SCALAR_TYPE_KIND), intent(in) :: fovy, aspect, near
    type(CAT(MATRIX_TYPE_NAME,4x4)) :: res

#if GLM_CONFIG_CLIP_CONTROL & GLM_CLIP_CONTROL_ZO_BIT
    res = CAT(infinitePerspectiveLH_ZO_,MATRIX_TYPE_NAME)(fovy, aspect, near)
#else
    res = CAT(infinitePerspectiveLH_NO_,MATRIX_TYPE_NAME)(fovy, aspect, near)
#endif
end function

pure function CAT(infinitePerspectiveRH_,MATRIX_TYPE_NAME)(fovy, aspect, near) result(res)
    implicit none
    real(kind=SCALAR_TYPE_KIND), intent(in) :: fovy, aspect, near
    type(CAT(MATRIX_TYPE_NAME,4x4)) :: res

#if GLM_CONFIG_CLIP_CONTROL & GLM_CLIP_CONTROL_ZO_BIT
    res = CAT(infinitePerspectiveRH_ZO_,MATRIX_TYPE_NAME)(fovy, aspect, near)
#else
    res = CAT(infinitePerspectiveRH_NO_,MATRIX_TYPE_NAME)(fovy, aspect, near)
#endif
end function

pure function CAT(infinitePerspective_,MATRIX_TYPE_NAME)(fovy, aspect, near) result(res)
    implicit none
    real(kind=SCALAR_TYPE_KIND), intent(in) :: fovy, aspect, near
    type(CAT(MATRIX_TYPE_NAME,4x4)) :: res

#if GLMFF_CONFIG_CLIP_CONTROL == GLMFF_CLIP_CONTROL_LH_ZO
    res = CAT(infinitePerspectiveLH_ZO_,MATRIX_TYPE_NAME)(fovy, aspect, near)
#elif GLMFF_CONFIG_CLIP_CONTROL == GLMFF_CLIP_CONTROL_LH_NO
    res = CAT(infinitePerspectiveLH_NO_,MATRIX_TYPE_NAME)(fovy, aspect, near)
#elif GLMFF_CONFIG_CLIP_CONTROL == GLMFF_CLIP_CONTROL_RH_ZO
    res = CAT(infinitePerspectiveRH_ZO_,MATRIX_TYPE_NAME)(fovy, aspect, near)
#elif GLMFF_CONFIG_CLIP_CONTROL == GLMFF_CLIP_CONTROL_RH_NO
    res = CAT(infinitePerspectiveRH_NO_,MATRIX_TYPE_NAME)(fovy, aspect, near)
#endif
end function

pure function CAT(tweakedInfinitePerspective_eps_,MATRIX_TYPE_NAME)(fovy, aspect, near, eps) result(res)
    implicit none
    real(kind=SCALAR_TYPE_KIND), intent(in) :: fovy, aspect, near, eps
    type(CAT(MATRIX_TYPE_NAME,4x4)) :: res

    real(kind=SCALAR_TYPE_KIND) :: rnge, left, right, bottom, top
    real(kind=SCALAR_TYPE_KIND), parameter :: zero = CAT(0_,SCALAR_TYPE_KIND)
    real(kind=SCALAR_TYPE_KIND), parameter :: half = CAT(0.5_,SCALAR_TYPE_KIND)
    real(kind=SCALAR_TYPE_KIND), parameter :: one = CAT(1_,SCALAR_TYPE_KIND)
    real(kind=SCALAR_TYPE_KIND), parameter :: two = CAT(2_,SCALAR_TYPE_KIND)

    rnge = tan(fovy * half) * near
    left = -rnge * aspect
    right = rnge * aspect
    bottom = -rnge
    top = rnge

    res%data = zero
    res%data(1,1) = (two * near) / (right - left)
    res%data(2,2) = (two * near) / (top - bottom)
    res%data(3,3) = eps - one
    res%data(4,3) = - one
    res%data(3,4) = - (eps - two) * near
end function

pure function CAT(tweakedInfinitePerspective_,MATRIX_TYPE_NAME)(fovy, aspect, near) result(res)
    implicit none
    real(kind=SCALAR_TYPE_KIND), intent(in) :: fovy, aspect, near
    type(CAT(MATRIX_TYPE_NAME,4x4)) :: res

    res = CAT(tweakedInfinitePerspective_eps_,MATRIX_TYPE_NAME)(fovy, aspect, near, epsilon(near))
end function

