elemental function CAT3(determinant_,MATRIX_TYPE_NAME,2x2)(m) result(res)
    implicit none
    type(CAT(MATRIX_TYPE_NAME,2x2)), intent(in) :: m
    real(kind=SCALAR_TYPE_KIND) :: res

    res = m%data(1,1)*m%data(2,2) - m%data(1,2)*m%data(2,1)
end function

elemental function CAT3(determinant_,MATRIX_TYPE_NAME,3x3)(m) result(res)
    implicit none
    type(CAT(MATRIX_TYPE_NAME,3x3)), intent(in) :: m
    real(kind=SCALAR_TYPE_KIND) :: res

    res = m%data(1,1) * (m%data(2,2) * m%data(3,3) - m%data(2,3) * m%data(3,2)) &
        - m%data(1,2) * (m%data(2,1) * m%data(3,3) - m%data(2,3) * m%data(3,1)) &
        + m%data(1,3) * (m%data(2,1) * m%data(3,2) - m%data(2,2) * m%data(3,1))
end function

elemental function CAT3(determinant_,MATRIX_TYPE_NAME,4x4)(m) result(res)
    implicit none
    type(CAT(MATRIX_TYPE_NAME,4x4)), intent(in) :: m
    real(kind=SCALAR_TYPE_KIND) :: res

    real(kind=SCALAR_TYPE_KIND), dimension(6) :: subs
    real(kind=SCALAR_TYPE_KIND), dimension(4) :: coeffs

    subs(1) = m%data(3,3) * m%data(4,4) - m%data(3,4) * m%data(4,3)
    subs(2) = m%data(2,3) * m%data(4,4) - m%data(2,4) * m%data(4,3)
    subs(3) = m%data(2,3) * m%data(3,4) - m%data(2,4) * m%data(3,3)
    subs(4) = m%data(1,3) * m%data(4,4) - m%data(1,4) * m%data(4,3)
    subs(5) = m%data(1,3) * m%data(3,4) - m%data(1,4) * m%data(3,3)
    subs(6) = m%data(1,3) * m%data(2,4) - m%data(1,4) * m%data(2,3)

    coeffs(1) =   (m%data(2,2) * subs(1) - m%data(3,2) * subs(2) + m%data(4,2) * subs(3))
    coeffs(2) = - (m%data(1,2) * subs(1) - m%data(3,2) * subs(4) + m%data(4,2) * subs(5))
    coeffs(3) =   (m%data(1,2) * subs(2) - m%data(2,2) * subs(4) + m%data(4,2) * subs(6))
    coeffs(4) = - (m%data(1,2) * subs(3) - m%data(2,2) * subs(5) + m%data(3,2) * subs(6))

    res = m%data(1,1) * coeffs(1) + m%data(2,1) * coeffs(2) + &
          m%data(3,1) * coeffs(3) + m%data(4,1) * coeffs(4)
end function

elemental function CAT3(inverse_,MATRIX_TYPE_NAME,2x2)(m) result(res)
    implicit none
    type(CAT(MATRIX_TYPE_NAME,2x2)), intent(in) :: m
    type(CAT(MATRIX_TYPE_NAME,2x2)) :: res

    real(kind=SCALAR_TYPE_KIND) :: idet
    real(kind=SCALAR_TYPE_KIND), parameter :: one = CAT(1_,SCALAR_TYPE_KIND)

    idet = m%data(1,1)*m%data(2,2) - m%data(1,2)*m%data(2,1)
    idet = one / idet

    res%data(1,1) =   m%data(2,2)
    res%data(2,1) = - m%data(2,1)
    res%data(1,2) = - m%data(1,2)
    res%data(2,2) =   m%data(1,1)

    res%data = res%data * idet
end function

elemental function CAT3(inverse_,MATRIX_TYPE_NAME,3x3)(m) result(res)
    implicit none
    type(CAT(MATRIX_TYPE_NAME,3x3)), intent(in) :: m
    type(CAT(MATRIX_TYPE_NAME,3x3)) :: res

    real(kind=SCALAR_TYPE_KIND) :: idet
    real(kind=SCALAR_TYPE_KIND), parameter :: one = CAT(1_,SCALAR_TYPE_KIND)

    idet = m%data(1,1) * (m%data(2,2) * m%data(3,3) - m%data(2,3) * m%data(3,2)) &
         - m%data(1,2) * (m%data(2,1) * m%data(3,3) - m%data(2,3) * m%data(3,1)) &
         + m%data(1,3) * (m%data(2,1) * m%data(3,2) - m%data(2,2) * m%data(3,1))
    idet = one / idet

    res%data(1,1) =  (m%data(2,2) * m%data(3,3) - m%data(2,3) * m%data(3,2));
    res%data(2,1) = -(m%data(2,1) * m%data(3,3) - m%data(2,3) * m%data(3,1));
    res%data(3,1) =  (m%data(2,1) * m%data(3,2) - m%data(2,2) * m%data(3,1));
    res%data(1,2) = -(m%data(1,2) * m%data(3,3) - m%data(1,3) * m%data(3,2));
    res%data(2,2) =  (m%data(1,1) * m%data(3,3) - m%data(1,3) * m%data(3,1));
    res%data(3,2) = -(m%data(1,1) * m%data(3,2) - m%data(1,2) * m%data(3,1));
    res%data(1,3) =  (m%data(1,2) * m%data(2,3) - m%data(1,3) * m%data(2,2));
    res%data(2,3) = -(m%data(1,1) * m%data(2,3) - m%data(1,3) * m%data(2,1));
    res%data(3,3) =  (m%data(1,1) * m%data(2,2) - m%data(1,2) * m%data(2,1));

    res%data = res%data * idet
end function

elemental function CAT3(inverse_,MATRIX_TYPE_NAME,4x4)(m) result(res)
    implicit none
    type(CAT(MATRIX_TYPE_NAME,4x4)), intent(in) :: m
    type(CAT(MATRIX_TYPE_NAME,4x4)) :: res

    real(kind=SCALAR_TYPE_KIND), dimension(18) :: coeffs
    real(kind=SCALAR_TYPE_KIND), dimension(4,6) :: facs
    real(kind=SCALAR_TYPE_KIND), dimension(4,4) :: vecs
    real(kind=SCALAR_TYPE_KIND), dimension(4,4) :: invs
    real(kind=SCALAR_TYPE_KIND), dimension(4) :: dot0
    real(kind=SCALAR_TYPE_KIND), dimension(4), parameter :: signA = [+1, -1, +1, -1]
    real(kind=SCALAR_TYPE_KIND), dimension(4), parameter :: signB = [-1, +1, -1, +1]
    real(kind=SCALAR_TYPE_KIND) :: idet
    real(kind=SCALAR_TYPE_KIND), parameter :: one = CAT(1_,SCALAR_TYPE_KIND)

    coeffs(1)  = m%data(3,3) * m%data(4,4) - m%data(3,4) * m%data(4,3)
    coeffs(2)  = m%data(3,2) * m%data(4,4) - m%data(3,4) * m%data(4,2)
    coeffs(3)  = m%data(3,2) * m%data(4,3) - m%data(3,3) * m%data(4,2)

    coeffs(4)  = m%data(2,3) * m%data(4,4) - m%data(2,4) * m%data(4,3)
    coeffs(5)  = m%data(2,2) * m%data(4,4) - m%data(2,4) * m%data(4,2)
    coeffs(6)  = m%data(2,2) * m%data(4,3) - m%data(2,3) * m%data(4,2)

    coeffs(7)  = m%data(2,3) * m%data(3,4) - m%data(2,4) * m%data(3,3)
    coeffs(8)  = m%data(2,2) * m%data(3,4) - m%data(2,4) * m%data(3,2)
    coeffs(9)  = m%data(2,2) * m%data(3,3) - m%data(2,3) * m%data(3,2)

    coeffs(10) = m%data(1,3) * m%data(4,4) - m%data(1,4) * m%data(4,3)
    coeffs(11) = m%data(1,2) * m%data(4,4) - m%data(1,4) * m%data(4,2)
    coeffs(12) = m%data(1,2) * m%data(4,3) - m%data(1,3) * m%data(4,2)

    coeffs(13) = m%data(1,3) * m%data(3,4) - m%data(1,4) * m%data(3,3)
    coeffs(14) = m%data(1,2) * m%data(3,4) - m%data(1,4) * m%data(3,2)
    coeffs(15) = m%data(1,2) * m%data(3,3) - m%data(1,3) * m%data(3,2)

    coeffs(16) = m%data(1,3) * m%data(2,4) - m%data(1,4) * m%data(2,3)
    coeffs(17) = m%data(1,2) * m%data(2,4) - m%data(1,4) * m%data(2,2)
    coeffs(18) = m%data(1,2) * m%data(2,3) - m%data(1,3) * m%data(2,2)

    facs(:,1) = [ coeffs(1) , coeffs(1) , coeffs(2) , coeffs(3)  ]
    facs(:,2) = [ coeffs(4) , coeffs(4) , coeffs(5) , coeffs(6)  ]
    facs(:,3) = [ coeffs(7) , coeffs(7) , coeffs(8) , coeffs(9)  ]
    facs(:,4) = [ coeffs(10), coeffs(10), coeffs(11), coeffs(12) ]
    facs(:,5) = [ coeffs(13), coeffs(13), coeffs(14), coeffs(15) ]
    facs(:,6) = [ coeffs(16), coeffs(16), coeffs(17), coeffs(18) ]

    vecs(:,1) = [ m%data(1,2), m%data(1,1), m%data(1,1), m%data(1,1) ]
    vecs(:,2) = [ m%data(2,2), m%data(2,1), m%data(2,1), m%data(2,1) ]
    vecs(:,3) = [ m%data(3,2), m%data(3,1), m%data(3,1), m%data(3,1) ]
    vecs(:,4) = [ m%data(4,2), m%data(4,1), m%data(4,1), m%data(4,1) ]

    invs(:,1) = (vecs(:,2) * facs(:,1) - vecs(:,3) * facs(:,2) + vecs(:,4) * facs(:,3))
    invs(:,2) = (vecs(:,1) * facs(:,1) - vecs(:,3) * facs(:,4) + vecs(:,4) * facs(:,4))
    invs(:,3) = (vecs(:,1) * facs(:,2) - vecs(:,2) * facs(:,4) + vecs(:,4) * facs(:,5))
    invs(:,4) = (vecs(:,1) * facs(:,3) - vecs(:,2) * facs(:,4) + vecs(:,3) * facs(:,5))

    res%data(:,1) = invs(:,1) * signA
    res%data(:,2) = invs(:,2) * signB
    res%data(:,3) = invs(:,3) * signA
    res%data(:,4) = invs(:,4) * signB

    dot0 = (m%data(:,1) * res%data(1,:))
    idet = (dot0(1) + dot0(2)) + (dot0(3) + dot0(4))
    idet = one / idet

    res%data = res%data * idet
end function

