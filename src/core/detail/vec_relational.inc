#undef BOOL_VECTOR_TYPE
#define BOOL_VECTOR_TYPE CAT(bvec,VECTOR_TYPE_SIZE)

pure function CAT(lessThan_,VECTOR_TYPE_FULL)(lhs, rhs) result(res)
      implicit none
      type(VECTOR_TYPE_FULL), intent(in) :: lhs, rhs
      type(BOOL_VECTOR_TYPE) :: res

      res%data = lhs%data < rhs%data
end function

pure function CAT(lessThanEqual_,VECTOR_TYPE_FULL)(lhs, rhs) result(res)
      implicit none
      type(VECTOR_TYPE_FULL), intent(in) :: lhs, rhs
      type(BOOL_VECTOR_TYPE) :: res

      res%data = lhs%data <= rhs%data
end function

pure function CAT(greaterThan_,VECTOR_TYPE_FULL)(lhs, rhs) result(res)
      implicit none
      type(VECTOR_TYPE_FULL), intent(in) :: lhs, rhs
      type(BOOL_VECTOR_TYPE) :: res

      res%data = lhs%data > rhs%data
end function

pure function CAT(greaterThanEqual_,VECTOR_TYPE_FULL)(lhs, rhs) result(res)
      implicit none
      type(VECTOR_TYPE_FULL), intent(in) :: lhs, rhs
      type(BOOL_VECTOR_TYPE) :: res

      res%data = lhs%data >= rhs%data
end function

pure function CAT(equal_,VECTOR_TYPE_FULL)(lhs, rhs) result(res)
      implicit none
      type(VECTOR_TYPE_FULL), intent(in) :: lhs, rhs
      type(BOOL_VECTOR_TYPE) :: res

      res%data = lhs%data == rhs%data
end function

pure function CAT(notEqual_,VECTOR_TYPE_FULL)(lhs, rhs) result(res)
      implicit none
      type(VECTOR_TYPE_FULL), intent(in) :: lhs, rhs
      type(BOOL_VECTOR_TYPE) :: res

      res%data = lhs%data /= rhs%data
end function

