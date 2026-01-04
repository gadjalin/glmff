#undef SCALAR_TYPE_FULL
#undef SCALAR_TYPE_KIND
#undef SCALAR_TYPE_BIT_SIZE
#undef VECTOR_TYPE_NAME
#undef COL_TYPE_FULL
#undef ROW_TYPE_FULL
#undef MATRIX_TYPE_NAME
#undef MATRIX_TYPE_FULL
#define SCALAR_TYPE_FULL real(kind=real32)
#define SCALAR_TYPE_KIND real32
#define SCALAR_TYPE_BIT_SIZE 32
#define VECTOR_TYPE_NAME vec
#define MATRIX_TYPE_NAME mat
#define COL_TYPE_FULL CAT(VECTOR_TYPE_NAME,MATRIX_TYPE_NROW)
#define ROW_TYPE_FULL CAT(VECTOR_TYPE_NAME,MATRIX_TYPE_NCOL)
#define MATRIX_TYPE_FULL CAT4(MATRIX_TYPE_NAME,MATRIX_TYPE_NCOL,x,MATRIX_TYPE_NROW)
#include "mat_arithmetic.inc"
#include "mat_comparison.inc"
#include "mat_common.inc"
#include "mat_extra.inc"

#undef SCALAR_TYPE_FULL
#undef SCALAR_TYPE_KIND
#undef SCALAR_TYPE_BIT_SIZE
#undef VECTOR_TYPE_NAME
#undef COL_TYPE_FULL
#undef ROW_TYPE_FULL
#undef MATRIX_TYPE_NAME
#undef MATRIX_TYPE_FULL
#define SCALAR_TYPE_FULL real(kind=real64)
#define SCALAR_TYPE_KIND real64
#define SCALAR_TYPE_BIT_SIZE 64
#define VECTOR_TYPE_NAME dvec
#define MATRIX_TYPE_NAME dmat
#define COL_TYPE_FULL CAT(VECTOR_TYPE_NAME,MATRIX_TYPE_NROW)
#define ROW_TYPE_FULL CAT(VECTOR_TYPE_NAME,MATRIX_TYPE_NCOL)
#define MATRIX_TYPE_FULL CAT4(MATRIX_TYPE_NAME,MATRIX_TYPE_NCOL,x,MATRIX_TYPE_NROW)
#include "mat_arithmetic.inc"
#include "mat_comparison.inc"
#include "mat_common.inc"
#include "mat_extra.inc"

