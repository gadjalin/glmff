#undef SCALAR_TYPE_FULL
#undef SCALAR_TYPE_BIT_SIZE
#undef VECTOR_TYPE_NAME
#undef VECTOR_TYPE_FULL
#define SCALAR_TYPE_FULL logical
#define VECTOR_TYPE_NAME bvec
#define VECTOR_TYPE_FULL CAT(VECTOR_TYPE_NAME,VECTOR_TYPE_SIZE)
#include "vec_conversion.inc"
#include "bvec_comparison.inc"
#include "bvec_relational.inc"
#include "bvec_common.inc"

#undef SCALAR_TYPE_FULL
#undef SCALAR_TYPE_KIND
#undef SCALAR_TYPE_BIT_SIZE
#undef VECTOR_TYPE_NAME
#undef VECTOR_TYPE_FULL
#define SCALAR_TYPE_FULL integer(kind=int32)
#define SCALAR_TYPE_KIND int32
#define SCALAR_TYPE_BIT_SIZE 32
#define VECTOR_TYPE_NAME ivec
#define VECTOR_TYPE_FULL CAT(VECTOR_TYPE_NAME,VECTOR_TYPE_SIZE)
#include "vec_conversion.inc"
#include "vec_arithmetic.inc"
#include "vec_comparison.inc"
#include "vec_relational.inc"
#include "vec_common.inc"

#undef SCALAR_TYPE_FULL
#undef SCALAR_TYPE_KIND
#undef SCALAR_TYPE_BIT_SIZE
#undef VECTOR_TYPE_NAME
#undef VECTOR_TYPE_FULL
#define SCALAR_TYPE_FULL real(kind=real32)
#define SCALAR_TYPE_KIND real32
#define SCALAR_TYPE_BIT_SIZE 32
#define VECTOR_TYPE_NAME vec
#define VECTOR_TYPE_FULL CAT(VECTOR_TYPE_NAME,VECTOR_TYPE_SIZE)
#include "vec_conversion.inc"
#include "vec_arithmetic.inc"
#include "vec_comparison.inc"
#include "vec_relational.inc"
#include "vec_common.inc"
#include "fvec_common.inc"
#include "fvec_trigonometric.inc"
#include "fvec_geometric.inc"

#undef SCALAR_TYPE_FULL
#undef SCALAR_TYPE_KIND
#undef SCALAR_TYPE_BIT_SIZE
#undef VECTOR_TYPE_NAME
#undef VECTOR_TYPE_FULL
#define SCALAR_TYPE_FULL real(kind=real64)
#define SCALAR_TYPE_KIND real64
#define SCALAR_TYPE_BIT_SIZE 64
#define VECTOR_TYPE_NAME dvec
#define VECTOR_TYPE_FULL CAT(VECTOR_TYPE_NAME,VECTOR_TYPE_SIZE)
#include "vec_conversion.inc"
#include "vec_arithmetic.inc"
#include "vec_comparison.inc"
#include "vec_relational.inc"
#include "vec_common.inc"
#include "fvec_common.inc"
#include "fvec_trigonometric.inc"
#include "fvec_geometric.inc"

