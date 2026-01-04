#undef SCALAR_TYPE_FULL
#undef VECTOR_TYPE_NAME
#undef VECTOR_TYPE_FULL
#define SCALAR_TYPE_FULL logical
#define VECTOR_TYPE_NAME bvec
#define VECTOR_TYPE_FULL CAT(VECTOR_TYPE_NAME,VECTOR_TYPE_SIZE)
#include "vec_constructors.inc"

#undef SCALAR_TYPE_FULL
#undef VECTOR_TYPE_NAME
#undef VECTOR_TYPE_FULL
#define SCALAR_TYPE_FULL integer(kind=int32)
#define VECTOR_TYPE_NAME ivec
#define VECTOR_TYPE_FULL CAT(VECTOR_TYPE_NAME,VECTOR_TYPE_SIZE)
#include "vec_constructors.inc"

#undef SCALAR_TYPE_FULL
#undef VECTOR_TYPE_NAME
#undef VECTOR_TYPE_FULL
#define SCALAR_TYPE_FULL real(kind=real32)
#define VECTOR_TYPE_NAME vec
#define VECTOR_TYPE_FULL CAT(VECTOR_TYPE_NAME,VECTOR_TYPE_SIZE)
#include "vec_constructors.inc"

#undef SCALAR_TYPE_FULL
#undef VECTOR_TYPE_NAME
#undef VECTOR_TYPE_FULL
#define SCALAR_TYPE_FULL real(kind=real64)
#define VECTOR_TYPE_NAME dvec
#define VECTOR_TYPE_FULL CAT(VECTOR_TYPE_NAME,VECTOR_TYPE_SIZE)
#include "vec_constructors.inc"

