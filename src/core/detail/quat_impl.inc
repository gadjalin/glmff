#undef SCALAR_TYPE_FULL
#undef SCALAR_TYPE_KIND
#undef VECTOR_TYPE_NAME
#undef MATRIX_TYPE_NAME
#undef QUAT_TYPE_FULL
#define SCALAR_TYPE_FULL real(kind=real32)
#define SCALAR_TYPE_KIND real32
#define VECTOR_TYPE_NAME vec
#define MATRIX_TYPE_NAME mat
#define QUAT_TYPE_FULL quat
#include "quat_conversion.inc"
#include "quat_arithmetic.inc"
#include "quat_comparison.inc"
#include "quat_common.inc"
#include "quat_geometric.inc"
#include "quat_relational.inc"
#include "quat_trigonometric.inc"

#undef SCALAR_TYPE_FULL
#undef SCALAR_TYPE_KIND
#undef VECTOR_TYPE_NAME
#undef MATRIX_TYPE_NAME
#undef QUAT_TYPE_FULL
#define SCALAR_TYPE_FULL real(kind=real64)
#define SCALAR_TYPE_KIND real64
#define VECTOR_TYPE_NAME dvec
#define MATRIX_TYPE_NAME dmat
#define QUAT_TYPE_FULL dquat
#include "quat_conversion.inc"
#include "quat_arithmetic.inc"
#include "quat_comparison.inc"
#include "quat_common.inc"
#include "quat_geometric.inc"
#include "quat_relational.inc"
#include "quat_trigonometric.inc"

