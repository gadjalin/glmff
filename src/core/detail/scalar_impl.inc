#undef SCALAR_TYPE_KIND
#undef SCALAR_TYPE_FULL
#undef SCALAR_TYPE_BIT_SIZE
#define SCALAR_TYPE_KIND int32
#define SCALAR_TYPE_BIT_SIZE 32
#define SCALAR_TYPE_FULL integer(kind=int32)
#include "int_common.inc"
#include "scalar_common.inc"

#undef SCALAR_TYPE_KIND
#undef SCALAR_TYPE_FULL
#undef SCALAR_TYPE_BIT_SIZE
#define SCALAR_TYPE_KIND int64
#define SCALAR_TYPE_BIT_SIZE 64
#define SCALAR_TYPE_FULL integer(kind=int64)
#include "int_common.inc"
#include "scalar_common.inc"

#undef SCALAR_TYPE_KIND
#undef SCALAR_TYPE_FULL
#undef SCALAR_TYPE_BIT_SIZE
#define SCALAR_TYPE_KIND real32
#define SCALAR_TYPE_BIT_SIZE 32
#define SCALAR_TYPE_FULL real(kind=real32)
#include "scalar_common.inc"
#include "real_geometric.inc"
#include "real_common.inc"

#undef SCALAR_TYPE_KIND
#undef SCALAR_TYPE_FULL
#undef SCALAR_TYPE_BIT_SIZE
#define SCALAR_TYPE_KIND real64
#define SCALAR_TYPE_BIT_SIZE 64
#define SCALAR_TYPE_FULL real(kind=real64)
#include "scalar_common.inc"
#include "real_geometric.inc"
#include "real_common.inc"

#include "scalar_constants.inc"

