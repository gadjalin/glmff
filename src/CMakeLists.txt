ADD_LIBRARY(glmff)

SET_TARGET_PROPERTIES(glmff PROPERTIES
    OUTPUT_NAME glmff
    VERSION ${GLMFF_VERSION_MAJOR}.${GLMFF_VERSION_MINOR}
    SOVERSION ${GLMFF_VERSION_MAJOR}
    POSITION_INDEPENDENT_CODE ON
    Fortran_FORMAT FREE
    Fortran_MODULE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/mod
    Fortran_PREPROCESS ON
    FOLDER "glmff"
)

TARGET_INCLUDE_DIRECTORIES(glmff PUBLIC
    "${CMAKE_CURRENT_BINARY_DIR}/mod"
    "${GLMFF_SRC_DIR}" # glmff_macros.h
)

TARGET_SOURCES(glmff PRIVATE
    glmff.F90

    core/glmff_scalar.F90
    core/glmff_vec2.F90
    core/glmff_vec3.F90
    core/glmff_vec4.F90
    core/glmff_vec.F90
    core/glmff_mat2x2.F90
    core/glmff_mat2x3.F90
    core/glmff_mat2x4.F90
    core/glmff_mat3x2.F90
    core/glmff_mat3x3.F90
    core/glmff_mat3x4.F90
    core/glmff_mat4x2.F90
    core/glmff_mat4x3.F90
    core/glmff_mat4x4.F90
    core/glmff_mat.F90
    core/glmff_quat.F90
)

