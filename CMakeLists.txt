CMAKE_MINIMUM_REQUIRED(VERSION 3.20 FATAL_ERROR)

# --- Project configuration
SET(GLMFF_SRC_DIR "${CMAKE_CURRENT_SOURCE_DIR}/src")

PROJECT(GLMFF
    VERSION 0.1.0
    DESCRIPTION "OpenGL Mathematics For Fortran"
    HOMEPAGE_URL https://github.com/gadjalin/glmff.git
    LANGUAGES C Fortran
)

SET_PROPERTY(GLOBAL PROPERTY USE_FOLDER ON)
SET(CMAKE_DEBUG_POSTFIX -d)
SET(CMAKE_EXPORT_COMPILE_COMMANDS ON)

SET(CMAKE_ARCHIVE_OUTPUT_DIRECTORY "${PROJECT_BINARY_DIR}")
SET(CMAKE_LIBRARY_OUTPUT_DIRECTORY "${PROJECT_BINARY_DIR}")
SET(CMAKE_RUNTIME_OUTPUT_DIRECTORY "${PROJECT_BINARY_DIR}")

OPTION(BUILD_SHARED_LIBS "Build shared libraries." OFF)

IF(CMAKE_Fortran_COMPILER_ID STREQUAL "GNU")
    # Language Standard
    ADD_COMPILE_OPTIONS("$<$<COMPILE_LANGUAGE:Fortran>:-std=f2008>")
    # Debug
    ADD_COMPILE_OPTIONS("$<$<AND:$<COMPILE_LANGUAGE:Fortran>,$<CONFIG:Debug>>:-fbacktrace>")
    ADD_COMPILE_OPTIONS("$<$<COMPILE_LANGUAGE:Fortran>:-Wall;-Wextra;-Wimplicit-interface;-Wimplicit-procedure>")
    #ADD_COMPILE_OPTIONS("-save-temps")
    # Optimisation
    ADD_COMPILE_OPTIONS("$<$<AND:$<COMPILE_LANGUAGE:Fortran>,$<CONFIG:Debug>>:-fcheck=all;-Og;-g>")
    ADD_COMPILE_OPTIONS("$<$<AND:$<COMPILE_LANGUAGE:Fortran>,$<CONFIG:Release>>:-O3>")
    # Enforce no implicit typing
    ADD_COMPILE_OPTIONS("$<$<COMPILE_LANGUAGE:Fortran>:-fimplicit-none>")
ELSEIF(CMAKE_Fortran_COMPILER_ID STREQUAL "Intel" OR CMAKE_Fortran_COMPILER_ID STREQUAL "IntelLLVM")
    # Language Standard
    ADD_COMPILE_OPTIONS("$<$<COMPILE_LANGUAGE:Fortran>:-stand;f2008>")
    # Debug
    ADD_COMPILE_OPTIONS("$<$<AND:$<COMPILE_LANGUAGE:Fortran>,$<CONFIG:Debug>>:-traceback>")
    ADD_COMPILE_OPTIONS("$<$<COMPILE_LANGUAGE:Fortran>:-warn;all>")
    # Optimisation
    ADD_COMPILE_OPTIONS("$<$<AND:$<COMPILE_LANGUAGE:Fortran>,$<CONFIG:Debug>>:-check;all;-debug;all;-g>")
    ADD_COMPILE_OPTIONS("$<$<AND:$<COMPILE_LANGUAGE:Fortran>,$<CONFIG:Release>>:-O3;-vec>")
ELSE()
    MESSAGE(WARNING
        "Unsupported Fortran compiler: ${CMAKE_Fortran_COMPILER_ID}."
        "Please use \"-DCMAKE_Fortran_FLAGS=...\" in the meantime and report the issue to the project maintainer."
    )
ENDIF()

ADD_SUBDIRECTORY(${GLMFF_SRC_DIR})

