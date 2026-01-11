# glmff

OpenGL Mathematics For Fortran (GLMFF).

---

This is a Fortran adaptation of the [glm](https://github.com/g-truc/glm)
library.
It is part of my Fortran Multimedia Framework Portability Project.

It contains most of the core features and works the same way as the
well-known [glm](https://github.com/g-truc/glm) C++ library.

You only need to use the `glmff` module to access all the core functionalities:
```fortran
program glmff_example
    use glmff

    implicit none
    type(mat4) :: mat, proj
    type(vec3) :: pos

    mat = mat4(1.0) ! Identity matrix
    pos = vec3(0.5, 1.0, 0.0)
    mat = translate(mat, pos) ! Build a translation matrix
    proj = perspective(radians(60), 0.25, 0.1, 100.0) ! Build a projection matrix
    ! ...
end program
```

Extensions have not been implemented yet.

It must be noted that the Fortran data types are not as fancy as the C++
ones. Instead of using unions and individual variables for each components (x,
y, z / r, g, b, ...), the components of matrix and vector types are stored in an array.
This is both because Fortran does not have any reliable `union` equivalent, and
because Fortran inherently takes advantage of array structures to optimise
calculations.
```fortran
my_vec%data(1) = ... ! Set the X component
my_vec%data(2) = ... ! Set the Y component
my_vec%data = ... ! Set all components at once
```

The `data` component can then be passed directly to OpenGL functions.

## Compilation

Unfortunately, Fortran is not an ABI-compatible language. This means that a precompiled binary
of the library is not guaranteed to work unless it is used in the same
environment with the same compiler.
It is thus simpler to build your own version for your project in your
environment.

This will require
 - A Fortran compiler supporting the Fortran 2008 standard (GNU or Intel are most common)
 - CMake

Clone this repository:
```shell
git clone https://github.com/gadjalin/glmff
```

### On macOS and Linux

Use CMake to build:
```shell
cd glmff
cmake -B bin
cmake --build bin
```

The library and `mod` files are located in the `bin` and `bin/src/mod`
directories.

### On Windows

You may use CMake with tools such as Visual Studio or MinGW.

### Include in your project

You can instead add glmff as a dependency in your CMake script
```cmake
ADD_SUBDIRECTORY(path_to_glmff)
...
TARGET_LINK_LIBRARIES(... glmff ...)
```

## Distribution

Except for macOS, you can compile a Fortran executable
statically to facilitate distribution:
```shell
cmake -DBUILD_SHARE_LIBS=OFF -B bin
cmake --build bin
```

## Fortran Multimedia Framework

 - GLFW bindings: [glf90w](https://github.com/gadjalin/glf90w)
 - OpenGL bindings: [glad-fortran](https://github.com/gadjalin/glad-fortran)
 - OpenGL Mathematics: [glmff](https://github.com/gadjalin/glmff)

