cmake_minimum_required(VERSION 3.15.3)
project(ovid)
set(CMAKE_CXX_STANDARD 17)

include(GNUInstallDirs)

# Set options for gcc and clang
if(CMAKE_CXX_COMPILER_ID MATCHES GNU OR CMAKE_CXX_COMPILER_ID MATCHES Clang)
    set(CMAKE_CXX_FLAGS         "${CMAKE_CXX_FLAGS} -Wall -Wextra -pedantic -Wno-unused-parameter")
    set(CMAKE_CXX_FLAGS_DEBUG   "-O0 -g3")
    set(CMAKE_CXX_FLAGS_RELEASE "-O3")
endif()

add_subdirectory(src)
add_subdirectory(runtime)