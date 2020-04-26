cmake_minimum_required(VERSION 3.15.3)
project(ovid)
set(CMAKE_CXX_STANDARD 17)

include_directories(src/)
set(SOURCES src/parser.cpp src/tokenizer.cpp src/error.cpp src/ast.cpp src/symbols.cpp)
set(DRIVER src/main.cpp)
set(TESTS test/test.cpp)

##################################
# LLVM stuff
find_package(LLVM)

if( NOT LLVM_FOUND )
    message(FATAL_ERROR "LLVM package can't be found. Set CMAKE_PREFIX_PATH variable to LLVM's installation prefix.")
endif()

include_directories( ${LLVM_INCLUDE_DIRS} ".")
link_directories( ${LLVM_LIBRARY_DIRS} )

llvm_map_components_to_libnames(REQ_LLVM_LIBRARIES  native)

##################################
# Download and install GoogleTest

include(ExternalProject)
ExternalProject_Add(gtest
        URL https://github.com/google/googletest/archive/release-1.10.0.zip
        PREFIX ${CMAKE_CURRENT_BINARY_DIR}/gtest
        INSTALL_COMMAND ""
        )
ExternalProject_Get_Property(gtest source_dir binary_dir)

################
# Test files
add_executable(runTests ${SOURCES} ${TESTS})

######################################
# Configure the test to use GoogleTest

add_dependencies(runTests gtest)
include_directories(${source_dir}/googletest/include)
target_link_libraries(runTests ${binary_dir}/lib/libgtest.a ${binary_dir}/lib/libgtest_main.a pthread)

target_link_libraries(runTests ${REQ_LLVM_LIBRARIES})

target_compile_options(runTests PRIVATE -Wall -Wextra -pedantic)

##################################
# make the test runnable with

enable_testing()
add_test(NAME    runTests
        COMMAND runTests)

##################################
# Main build
# Compiler options
add_definitions(-D_DEBUG -D__STDC_CONSTANT_MACROS -D__STDC_FORMAT_MACROS -D__STDC_LIMIT_MACROS)

include_directories(${CMAKE_CURRENT_BINARY_DIR})


# Main executable
add_executable(ovid ${SOURCES} ${DRIVER})
target_compile_options(ovid PRIVATE -Wall -Wextra -pedantic)

#set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "/usr/local/share/llvm/cmake")

target_link_libraries(ovid ${REQ_LLVM_LIBRARIES})
