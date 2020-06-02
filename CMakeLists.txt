cmake_minimum_required(VERSION 3.15.3)
project(ovid)
set(CMAKE_CXX_STANDARD 17)

include_directories(src/compile src/test src/printers)
set(SOURCES src/compile/parser.cpp src/compile/tokenizer.cpp src/compile/error.cpp src/compile/ast.cpp src/compile/resolve_pass.cpp src/printers/ast_printer.cpp)
set(DRIVER src/compile/main.cpp)
set(UNIT_TESTS src/test/misc_unit.cpp)
set(TEST_FRAMEWORK src/test/tester.cpp)

##################################
# Compiler options
add_definitions(-D_DEBUG -D__STDC_CONSTANT_MACROS -D__STDC_FORMAT_MACROS -D__STDC_LIMIT_MACROS)
include_directories(${CMAKE_CURRENT_BINARY_DIR})
set(CCFLAGS -Wall -Wextra -g)

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
# Unit test files
add_executable(runUnitTests ${SOURCES} ${UNIT_TESTS})

add_dependencies(runUnitTests gtest)
include_directories(${source_dir}/googletest/include)
target_link_libraries(runUnitTests ${binary_dir}/lib/libgtest.a ${binary_dir}/lib/libgtest_main.a pthread)

target_link_libraries(runUnitTests ${REQ_LLVM_LIBRARIES})

target_compile_options(runUnitTests PRIVATE ${CCFLAGS})

enable_testing()
add_test(NAME    runUnitTests
        COMMAND runUnitTests)

##################################
# custom test framework

add_executable(runTests ${SOURCES} ${TEST_FRAMEWORK})
target_compile_options(runTests PRIVATE ${CCFLAGS})
add_test(NAME runTests
        COMMAND runTests)

##################################
# Main build
# Main executable
add_executable(ovid ${SOURCES} ${DRIVER})
target_compile_options(ovid PRIVATE ${CCFLAGS})
target_link_libraries(ovid ${REQ_LLVM_LIBRARIES})
