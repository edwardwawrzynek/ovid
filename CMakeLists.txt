cmake_minimum_required(VERSION 3.15.3)
project(ovid)
set(CMAKE_CXX_STANDARD 17)


add_definitions(-D_DEBUG -D__STDC_CONSTANT_MACROS -D__STDC_FORMAT_MACROS -D__STDC_LIMIT_MACROS)

include_directories(${CMAKE_CURRENT_BINARY_DIR})

add_executable(ovid src/main.cpp src/parser.cpp src/tokenizer.cpp src/error.cpp src/ast.cpp)
target_compile_options(ovid PRIVATE -Wall -Wextra -pedantic)

#set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "/usr/local/share/llvm/cmake")
find_package(LLVM)

if( NOT LLVM_FOUND )
    message(FATAL_ERROR "LLVM package can't be found. Set CMAKE_PREFIX_PATH variable to LLVM's installation prefix.")
endif()

include_directories( ${LLVM_INCLUDE_DIRS} ".")
link_directories( ${LLVM_LIBRARY_DIRS} )

llvm_map_components_to_libnames(REQ_LLVM_LIBRARIES  native)

target_link_libraries(ovid ${REQ_LLVM_LIBRARIES})
