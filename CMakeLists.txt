cmake_minimum_required(VERSION 3.15.3)
project(ovid)
set(CMAKE_CXX_STANDARD 17)

find_package(BISON)
find_package(FLEX)

add_definitions(-D_DEBUG -D__STDC_CONSTANT_MACROS -D__STDC_FORMAT_MACROS -D__STDC_LIMIT_MACROS)

BISON_TARGET(MyParser parser.y ${CMAKE_CURRENT_BINARY_DIR}/parser.cpp)
FLEX_TARGET(MyScanner tokens.l ${CMAKE_CURRENT_BINARY_DIR}/tokens.cpp)
ADD_FLEX_BISON_DEPENDENCY(MyScanner MyParser)

include_directories(${CMAKE_CURRENT_BINARY_DIR})

add_executable(ovid ${BISON_MyParser_OUTPUTS} ${FLEX_MyScanner_OUTPUTS} main.cpp)

#set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "/usr/local/share/llvm/cmake")
find_package(LLVM)

if( NOT LLVM_FOUND )
    message(FATAL_ERROR "LLVM package can't be found. Set CMAKE_PREFIX_PATH variable to LLVM's installation prefix.")
endif()

include_directories( ${LLVM_INCLUDE_DIRS} ".")
link_directories( ${LLVM_LIBRARY_DIRS} )

llvm_map_components_to_libnames(REQ_LLVM_LIBRARIES jit native)

target_link_libraries(ovid ${REQ_LLVM_LIBRARIES})
