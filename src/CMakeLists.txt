# LLVM stuff
find_package(LLVM 11.1.0 REQUIRED)
message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")

if (NOT LLVM_FOUND)
    message(FATAL_ERROR "LLVM package can't be found. Set CMAKE_PREFIX_PATH variable to LLVM's installation prefix.")
endif ()

include_directories(${LLVM_INCLUDE_DIRS} ".")
link_directories(${LLVM_LIBRARY_DIRS})

llvm_map_components_to_libnames(REQ_LLVM_LIBRARIES native passes)

# Main sources shared by all executables
set(SOURCES ast.cpp ast_printer.cpp error.cpp escape_analysis.cpp ir.cpp ir_printer.cpp llvm_codegen.cpp name_mangle.cpp parser.cpp resolve_pass.cpp tokenizer.cpp type_check.cpp generics.cpp generics_pass.cpp)

# ovidc command line driver
set(DRIVER main.cpp)

# ovidc lib
add_library(ovidc_core STATIC ${SOURCES})

#ovidc bin
add_executable(ovidc ${DRIVER})
target_link_libraries(ovidc ovidc_core ${REQ_LLVM_LIBRARIES})

install(TARGETS ovidc DESTINATION ${CMAKE_INSTALL_BINDIR})

if (${ENABLE_TESTING})
    add_subdirectory(test_framework)
endif ()
