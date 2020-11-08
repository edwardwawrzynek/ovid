include_directories(../)

set(UNIT_TESTS misc_unit.cpp)
set(TEST_FRAMEWORK tester.cpp)

# Download and install GoogleTest
include(ExternalProject)
ExternalProject_Add(gtest
        URL https://github.com/google/googletest/archive/release-1.10.0.zip
        PREFIX ${CMAKE_CURRENT_BINARY_DIR}/gtest
        INSTALL_COMMAND ""
        )
ExternalProject_Get_Property(gtest source_dir binary_dir)

# Gtests
add_executable(runUnitTests ${UNIT_TESTS})

add_dependencies(runUnitTests gtest)
target_link_libraries(runUnitTests ovidc_core)
include_directories(${source_dir}/googletest/include)
target_link_libraries(runUnitTests ${binary_dir}/lib/libgtest.a ${binary_dir}/lib/libgtest_main.a pthread)
target_link_libraries(runUnitTests ${REQ_LLVM_LIBRARIES})

enable_testing()
add_test(NAME runUnitTests
        COMMAND runUnitTests)

# Custom Test Framework
add_executable(runTests ${TEST_FRAMEWORK})
target_link_libraries(runTests ovidc_core ${REQ_LLVM_LIBRARIES})
add_test(NAME runTests
        COMMAND runTests)

install(TARGETS runTests runUnitTests DESTINATION ${CMAKE_BINARY_DIR})