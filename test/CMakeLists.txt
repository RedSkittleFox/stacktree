cmake_minimum_required(VERSION 3.5)

include(FetchContent)
FetchContent_Declare(
    googletest
    URL https://github.com/google/googletest/archive/refs/tags/v1.13.0.zip
)
# For Windows: Prevent overriding the parent project's compiler/linker settings
set(gtest_force_shared_crt ON CACHE BOOL "" FORCE)
FetchContent_MakeAvailable(googletest)

file(GLOB_RECURSE sources 
    "${CMAKE_CURRENT_SOURCE_DIR}/*.cpp" 
    "${CMAKE_CURRENT_SOURCE_DIR}/*.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/*.cc"
    )

source_group(TREE ${CMAKE_CURRENT_SOURCE_DIR} FILES ${sources})

add_executable(
    stacktree_test
    ${sources}
)

target_link_libraries(
    stacktree_test
    GTest::gtest_main
    GTest::gmock_main
    stacktree
)

include(GoogleTest)
gtest_discover_tests(stacktree_test)

set_target_properties(gtest_main PROPERTIES FOLDER "vendor")
set_target_properties(gtest PROPERTIES FOLDER "vendor")
set_target_properties(gmock_main PROPERTIES FOLDER "vendor")
set_target_properties(gmock PROPERTIES FOLDER "vendor")