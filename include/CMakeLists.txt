cmake_minimum_required(VERSION 3.21)

file(GLOB_RECURSE sources 
    "${CMAKE_CURRENT_SOURCE_DIR}/*.cpp" 
    "${CMAKE_CURRENT_SOURCE_DIR}/*.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/*.inl"
    )

source_group(TREE ${CMAKE_CURRENT_SOURCE_DIR} FILES ${sources})

add_library(
    stacktree
    INTERFACE
    ${sources}
)

if(${IPO_SUPPORTED})
    set_target_properties(stacktree PROPERTIES INTERPROCEDURAL_OPTIMIZATION TRUE)
endif()

target_include_directories(
    stacktree
    INTERFACE
    ${CMAKE_CURRENT_SOURCE_DIR}
)
