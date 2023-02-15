cmake_minimum_required(VERSION 3.24)
project(leon9cc C)

set(CMAKE_C_STANDARD 11)

set(CMAKE_RUNTIME_OUTPUT_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}")
add_executable(leon9cc leon9cc.c)
