cmake_minimum_required(VERSION 3.0.0)
project(bitwise C)

include(CTest)
enable_testing()

add_executable(bitwise ion/main.c ion/common.c ion/lex.c ion/ast.c ion/print.c ion/parse.c)

set(CPACK_PROJECT_NAME ${PROJECT_NAME})
set(CPACK_PROJECT_VERSION ${PROJECT_VERSION})
include(CPack)
