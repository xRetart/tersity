cmake_minimum_required(VERSION 3.13.4)

# build compiler
add_executable(vector source/main.cpp)

# target_compile_features(vector PUBLIC cxx_std_20)
target_compile_options(vector PRIVATE -std=c++2a)

target_include_directories(vector PRIVATE /usr/include/llvm-11 /usr/include/llvm-c-11)

# llvm dependencies
target_link_libraries(vector PRIVATE LLVM-11)

install(TARGETS vector DESTINATION bin)