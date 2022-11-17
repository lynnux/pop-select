cmake_minimum_required(VERSION 3.2)

# 由于rust编译cpp时看不到错误消息，故而建立一个临时cmake项目
project(shell_pass_compile)

add_definitions(-DUNICODE -D_UNICODE)
add_library(shell_pass_compile shellmenu_wrap.cpp ShellContextMenu.cpp Utils.cpp)
# target_link_libraries(shell_pass_compile )
