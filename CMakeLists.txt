cmake_minimum_required(VERSION 3.19)
project(G4SSG)

enable_testing()

set(CMAKE_BUILD_PARALLEL_LEVEL 8)
set(CMAKE_CURRENT_BINARY_DIR "${CMAKE_CURRENT_BINARY_DIR}/bin")

set(CPY_DIR CPY)
set(CPY_DEPS ${CPY_DIR}/*.CPY)

set(COBOL_COMPILER cobc)
set(COBOL_FLAGS -x -I${CPY_DIR})
set(COBOL_DEBUG )

#
# Main App.
#
set(MAIN_SOURCE WRAPPER.CBL CBL/MAIN.CBL)
set(MAIN_EXE budget)
set(MAIN_PATH ${CMAKE_CURRENT_BINARY_DIR}/${MAIN_EXE})
add_custom_command(
    OUTPUT ${MAIN_PATH}
    COMMAND ${COBOL_COMPILER} ${COBOL_FLAGS} ${COBOL_DEBUG} -o ${MAIN_PATH} ${MAIN_SOURCE}
    DEPENDS ${MAIN_SOURCE} ${CPY_DEPS}
    COMMENT "Compiling main application (${MAIN_EXE}) ..."
)
add_custom_target(build_cobol ALL DEPENDS ${MAIN_PATH})
