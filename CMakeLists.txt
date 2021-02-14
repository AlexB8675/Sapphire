cmake_minimum_required(VERSION 3.17 FATAL_ERROR)
project(Sapphire CXX)

set(CMAKE_CXX_STANDARD 17)

#find_package(LLVM REQUIRED)

add_executable(${PROJECT_NAME}
    src/compiler/ast.cpp
    src/compiler/ast.hpp
    src/compiler/parser.cpp
    src/compiler/parser.hpp
    src/main.cpp)

#[[llvm_map_components_to_libnames(LLVM_LIBS
    X86CodeGen
    X86AsmParser
    X86Desc
    X86Info
    TextAPI
    OrcJIT
    JITLink
    ObjectYAML
    WindowsManifest
    Coverage
    TableGen
    LTO
    Passes
    ObjCARCOpts
    Coroutines
    LibDriver
    XRay
    MIRParser
    ipo
    Instrumentation
    Vectorize
    Linker
    IRReader
    AsmParser
    Symbolize
    DebugInfoPDB
    FuzzMutate
    LineEditor
    MCA
    DebugInfoGSYM
    GlobalISel
    SelectionDAG
    AsmPrinter
    DebugInfoDWARF
    MCJIT
    Interpreter
    ExecutionEngine
    RuntimeDyld
    CodeGen
    Target
    ScalarOpts
    InstCombine
    AggressiveInstCombine
    TransformUtils
    BitWriter
    Analysis
    ProfileData
    DlltoolDriver
    Option
    Object
    MCParser
    MC
    DebugInfoCodeView
    DebugInfoMSF
    BitReader
    Core
    Remarks
    BinaryFormat
    BitstreamReader
    Support
    Demangle)]]

set_target_properties(${PROJECT_NAME} PROPERTIES OUTPUT_NAME "sphc")
target_include_directories(${PROJECT_NAME} PRIVATE "src" #[["${LLVM_INCLUDE_DIRS}"]])
#[[target_compile_definitions(${PROJECT_NAME} PRIVATE ${LLVM_DEFINITIONS})
target_link_libraries(${PROJECT_NAME} PRIVATE ${LLVM_LIBS})]]
