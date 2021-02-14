#include <compiler/parser.hpp>

#include <iostream>
#include <fstream>
#include <string>

int main(int argc, char* argv[]) {
    if (argc < 2) {
        std::cout << "Usage: ./sphc <filename> [--dump-ast]";
        return -1;
    }

    const std::string_view path = argv[1];
    const std::string_view dump = argv[2];

    bool dump_ast = dump == "--dump-ast";
    std::ofstream ast_file("../ast.txt");
    auto ast = spc::parse(path);
    if (ast && dump_ast) {
        spc::print(*ast, ast_file, 0);
    }

    return 0;
}