#include <compiler/ast.hpp>

#include <iostream>

namespace spc {
    struct Indent {
        std::size_t count;
    };

    static std::ostream& operator <<(std::ostream& os, Indent indent) noexcept {
        for (std::size_t i = 0; i < indent.count; ++i) {
            os << "   ";
        }
        return os;
    }

    void print(const Node& node, std::ostream& os, std::size_t indent) noexcept {
        switch (node.kind) {
            case Node::Kind::unit: {
                const auto& unit = static_cast<const Unit&>(node);
                os << Indent{ indent } << "translation unit:\n";
                for (const auto& decl : unit.declarations) {
                    print(*decl, os, indent + 1);
                }
            } break;

            case Node::Kind::identifier: {
                const auto& ident = static_cast<const Identifier&>(node);
                os << Indent{ indent } << "identifier:\n";
                os << Indent{ indent + 1 } << "name: " << ident.name << '\n';
                os << Indent{ indent + 1 } << "scopes: ";
                if (!ident.scopes.empty()) {
                    if (ident.scopes[0] == ".globl") {
                        os << "::";
                        if (ident.scopes.size() > 1) {
                            for (std::size_t i = 1; i < ident.scopes.size() - 1; ++i) {
                                os << ident.scopes[i] << "::";
                            }
                            os << ident.scopes.back();
                        }
                    } else {
                        for (std::size_t i = 0; i < ident.scopes.size() - 1; ++i) {
                            os << ident.scopes[i] << "::";
                        }
                        os << ident.scopes.back();
                    }
                } else {
                    os << "unqualified.";
                }
                os << '\n';
            } break;

            case Node::Kind::plain_type: {
                const auto& type = static_cast<const PlainType&>(node);
                os << Indent{ indent } << "plain type:\n";
                os << Indent{ indent + 1 } << "qualifiers:\n";
                if (!type.quals.empty()) {
                    os << Indent{ indent + 2 } << "kind: ";
                    for (const auto& [kind, mut] : type.quals) {
                         os << (kind == TypeQualifier::Kind::pointer ? '*' : '&')
                           << (mut ? "mut " : "const ");
                    }
                    os << '\n';
                } else {
                    os << Indent{ indent + 2 } << "empty.\n";
                }
                os << Indent{ indent + 1 } << "name: " << type.name << '\n';
                os << Indent{ indent + 1 } << "scopes: ";
                if (!type.scopes.empty()) {
                    if (type.scopes[0] == ".globl") {
                        os << "::";
                        if (type.scopes.size() > 1) {
                            for (std::size_t i = 1; i < type.scopes.size() - 1; ++i) {
                                os << type.scopes[i] << "::";
                            }
                            os << type.scopes.back();
                        }
                    } else {
                        for (std::size_t i = 0; i < type.scopes.size() - 1; ++i) {
                            os << type.scopes[i] << "::";
                        }
                        os << type.scopes.back();
                    }
                } else {
                    os << "unqualified.";
                }
                os << '\n';
            } break;

            case Node::Kind::tuple_type: {
                const auto& type = static_cast<const TupleType&>(node);
                os << Indent{ indent } << "tuple type:\n";
                os << Indent{ indent + 1 } << "qualifiers:\n";
                if (!type.quals.empty()) {
                    os << Indent{ indent + 2 } << "kind: ";
                    for (const auto& [kind, mut] : type.quals) {
                        os << (kind == TypeQualifier::Kind::pointer ? '*' : '&')
                           << (mut ? "mut " : "const ");
                    }
                    os << '\n';
                } else {
                    os << Indent{ indent + 2 } << "empty.\n";
                }
                os << Indent{ indent + 1 } << "type list:\n";
                for (const auto& each : type.types) {
                    print(*each, os, indent + 2);
                }
            } break;

            case Node::Kind::array_type: {
                const auto& type = static_cast<const ArrayType&>(node);
                os << Indent{ indent } << "array type:\n";
                os << Indent{ indent + 1 } << "qualifiers:\n";
                if (!type.quals.empty()) {
                    os << Indent{ indent + 2 } << "kind: ";
                    for (const auto& [kind, mut] : type.quals) {
                        os << (kind == TypeQualifier::Kind::pointer ? '*' : '&')
                           << (mut ? "mut " : "const ");
                    }
                    os << '\n';
                } else {
                    os << Indent{ indent + 2 } << "empty.\n";
                }
                os << Indent{ indent + 1 } << "element type:\n";
                print(*type.type, os, indent + 2);
                os << Indent{ indent + 1 } << "size:\n";
                if (type.size) {
                    print(*type.size, os, indent + 2);
                } else {
                    os << Indent{ indent + 2 } << "dynamic.\n";
                }
            } break;

            case Node::Kind::func_type: {
                const auto& type = static_cast<const FuncType&>(node);
                os << Indent{ indent } << "function type:\n";
                os << Indent{ indent + 1 } << "parameters types: "
                   << (type.param_types.empty() ? "empty." : "\n");
                for (const auto& each : type.param_types) {
                    print(*each, os, indent + 2);
                }
                os << Indent{ indent + 1 } << "return type:\n";
                print(*type.ret_type, os, indent + 2);
            } break;

            case Node::Kind::func_param: {
                const auto& param = static_cast<const FuncParam&>(node);
                os << Indent{ indent } << "function parameter:\n";
                os << Indent{ indent + 1 } << "mutable: " << (param.is_mut ? "true" : "false") << '\n';
                os << Indent{ indent + 1 } << "name: " << param.name << '\n';
                print(*param.type, os, indent + 1);
            } break;

            case Node::Kind::var_decl: {
                const auto& var_decl = static_cast<const VarDecl&>(node);
                os << Indent{ indent } << "variable declaration:\n";
                os << Indent{ indent + 1 } << "name: " << var_decl.name << '\n';
                print(*var_decl.type, os, indent + 1);
                os << Indent{ indent + 1 } << "mutable: " << (var_decl.mut ? "true" : "false")  << '\n';
                if (var_decl.initializer) {
                    os << Indent{ indent + 1 } << "initializer:\n";
                    print(*var_decl.initializer, os, indent + 2);
                }
            } break;

            case Node::Kind::func_decl: {
                const auto& func_decl = static_cast<const FuncDecl&>(node);
                os << Indent{ indent } << "function declaration:\n";
                os << Indent{ indent + 1 } << "name: " << func_decl.name << '\n';
                os << Indent{ indent + 1 } << "parameters:" << (func_decl.params.empty() ? " empty.\n" : "\n");
                for (const auto& param : func_decl.params) {
                    print(*param, os, indent + 2);
                }
                os << Indent{ indent + 1 } << "return type:\n";
                print(*func_decl.ret_type, os, indent + 2);
                if (func_decl.stmt_list) {
                    os << Indent{ indent + 1 } << "compound statement:" << (func_decl.stmt_list->empty() ? "empty.\n" : "\n");
                    for (const auto& stmt : *func_decl.stmt_list) {
                        print(*stmt, os, indent + 2);
                    }
                }
            } break;

            case Node::Kind::literal_expr: {
                const auto& expr = static_cast<const LiteralExpr&>(node);
                os << Indent{ indent } << "literal expression:\n";
                os << Indent{ indent + 1 } << "value: " << expr.literal << '\n';
            } break;

            case Node::Kind::tuple_literal_expr: {
                const auto& expr = static_cast<const TupleLiteralExpr&>(node);
                os << Indent{ indent } << "tuple literal expression:\n";
                os << Indent{ indent + 1 } << "value:\n";
                for (const auto& each : expr.literal) {
                    print(*each, os, indent + 2);
                }
            } break;

            case Node::Kind::array_literal_expr: {
                const auto& expr = static_cast<const ArrayLiteralExpr&>(node);
                os << Indent{ indent } << "array literal expression:\n";
                os << Indent{ indent + 1 } << "values:\n";
                for (const auto& each : expr.literal) {
                    print(*each, os, indent + 2);
                }
            } break;

            case Node::Kind::borrow_expr: {
                const auto& expr = static_cast<const BorrowExpr&>(node);
                os << Indent{ indent } << "borrow expression:\n";
                os << Indent{ indent + 1 } << "is mutable: " << (expr.is_mut ? "true" : "false") << '\n';
                os << Indent{ indent + 1 } << "expression:\n";
                print(*expr.expr, os, indent + 2);
            } break;

            case Node::Kind::raw_pointer_expr: {
                const auto& expr = static_cast<const RawPointerExpr&>(node);
                os << Indent{ indent } << "raw pointer expression:\n";
                os << Indent{ indent + 1 } << "is mutable: " << (expr.is_mut ? "true" : "false") << '\n';
                os << Indent{ indent + 1 } << "expression:\n";
                print(*expr.expr, os, indent + 2);
            } break;

            case Node::Kind::prefix_unary_expr: {
                const auto& expr = static_cast<const PrefixUnaryExpr&>(node);
                os << Indent{ indent } << "prefix unary expression:\n";
                os << Indent{ indent + 1 } << "operator: " << expr.op << '\n';
                print(*expr.expr, os, indent + 1);
            } break;

            case Node::Kind::postfix_unary_expr: {
                const auto& expr = static_cast<const PostUnaryExpr&>(node);
                os << Indent{ indent } << "postfix unary expression:\n";
                os << Indent{ indent + 1 } << "operator: " << expr.op << '\n';
                print(*expr.expr, os, indent + 1);
            } break;

            case Node::Kind::binary_expr: {
                const auto& expr = static_cast<const BinaryExpr&>(node);
                os << Indent{ indent } << "binary expression:\n";
                print(*expr.lhs, os, indent + 1);
                os << Indent{ indent + 1 } << "operator: " << expr.op << '\n';
                print(*expr.rhs, os, indent + 1);
            } break;

            case Node::Kind::ident_expr: {
                const auto& expr = static_cast<const IdentExpr&>(node);
                os << Indent{ indent } << "identifier expression:\n";
                print(*expr.val, os, indent + 1);
            } break;

            case Node::Kind::call_expr: {
                const auto& call = static_cast<const CallExpr&>(node);
                os << Indent{ indent } << "function call expression:\n";
                os << Indent{ indent + 1 } << "function name:\n";
                print(*call.name, os, indent + 2);
                os << Indent{ indent + 1 } << "argument list:" << (call.args.empty() ? " empty.\n" : "\n");
                for (const auto& each : call.args) {
                    print(*each, os, indent + 2);
                }
            } break;

            case Node::Kind::subscript_expr: {
                const auto& subscript = static_cast<const SubscriptExpr&>(node);
                os << Indent{ indent } << "subscript expression:\n";
                os << Indent{ indent + 1 } << "left expression:\n";
                print(*subscript.left, os, indent + 2);
                os << Indent{ indent + 1 } << "argument list:\n";
                for (const auto& arg : subscript.args) {
                    print(*arg, os, indent + 2);
                }
            } break;

            case Node::Kind::return_stmt: {
                const auto& stmt = static_cast<const ReturnStmt&>(node);
                os << Indent{ indent } << "return statement:\n";
                print(*stmt.expr, os, indent + 1);
            } break;

            case Node::Kind::compound_stmt: {
                const auto& compound = static_cast<const CompoundStmt&>(node);
                os << Indent{ indent } << "compound statement:\n";
                for (const auto& stmt : compound.stmt_list) {
                    print(*stmt, os, indent + 1);
                }
            } break;

            case Node::Kind::var_decl_stmt: {
                const auto& decl = static_cast<const VarDeclStmt&>(node);
                os << Indent{ indent } << "variable declaration statement:\n";
                print(*decl.var, os, indent + 1);
            } break;

            case Node::Kind::if_stmt: {
                const auto& if_stmt = static_cast<const IfStmt&>(node);
                os << Indent{ indent } << "if statement:\n";
                os << Indent{ indent + 1 } << "condition:\n";
                print(*if_stmt.cond, os, indent + 2);

                os << Indent{ indent + 1 } << "true block:\n";
                print(*if_stmt.true_block, os, indent + 2);

                if (if_stmt.false_block) {
                    os << Indent{ indent + 1 } << "false block:\n";
                    print(*if_stmt.false_block, os, indent + 2);
                }
            } break;

            case Node::Kind::while_stmt: {
                const auto& while_stmt = static_cast<const WhileStmt&>(node);
                os << Indent{ indent } << "while statement:\n";
                os << Indent{ indent + 1 } << "condition:\n";
                print(*while_stmt.cond, os, indent + 2);
                os << Indent{ indent + 1 } << "block:\n";
                print(*while_stmt.block, os, indent + 2);
            } break;

            case Node::Kind::expr_stmt: {
                const auto& block = static_cast<const ExprStmt&>(node);
                os << Indent{ indent } << "expression statement:\n";
                print(*block.expr, os, indent + 1);
            } break;
        }
    }
} // namespace spc