#pragma once

#include <optional>
#include <utility>
#include <vector>
#include <memory>
#include <string>

namespace spc {
    struct Decl;
    struct Expr;
    struct Stmt;
    struct Type;
    struct PlainType;
    struct VarDecl;
    struct FuncParam;

    template <typename T>
    using UniqueVec = std::vector<std::unique_ptr<T>>;

    using FuncParamList = UniqueVec<FuncParam>;
    using SubscriptArgList = UniqueVec<Expr>;
    using TupleTypesList = UniqueVec<Type>;
    using TupleExprList = UniqueVec<Expr>;
    using ArrayExprList = UniqueVec<Expr>;
    using FuncArgList = UniqueVec<Expr>;
    using DeclList = UniqueVec<Decl>;
    using StmtList = UniqueVec<Stmt>;

    struct TypeQualifier {
        enum class Kind {
            pointer,
            reference
        } kind;
        bool is_mut;
    };

    using TypeQualifiers = std::vector<TypeQualifier>;
    using ScopeList = std::vector<std::string>;

    struct Node {
        enum class Kind {
            unit,
            match_branch,
            identifier,
            plain_type,
            tuple_type,
            array_type,
            func_param,
            func_decl,
            var_decl,
            literal_expr,
            tuple_literal_expr,
            array_literal_expr,
            ident_expr,
            borrow_expr,
            raw_pointer_expr,
            prefix_unary_expr,
            postfix_unary_expr,
            binary_expr,
            call_expr,
            subscript_expr,
            compound_stmt,
            var_decl_stmt,
            return_stmt,
            if_stmt,
            while_stmt,
            match_stmt,
            expr_stmt
        };

        Node(Kind kind) noexcept : kind(kind) {}
        virtual ~Node() noexcept = default;

        Kind kind;
    };

    struct Unit : Node {
        Unit() noexcept : Node(Kind::unit) {}

        void push_decl(Decl* decl) noexcept {
            declarations.emplace_back(decl);
        }

        DeclList declarations;
    };

    struct Identifier : Node {
        Identifier(std::string&& name, ScopeList&& scopes) noexcept
            : Node(Kind::identifier),
              name(std::move(name)),
              scopes(std::move(scopes)) {}

        std::string name;
        ScopeList scopes;
    };

    struct Type : Node {
        using Node::Node;
    };

    struct PlainType : Type {
        PlainType(std::string&& name, TypeQualifiers&& quals, ScopeList&& scopes) noexcept
        : Type(Kind::plain_type),
          name(std::move(name)),
          quals(std::move(quals)),
          scopes(std::move(scopes)) {}

        std::string name;
        TypeQualifiers quals;
        ScopeList scopes;
    };

    struct TupleType : Type {
        TupleType(TupleTypesList&& types, TypeQualifiers&& quals) noexcept
            : Type(Kind::tuple_type),
              types(std::move(types)),
              quals(std::move(quals)) {}

        TupleTypesList types;
        TypeQualifiers quals;
    };

    struct ArrayType : Type {
        ArrayType(Expr* size, Type* type, TypeQualifiers&& quals) noexcept
            : Type(Kind::array_type),
              size(size),
              type(type),
              quals(quals) {}

        std::unique_ptr<Expr> size;
        std::unique_ptr<Type> type;
        TypeQualifiers quals;
    };

    struct FuncParam : Node {
        FuncParam(bool is_mut, std::string&& name, Type* type) noexcept
            : Node(Kind::func_param),
              is_mut(is_mut),
              name(std::move(name)),
              type(type) {}

        bool is_mut;
        std::string name;
        std::unique_ptr<Type> type;
    };

    struct Decl : Node {
        using Node::Node;
    };

    struct FuncDecl : Decl {
        FuncDecl(std::string&& name, FuncParamList&& func_params, Type* ret_type, std::optional<StmtList>&& stmt_list) noexcept
            : Decl(Kind::func_decl),
              name(std::move(name)),
              params(std::move(func_params)),
              ret_type(ret_type),
              stmt_list(std::move(stmt_list)) {}

        std::string name;
        FuncParamList params;
        std::unique_ptr<Type> ret_type;
        std::optional<StmtList> stmt_list;
    };

    struct VarDecl : Decl {
        VarDecl(bool mut, std::string&& name, Type* type, Expr* initializer) noexcept
            : Decl(Kind::var_decl),
            name(std::move(name)),
            type(type),
            mut(mut),
            initializer(initializer) {}

        bool mut;
        std::string name;
        std::unique_ptr<Type> type;
        std::unique_ptr<Expr> initializer;
    };

    struct Expr : Node {
        using Node::Node;
    };

    struct LiteralExpr : Expr {
        LiteralExpr(std::string&& literal) noexcept
            : Expr(Kind::literal_expr),
              literal(std::move(literal)) {}

        std::string literal;
    };

    struct TupleLiteralExpr : Expr {
        TupleLiteralExpr(TupleExprList&& literal) noexcept
        : Expr(Kind::tuple_literal_expr),
          literal(std::move(literal)) {}

        TupleExprList literal;
    };

    struct ArrayLiteralExpr : Expr {
        ArrayLiteralExpr(ArrayExprList&& literal) noexcept
            : Expr(Kind::array_literal_expr),
            literal(std::move(literal)) {}

        ArrayExprList literal;
    };

    struct IdentExpr : Expr {
        IdentExpr(Identifier* val) noexcept
            : Expr(Kind::ident_expr),
              val(val) {}

        std::unique_ptr<Identifier> val;
    };

    struct BorrowExpr : Expr {
        BorrowExpr(bool is_mut, Expr* expr) noexcept
        : Expr(Kind::borrow_expr),
          is_mut(is_mut),
          expr(expr) {}

        bool is_mut;
        std::unique_ptr<Expr> expr;
    };

    struct RawPointerExpr : Expr {
        RawPointerExpr(bool is_mut, Expr* expr) noexcept
        : Expr(Kind::raw_pointer_expr),
          is_mut(is_mut),
          expr(expr) {}

        bool is_mut;
        std::unique_ptr<Expr> expr;
    };

    struct PrefixUnaryExpr : Expr {
        PrefixUnaryExpr(std::string&& op, Expr* expr) noexcept
            : Expr(Kind::prefix_unary_expr),
              op(std::move(op)),
              expr(expr) {}

        std::string op;
        std::unique_ptr<Expr> expr;
    };

    struct PostUnaryExpr : Expr {
        PostUnaryExpr(Expr* expr, std::string&& op) noexcept
            : Expr(Kind::postfix_unary_expr),
              expr(expr),
              op(std::move(op)) {}

        std::unique_ptr<Expr> expr;
        std::string op;
    };

    struct BinaryExpr : Expr {
        BinaryExpr(Expr* lhs, std::string&& op, Expr* rhs) noexcept
            : Expr(Kind::binary_expr),
              lhs(lhs),
              op(std::move(op)),
              rhs(rhs) {}

        std::unique_ptr<Expr> lhs;
        std::string op;
        std::unique_ptr<Expr> rhs;
    };

    struct CallExpr : Expr {
        CallExpr(Expr* name, FuncArgList&& args) noexcept
            : Expr(Kind::call_expr), name(name), args(std::move(args)) {}

        std::unique_ptr<Expr> name;
        FuncArgList args;
    };

    struct SubscriptExpr : Expr {
        SubscriptExpr(Expr* left, SubscriptArgList&& args) noexcept
            : Expr(Kind::subscript_expr), left(left), args(std::move(args)) {}

        std::unique_ptr<Expr> left;
        SubscriptArgList args;
    };

    struct Stmt : Node {
        using Node::Node;
    };

    struct CompoundStmt : Stmt {
        CompoundStmt(StmtList&& stmt_list) noexcept : Stmt(Kind::compound_stmt), stmt_list(std::move(stmt_list)) {}

        StmtList stmt_list;
    };

    struct VarDeclStmt : Stmt {
        VarDeclStmt(VarDecl* var) noexcept : Stmt(Kind::var_decl_stmt), var(var) {}

        std::unique_ptr<VarDecl> var;
    };

    struct ReturnStmt : Stmt {
        ReturnStmt(Expr* expr) noexcept : Stmt(Kind::return_stmt), expr(expr) {}

        std::unique_ptr<Expr> expr;
    };

    struct IfStmt : Stmt {
        IfStmt(Expr* cond, Stmt* true_block, Stmt* false_block) noexcept
            : Stmt(Kind::if_stmt), cond(cond), true_block(true_block), false_block(false_block) {}

        std::unique_ptr<Expr> cond;
        std::unique_ptr<Stmt> true_block;
        std::unique_ptr<Stmt> false_block;
    };

    struct WhileStmt : Stmt {
        WhileStmt(Expr* cond, Stmt* block) noexcept
            : Stmt(Kind::while_stmt), cond(cond), block(block) {}

        std::unique_ptr<Expr> cond;
        std::unique_ptr<Stmt> block;
    };

    struct MatchStmt : Stmt {
        MatchStmt(Expr* cond) : Stmt(Kind::match_stmt) {}
    };

    struct ExprStmt : Stmt {
        ExprStmt(Expr* expr) noexcept : Stmt(Kind::expr_stmt), expr(expr) {}

        std::unique_ptr<Expr> expr;
    };

    void print(const Node&, std::ostream&, std::size_t) noexcept;
} // namespace spc