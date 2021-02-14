#include <compiler/parser.hpp>
#include <compiler/ast.hpp>

#include <algorithm>
#include <cstdint>
#include <sstream>
#include <fstream>
#include <string>
#include <array>

namespace spc {
    using namespace std::literals;

    constexpr static std::array reserved_keywords = {
        "fn"sv,
        "if"sv,
        "else"sv,
        "while"sv,
        "let"sv,
        "mut"sv,
        "true"sv,
        "false"sv,
        "null"sv,
        "return"sv,
        "i8"sv,
        "i16"sv,
        "i32"sv,
        "i64"sv,
        "u8"sv,
        "u16"sv,
        "u32"sv,
        "u64"sv,
        "char"sv,
        "c8"sv,
        "c16"sv,
        "c32"sv,
        "f32"sv,
        "f64"sv,
        "bool"sv,
        "void"sv
    };

    enum class Token {
        kw_fn,
        kw_if,
        kw_else,
        kw_while,
        kw_let,
        kw_mut,
        kw_const,
        kw_true,
        kw_false,
        kw_null,
        kw_return,

        pn_open_paren,
        pn_close_paren,
        pn_open_bracket,
        pn_close_bracket,
        pn_open_brace,
        pn_close_brace,
        pn_arrow,
        pn_fat_arrow,
        pn_dot,
        pn_comma,
        pn_colon,
        pn_scope_res,
        pn_semicolon,
        pn_modulo,
        pn_divide,
        pn_multiply,
        pn_minus,
        pn_decrement,
        pn_plus,
        pn_increment,
        pn_greater_equal,
        pn_less_equal,
        pn_greater,
        pn_less,
        pn_equal,
        pn_not,
        pn_not_equal,
        pn_logic_or,
        pn_bit_or,
        pn_logic_and,
        pn_bit_and,
        pn_assign,
        pn_at
    };

    constexpr static std::string_view token_to_string(Token token) noexcept {
        switch (token) {
            case Token::kw_fn:            return "fn";
            case Token::kw_if:            return "if";
            case Token::kw_else:          return "else";
            case Token::kw_while:         return "while";
            case Token::kw_let:           return "let";
            case Token::kw_mut:           return "mut";
            case Token::kw_const:         return "const";
            case Token::kw_true:          return "true";
            case Token::kw_false:         return "false";
            case Token::kw_null:          return "null";
            case Token::kw_return:        return "return";

            case Token::pn_open_paren:    return "(";
            case Token::pn_close_paren:   return ")";
            case Token::pn_open_bracket:  return "[";
            case Token::pn_close_bracket: return "]";
            case Token::pn_open_brace:    return "{";
            case Token::pn_close_brace:   return "}";
            case Token::pn_arrow:         return "->";
            case Token::pn_fat_arrow:     return "=>";
            case Token::pn_dot:           return ".";
            case Token::pn_comma:         return ",";
            case Token::pn_colon:         return ":";
            case Token::pn_scope_res:     return "::";
            case Token::pn_semicolon:     return ";";
            case Token::pn_minus:         return "-";
            case Token::pn_decrement:     return "--";
            case Token::pn_plus:          return "+";
            case Token::pn_increment:     return "++";
            case Token::pn_modulo:        return "%";
            case Token::pn_divide:        return "/";
            case Token::pn_multiply:      return "*";
            case Token::pn_greater_equal: return ">=";
            case Token::pn_less_equal:    return "<=";
            case Token::pn_greater:       return ">";
            case Token::pn_less:          return "<";
            case Token::pn_equal:         return "==";
            case Token::pn_not:           return "!";
            case Token::pn_not_equal:     return "!=";
            case Token::pn_logic_or:      return "||";
            case Token::pn_bit_or:        return "|";
            case Token::pn_logic_and:     return "&&";
            case Token::pn_bit_and:       return "&";
            case Token::pn_assign:        return "=";
            case Token::pn_at:            return "@";
        }
        return {};
    }

    static bool is_reserved(const std::string& token) noexcept {
        return std::any_of(reserved_keywords.begin(), reserved_keywords.end(), [token](const auto& keyword) {
            return keyword == token;
        });
    }

    static bool is_whitespace(char ch) noexcept {
        return ch == '\n' ||
               ch == '\r' ||
               ch == '\t' ||
               ch == '\v' ||
               ch == '\f' ||
               ch == ' ';
    }

    static bool is_alpha(char ch) noexcept {
        return ('a' <= ch && ch <= 'z') ||
               ('A' <= ch && ch <= 'Z');
    }

    static bool is_digit(char ch) noexcept {
        return '0' <= ch && ch <= '9';
    }

    static bool is_alphanumeric(char ch) noexcept {
        return is_alpha(ch) || is_digit(ch);
    }

    class Lexer {
    public:
        struct State {
            std::size_t offset;
            std::size_t line;
            std::size_t col;
        };

        static Lexer create(std::string_view path) noexcept {
            Lexer lexer;
            lexer._state = {
                0, 1, 1
            };
            lexer._source = (std::stringstream() << std::ifstream(path.data()).rdbuf()).str();
            return lexer;
        }

        bool eof() const noexcept {
            return _state.offset == _source.size();
        }

        char peek() const noexcept {
            return eof() ? '\0' : _source[_state.offset];
        }

        void update_line_state(char ch) noexcept {
            if (ch == '\n') {
                _state.line++;
                _state.col = 1;
            } else {
                _state.col++;
            }
        }

        void skip_whitespaces_and_comments() noexcept {
            while (true) {
                if (is_whitespace(peek())) {
                    next();
                    continue;
                }

                if (peek() == '/') {
                    const auto backup = _state;
                    next();
                    if (peek() == '/') {
                        next();
                        while (next() != '\n') {}
                        continue;
                    } else if (peek() == '*') {
                        next();
                        auto c1 = next(), c2 = peek();
                        for (; c1 != '*' || c2 != '/';) {
                            c1 = next(), c2 = peek();
                        }
                        next();
                        continue;
                    } else {
                        _state = backup;
                        break;
                    }
                }
                break;
            }
        }

        char next() noexcept {
            if (eof()) {
                return '\0';
            }

            const char next = _source[_state.offset++];
            update_line_state(next);
            return next;
        }

        void eat() noexcept {
            if (!eof()) {
                update_line_state(_source[_state.offset++]);
            }
        }

        template <typename... Args>
        bool match_token(Args&&... args) noexcept {
            const std::array tokens{ args... };
            skip_whitespaces_and_comments();

            return std::any_of(tokens.begin(), tokens.end(), [this](Token token) {
                const auto token_str = token_to_string(token);
                const auto state_backup = _state;

                if (std::all_of(token_str.begin(), token_str.end(), [this](char ch) { return next() == ch; })) {
                    _current = token;
                    return true;
                }
                _state = state_backup;
                return false;
            });
        }

        Token current() const noexcept {
            return _current;
        }

        std::string match_identifier() noexcept {
            skip_whitespaces_and_comments();
            std::string out;
            while (peek() == '_' || is_alphanumeric(peek())) {
                out += next();
            }
            return out;
        }

        std::string match_literal() noexcept {
            skip_whitespaces_and_comments();
            std::string literal;

            if (is_digit(peek())) {
                while (is_digit(peek())) {
                    literal += next();
                }
                return literal;
            }

            if (match_token(Token::kw_true)) {
                return "true";
            } else if (match_token(Token::kw_false)) {
                return "false";
            }

            if (peek() == '"') {
                eat();
                while (peek() != '"') {
                    literal += next();
                }
                return eat(), literal;
            }

            return {};
        }

        State state() const noexcept {
            return _state;
        }

        void restore(State state) noexcept {
            _state = state;
        }
    private:
        State _state;
        Token _current;
        std::string _source;
    };

    class Parser {
    public:
        static Parser create(std::string_view path) noexcept {
            Parser parser;
            parser._lexer = Lexer::create(path);
            return parser;
        }

        LiteralExpr* parse_literal_expr() noexcept {
            auto literal = _lexer.match_literal();
            if (literal.empty()) {
                return nullptr;
            }
            return new LiteralExpr(std::move(literal));
        }

        Identifier* parse_identifier() noexcept {
            ScopeList scopes;
            if (_lexer.match_token(Token::pn_scope_res)) {
                scopes.emplace_back(".globl");
            }

            auto ident = _lexer.match_identifier();
            if (ident.empty()) {
                if (!scopes.empty()) {
                    _emit_parse_error("expected identifier after '::' token");
                }
                return nullptr;
            }

            while (_lexer.match_token(Token::pn_scope_res)) {
                scopes.emplace_back(std::move(ident));
                if ((ident = _lexer.match_identifier()).empty()) {
                    _emit_parse_error("expected identifier after '::' token");
                    return nullptr;
                }
            }
            return new Identifier(std::move(ident), std::move(scopes));
        }

        TypeQualifiers parse_type_quals() noexcept {
            TypeQualifiers quals;
            while (_lexer.match_token(Token::pn_multiply, Token::pn_bit_and)) {
                char type;
                const auto kind = [this, &type]() {
                    switch (_lexer.current()) {
                        case Token::pn_multiply: {
                            type = '*';
                            return TypeQualifier::Kind::pointer;
                        }
                        case Token::pn_bit_and: {
                            type = '&';
                            return TypeQualifier::Kind::reference;
                        }
                    }
                }();

                if (_lexer.match_token(Token::kw_mut, Token::kw_const)) {
                    const auto is_mut = _lexer.current() == Token::kw_mut;
                    quals.push_back({ kind, is_mut });
                } else {
                    _emit_parse_error("expected 'mut' or 'const' token after '"s + type + "' token");
                    return {};
                }
            }
            return quals;
        }

        Type* parse_tuple_type() noexcept {
            const auto backup = _lexer.state();
            auto quals = parse_type_quals();
            if (!_lexer.match_token(Token::pn_open_paren)) {
                _lexer.restore(backup);
                return nullptr;
            }

            TupleTypesList types;
            do {
                std::unique_ptr<Type> type(parse_type());
                if (!type) {
                    _emit_parse_error("expected type in tuple type list");
                    return nullptr;
                } else {
                    types.emplace_back(std::move(type));
                }
            } while (_lexer.match_token(Token::pn_comma));

            if (!_lexer.match_token(Token::pn_close_paren)) {
                _emit_parse_error("expected ')' token after tuple type list");
                return nullptr;
            }
            return new TupleType(std::move(types), std::move(quals));
        }

        Type* parse_array_type() noexcept {
            const auto backup = _lexer.state();
            auto quals = parse_type_quals();
            if (!_lexer.match_token(Token::pn_open_bracket)) {
                _lexer.restore(backup);
                return nullptr;
            }

            std::unique_ptr<Type> type(parse_type());
            if (!type) {
                _emit_parse_error("expected type after token '['");
                _lexer.restore(backup);
                return nullptr;
            }

            if (_lexer.match_token(Token::pn_close_bracket)) {
                return new ArrayType(nullptr, type.release(), std::move(quals));
            }

            if (!_lexer.match_token(Token::pn_semicolon)) {
                _emit_parse_error("expected token ';' or ']' after type");
                _lexer.restore(backup);
                return nullptr;
            }

            std::unique_ptr<Expr> size(parse_expr());
            if (!size) {
                _emit_parse_error("expected core-constant expression after token ';'");
                _lexer.restore(backup);
                return nullptr;
            }

            if (!_lexer.match_token(Token::pn_close_bracket)) {
                _emit_parse_error("expected token ']' after array type");
                _lexer.restore(backup);
                return nullptr;
            }
            return new ArrayType(size.release(), type.release(), std::move(quals));
        }

        Type* parse_plain_type() noexcept {
            const auto backup = _lexer.state();
            auto quals = parse_type_quals();
            ScopeList scopes;
            if (_lexer.match_token(Token::pn_scope_res)) {
                scopes.emplace_back(".globl");
            }

            auto type = _lexer.match_identifier();
            if (type.empty()) {
                if (!scopes.empty()) {
                    _emit_parse_error("expected typename after '::' token");
                }
                _lexer.restore(backup);
                return nullptr;
            }

            while (_lexer.match_token(Token::pn_scope_res)) {
                scopes.emplace_back(std::move(type));
                if ((type = _lexer.match_identifier()).empty()) {
                    _emit_parse_error("expected identifier after '::' token");
                    return nullptr;
                }
            }
            return new PlainType(std::move(type), std::move(quals), std::move(scopes));
        }

        Type* parse_type() noexcept {
            if (auto type = parse_plain_type()) {
                return type;
            }

            if (auto type = parse_tuple_type()) {
                return type;
            }

            if (auto type = parse_array_type()) {
                return type;
            }
            return nullptr;
        }

        IdentExpr* parse_ident_expr() noexcept {
            if (auto ident = parse_identifier()) {
                return new IdentExpr(ident);
            }
            return nullptr;
        }

        Expr* parse_grouped_expr() noexcept {
            const auto backup = _lexer.state();
            if (!_lexer.match_token(Token::pn_open_paren)) {
                return nullptr;
            }

            std::unique_ptr<Expr> expr(parse_expr());
            if (!expr) {
                _lexer.restore(backup);
                return nullptr;
            }

            if (_lexer.match_token(Token::pn_comma)) {
                _lexer.restore(backup);
                return nullptr;
            }

            if (!_lexer.match_token(Token::pn_close_paren)) {
                _emit_parse_error("expected token ')' at end of expression");
                _lexer.restore(backup);
                return nullptr;
            }
            return expr.release();
        }

        Expr* parse_tuple_literal_expr() noexcept {
            if (!_lexer.match_token(Token::pn_open_paren)) {
                return nullptr;
            }

            if (_lexer.match_token(Token::pn_close_paren)) {
                _emit_parse_error("tuple literal shall not be empty");
                return nullptr;
            }

            TupleExprList arguments;
            do {
                std::unique_ptr<Expr> expr(parse_expr());
                if (!expr) {
                    _emit_parse_error("expected expression in tuple literal");
                    return nullptr;
                } else {
                    arguments.emplace_back(std::move(expr));
                }
            } while (_lexer.match_token(Token::pn_comma));

            if (!_lexer.match_token(Token::pn_close_paren)) {
                _emit_parse_error("expected token ')' after expression list in tuple literal");
                return nullptr;
            }
            return new TupleLiteralExpr(std::move(arguments));
        }

        Expr* parse_array_literal_expr() noexcept {
            if (!_lexer.match_token(Token::pn_open_bracket)) {
                return nullptr;
            }

            ArrayExprList literal;
            do {
                std::unique_ptr<Expr> expr(parse_expr());
                if (!expr) {
                    _emit_parse_error("expected expression in array literal");
                    return nullptr;
                }
                literal.emplace_back(std::move(expr));
            } while (_lexer.match_token(Token::pn_comma));

            if (!_lexer.match_token(Token::pn_close_bracket)) {
                _emit_parse_error("expected token ']' after array literal expression");
                return nullptr;
            }
            return new ArrayLiteralExpr(std::move(literal));
        }

        Expr* parse_primary_expr() noexcept {
            if (auto expr = parse_literal_expr()) {
                return expr;
            }

            if (auto expr = parse_ident_expr()) {
                return expr;
            }

            if (auto expr = parse_grouped_expr()) {
                return expr;
            }

            if (auto expr = parse_tuple_literal_expr()) {
                return expr;
            }

            if (auto expr = parse_array_literal_expr()) {
                return expr;
            }
            return nullptr;
        }

        Expr* parse_expr() noexcept {
            if (auto expr = parse_assign_expr()) {
                return expr;
            }

            return nullptr;
        }

        Expr* parse_dot_expr() noexcept {
            std::unique_ptr<Expr> expr(parse_primary_expr());
            if (!expr) {
                return nullptr;
            }

            while (_lexer.match_token(Token::pn_dot, Token::pn_open_bracket, Token::pn_open_paren,
                                      Token::pn_increment, Token::pn_decrement)) {
                switch (_lexer.current()) {
                    case Token::pn_dot: {
                        std::unique_ptr<Expr> next(parse_ident_expr());
                        if (!next) {
                            _emit_parse_error("expected identifier after token '.'");
                            return nullptr;
                        }

                        expr = std::make_unique<BinaryExpr>(expr.release(), ".", next.release());
                    } break;

                    case Token::pn_open_bracket: {
                        SubscriptArgList args;
                        do {
                            if (auto arg = parse_expr()) {
                                args.emplace_back(arg);
                            } else {
                                _emit_parse_error("expected expression in subscript argument list");
                                return nullptr;
                            }
                        } while (_lexer.match_token(Token::pn_comma));

                        if (!_lexer.match_token(Token::pn_close_bracket)) {
                            _emit_parse_error("expected token ']' after subscript argument list");
                            return nullptr;
                        }
                        expr = std::make_unique<SubscriptExpr>(expr.release(), std::move(args));
                    } break;

                    case Token::pn_open_paren: {
                        if (_lexer.match_token(Token::pn_close_paren)) {
                            expr = std::make_unique<CallExpr>(expr.release(), FuncArgList());
                            continue;
                        }

                        FuncArgList args;
                        do {
                            if (auto arg = parse_expr()) {
                                args.emplace_back(arg);
                            } else {
                                _emit_parse_error("expected expression in function argument list");
                                return nullptr;
                            }
                        } while (_lexer.match_token(Token::pn_comma));

                        if (!_lexer.match_token(Token::pn_close_paren)) {
                            _emit_parse_error("expected token ')' after function argument list");
                            return nullptr;
                        }
                        expr = std::make_unique<CallExpr>(expr.release(), std::move(args));
                    } break;

                    case Token::pn_increment: case Token::pn_decrement: {
                        std::string token = token_to_string(_lexer.current()).data();
                        expr = std::make_unique<PostUnaryExpr>(expr.release(), std::move(token));
                    } break;

                    default: {
                        _emit_parse_error("internal compiler error.");
                    } break;
                }
            }
            return expr.release();
        }

        Expr* parse_prefix_unary_expr() noexcept {
            if (_lexer.match_token(Token::pn_increment, Token::pn_decrement, Token::pn_plus,
                                   Token::pn_minus, Token::pn_not, Token::pn_multiply)) {
                std::string token = token_to_string(_lexer.current()).data();
                std::unique_ptr<Expr> next(parse_prefix_unary_expr());
                if (!next) {
                    _emit_parse_error("expected expression after token '" + token + '\'');
                    return nullptr;
                }
                return new PrefixUnaryExpr(std::move(token), next.release());
            } else if (_lexer.match_token(Token::pn_at, Token::pn_bit_and)) {
                const auto type = _lexer.current();
                std::string token = token_to_string(_lexer.current()).data();
                bool is_mut;
                if (_lexer.match_token(Token::kw_mut, Token::kw_const)) {
                    is_mut = _lexer.current() == Token::kw_mut;
                } else {
                    _emit_parse_error("expected 'mut' or 'const' token after '" + token + "' token");
                    return nullptr;
                }
                std::unique_ptr<Expr> next(parse_prefix_unary_expr());
                switch (type) {
                    case Token::pn_at:      return new RawPointerExpr(is_mut, next.release());
                    case Token::pn_bit_and: return new BorrowExpr(is_mut, next.release());
                }
            }

            std::unique_ptr<Expr> expr(parse_dot_expr());
            if (!expr) {
                return nullptr;
            }
            return expr.release();
        }

        Expr* parse_mult_expr() noexcept {
            std::unique_ptr<Expr> expr(parse_prefix_unary_expr());
            if (!expr) {
                return nullptr;
            }

            while (_lexer.match_token(Token::pn_modulo, Token::pn_divide, Token::pn_multiply)) {
                std::string token = token_to_string(_lexer.current()).data();
                std::unique_ptr<Expr> next(parse_prefix_unary_expr());
                if (!next) {
                    _emit_parse_error("expected expression after token '" + token + '\'');
                    return nullptr;
                }
                expr = std::make_unique<BinaryExpr>(expr.release(), std::move(token), next.release());
            }
            return expr.release();
        }

        Expr* parse_sum_expr() noexcept {
            std::unique_ptr<Expr> expr(parse_mult_expr());
            if (!expr) {
                return nullptr;
            }

            while (_lexer.match_token(Token::pn_minus, Token::pn_plus)) {
                std::string token = token_to_string(_lexer.current()).data();
                std::unique_ptr<Expr> next(parse_mult_expr());
                if (!next) {
                    _emit_parse_error("expected expression after token '" + token + '\'');
                    return nullptr;
                }
                expr = std::make_unique<BinaryExpr>(expr.release(), std::move(token), next.release());
            }
            return expr.release();
        }

        Expr* parse_relational_cmp_expr() noexcept {
            std::unique_ptr<Expr> expr(parse_sum_expr());
            if (!expr) {
                return nullptr;
            }

            while (_lexer.match_token(Token::pn_greater_equal, Token::pn_less_equal, Token::pn_greater, Token::pn_less)) {
                std::string token = token_to_string(_lexer.current()).data();
                std::unique_ptr<Expr> next(parse_sum_expr());
                if (!next) {
                    _emit_parse_error("expected expression after token '" + token + '\'');
                    return nullptr;
                }
                expr = std::make_unique<BinaryExpr>(expr.release(), std::move(token), next.release());
            }
            return expr.release();
        }

        Expr* parse_equality_expr() noexcept {
            std::unique_ptr<Expr> expr(parse_relational_cmp_expr());
            if (!expr) {
                return nullptr;
            }

            while (_lexer.match_token(Token::pn_equal, Token::pn_not_equal)) {
                std::string token = token_to_string(_lexer.current()).data();
                std::unique_ptr<Expr> next(parse_relational_cmp_expr());
                if (!next) {
                    _emit_parse_error("expected expression after token '" + token + '\'');
                    return nullptr;
                }
                expr = std::make_unique<BinaryExpr>(expr.release(), std::move(token), next.release());
            }
            return expr.release();
        }

        Expr* parse_logical_and_expr() noexcept {
            std::unique_ptr<Expr> expr(parse_equality_expr());
            if (!expr) {
                return nullptr;
            }

            while (_lexer.match_token(Token::pn_logic_and)) {
                std::unique_ptr<Expr> next(parse_equality_expr());
                if (!next) {
                    _emit_parse_error("expected expression after token '&&'");
                    return nullptr;
                }
                expr = std::make_unique<BinaryExpr>(expr.release(), "&&", next.release());
            }
            return expr.release();
        }

        Expr* parse_logical_or_expr() noexcept {
            std::unique_ptr<Expr> expr(parse_logical_and_expr());
            if (!expr) {
                return nullptr;
            }

            while (_lexer.match_token(Token::pn_logic_or)) {
                std::unique_ptr<Expr> next(parse_logical_and_expr());
                if (!next) {
                    _emit_parse_error("expected expression after token '||'");
                    return nullptr;
                }
                expr = std::make_unique<BinaryExpr>(expr.release(), "||", next.release());
            }
            return expr.release();
        }

        Expr* parse_assign_expr() noexcept {
            std::unique_ptr<Expr> expr(parse_logical_or_expr());
            if (!expr) {
                return nullptr;
            }

            if (_lexer.match_token(Token::pn_assign)) {
                std::unique_ptr<Expr> next(parse_assign_expr());
                if (!next) {
                    _emit_parse_error("expected expression after token '='");
                    return nullptr;
                }
                return new BinaryExpr(expr.release(), "=", next.release());
            }
            return expr.release();
        }

        CompoundStmt* parse_compound_stmt() noexcept {
            if (!_lexer.match_token(Token::pn_open_brace)) {
                return nullptr;
            }

            StmtList stmt_list;
            while (!_lexer.match_token(Token::pn_close_brace)) {
                if (auto stmt = parse_stmt()) {
                    stmt_list.emplace_back(stmt);
                } else {
                    _emit_parse_error("expected statement");
                    return nullptr;
                }
            }
            return new CompoundStmt(std::move(stmt_list));
        }

        IfStmt* parse_if_stmt() noexcept {
            if (!_lexer.match_token(Token::kw_if)) {
                return nullptr;
            }

            std::unique_ptr<Expr> cond(parse_expr());
            if (!cond) {
                _emit_parse_error("expected expression after 'if' token");
                return nullptr;
            }

            std::unique_ptr<CompoundStmt> true_block(parse_compound_stmt());
            if (!true_block) {
                _emit_parse_error("expected compound statement after expression");
                return nullptr;
            }

            if (_lexer.match_token(Token::kw_else)) {
                if (auto else_if = parse_if_stmt()) {
                    return new IfStmt(cond.release(), true_block.release(), else_if);
                }

                std::unique_ptr<CompoundStmt> false_block(parse_compound_stmt());
                if (!false_block) {
                    _emit_parse_error("expected compound statement after 'else' token");
                    return nullptr;
                }
                return new IfStmt(cond.release(), true_block.release(), false_block.release());
            }
            return new IfStmt(cond.release(), true_block.release(), nullptr);
        }

        WhileStmt* parse_while_stmt() noexcept {
            if (!_lexer.match_token(Token::kw_while)) {
                return nullptr;
            }

            std::unique_ptr<Expr> cond(parse_expr());
            if (!cond) {
                _emit_parse_error("expected expression after 'while' token");
                return nullptr;
            }

            std::unique_ptr<CompoundStmt> block(parse_compound_stmt());
            if (!block) {
                _emit_parse_error("expected compound statement after expression");
                return nullptr;
            }
            return new WhileStmt(cond.release(), block.release());
        }

        ReturnStmt* parse_return_stmt() noexcept {
            if (!_lexer.match_token(Token::kw_return)) {
                return nullptr;
            }

            if (_lexer.match_token(Token::pn_semicolon)) {
                return new ReturnStmt(nullptr);
            }

            std::unique_ptr<Expr> expr(parse_expr());
            if (!expr) {
                _emit_parse_error("expected expression or token ';' after return statement");
                return nullptr;
            }

            if (!_lexer.match_token(Token::pn_semicolon)) {
                _emit_parse_error("expected token ';' after expression");
                return nullptr;
            }
            return new ReturnStmt(expr.release());
        }

        VarDeclStmt* parse_var_decl_stmt() noexcept {
            if (auto decl = parse_var_decl()) {
                return new VarDeclStmt(decl);
            }
            return nullptr;
        }

        ExprStmt* parse_expr_stmt() noexcept {
            std::unique_ptr<Expr> expr(parse_expr());
            if (!expr) {
                return nullptr;
            }

            if (!_lexer.match_token(Token::pn_semicolon)) {
                _emit_parse_error("expected semicolon after expression statement");
                return nullptr;
            }
            return new ExprStmt(expr.release());
        }

        Stmt* parse_stmt() noexcept {
            if (auto stmt = parse_if_stmt()) {
                return stmt;
            }

            if (auto stmt = parse_while_stmt()) {
                return stmt;
            }

            if (auto stmt = parse_return_stmt()) {
                return stmt;
            }

            if (auto stmt = parse_var_decl_stmt()) {
                return stmt;
            }

            if (auto stmt = parse_expr_stmt()) {
                return stmt;
            }

            if (auto stmt = parse_compound_stmt()) {
                return stmt;
            }

            return nullptr;
        }

        FuncParam* parse_func_param() noexcept {
            bool is_mut = false;
            if (_lexer.match_token(Token::kw_mut)) {
                is_mut = true;
            }

            auto param_name = _lexer.match_identifier();
            if (param_name.empty()) {
                return nullptr;
            }

            if (is_reserved(param_name)) {
                _emit_parse_error("use of reserved identifier: '" + param_name + '\'');
                return nullptr;
            }

            if (!_lexer.match_token(Token::pn_colon)) {
                _emit_parse_error("expected token ':' after token '" + param_name + '\'');
                return nullptr;
            }

            std::unique_ptr<Type> param_type(parse_type());
            if (!param_type) {
                _emit_parse_error("exected typename after token: ':'");
                return nullptr;
            }

            return new FuncParam(is_mut, std::move(param_name), param_type.release());
        }

        FuncDecl* parse_func_decl() noexcept {
            if (!_lexer.match_token(Token::kw_fn)) {
                return nullptr;
            }

            auto func_name = _lexer.match_identifier();
            if (func_name.empty()) {
                _emit_parse_error("expected identifier after token: 'fn'");
                return nullptr;
            }

            if (is_reserved(func_name)) {
                _emit_parse_error("use of reserved identifier: '" + func_name + "'");
                return nullptr;
            }

            if (!_lexer.match_token(Token::pn_open_paren)) {
                _emit_parse_error("expected token '(' after token " + func_name);
                return nullptr;
            }

            FuncParamList func_params;
            if (!_lexer.match_token(Token::pn_close_paren)) {
                do {
                    if (auto func_param = parse_func_param()) {
                        func_params.emplace_back(func_param);
                    } else {
                        _emit_parse_error("expected function parameter");
                        return nullptr;
                    }
                } while (_lexer.match_token(Token::pn_comma));

                if (!_lexer.match_token(Token::pn_close_paren)) {
                    _emit_parse_error("expected token ')' at end of function parameter list");
                    return nullptr;
                }
            }

            if (!_lexer.match_token(Token::pn_arrow)) {
                _emit_parse_error("expected token '->' after token ')'");
                return nullptr;
            }

            std::unique_ptr<Type> func_ret_type(parse_type());
            if (!func_ret_type) {
                _emit_parse_error("expected typename after token '->'");
                return nullptr;
            }

            if (_lexer.match_token(Token::pn_semicolon)) {
                return new FuncDecl(std::move(func_name), std::move(func_params), func_ret_type.release(), std::nullopt);
            }

            if (!_lexer.match_token(Token::pn_open_brace)) {
                _emit_parse_error("expected either token ';' or token '{' after function declaration");
                return nullptr;
            }

            StmtList stmt_list;
            while (!_lexer.match_token(Token::pn_close_brace)) {
                if (auto stmt = parse_stmt()) {
                    stmt_list.emplace_back(stmt);
                } else {
                    _emit_parse_error("expected statement");
                    return nullptr;
                }
            }
            return new FuncDecl(std::move(func_name), std::move(func_params), func_ret_type.release(), std::move(stmt_list));
        }

        VarDecl* parse_var_decl() noexcept {
            if (!_lexer.match_token(Token::kw_let)) {
                return nullptr;
            }

            bool mut = false;
            if (_lexer.match_token(Token::kw_mut)) {
                mut = true;
            }

            auto name = _lexer.match_identifier();
            if (name.empty()) {
                _emit_parse_error("expected identifier after token "s + (mut ? "'mut'" : "'let'"));
                return nullptr;
            }

            if (is_reserved(name)) {
                _emit_parse_error("use of reserved identifier: '" + name + "'");
                return nullptr;
            }

            if (!_lexer.match_token(Token::pn_colon)) {
                _emit_parse_error("expected token ':' after token '" + name + "'");
                return nullptr;
            }

            std::unique_ptr<Type> type(parse_type());
            if (!type) {
                _emit_parse_error("expected type after token ':'");
                return nullptr;
            }

            if (_lexer.match_token(Token::pn_semicolon)) {
                return new VarDecl(mut, std::move(name), type.release(), nullptr);
            }

            if (!_lexer.match_token(Token::pn_assign)) {
                _emit_parse_error("expected token '=' or token ';' after declaration");
                return nullptr;
            }

            std::unique_ptr<Expr> expr(parse_expr());
            if (!expr) {
                _emit_parse_error("expected initializer expression after token '='");
                return nullptr;
            }

            if (!_lexer.match_token(Token::pn_semicolon)) {
                _emit_parse_error("expected semicolon after initializer");
                return nullptr;
            }

            return new VarDecl(mut, std::move(name), type.release(), expr.release());
        }

        Decl* parse_top_level_decl() noexcept {
            if (auto decl = parse_func_decl()) {
                return decl;
            }

            if (auto decl = parse_var_decl()) {
                return decl;
            }

            return nullptr;
        }

        std::unique_ptr<Unit> parse() noexcept {
            auto unit = std::make_unique<Unit>();

            while (!_lexer.eof()) {
                if (auto decl = parse_top_level_decl()) {
                    unit->push_decl(decl);
                } else {
                    return nullptr;
                }
                _lexer.skip_whitespaces_and_comments();
            }

            return unit;
        }
    private:
        void _emit_parse_error(std::string_view msg) noexcept {
            const auto state = _lexer.state();
            std::printf("Parse error: %s at line: %zu, column: %zu\n", msg.data(), state.line, state.col);
        }

        Lexer _lexer;
    };

    std::unique_ptr<Unit> parse(std::string_view path) noexcept {
        return Parser::create(path).parse();
    }
} // namespace spc
