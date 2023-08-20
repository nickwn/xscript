#include "parser.hpp"

#include <iostream>

#include "extern/ctpg.hpp"

#include "ast/context.hpp"
#include "util.hpp"

using namespace xs;

namespace xs
{

constexpr char number_pattern[] = R"_(\-?(0|[1-9][0-9]*)(\.[0-9]+)?((e|E)(\+|\-)?[0-9]+)?)_";
constexpr char ident_pattern[] = R"_([a-zA-Z_][0-9a-zA-Z_]*)_";

constexpr ctpg::regex_term<number_pattern> t_number("number");
constexpr ctpg::regex_term<ident_pattern> t_ident("ident");
constexpr ctpg::string_term t_live("live");
constexpr ctpg::string_term t_ex("ex");
constexpr ctpg::typed_term t_add(ctpg::char_term('+', 1), ctpg::ftors::create<ast::prims::add>{});
constexpr ctpg::typed_term t_sub(ctpg::char_term('-', 1), ctpg::ftors::create<ast::prims::sub>{});
constexpr ctpg::typed_term t_mul(ctpg::char_term('*', 2), ctpg::ftors::create<ast::prims::mul>{});
constexpr ctpg::typed_term t_div(ctpg::char_term('/', 2), ctpg::ftors::create<ast::prims::div>{});

using named_decl = std::pair<ast::string_binding, ast::decl<ast::sb_node>>;
using named_def = std::pair<ast::string_binding, id<ast::sb_node>>;
constexpr ctpg::nterm<id<ast::sb_node>> nt_expr("expr");
constexpr ctpg::nterm<named_decl> nt_decl("decl");
constexpr ctpg::nterm<named_def> nt_def("def");
constexpr ctpg::nterm<ast::mod<ast::sb_node>> nt_mod("mod");
constexpr ctpg::nterm<ast::qualifier> nt_qualifier("qualifier");

struct make_arith
{
    std::reference_wrapper<ast::builder<ast::sb_node>> b;

    constexpr make_arith(ast::builder<ast::sb_node>& b_) : b(b_) {}

    template<typename OpNodeValType>
    id<ast::sb_node> operator()(const id<ast::sb_node> l, OpNodeValType op_node, const id<ast::sb_node> r) const
    {
        return b.get().build(ast::nodes::app(
            b.get().build(ast::nodes::app(
                b.get().build(op_node.get_value()),
                l
            )), 
            r
        ));
    }
};

parser::parser(ast::context<ast::sb_node>& ctx) :
    builder_(ctx.builder())
{}

std::optional<ast::mod<ast::sb_node>> parser::operator()(const ctpg::buffers::string_buffer& source)
{
    ast::builder<ast::sb_node> b = builder_;
    ctpg::parser parser(
        nt_mod,
        terms(t_number, t_ident, t_live, t_ex, t_add, t_sub, t_mul, t_div, '\\', "->", ':', '=', '\n'),
        nterms(nt_expr, nt_decl, nt_def, nt_mod, nt_qualifier),
        rules(
            nt_expr(t_number) >=
                [&b](const std::string_view str) { return b.num(str); },
            nt_expr(t_ident) >=
                [&b](const std::string_view str) { 
                    return b.prim(str).value_or(b.build(ast::nodes::var<ast::sb_node>(ast::string_binding(str)))); 
                },
            nt_expr(nt_expr, nt_expr) >=
                [&b](const id<ast::sb_node> e0, const id<ast::sb_node> e1) { 
                    return b.build(ast::nodes::app<ast::sb_node>(e0, e1)); 
                },
            nt_expr(nt_expr, t_add, nt_expr) >= make_arith{b},
            nt_expr(nt_expr, t_sub, nt_expr) >= make_arith{b},
            nt_expr(nt_expr, t_mul, nt_expr) >= make_arith{b},
            nt_expr(nt_expr, t_div, nt_expr) >= make_arith{b},
            nt_expr(nt_expr, "->", nt_expr) >=
                [&b](const id<ast::sb_node> e0, std::string_view, const id<ast::sb_node> e1) { return b.fun(e0, e1); },
            nt_expr('\\', t_ident, "->", nt_expr) >=
                [&b](char, const std::string_view& str, std::string_view, const id<ast::sb_node> e) {
                    return b.build(ast::nodes::lam<ast::sb_node>(ast::string_binding(str), e));
                },
            nt_qualifier(t_live) >=
                [](std::string_view) { return ast::qualifier::live; },
            nt_qualifier(t_ex) >=
                [](std::string_view) { return ast::qualifier::ex; },
            nt_decl(t_ident, ':', nt_expr) >=
                [](const std::string_view& str, char, const id<ast::sb_node> e) {
                    return std::make_pair(ast::string_binding(str), ast::decl(e));
                },
            nt_decl(nt_qualifier, t_ident, ':', nt_expr) >=
                [](const ast::qualifier q, const std::string_view& str, char, const id<ast::sb_node> e) {
                    return std::make_pair(ast::string_binding(str), ast::decl(e, q));
                },
            nt_def(t_ident, '=', nt_expr) >=
                [](const std::string_view str, char, const id<ast::sb_node> e) { 
                    return std::make_pair(ast::string_binding(str), e); 
                },
            nt_mod(nt_def) >=
                [](const named_def& d) { return ast::mod<ast::sb_node>(d.first, d.second); },
            nt_mod(nt_decl) >=
                [](const named_decl& d) { return ast::mod<ast::sb_node>(d.first, d.second); },
            nt_mod(nt_mod, '\n', nt_decl) >=
                [](const ast::mod<ast::sb_node>& m, char, const named_decl& d) { 
                    return ast::mod<ast::sb_node>(m, d.first, d.second); 
                },
            nt_mod(nt_mod, '\n', nt_def) >=
                [](const ast::mod<ast::sb_node>& m, char, const named_def& d) { 
                    return ast::mod<ast::sb_node>(m, d.first, d.second); 
                }
        )
    );

    //parser.write_diag_str(std::cout);
    const ctpg::parse_options opt = ctpg::parse_options{}
        .set_skip_newline(false)
        .set_verbose();


    return parser.parse(opt, source, std::cerr);
}

} // namespace xs