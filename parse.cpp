#include "parse.hpp"

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

using named_decl = std::pair<ast::string_binding, ast::decl<ast::sb_node>>;
using named_def = std::pair<ast::string_binding, id<ast::sb_node>>;
constexpr ctpg::nterm<id<ast::sb_node>> nt_expr("expr");
constexpr ctpg::nterm<named_decl> nt_decl("decl");
constexpr ctpg::nterm<named_def> nt_def("def");
constexpr ctpg::nterm<ast::mod<ast::sb_node>> nt_mod("mod");
constexpr ctpg::nterm<ast::qualifier> nt_qualifier("qualifier");

parse::parse(ast::context<ast::sb_node>& ctx) :
    builder_(ctx.builder())
{}

std::optional<ast::mod<ast::sb_node>> parse::operator()(const ctpg::buffers::string_buffer& source)
{
    ast::builder<ast::sb_node> b = builder_;
    constexpr ctpg::parser parser(
        nt_mod,
        terms(t_number, t_ident, t_live, t_ex, '\\', "->", ':', '=', '\n'),
        nterms(nt_expr, nt_decl, nt_def, nt_mod, nt_qualifier),
        rules(
            nt_expr(t_number) >=
                [&b](const std::string_view str) { return b.num(str); },
            nt_expr(t_ident) >=
                [&b](const std::string_view str) { return b.prim(str).value_or(b.build(ast::nodes::var<ast::sb_node>(ast::string_binding(str)))); },
            nt_expr(nt_expr, nt_expr) >=
                [&b](const id<ast::sb_node> e0, const id<ast::sb_node> e1) { return b.build(ast::nodes::app<ast::sb_node>(e0, e1)); },
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
                [](const std::string_view str, char, const id<ast::sb_node> e) { return std::make_pair(ast::string_binding(str), e); },
            nt_mod(nt_def) >=
                [](const named_def& d) { return ast::mod<ast::sb_node>(d.first, d.second); },
            nt_mod(nt_decl) >=
                [](const named_decl& d) { return ast::mod<ast::sb_node>(d.first, d.second); },
            nt_mod(nt_mod, '\n', nt_decl) >=
                [](const ast::mod<ast::sb_node>& m, char, const named_decl& d) { return ast::mod<ast::sb_node>(m, d.first, d.second); },
            nt_mod(nt_mod, '\n', nt_def) >=
                [](const ast::mod<ast::sb_node>& m, char, const named_def& d) { return ast::mod<ast::sb_node>(m, d.first, d.second); }
        )
    );

    //parser.write_diag_str(std::cout);
    const ctpg::parse_options opt = ctpg::parse_options{}
        .set_skip_newline(false)
        .set_verbose();


    return parser.parse(opt, source, std::cerr);
}

} // namespace xs