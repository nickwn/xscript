#include <deque>
#include <array>
#include <ranges>
#include <iostream>

#include "extern/ctpg.hpp"

#include "parser.hpp"

#include "passes.hpp"
#include "util.hpp"

using namespace xs;

int main()
{   
    const auto source = ctpg::buffers::string_buffer(util::read_file("test/test.xs"));

    std::cout 
        << "source:" << util::newl
        << source.get_view(std::begin(source), std::end(source)) << util::newl;
    
    ast::context<ast::sb_node> named_ctx;
    auto parse = parser(named_ctx);
    const auto maybe_parsed = parse(source);

    if (maybe_parsed)
    {
        ast::mod<ast::sb_node> named_mod = maybe_parsed.value();

        auto to_string = passes::to_string(named_ctx);
        std::cout << to_string(named_mod) << util::newl;

        ast::context<ast::rb_node> resolved_ctx;
        auto resolve_vars = passes::resolve_vars(named_ctx, resolved_ctx);
        auto resolved_mod = resolve_vars(named_mod);

        auto evaluate = passes::evaluate(resolved_ctx);
        auto val_gen = evaluate.gen(resolved_mod);

        while (val_gen)
        {
            const id<ast::rb_node> val = val_gen();
        }
    }

}