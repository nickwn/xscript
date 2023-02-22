#pragma once

#include <optional>

#include "ast/context.hpp"

namespace ctpg
{
namespace buffers
{

class string_buffer;

}
}

namespace xs
{

class parse
{
public:
	parse(ast::context<ast::sb_node>& ctx);

	std::optional<ast::mod<ast::sb_node>> operator()(const ctpg::buffers::string_buffer& source);

private:

	ast::builder<ast::sb_node> builder_;
};

} // namespace xs