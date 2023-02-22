#pragma once

#include <format>

#include "util.hpp"

#include "ast/ast.hpp"
#include "ast/context.hpp"

namespace xs
{
namespace passes
{

template<class CharT>
struct std::formatter<ast::string_binding, CharT> : std::formatter<std::string_view, CharT> {
	template<class FormatContext>
	auto format(xs::ast::string_binding b, FormatContext& fc) const {
		return std::formatter<std::string_view, CharT>::format(b.str, fc);
	}
};

namespace node_passes
{
	struct to_string : public ast::traverser<to_string, ast::sb_node, std::string()>
	{
		to_string(const pool_t<ast::sb_node>& node_pool) :
			traverser(node_pool)
		{}

		template<ast::prim P>
		std::string operator()(const P&) const
		{
			return std::string(P::name.view());
		}

		template<ast::prim P>
		std::string operator()(const ast::nodes::constant<P>& v) const
		{
			return std::format("{}({}, {})", v.name.view(), P::name.view(), v.payload);
		}

		std::string operator()(const ast::nodes::var<ast::sb_node>& v) const
		{
			return std::format("{}({})", v.name.view(), v.bind);
		}

		std::string operator()(const ast::nodes::app<ast::sb_node>& v) const
		{
			return std::format("{}({}, {})", v.name.view(), traverse(v.expr0), traverse(v.expr1));
		}

		std::string operator()(const ast::nodes::lam<ast::sb_node>& v) const
		{
			return std::format("{}({}, {})", v.name.view(), v.bind, traverse(v.expr));
		}
	};
}

class to_string
{
public:
	to_string(const ast::context<ast::sb_node>& ctx) :
		node_to_string_(ctx.traversal<node_passes::to_string>())
	{}

	std::string operator()(const ast::mod<ast::sb_node>& mod)
	{
		std::string res;
		for (const auto& [b, d] : mod.decls)
		{
			std::format_to(std::back_inserter(res), "{}: {} \n", b, node_to_string_(d.type));
		}

		for (const auto& [b, d] : mod.defs)
		{
			std::format_to(std::back_inserter(res), "{} = {} \n", b, node_to_string_(d));
		}

		return res;
	}

private:
	ast::traversal<node_passes::to_string> node_to_string_;
};

using env = ska::bytell_hash_map<ast::string_binding, ast::resolved_binding, ast::hash<ast::string_binding>>;

namespace node_passes
{
	struct resolve_vars : public ast::traverser<
		resolve_vars, ast::sb_node, 
		id<ast::rb_node>(ast::builder<ast::rb_node>&, env&, const std::size_t)
	>
	{
		resolve_vars(const pool_t<ast::sb_node>& node_pool) :
			traverser(node_pool)
		{}

		template<ast::prim P>
		id<ast::rb_node> operator()(const P& v, ast::builder<ast::rb_node>& b, env&, const std::size_t) const
		{
			return b.build(v);
		}

		template<ast::prim P>
		id<ast::rb_node> operator()(const ast::nodes::constant<P>& v, ast::builder<ast::rb_node>& b, env& e, const std::size_t i) const
		{
			return b.build(ast::nodes::constant<P>(v.payload));
		}

		id<ast::rb_node> operator()(const ast::nodes::var<ast::sb_node>& v, ast::builder<ast::rb_node>& b, env& e, const std::size_t i) const
		{
			return b.build(ast::nodes::var<ast::rb_node>(e.at(v.bind)));
		}

		id<ast::rb_node> operator()(const ast::nodes::app<ast::sb_node>& v, ast::builder<ast::rb_node>& b, env& e, const std::size_t i) const
		{
			const id<ast::rb_node> e0 = traverse(v.expr0, b, e, i);
			const id<ast::rb_node> e1 = traverse(v.expr1, b, e, i);
			return b.build(ast::nodes::app<ast::rb_node>(e0, e1));
		}

		id<ast::rb_node> operator()(const ast::nodes::lam<ast::sb_node>& v, ast::builder<ast::rb_node>& b, env& e, const std::size_t i) const
		{
			e.insert(std::make_pair(v.bind, ast::resolved_binding(i, false)));
			const id<ast::rb_node> res = traverse(v.expr, b, e, i + 1);
			e.erase(v.bind);

			return res;
		}
	};
}


class resolve_vars
{
public:
	resolve_vars(const ast::context<ast::sb_node>& in_ctx, ast::context<ast::rb_node>& out_ctx) :
		in_ctx_(in_ctx), out_ctx_(out_ctx)
	{}

	ast::mod<ast::rb_node> operator()(const ast::mod<ast::sb_node>& mod)
	{
		using namespace std::placeholders;
		using named_def = std::pair<ast::string_binding, id<ast::sb_node>>;
		using named_decl = std::pair<ast::string_binding, ast::decl<ast::sb_node>>;

		const auto get_quals = [](const ast::decl<ast::sb_node>& d) {
			return d.quals;
		};

		const auto get_type = [](const ast::decl<ast::sb_node>& d) {
			return d.type;
		};

		const auto construct_decl = [](const std::tuple<ast::qualifier, id<ast::rb_node>>& tup) {
			const auto [q, t] = tup;
			return ast::decl(t, q);
		};

		const auto def_to_binding_pair = [](const std::tuple<std::size_t, named_def>& id) {
			const auto [i, d] = id;
			return std::make_pair(d.first, ast::resolved_binding(i, true));
		};

		auto globals = util::ranges::to<env>(util::ranges::enumerate(mod.defs) | std::views::transform(def_to_binding_pair));

		const auto resolve_vars = in_ctx_.traversal<node_passes::resolve_vars>();
		const auto resolve_vars_in_expr = std::bind(resolve_vars, _1, out_ctx_.builder(), globals, std::size_t(0));

		const auto decls_view = mod.decls | std::views::values;
		const ast::mod<ast::rb_node> res_mod = ast::mod<ast::rb_node>(
			util::ranges::to<std::vector<ast::decl<ast::rb_node>>>(
				util::ranges::zip(
					decls_view | std::views::transform(get_quals),
					decls_view | std::views::transform(get_type) | std::views::transform(resolve_vars_in_expr)
				) | std::views::transform(construct_decl)
			),
			util::ranges::to<std::vector<id<ast::rb_node>>>(
				mod.defs |
				std::views::values |
				std::views::transform(resolve_vars_in_expr)
			)
		);

		return res_mod;
	}

private:
	const ast::context<ast::sb_node>& in_ctx_;
	ast::context<ast::rb_node>& out_ctx_;
};

struct typed_value
{
	const std::uint64_t value;
	const id<ast::rb_node> type;
};

struct typed_stack
{
	std::vector<std::uint64_t> values;
	std::vector<id<ast::rb_node>> types;

	typed_value at(const std::size_t i) const
	{
		return typed_value{
			.value = values.at(i),
			.type = types.at(i)
		};
	}

	auto scoped_push(const typed_value& tv)
	{
		struct scoped_push_
		{
			typed_stack& stack;

			scoped_push_(typed_stack& stack_, typed_value tv) :
				stack(stack_)
			{
				stack.values.push_back(tv.value);
				stack.types.push_back(tv.type);
			}

			~scoped_push_()
			{
				stack.values.pop_back();
				stack.types.pop_back();
			}

			operator typed_stack& ()
			{
				return stack;
			}
		};

		return scoped_push_(*this, tv);
	}
};

namespace node_passes
{
	class evaluate : public ast::traverser<evaluate, ast::rb_node, typed_value(id<ast::rb_node>, typed_stack&)>
	{
	public:
		evaluate(pool_t<ast::rb_node>& node_pool, const ast::mod<ast::rb_node>& mod) :
			traverser(node_pool), mod_(mod), node_pool_(node_pool),
			type_id_(node_pool.emplace(ast::prims::type())),
			fun_id_(node_pool.emplace(ast::prims::fun()))
		{}

		template<ast::prim P>
		typed_value operator()(const P&, const id<ast::rb_node> id, typed_stack&) const
		{
			return typed_value{
				.value = std::uint64_t(id),
				.type = type_id_
			};
		}

		template<ast::prim P>
		typed_value operator()(const ast::nodes::constant<P>& v, const id<ast::rb_node> id, typed_stack&) const
		{
			return typed_value{
				.value = v.payload,
				.type = id
			};
		}

		typed_value operator()(const ast::nodes::var<ast::rb_node>& v, const id<ast::rb_node> id, typed_stack& stack) const
		{
			return v.bind.is_local ? stack.at(v.bind.idx) : typed_value{
				.value = std::uint64_t(mod_.defs.at(v.bind.idx)),
				.type = mod_.decls.at(v.bind.idx).type
			};
		}

		typed_value operator()(const ast::nodes::app<ast::rb_node>& v, const id<ast::rb_node> id, typed_stack& stack) const
		{
			const typed_value v1 = traverse_(v.expr1, stack);
			const typed_value v0 = traverse_(v.expr0, stack.scoped_push(v1));
			return v0;
		}

		typed_value operator()(const ast::nodes::lam<ast::rb_node>& v, const id<ast::rb_node> id, typed_stack& stack) const
		{
			return typed_value{
				.value = std::uint64_t(id),
				.type = fun_id_
			};
		}

	private:

		typed_value traverse_(const id<ast::rb_node> id, typed_stack& stack) const
		{
			return traverse(id, id, stack);
		}

		const ast::mod<ast::rb_node>& mod_;
		const pool_t<ast::rb_node>& node_pool_;

		const id<ast::rb_node> type_id_;
		const id<ast::rb_node> fun_id_;
	};
}
/*
class evaluate
{
public:
	evaluate(const ast::context<ast::rb_node>& ctx) :
		node_evaluate_(ctx.traversal<node_passes::evaluate>())
	{}

	void operator()(const ast::mod<ast::rb_node>& mod)
	{
		
	}

private:
	ast::traversal<node_passes::evaluate> node_evaluate_;
};*/


} // namespace passes
} // namespace xs