#pragma once

#include <optional>
#include <format>

#include "ast.hpp"

namespace xs
{
namespace ast
{

template<node N>
using node_interner = interner<N, hash<N>, std::equal_to<N>>;

template<node N>
class builder
{
public:
	builder(node_interner<N>& interner);

	template<node_variant<N> T>
	id<N> build(const T& v)
	{
		return interner_.intern(v.to<N>());
	}

	id<N> num(const std::string_view& str)
	{
		const double val = std::atof(str.data());
		return build(nodes::constant<prims::f<64>>(val));
	}

	id<N> fun(const id<N> e0, const id<N> e1)
	{
		const id<N> fun_id = build(prims::fun());
		const id<N> app_arg = build(nodes::app<N>(fun_id, e0));
		const id<N> app_ret = build(nodes::app<N>(app_arg, e1));
		return app_ret;
	}

	std::optional<id<N>> prim(const std::string_view& str)
	{
		const auto maybe_prim = prim_map_.find(string_binding(str));
		if (maybe_prim != std::end(prim_map_))
		{
			return maybe_prim->second;
		}

		return std::optional<id<N>>();
	}

private:
	node_interner<N>& interner_;
	ska::bytell_hash_map<string_binding, id<N>, hash<string_binding>> prim_map_;
};

template<typename Traverser>
class traversal;

template<typename Child, node N, typename FuncType>
class traverser;

template<class Child, node N, typename Ret, typename... Params>
class traverser<Child, N, Ret(Params...)> : public util::enable_crtp<Child>
{
public:
	friend class traversal<Child>;

	traverser(const pool_t<N>& node_pool) :
		node_pool_(node_pool)
	{}

protected:
	Ret traverse(const id<N> id, Params... params) const
	{
		const N& node = node_pool_.get(id);
		return std::visit(
			[&params..., derived = traverser<Child, N, Ret(Params...)>::as_derived()](auto n){
				return derived(n, params...);
			}, 
			node
		);
	}

private:
	const pool_t<N>& node_pool_;
};

template<typename Traverser>
class traversal
{
public:
	template<typename... Args>
	traversal(Args&&... args) :
		traverser_(args...)
	{}

	template<node N>
	auto operator()(const id<N> node, auto... params) const
	{
		return traverser_.traverse(node, params...);
	}

private:
	Traverser traverser_;
};

template<typename N>
class context
{
public:
	context() :
		node_pool_(), interner_(node_pool_)
	{
		interner_.intern(prims::type().to<N>());
	}

	ast::builder<N> builder()
	{
		return ast::builder<N>(interner_);
	}

	template<typename Traverser, typename... Params>
	ast::traversal<Traverser> traversal(Params&&... params) const
	{
		return ast::traversal<Traverser>(node_pool_, std::forward(params)...);
	}

	template<typename Traverser>
	ast::traversal<Traverser> traversal() const
	{
		return ast::traversal<Traverser>(node_pool_);
	}

private:
	pool_t<N> node_pool_;
	node_interner<N> interner_;
};

} // namespace ast
} // namespace xs