#pragma once

#include <iostream>
#include <vector>
#include <string_view>
#include <span>
#include <variant>
#include <memory>
#include <type_traits>

#include "../util.hpp"

namespace xs
{
namespace ast
{

struct string_binding
{
	std::string_view str;
	std::uint64_t hash;

	explicit string_binding(const std::string_view& str_) :
		str(str_), hash(xs::hash<std::string_view>{}(str_))
	{
		static std::uint32_t counter = 0;
	}

	friend bool operator==(const string_binding&, const string_binding&) = default;
};

struct resolved_binding
{
	const unsigned is_local : 1;
	const unsigned idx : 24;

	resolved_binding(const std::size_t is_local_, const std::size_t idx_) :
		is_local(std::uint32_t(is_local_)), idx(std::uint32_t(idx_))
	{}

	friend bool operator==(const resolved_binding&, const resolved_binding&) = default;
};

static_assert(sizeof(resolved_binding) == sizeof(std::uint32_t));

template<typename T>
struct hash;

template<>
struct hash<string_binding>
{
	std::size_t operator()(const string_binding& b) const
	{
		return b.hash;
	}
};

template<>
struct hash<resolved_binding>
{
	std::size_t operator()(const resolved_binding& b) const
	{
		return std::size_t(std::bit_cast<std::uint32_t>(b));
	}
};

struct sb_node;
struct rb_node;

template<typename T>
concept hashable = requires(T a)
{
	{ ast::hash<T>{}(a) } -> std::convertible_to<std::size_t>;
};

namespace detail
{
	template<typename T>
	concept storable =
		std::move_constructible<T> &&
		std::equality_comparable<T> &&
		hashable<T>;
}

template<typename T>
concept binding = detail::storable<T>;

static_assert(binding<string_binding>);
static_assert(binding<resolved_binding>);

namespace detail
{
	template<typename N>
	struct node_binding;

	template<>
	struct node_binding<sb_node>
	{
		using type = string_binding;
	};

	template<>
	struct node_binding<rb_node>
	{
		using type = resolved_binding;
	};

	template<binding B>
	struct node;

	template<>
	struct node<string_binding>
	{
		using type = sb_node;
	};

	template<>
	struct node<resolved_binding>
	{
		using type = rb_node;
	};
}

template<typename N>
using node_binding_t = detail::node_binding<N>::type;

template<binding B>
using node_t = detail::node<B>::type;

template<typename T>
concept node = binding<node_binding_t<T>> &&
	std::same_as<node_t<node_binding_t<T>>, T> &&
	detail::storable<T> &&
	requires(T) {
		typename node_binding_t<T>;
		typename node_t<node_binding_t<T>>;
	};

template<typename Derived>
struct node_variant_base : util::enable_crtp<Derived>
{
	node_variant_base() = default;

	template<node N>
	constexpr N to() const;

	friend bool operator==(const node_variant_base&, const node_variant_base&) = default;

	virtual ~node_variant_base() = default;
};

namespace prims
{
	template<std::size_t width, bool has_sign>
	struct integer : public node_variant_base<integer<width, has_sign>>
	{
		constexpr static bool is_prim = true;
		constexpr static auto name = make_hashed_string(
			util::concat(util::to_array(has_sign ? "i" : "u"), util::itoa<width>())
		);
	};

	template<std::size_t width>
	using i = integer<width, true>;

	template<std::size_t width>
	using u = integer<width, false>;

	template<std::size_t width>
	struct real : public node_variant_base<real<width>>
	{
		constexpr static bool is_prim = true;
		constexpr static auto name = make_hashed_string(
			util::concat(util::to_array("f"), util::itoa<width>())
		);
	};

	template<std::size_t width>
	using f = real<width>;

	struct boolean : public node_variant_base<boolean>
	{
		constexpr static bool is_prim = true;
		constexpr static auto name = make_hashed_string("boolean");
	};

	struct none : public node_variant_base<none>
	{
		constexpr static bool is_prim = true;
		constexpr static auto name = make_hashed_string("none");
	};

	// type of all types
	struct type : public node_variant_base<type>
	{
		constexpr static bool is_prim = true;
		constexpr static auto name = make_hashed_string("type");
	};

	struct fun : public node_variant_base<fun>
	{
		constexpr static bool is_prim = true;
		constexpr static auto name = make_hashed_string("fun");
	};

	struct anon_fun : public node_variant_base<anon_fun>
	{
		constexpr static bool is_prim = true;
		constexpr static auto name = make_hashed_string("anon_fun");
	};

	struct add : public node_variant_base<add>
	{
		constexpr static bool is_prim = true;
		constexpr static auto name = make_hashed_string("add");
	};

	struct mul : public node_variant_base<mul>
	{
		constexpr static bool is_prim = true;
		constexpr static auto name = make_hashed_string("mul");
	};
}

namespace nodes
{
	template<typename Ty>
	struct constant : public node_variant_base<constant<Ty>>
	{
		constexpr static bool is_prim = false;
		constexpr static auto name = make_hashed_string("constant");

		using payload_type = std::uint64_t;

		const payload_type payload;

		template<typename U> // requires sizeof(U) == sizeof(payload_type)
		U data_cast() const { return std::bit_cast<U>(payload); }

		template<typename U> // requires sizeof(U) == sizeof(payload_type)
		constant(const U& payload_) :
			payload(std::bit_cast<payload_type>(payload_))
		{}
	};			

	template<typename N>
	struct var : public node_variant_base<var<N>>
	{
		constexpr static bool is_prim = false;
		constexpr static auto name = make_hashed_string("var");

		const node_binding_t<N> bind;

		var(node_binding_t<N> bind_) :
			bind(std::move(bind_))
		{}
	};

	template<typename N>
	struct app : public node_variant_base<app<N>>
	{
		constexpr static bool is_prim = false;
		constexpr static auto name = make_hashed_string("app");

		const id<N> expr0, expr1;
		app(const id<N> expr0_, const id<N> expr1_) :
			expr0(expr0_), expr1(expr1_)
		{}
	};

	template<typename N>
	struct lam;

	template<>
	struct lam<sb_node> : public node_variant_base<lam<sb_node>>
	{
		constexpr static bool is_prim = false;
		constexpr static auto name = make_hashed_string("lam");

		const string_binding bind;
		const id<sb_node> expr;
		lam(string_binding bind_, const id<sb_node> expr_) :
			bind(std::move(bind_)), expr(expr_)
		{}
	};

	template<>
	struct lam<rb_node> : public node_variant_base<lam<rb_node>>
	{
		constexpr static bool is_prim = false;
		constexpr static auto name = make_hashed_string("lam");

		const id<rb_node> expr;
		lam(const id<rb_node> expr_) :
			expr(expr_)
		{}
	};
}

namespace detail
{
	template<typename N, typename... Prims>
	using node_base_with_prims = std::variant<
		nodes::constant<Prims>...,
		nodes::var<N>,
		nodes::app<N>,
		nodes::lam<N>,
		Prims...
	>;

	template<typename N, typename... NonTypePrims>
	using node_base_with_non_type_prims_t = node_base_with_prims<
		N,

		prims::i<8>,
		prims::i<16>,
		prims::i<32>,
		prims::i<64>,

		prims::u<8>,
		prims::u<16>,
		prims::u<32>,
		prims::u<64>,

		prims::f<16>,
		prims::f<32>,
		prims::f<64>,

		prims::none,
		prims::type,

		prims::fun,
		prims::anon_fun,

		NonTypePrims...
	>;

	template<typename N>
	using node_base_t = node_base_with_non_type_prims_t<
		N,

		prims::add,
		prims::mul
	>;
}

struct sb_node : public detail::node_base_t<sb_node>
{
	using detail::node_base_t<sb_node>::node_base_t;
	using detail::node_base_t<sb_node>::operator=;
};

struct rb_node : public detail::node_base_t<rb_node>
{
	using detail::node_base_t<rb_node>::node_base_t;
	using detail::node_base_t<rb_node>::operator=;
};

namespace detail
{
	template<typename T, typename N>
	concept node_variant_ =
		util::is_in_variant_v<T, detail::node_base_t<N>>&&
		std::derived_from<T, node_variant_base<T>>&&
		requires {
			util::is_std_array_v<decltype(T::name), char>;
			std::is_same_v<decltype(T::is_prim), bool>;
		};

	template<typename T>
	concept prim_ =
		(node_variant_<T, sb_node> || node_variant_<T, rb_node>) &&
		requires {
			T::is_prim == true;
		};
}

template<detail::prim_ P>
struct hash<P>
{
	std::uint64_t operator()(const P&) const
	{
		return 0;
	}
};

template<detail::prim_ Ty>
struct hash<nodes::constant<Ty>>
{
	std::uint64_t operator()(const nodes::constant<Ty>& v) const
	{
		return Ty::name.hash ^ v.payload;
	}
};

template<typename N>
struct hash<nodes::var<N>>
{
	std::uint64_t operator()(const nodes::var<N>& v) const
	{
		return hash<node_binding_t<N>>{}(v.bind);
	}
};

template<typename N>
struct hash<nodes::app<N>>
{
	std::uint64_t operator()(const nodes::app<N>& v) const
	{
		return xs::hash<id<N>>{}(v.expr0) ^ xs::hash<id<N>>{}(v.expr1);
	}
};

template<>
struct hash<nodes::lam<sb_node>>
{
	std::uint64_t operator()(const nodes::lam<sb_node>& v) const
	{
		return xs::hash<id<sb_node>>{}(v.expr) ^ hash<string_binding>{}(v.bind);
	}
};

template<>
struct hash<nodes::lam<rb_node>>
{
	std::uint64_t operator()(const nodes::lam<rb_node>& v) const
	{
		return xs::hash<id<rb_node>>{}(v.expr);
	}
};

template<typename N> 
requires std::same_as<N, sb_node> || std::same_as<N, rb_node>
struct hash<N>
{
	std::uint64_t operator()(const N& v) const
	{
		const std::uint64_t node_hash = std::visit(
			[](auto n) { return hash<decltype(n)>{}(n); }, 
			v
		);

		return std::uint64_t(util::scramble(v.index())) ^ node_hash;
	}
};

template<typename T, typename N>
concept node_variant =
	detail::node_variant_<T, N> &&
	detail::storable<T>;


template<typename T>
concept prim =
	(node_variant<T, sb_node> || node_variant<T, rb_node>) &&
	std::default_initializable<T> &&
	requires {
		T::is_prim == true;
	};

static_assert(node<sb_node>);
static_assert(node<rb_node>);

template<typename Derived>
template<node N>
constexpr N node_variant_base<Derived>::to() const
{
	return N(node_variant_base<Derived>::as_derived());
}

template<node N, typename Value>
struct node_map;

template<typename Value>
struct node_map<sb_node, Value>
{
	using type = ska::bytell_hash_map<string_binding, Value, hash<string_binding>>;
	using key_type = string_binding;
};

template<typename Value>
struct node_map<rb_node, Value>
{
	using type = std::vector<Value>;
	using key_type = resolved_binding;
};

template<node N, typename Value>
using node_map_t = node_map<N, Value>::type;

template<node N, typename Value>
using node_map_key_t = node_map<N, Value>::key_type;

enum class qualifier : std::uint8_t
{
	none, live, ex
};

template<node N>
struct decl
{
	qualifier quals;
	id<N> type;

	decl(const id<N> type_, const qualifier quals_ = qualifier::none) :
		quals(quals_), type(type_)
	{}
};

template<typename N>
struct mod
{
private:

	template<typename Value>
	static auto make_elem(const node_map_key_t<N, Value>& b, const auto& d) 
	{
		if constexpr (std::is_same_v<N, sb_node>)
		{
			return std::make_pair(b, d);
		}
		else
		{
			return d;
		}
	}

	template<typename Value>
	static node_map_t<N, Value> insert_elem(
			const node_map_t<N, Value>& m, 
			const node_map_key_t<N, Value>& b, 
			const auto& d
	) 
	{
		node_map_t<N, Value> res = m;
		if constexpr (std::is_same_v<N, sb_node>)
		{
			res.insert(std::make_pair(b, d));
		}
		else
		{
			res.push_back(d);
		}

		return res;
	}

public:

	node_map_t<N, decl<N>> decls;
	node_map_t<N, id<N>> defs;

	mod(const node_map_key_t<N, decl<N>>& b, const decl<N>& d) :
		decls({ make_elem<decl<N>>(b, d) }), defs()
	{}

	mod(const node_map_key_t<N, id<N>>& b, const id<N> d) :
		decls(), defs({ make_elem<id<N>>(b, d) })
	{}

	mod(const mod& m, const node_map_key_t<N, decl<N>>& b, const decl<N>& d) :
		decls(insert_elem<decl<N>>(m.decls, b, d)), defs(m.defs)
	{}

	mod(const mod& m, const node_map_key_t<N, id<N>>& b, const id<N>& d) :
		decls(m.decls), defs(insert_elem<id<N>>(m.defs, b, d))
	{}

	mod(node_map_t<N, decl<N>> decls_, node_map_t<N, id<N>> defs_) :
		decls(std::move(decls_)), defs(std::move(defs_))
	{}
};

} // namespace ast
} // namespace xs