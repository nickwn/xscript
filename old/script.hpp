#pragma once

#include <vector>
#include <string_view>
#include <span>
#include <variant>
#include <memory>
#include <type_traits>

#include "util.hpp"

namespace xs
{
// shitty ast stuff
namespace ast
{
	struct binding
	{
		binding(const std::string_view str_) :
			str(str_), hash(xs::hash<std::string_view>{}(str_))
		{
			static std::uint32_t counter = 0;
		}

		const char* c_str() const { return str.data(); }

		friend bool operator==(const binding&, const binding&) = default;

		std::string_view str;
		std::uint64_t hash;
	};

	template<typename T>
	struct hash;

	template<>
	struct hash<binding>
	{
		std::size_t operator()(const binding& b) const
		{
			return b.hash;
		}
	};

	std::ostream& operator<<(std::ostream& os, const binding& b);

	struct node;

	template<typename Child>
	struct node_base : util::enable_crtp<Child>
	{
		using const_iterator = const id<node>*;

		node_base() = default;

		constexpr node to_node() const;

		friend bool operator==(const node_base&, const node_base&) = default;

		virtual ~node_base() = default;
	};

	namespace prims
	{
		template<std::size_t width, bool has_sign>
		struct integer : public node_base<integer<width, has_sign>>
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
		struct real : public node_base<real<width>>
		{
			constexpr static bool is_prim = true;
			constexpr static auto name = make_hashed_string(
				util::concat(util::to_array("f"), util::itoa<width>())
			);
		};

		template<std::size_t width>
		using f = real<width>;

		struct boolean : public node_base<boolean>
		{
			constexpr static bool is_prim = true;
			constexpr static auto name = make_hashed_string("boolean");
		};

		struct none : public node_base<none>
		{
			constexpr static bool is_prim = true;
			constexpr static auto name = make_hashed_string("none");
		};

		struct fun : public node_base<fun> 
		{
			constexpr static bool is_prim = true;
			constexpr static auto name = make_hashed_string("fun");
		};

		struct add : public node_base<add> 
		{
			constexpr static bool is_prim = true;
			constexpr static auto name = make_hashed_string("add");
		};

		struct mul : public node_base<mul> 
		{
			constexpr static bool is_prim = true;
			constexpr static auto name = make_hashed_string("mul");
		};
	}

	namespace nodes
	{
		struct constant : public node_base<constant>
		{
			constexpr static bool is_prim = false;
			constexpr static auto name = make_hashed_string("constant");

			const id<node> type;
			const std::uint64_t payload;

			template<typename U>
			U data_cast() const { return *((U*)payload); }

			template<typename U>
			constant(id<node> type_, const U& payload_) :
				node_base(), type(type_), payload(std::bit_cast<std::uint64_t>(payload_))
			{
				static_assert(sizeof(U) == sizeof(std::uint64_t));
			}
		};			

		struct var : public node_base<var>
		{
			constexpr static bool is_prim = false;
			constexpr static auto name = make_hashed_string("var");

			const binding bind;
			var(const binding& bind_) :
				node_base(), bind(bind_)
			{}
		};

		struct app : public node_base<app>
		{
			constexpr static bool is_prim = false;
			constexpr static auto name = make_hashed_string("app");

			const id<node> expr0, expr1;
			app(const id<node> expr0_, const id<node> expr1_) :
				node_base(), expr0(expr0_), expr1(expr1_)
			{}
		};

		struct lam : public node_base<lam>
		{
			constexpr static bool is_prim = false;
			constexpr static auto name = make_hashed_string("lam");

			const id<node> expr;
			const binding bind;
			lam(const id<node> expr_, const binding bind_) :
				node_base(), expr(expr_), bind(bind_)
			{}
		};

		struct con : public node_base<con>
		{
			constexpr static bool is_prim = false;
			constexpr static auto name = make_hashed_string("con");

			const binding bind;
			const std::uint32_t rank;
			con(const binding bind_, const std::uint32_t rank_) :
				bind(bind_), rank(rank_)
			{}
		};
	}

	using node_variant = std::variant<
		nodes::constant,
		nodes::var,
		nodes::app,
		nodes::lam,
		nodes::con,

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

		prims::fun,
		prims::add,
		prims::mul
	>;

	struct node : 
		public node_variant
	{
		using node_variant::node_variant;
		using node_variant::operator=;
	};

	template<typename T>
	struct is_node : std::false_type {};

	template<typename N> requires util::is_in_variant_v<N, node_variant>
	struct is_node<N> : std::true_type {};

	template<typename N>
	constexpr bool is_node_v = is_node<N>::value;

	template<typename T>
	struct is_prim : std::false_type {};

	template<typename N> requires is_node_v<N>
	struct is_prim<N> : std::bool_constant<N::is_prim> {};

	template<typename T>
	constexpr bool is_prim_v = is_prim<T>::value;

	template<typename T>
	concept prim = is_prim_v<T>;

	template<prim P>
	struct hash<P>
	{
		std::uint64_t operator()(const P&) const
		{
			return 0;
		}
	};

	template<>
	struct hash<nodes::constant>
	{
		std::uint64_t operator()(const nodes::constant& v) const
		{
			return xs::hash<id<node>>{}(v.type) ^ v.payload;
		}
	};

	template<>
	struct hash<nodes::var>
	{
		std::uint64_t operator()(const nodes::var& v) const
		{
			return v.bind.hash;
		}
	};

	template<>
	struct hash<nodes::app>
	{
		std::uint64_t operator()(const nodes::app& v) const
		{
			return xs::hash<id<node>>{}(v.expr0) ^ xs::hash<id<node>>{}(v.expr1);
		}
	};

	template<>
	struct hash<nodes::lam>
	{
		std::uint64_t operator()(const nodes::lam& v) const
		{
			return xs::hash<id<node>>{}(v.expr) ^ v.bind.hash;
		}
	};

	template<>
	struct hash<nodes::con>
	{
		std::uint64_t operator()(const nodes::con& v) const
		{
			return v.bind.hash ^ v.rank;
		}
	};

	template<>
	struct hash<node>
	{
		std::uint64_t operator()(const node& v) const
		{
			const std::uint64_t node_hash = std::visit(
				[](auto n) { return hash<decltype(n)>{}(n); }, 
				v
			);

			return util::scramble_fnv(v.index()) ^ node_hash;
		}
	};

	template<typename Child>
	constexpr node node_base<Child>::to_node() const
	{
		return node(node_base<Child>::as_child());
	}

	enum class qualifier : std::uint8_t
	{
		none, live, ex
	};

	struct decl
	{
		qualifier quals;
		binding name;
		id<node> type;

		decl(const std::string_view str, const id<node> type_, const qualifier quals_ = qualifier::none) :
			quals(quals_), name(binding(str)), type(type_)
		{}
	};

	struct def
	{
		binding name;
		id<node> value;

		def(const std::string_view str, const id<node> v) :
			name(binding(str)), value(v)
		{}
	};

	struct mod
	{
		std::vector<decl> decls;
		std::vector<def> defs;

		mod(const decl& d) :
			decls({ d }), defs()
		{}

		mod(const def& d) :
			decls(), defs({ d })
		{}

		mod(const mod& m, const decl& d) :
			mod(m)
		{
			decls.push_back(d);
		}

		mod(const mod& m, const def& d) :
			mod(m)
		{
			defs.push_back(d);
		}
	};

	using node_interner = interner<node, hash<node>, std::equal_to<node>>;

	template<typename Traversal>
	class traversal;

	template<typename Child, typename FuncType>
	class traverser;

	template<typename Child, typename Ret, typename... Params>
	class traverser<Child, Ret(Params...)> : public util::enable_crtp<Child>
	{
	public:
		friend class traversal<Child>;

		traverser(const pool_t<node>& node_pool) :
			node_pool_(node_pool)
		{}

	protected:
		Ret traverse(const id<node> id, Params... params) const
		{
			const node& node = node_pool_.get(id);
			return std::visit([&params..., child = traverser<Child, Ret(Params...)>::as_child()](auto n){
				return child(n, params...); 
			}, node);
		}

	private:
		const pool_t<node>& node_pool_;
	};

	template<typename Traverser>
	class traversal
	{
	public:
		template<typename... Args>
		traversal(Args&&... args) :
			traverser_(args...)
		{}

		auto operator()(const id<node> node, auto... params) const
		{
			return traverser_.traverse(node, params...); 
		}
			
	private:
		Traverser traverser_;
	};

	struct printer : public traverser<printer, void(std::ostream& os)>
	{
		printer(const pool_t<node>& node_pool);

		template<prim P>
		void operator()(const P&, std::ostream& os) const
		{
			os << P::name;
		}

		void operator()(const nodes::constant& v, std::ostream& os) const;
		void operator()(const nodes::var& v, std::ostream& os) const;
		void operator()(const nodes::app& v, std::ostream& os) const;
		void operator()(const nodes::lam& v, std::ostream& os) const;
		void operator()(const nodes::con& v, std::ostream& os) const;
	};

	class builder
	{
	public:
		builder(node_interner& interner);

		template<class T> requires is_node_v<T>
		id<node> build(const T& v)
		{
			return interner_.intern(v.to_node());
		}

		id<node> num(const std::string_view str);
		id<node> var(const std::string_view str);
		id<node> app(const id<node> e0, const id<node> e1);
		id<node> lam(const std::string_view str, const id<node> e);
		id<node> fun(const id<node> e0, const id<node> e1);

		id<node> prim_or_var(const std::string_view str);

	private:
		node_interner& interner_;

		ska::bytell_hash_map<binding, id<node>, hash<binding>> prim_map_;
	};

	struct context
	{
		pool_t<node> node_pool;
		node_interner interner;
		ska::bytell_hash_map<binding, id<node>, hash<binding>> functions;

		context() : 
			node_pool(), interner(node_pool) 
		{}

		ast::builder builder();

		template<typename Traverser, typename... Params>
		ast::traversal<Traverser> traversal(Params&&... params)
		{
			return ast::traversal<Traverser>(node_pool, std::forward(params)...);
		}

		template<typename Traverser>
		ast::traversal<Traverser> traversal()
		{
			return ast::traversal<Traverser>(node_pool);
		}
	};
}


	class script_context
	{
	public:
		//script_context() = default;
		script_context();
		~script_context();

		std::uint64_t add(std::vector<std::uint32_t> spirv);
		id<ast::node> parse(const std::string_view filename);
		std::vector<std::uint32_t> compile(id<ast::node> root);
		std::vector<std::uint32_t> link(std::span<const std::uint64_t> module_ids, const bool optimize = true);

		//private:	
		std::unique_ptr<struct tools_impl> tools_;

	};

}

