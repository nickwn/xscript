#pragma once

#include <cassert>
#include <format>
#include <queue>
#include <coroutine>

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
			const ast::resolved_binding global_rb = e.at(v.bind);
			const ast::resolved_binding local_rb = global_rb.is_local ? 
				ast::resolved_binding(true, i - global_rb.idx) : 
				global_rb;
			
			return b.build(ast::nodes::var<ast::rb_node>(local_rb));
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

template<bool has_value, typename T>
struct optional_value : T {};

struct typed_value
{
	const std::uint64_t value;
	const id<ast::rb_node> type;
	
	static typed_value thunk(const id<ast::rb_node> node)
	{
		return typed_value{
			.value = util::zero_pad_bit_cast<std::uint64_t>(node),
			.type = invalid_id<ast::rb_node>
		};
	}

	static typed_value invalid()
	{
		return typed_value{
			.value = invalid_id<ast::rb_node>,
			.type = invalid_id<ast::rb_node>
		};
	}

	bool is_ip() const
	{
		return type == invalid_id<ast::rb_node>;
	}

	id<ast::rb_node> as_ip() const
	{
		return util::truncate_bit_cast<id<ast::rb_node>>(value);
	}
};

class lazy_stack
{
public:
	lazy_stack() :
		sp_(0), values_()
	{}

	id<ast::rb_node> at(const std::size_t i) const
	{
		return values_.at(idx(i));
	}

	id<ast::rb_node>& at(const std::size_t i)
	{
		return values_.at(idx(i));
	}

	auto scoped_push(const id<ast::rb_node> elem)
	{
		struct scoped_push_
		{
			lazy_stack& stack;

			scoped_push_(lazy_stack& stack_, id<ast::rb_node> elem_) :
				stack(stack_)
			{
				stack.values_.push_back(elem_);
			}

			~scoped_push_()
			{
				stack.values_.pop_back();
			}

			operator lazy_stack& ()
			{
				return stack;
			}
		};

		return scoped_push_(*this, elem);
	}

	auto scoped_frame()
	{
		struct scoped_frame_
		{
			lazy_stack& stack;

			scoped_frame_(lazy_stack& stack_) :
				stack(stack_)
			{
				stack.sp_++;
			}

			~scoped_frame_()
			{
				stack.sp_--;
			}

			operator lazy_stack& ()
			{
				return stack;
			}
		};

		return sp_ < values_.size() ? std::optional(scoped_frame_(*this)) : std::optional<scoped_frame_>();
	}

	auto scoped_rewind(const std::size_t to_i_local)
	{
		struct scoped_rewind_
		{
			lazy_stack& stack;
			const std::size_t from_i;

			scoped_rewind_(lazy_stack& stack_, const std::size_t to_i) :
				stack(stack_), from_i(stack_.sp_)
			{
				stack.sp_ = to_i;
			}

			~scoped_rewind_()
			{
				stack.sp_ = from_i;
			}

			operator lazy_stack& ()
			{
				return stack;
			}
		};

		return scoped_rewind_(*this, idx(to_i_local));
	}

private:
	std::size_t idx(const std::size_t i) const
	{
		assert(sp_ >= i);
		assert(sp_ <= values_.size());

		return sp_ - i;
	}

	std::size_t sp_;
	std::vector<id<ast::rb_node>> values_;
};

namespace node_passes
{
	namespace detail
	{
		template<typename T>
		struct cpp_type;

		template<> struct cpp_type<ast::prims::i<8>> : public util::type_constant<std::int8_t> {};
		template<> struct cpp_type<ast::prims::i<16>> : public util::type_constant<std::int16_t> {};
		template<> struct cpp_type<ast::prims::i<32>> : public util::type_constant<std::int32_t> {};
		template<> struct cpp_type<ast::prims::i<64>> : public util::type_constant<std::int64_t> {};

		template<> struct cpp_type<ast::prims::u<8>> : public util::type_constant<std::uint8_t> {};
		template<> struct cpp_type<ast::prims::u<16>> : public util::type_constant<std::uint16_t> {};
		template<> struct cpp_type<ast::prims::u<32>> : public util::type_constant<std::uint32_t> {};
		template<> struct cpp_type<ast::prims::u<64>> : public util::type_constant<std::uint64_t> {};

		template<> struct cpp_type<ast::prims::f<16>> : public util::type_constant<std::uint16_t> {};
		template<> struct cpp_type<ast::prims::f<32>> : public util::type_constant<float> {};
		template<> struct cpp_type<ast::prims::f<64>> : public util::type_constant<double> {};

		template<> struct cpp_type<ast::prims::boolean> : public util::type_constant<bool> {};

		template<typename T>
		concept type_prim = requires {
			typename cpp_type<T>::type;
		};

		template<typename T>
		using cpp_type_t = cpp_type<T>::type;

		struct add_
		{
			template<typename T> requires std::is_trivially_copyable_v<T>
			T operator()(const T v0, const T v1) const
			{
				return v0 + v1;
			}
		};

		static constexpr add_ add;

		template<typename T, typename Fn>
		auto binop_wrap_cast(Fn&& fn)
		{
			return [&fn](const std::uint64_t v0, const std::uint64_t v1) {
				return util::zero_pad_bit_cast<std::uint64_t>(
					fn(util::truncate_bit_cast<T>(v0), util::truncate_bit_cast<T>(v1))
				);
			};
		}

		template<typename Fn>
		struct binop_visitor
		{
			const Fn fn;
			const std::uint64_t v0, v1;
			const ast::node_interner<ast::rb_node>& interner;

			template<type_prim P>
			std::optional<typed_value> operator()(const P&, const P&)
			{
				using type = cpp_type_t<P>;

				return typed_value{
					.value = binop_wrap_cast<type>(fn)(v0, v1),
					.type = interner.get(P{}.to<ast::rb_node>())
				};
			}

			std::optional<typed_value> operator()(const auto&, const auto&) 
			{
				return std::optional<typed_value>();
			}
		};
	}

	class evaluate : 
		public ast::traverser<evaluate, ast::rb_node, typed_value(id<ast::rb_node>, lazy_stack&)>
	{
	public:
		evaluate(pool_t<ast::rb_node>&, ast::context<ast::rb_node>& ctx, ast::mod<ast::rb_node>& mod) :
			traverser(ctx.pool()), ctx_(ctx), mod_(mod), cache_start_(ctx.pool().size())
		{}

		typed_value operator()(const ast::prims::add&, const id<ast::rb_node> id, lazy_stack& stack) const
		{
			if (stack.scoped_frame() && stack.scoped_frame())
			{
				const typed_value a0 = eval_local_var(0, stack);
				const typed_value a1 = eval_local_var(1, stack);

				auto maybe_res_val = std::visit(
					make_binop_visitor(detail::add, a0, a1), 
					pool().get(a0.type),
					pool().get(a1.type)
				);

				return maybe_res_val.value_or(typed_value{
					.value = invalid_id<ast::rb_node>,
					.type = invalid_id<ast::rb_node>
				});
			}

			return typed_value{
				.value = id,
				.type = invalid_id<ast::rb_node>
			};
		}

		// simple type check
		template<ast::prim P>
		typed_value operator()(const P&, const id<ast::rb_node> id, lazy_stack& stack) const
		{
			if (stack.scoped_frame())
			{
				const typed_value a0 = eval_local_var(0, stack);
				const bool is_type = a0.type == interner().get(P{}.to<ast::rb_node>());
				return typed_value{
					.value = util::zero_pad_bit_cast<std::uint64_t>(is_type),
					.type = interner().get(ast::prims::boolean{}.to<ast::rb_node>())
				};
			}

			return typed_value{
				.value = id,
				.type = invalid_id<ast::rb_node>
			};
		}

		template<ast::prim P>
		typed_value operator()(const ast::nodes::constant<P>& v, const id<ast::rb_node> id, lazy_stack&) const
		{
			return typed_value{
				.value = v.payload,
				.type = interner().get(P{}.to<ast::rb_node>())
			};
		}

		typed_value operator()(const ast::nodes::var<ast::rb_node>& v, const id<ast::rb_node>, lazy_stack& stack) const
		{
			return v.bind.is_local ? eval_local_var(v.bind.idx, stack) : eval_global_var(v.bind.idx, stack);
		}

		typed_value operator()(const ast::nodes::app<ast::rb_node>& v, const id<ast::rb_node> cur_id, lazy_stack& stack) const
		{
			const typed_value v0 = traverse_(v.expr0, stack);
			if (!v0.is_ip())
			{
				// error
				return typed_value{
					.value = invalid_id<ast::rb_node>,
					.type = invalid_id<ast::rb_node>
				};
			}

			const id<ast::rb_node> ip = v0.as_ip();
			const typed_value prim_res = traverse_(ip, stack.scoped_push(v.expr1));
			const typed_value lazy_res = typed_value::thunk(cur_id);
			return prim_res.is_ip() ? lazy_res : prim_res; // TODO: remove branch somehow?
		}

		typed_value operator()(const ast::nodes::lam<ast::rb_node>& v, const id<ast::rb_node> id, lazy_stack& stack) const
		{
			return stack.scoped_frame() ? traverse_(v.expr, stack) : typed_value{
				.value = id,
				.type = invalid_id<ast::rb_node>
			};
		}

	private:
		typed_value traverse_(const id<ast::rb_node> id, lazy_stack& stack) const
		{
			return traverse(id, id, stack);
		}

		bool is_cached_id(const id<ast::rb_node> i) const
		{
			return i.idx >= cache_start_;
		}

		template<typename Fn>
		auto make_binop_visitor(const Fn& fn_, const typed_value& a0_, const typed_value& a1_) const
		{
			return detail::binop_visitor<std::decay_t<Fn>>{
				.fn = fn_,
				.v0 = a0_.value,
				.v1 = a1_.value,
				.interner = interner()
			};
		}

		template<typename Storage, typename TraverseFn>
		typed_value eval_var(const std::size_t i, Storage& stack, TraverseFn&& traverse_fn) const
		{
			const id<ast::rb_node> ip = stack.at(i);
			// TODO: merge branch with below
			if (is_cached_id(ip))
			{
				lazy_stack tmp_stack;
				const typed_value var_tv = traverse_(ip, tmp_stack);
				return var_tv;
			}

			// lambda to enforce scope
			const typed_value var_tv = traverse_fn(ip);
			const ast::rb_node& var_type = pool().get(var_tv.type);
			const ast::rb_node new_imm = std::visit(util::overloaded{
					[val = var_tv.value](const ast::prim_type auto& t) {
						return ast::nodes::constant<std::decay_t<decltype(t)>>(val)
							.to<ast::rb_node>();
					},
					[](const auto& t) {
						util::unreachable("eval type is not prim");
						return ast::prims::none().to<ast::rb_node>();
					}
				},
				var_type
			);

			const id<ast::rb_node> new_imm_id = pool().emplace(new_imm);
			stack.at(i) = new_imm_id;

			return var_tv;
		}

		typed_value eval_local_var(const std::size_t i, lazy_stack& stack) const
		{
			return eval_var(i, stack, 
				[&](const id<ast::rb_node> ip){ 
					return traverse_(ip, stack.scoped_rewind(i)); 
				}
			);
		}

		typed_value eval_global_var(const std::size_t i, lazy_stack& stack) const
		{
			return eval_var(i, mod_.get().defs,
				[&](const id<ast::rb_node> ip) {
					return traverse_(ip, stack);
				}
			);
		}

		ast::node_interner<ast::rb_node>& interner() const { return ctx_.get().interner(); }
		pool_t<ast::rb_node>& pool() const { return ctx_.get().pool(); }

		// TODO reference_wrapper
		std::reference_wrapper<ast::context<ast::rb_node>> ctx_;
		std::reference_wrapper<ast::mod<ast::rb_node>> mod_;
		std::size_t cache_start_;
	};
}

template <typename T>
struct generator
{
	struct promise_type;
	using handle_type = std::coroutine_handle<promise_type>;

	struct promise_type // required
	{
		T value_;
		std::exception_ptr exception_;

		generator get_return_object()
		{
			return generator(handle_type::from_promise(*this));
		}
		std::suspend_always initial_suspend() { return {}; }
		std::suspend_always final_suspend() noexcept { return {}; }
		void unhandled_exception() { exception_ = std::current_exception(); } // saving
		// exception

		template <std::convertible_to<T> From> // C++20 concept
		std::suspend_always yield_value(From&& from)
		{
			value_ = std::forward<From>(from); // caching the result in promise
			return {};
		}
		void return_void() { }
	};

	handle_type h_;

	generator(handle_type h)
		: h_(h)
	{
	}
	~generator() { h_.destroy(); }
	explicit operator bool()
	{
		fill();
		return !h_.done();
	}

	T operator()()
	{
		fill();
		full_ = false;
		return std::move(h_.promise().value_);
	}

private:
	bool full_ = false;

	void fill()
	{
		if (!full_)
		{
			h_();
			if (h_.promise().exception_)
			{
				std::rethrow_exception(h_.promise().exception_);
			}

			full_ = true;
		}
	}
};

class evaluate
{
public:
	evaluate(ast::context<ast::rb_node>& ctx) : 
		ctx_(ctx)
	{}

	generator<id<ast::rb_node>> gen(ast::mod<ast::rb_node> mod)
	{
		lazy_stack stack;
		using namespace std::placeholders;
		const auto evaluate_expr = std::bind(
			ctx_.get().traversal<node_passes::evaluate>(ctx_.get(), mod),
			_1, _1, stack
		);

		struct typed_expr
		{
			const id<ast::rb_node> expr;
			const id<ast::rb_node> type;
		};

		auto is_live = [](const std::tuple<ast::decl<ast::rb_node>, id<ast::rb_node>>& decl_def) { 
			return std::get<ast::decl<ast::rb_node>>(decl_def).quals == ast::qualifier::live; 
		};
		auto is_rank_zero = [&evaluate_expr](const std::tuple<ast::decl<ast::rb_node>, id<ast::rb_node>>& decl_def) {
			const id<ast::rb_node> expr_id = std::get<id<ast::rb_node>>(decl_def);
			return !evaluate_expr(expr_id).is_ip();
		};

		auto get_decl_node = [](const std::tuple<ast::decl<ast::rb_node>, id<ast::rb_node>>& decl_def) {
			return std::get<id<ast::rb_node>>(decl_def);
		};
		auto to_typed_expr = [](const std::tuple<ast::decl<ast::rb_node>, id<ast::rb_node>>& decl_def) {
			const id<ast::rb_node> expr_id = std::get<id<ast::rb_node>>(decl_def);
			const ast::decl<ast::rb_node>& decl = std::get<ast::decl<ast::rb_node>>(decl_def);
			return typed_expr{
				.expr = expr_id,
				.type = decl.type
			};
		};

		auto live_nodes = util::ranges::zip(mod.decls, mod.defs) |
			std::views::filter(is_live);
		auto root_live_nodes = live_nodes | 
			std::views::filter(is_rank_zero) |
			std::views::transform(get_decl_node);
		auto internal_live_nodes = live_nodes |
			std::views::filter(std::not_fn(is_rank_zero)) |
			std::views::transform(to_typed_expr);

		struct value_queue
		{
			std::size_t cur_idx = 0;
			std::vector<id<ast::rb_node>> data;

			void push_back(const id<ast::rb_node> n) { data.push_back(n); }
			id<ast::rb_node> front() const { return data.at(cur_idx); }
			void pop() { cur_idx++; }
			bool empty() const { return cur_idx == data.size(); }
		};

		value_queue values;
		std::vector<typed_expr> typed_funs;
		util::push_back_range(values, root_live_nodes);
		util::push_back_range(typed_funs, internal_live_nodes);
		while (!values.empty())
		{
			const id<ast::rb_node> cur = values.front();
			co_yield cur;

			auto make_app = [this, cur](const id<ast::rb_node> expr_id) {
				return interner().intern(ast::nodes::app<ast::rb_node>(expr_id, cur));
			};
			auto type_check = [this, &make_app, &evaluate_expr](const id<ast::rb_node> type) {
				const id<ast::rb_node> tmp_app = make_app(type);
				return bool(evaluate_expr(tmp_app).value);
			};

			const std::size_t evaluators_size = typed_funs.size();
			for (std::size_t i = 0; i < evaluators_size; i++)
			{
				const typed_expr& typed_fun = typed_funs.at(i);
				const std::optional<fun_type> maybe_ft = decomp_fun_type(typed_fun.type);
				if (!maybe_ft)
				{
					co_return;
				}

				const fun_type& ft = maybe_ft.value();
				if (!type_check(ft.arg))
				{
					continue;
				}

				const id<ast::rb_node> tmp_app = make_app(typed_fun.expr);
				const bool is_evaluated = !evaluate_expr(tmp_app).is_ip();
				// TODO: type check return

				if (is_evaluated)
				{
					values.push_back(tmp_app);
				}
				else
				{
					typed_funs.push_back(typed_expr{
						.expr = tmp_app,
						.type = ft.ret
					});
				}
			}

			values.pop();
		}
	}

private:

	struct fun_type
	{
		id<ast::rb_node> arg;
		id<ast::rb_node> ret;
	};

	std::optional<fun_type> decomp_fun_type(const id<ast::rb_node> type_id) const
	{
		const ast::rb_node& type = pool().get(type_id);
		const bool has_ret_app = std::holds_alternative<ast::nodes::app<ast::rb_node>>(type);
		if (has_ret_app)
		{
			auto ret_app = std::get<ast::nodes::app<ast::rb_node>>(type);
			const ast::rb_node& ret_app0 = pool().get(ret_app.expr0);
			const bool has_arg_app = std::holds_alternative<ast::nodes::app<ast::rb_node>>(ret_app0);
			if (has_arg_app)
			{
				auto arg_app = std::get<ast::nodes::app<ast::rb_node>>(ret_app0);
				const ast::rb_node& arg_app0 = pool().get(arg_app.expr0);
				const bool is_fun = std::holds_alternative<ast::prims::fun>(arg_app0);
				if (is_fun)
				{
					return fun_type{
						.arg = arg_app.expr1,
						.ret = ret_app.expr1
					};
				}
			}
		}

		return std::optional<fun_type>();
	}

	ast::node_interner<ast::rb_node>& interner() const { return ctx_.get().interner(); }
	pool_t<ast::rb_node>& pool() const { return ctx_.get().pool(); }

	std::reference_wrapper<ast::context<ast::rb_node>> ctx_;
};


} // namespace passes
} // namespace xs