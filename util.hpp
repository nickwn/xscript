
#pragma once
#include <span>
#include <deque>
#include <tuple>
#include <string>
#include <format>
#include <ranges>
#include <cstdint>
#include <fstream>
#include <sstream>
#include <utility>
#include <string_view>

#define XXH_INLINE_ALL
#include "extern/xxhash.h"
#include "extern/bytell_hash_map.hpp"

namespace xs
{

namespace util
{
	// https://graphics.stanford.edu/~seander/bithacks.html#RoundUpPowerOf2
	constexpr static std::uint32_t round_up_po2(std::uint32_t v) 
	{
		v--;
		v |= v >> 1;
		v |= v >> 2;
		v |= v >> 4;
		v |= v >> 8;
		v |= v >> 16;
		v++;

		return v;
	}

	template<typename T> requires std::is_integral_v<T>
	constexpr static T div_round_up(const T x, const T y)
	{
		return x == 0 ? 0 : 1 + ((std::make_signed_t<T>(x) - 1) / std::make_signed_t<T>(y));
	}

	template<typename T>
	constexpr static std::size_t bitsof()
	{
		constexpr std::size_t bits_per_byte = 8;
		return sizeof(T) * bits_per_byte;
	}

	template<typename Derived>
	class enable_crtp
	{
	public:
		friend bool operator==(const enable_crtp&, const enable_crtp&) = default;

	protected:
		const Derived& as_derived() const
		{
			return *static_cast<const Derived*>(this);
		}

		Derived& as_derived()
		{
			return *static_cast<Derived*>(this);
		}
	};

	constexpr static void copy(char* out, const char* in, const std::size_t N)
	{
		for (std::size_t i = 0; i < N; i++)
		{
			out[i] = in[i];
		}
	}

	template<typename T, typename U, std::size_t N>
	constexpr static std::array<T, N> bit_cast(const std::array<U, N>& in)
	{
		std::array<T, N> res;
		for (std::size_t i = 0; i < N; i++)
		{
			res[i] = std::bit_cast<T>(in[i]);
		}

		return res;
	}

	template<std::size_t N>
	constexpr static std::array<char, N + 1> concat(const std::array<char, N>& a, const char c)
	{
		std::array<char, N + 1> res;
		copy(std::data(res), std::data(a), std::size(a));
		res[N] = c;
		return res;
	}

	template<std::size_t N0, std::size_t N1>
	constexpr static std::array<char, N0 + N1> concat(const std::array<char, N0>& a0, const std::array<char, N1>& a1)
	{
		std::array<char, N0 + N1> res;
		copy(std::data(res), std::data(a0), std::size(a0));
		copy(std::data(res) + N0, std::data(a1), std::size(a1));
		return res;
	}

	template<std::size_t N>
	constexpr static std::array<char, N - 1> to_array(const char(&v)[N])
	{
		std::array<char, N - 1> res;
		copy(std::data(res), std::data(v), std::size(res));
		return res;
	}

	template<std::size_t N>
	constexpr static std::size_t num_digits()
	{
		if constexpr (N == 0)
		{
			return 0;
		}
		else
		{
			return num_digits<N / 10>() + 1;
		}
	}

	template<std::size_t val>
	constexpr static std::array<char, num_digits<val>()> itoa()
	{
		if constexpr (val == 0)
		{
			return std::array<char, 0>{};
		}
		else
		{
			const std::size_t d = val % 10;
			const char c = '0' + d;

			const auto a0 = itoa<val / 10>();
			return concat(a0, c);
		}
	}

	template<typename T> requires std::is_unsigned_v<T>
	struct fnv_traits;

	template<>
	struct fnv_traits<std::uint32_t>
	{
		constexpr static std::uint32_t offset_basis = 0x811c9dc5;
		constexpr static std::uint32_t prime = 0x01000193;
	};

	template<>
	struct fnv_traits<std::uint64_t>
	{
		constexpr static std::uint64_t offset_basis = 0xcbf29ce484222325;
		constexpr static std::uint64_t prime = 0x00000100000001B3;
	};

	template<typename T> requires std::is_unsigned_v<T>
	constexpr static T fnv(const std::uint8_t* data, const std::size_t size)
	{
		using unsigned_t = std::decay_t<T>;
		using traits = fnv_traits<T>;
		
		unsigned_t hash = traits::offset_basis;
		for (const std::uint8_t byte : std::span{ data, size })
		{
			hash *= traits::prime;
			hash ^= byte;
		}

		return hash;
	}

	// https://github.com/skeeto/hash-prospector
	template<typename T> requires std::is_unsigned_v<T>
	constexpr static std::uint32_t scramble(const T v)
	{
		std::uint32_t x = std::uint32_t(v);
		x ^= x >> 16;
		x *= 0x21f0aaad;
		x ^= x >> 15;
		x *= 0xd35a2d97;
		x ^= x >> 15;
		return x;
	}


	template<typename T, typename V>
	struct is_in_variant : std::false_type {};

	template<typename T, typename U, typename... Us> requires std::negation_v<std::is_same<T, U>>
	struct is_in_variant<T, std::variant<U, Us...>> : is_in_variant<T, std::variant<Us...>> {};

	template<typename T, typename... Us>
	struct is_in_variant<T, std::variant<T, Us...>> : std::true_type {};

	template<typename T, typename V>
	constexpr bool is_in_variant_v = is_in_variant<T, V>::value;

	template <typename T, typename = void>
	struct is_span : std::false_type {};

	template <typename T>
	struct is_span<T
		, std::void_t<decltype(std::declval<T>().data())
		, decltype(std::declval<T>().size())>> : std::true_type {};

	template<typename T>
	constexpr bool is_span_v = is_span<T>::value;

	static std::string read_file(const std::string_view filename)
	{
		std::ifstream file = std::ifstream(filename.data());

		std::stringstream buffer;
		buffer << file.rdbuf();
		return buffer.str();
	}

	constexpr char newl = '\n';

	template<typename T, typename V>
	struct is_std_array : std::bool_constant<false> {};

	template<std::size_t N, typename V>
	struct is_std_array<std::array<V, N>, V> : std::bool_constant<true> {};

	template<typename T, typename V>
	constexpr static bool is_std_array_v = is_std_array<T, V>::value;

	namespace detail
	{
		template<typename T, typename Fn, std::size_t... Is>
		static auto tuple_map_(T&& v, Fn&& f, std::index_sequence<Is...>)
		{
			return std::tuple<
				std::invoke_result_t<
					Fn,
					decltype(std::get<Is>(std::forward<T>(v)))&&
				>...
			>{
				f(std::get<Is>(std::forward<T>(v)))...
			};
		}

		template<typename T0, typename T1, typename Fn, std::size_t... Is>
		static auto tuple_map_(T0&& v0, T1&& v1, Fn&& f, std::index_sequence<Is...>)
		{
			return std::tuple<
				std::invoke_result_t<
					Fn, 
					std::tuple_element_t<Is, std::remove_cvref_t<T0>>&&,
					std::tuple_element_t<Is, std::remove_cvref_t<T1>>&&
				>...
			>{
				f(std::get<Is>(std::forward<T0>(v0)), std::get<Is>(std::forward<T1>(v1)))...
			};
		}
	}

	template<typename Tup>
	using tuple_indices_t = std::make_index_sequence<
		std::tuple_size_v<typename std::remove_reference_t<Tup>>
	>;

	template<std::size_t i = 0, typename Fn, typename... Ts>
	static void tuple_for_each(std::tuple<Ts...>& v, Fn&& f)
	{
		if constexpr (i < sizeof...(Ts))
		{ 
			f(std::get<i>(v));
			tuple_for_each<i + 1>(v, std::forward<Fn>(f));
		}
	}

	template<typename T, typename Fn>
	static auto tuple_map(T&& v, Fn&& f)
	{
		return detail::tuple_map_(
			std::forward<T>(v), 
			std::forward<Fn>(f), 
			tuple_indices_t<std::remove_cvref_t<T>>{}
		);
	}

	template<typename T0, typename T1, typename Fn>
	static auto tuple_map(T0&& v0, T1&& v1, Fn&& f)
	{
		return detail::tuple_map_(
			std::forward<T0>(v0),
			std::forward<T1>(v1),
			std::forward<Fn>(f),
			tuple_indices_t<std::remove_cvref_t<T0>>{}
		);
	}

	template<std::size_t i = 0, typename Fn, typename A, typename... Ts>
	static auto tuple_fold(const std::tuple<Ts...>& v, A&& acc_init, Fn&& f)
	{
		if constexpr (i < sizeof...(Ts))
		{
			return f(tuple_fold<i + 1>(v, acc_init, std::forward<Fn>(f)), std::get<i>(v));
		}
		else
		{
			return acc_init;
		}
	}

	// shitty ranges library for things not implemented in c++20
	namespace ranges
	{
		namespace detail
		{
			static bool and_(const bool a, const bool b)
			{
				return a && b;
			}

			static bool or_(const bool a, const bool b)
			{
				return a || b;
			}

			struct min_
			{
				template<typename T, typename U>
				constexpr auto operator()(const T& t, const U& u) const
				{
					return u < t ? u : t;
				}
			};

			static constexpr min_ min;

			struct equal_to_
			{
				template<typename T0, typename T1>
				bool operator()(const T0& a, const T1& b) const
				{
					return a == b;
				}
			};

			static constexpr equal_to_ equal_to;

			struct distance_to_
			{
				template<typename T>
				constexpr auto operator()(const T& l, const T& r) const
				{
					return r - l;
				}
			};

			static constexpr distance_to_ distance_to;
			
			struct deref_
			{
				template<typename I>
				auto operator()(I&& iter) const
				{
					return *iter;
				}
			};

			static constexpr deref_ deref;

			struct insert_
			{
				template<typename T>
				void operator()(std::vector<T>& c, T&& v)
				{
					c.push_back(v);
				}

				template<typename K, typename V, typename H, typename E>
				void operator()(ska::bytell_hash_map<K, V, H, E>& c, std::pair<K, V> v)
				{
					c.insert(std::move(v));
				}
			};

			static constexpr insert_ insert;
		}

		template<typename Container, std::ranges::input_range View>
		static Container to(View view)
		{
			Container res;
			for (auto val : view)
			{
				detail::insert(res, std::move(val));
			}

			return res;
		}

		template<std::ranges::input_range... Rs>
		class zip : public std::ranges::view_interface<zip<Rs...>>
		{
		public:
			class sentinel
			{
				friend class iterator;
			public:
				sentinel() = default;
				sentinel(const sentinel&) = default;
				explicit sentinel(std::tuple<std::ranges::sentinel_t<const Rs>...> ends)
					: ends_(std::move(ends))
				{}

			private:
				std::tuple<std::ranges::sentinel_t<const Rs>...> ends_;
			};

			class iterator
			{
			public:
				using iterator_category = std::input_iterator_tag;
				using value_type = std::tuple<std::ranges::range_value_t<Rs>...>;
				using difference_type = std::common_type_t<std::ranges::range_difference_t<Rs>...>;
				using pointer = value_type*;
				using reference = value_type&;

				iterator() = default;
				iterator(const iterator&) = default;
				explicit iterator(const std::tuple<std::ranges::iterator_t<const Rs>...>& iters) : 
					iters_(iters) 
				{}

				iterator& operator++ () 
				{ 
					tuple_for_each(iters_, [](auto& i) { ++i; }); 
					return *this; 
				}

				iterator operator++ (int) 
				{ 
					iterator res = *this; 
					++(*this); 
					return res; 
				}

				template<typename S>
				friend auto operator-(const iterator& l, const S& r)
				{
					return tuple_fold(
						tuple_map(l.iters_, r.iters_, detail::distance_to),
						std::numeric_limits<difference_type>::max(),
						detail::min_{}
					);
				}

				bool operator==(const iterator& other) const 
				{ 
					return tuple_fold(
						tuple_map(iters_, other.iters_, detail::equal_to),
						false, 
						detail::or_
					); 
				}

				bool operator==(const sentinel& other) const
				{
					return tuple_fold(
						tuple_map(iters_, other.ends_, detail::equal_to),
						false,
						detail::or_
					);
				}

				bool operator!=(const iterator& other) const 
				{ 
					return !(*this == other); 
				}

				bool operator!=(const sentinel& other) const
				{
					return !(*this == other);
				}

				value_type operator* () const 
				{ 
					return tuple_map(iters_, detail::deref); 
				}

			private:

				std::tuple<std::ranges::iterator_t<const Rs>...> iters_;
			};

			static_assert(std::input_iterator<iterator>);

			zip(const Rs&... rngs) : 
				rngs_(rngs...) 
			{}

			iterator begin() const
			{ 
				return iterator(tuple_map(rngs_, std::ranges::begin)); 
			}

			sentinel end() const
			{ 
				return sentinel(tuple_map(rngs_, std::ranges::end)); 
			}

			std::size_t size() const
			{
				return tuple_fold(
					tuple_map(rngs_, std::ranges::size),
					std::numeric_limits<std::size_t>::max(),
					detail::min_{}
				);
			}

		private:
			std::tuple<Rs...> rngs_;
		};

		template <std::ranges::input_range R>
		constexpr std::ranges::input_range auto enumerate(const R& r)
		{
			using zip_type = zip<std::ranges::iota_view<std::size_t>, R>;
			
			return zip_type(
				std::views::iota(std::size_t(0)), 
				r
			);
		}
	}
}

template<typename T>
struct id
{
	using type = std::uint32_t;

	id();

	explicit constexpr id(const type idx_) : 
		idx(idx_) 
	{}

	operator type() const
	{
		return idx;
	}

	type idx;
};

template<typename T>
constexpr static id<T> invalid_id = id<T>(std::numeric_limits<id<T>::type>::max());

template<typename T>
id<T>::id() : id(invalid_id<T>) {}

template<typename T>
static bool operator==(const id<T> i, const id<T> j)
{
	return i.idx == j.idx;
}

template<typename T>
static bool operator!=(const id<T> i, const id<T> j)
{
	return i.idx != j.idx;
}

template<typename T, std::size_t align = alignof(T)>
class deque_pool_base 
{
public:
	deque_pool_base()
		: data_()
	{}

	id<T> alloc() 
	{
		id<T> res = id<T>(std::uint32_t(data_.size()));
		data_.push_back({});
		return res;
	}

	T& get(id<T> i) 
	{
		return *reinterpret_cast<T*>(&data_[i.idx]);
	}

	const T& get(id<T> i) const 
	{
		return *reinterpret_cast<const T*>(&data_[i.idx]);
	}

private:
	struct alignas(align) elem 
	{
		std::uint8_t x[sizeof(T)];
	};

	std::deque<elem> data_;
};

template<typename T, class pool_base>
class pool : public pool_base
{
public:
	template<typename T>
	struct generator {
		pool* owner;
		id<T> operator()()
		{
			return owner->alloc();
		}
	};

	generator<T> gen()
	{
		return generator<T>{
			.owner = this
		};
	}

	template<typename... As>
	id<T> emplace(As&&... as)
	{
		const id<T> res = pool_base::template alloc();
		T& ref = pool_base::template get(res);
		new (&ref) T(std::forward<As>(as)...);
		return res;
	}
};

template<typename T>
using pool_t = pool<T, deque_pool_base<T>>;

template<typename T>
struct hashed
{
	const T value;
	const std::uint64_t hash;
};

template<typename T, typename Hash, typename... Args>
static hashed<T> make_hashed(Args&&... args)
{
	const T value = T(std::forward(args)...);
	const std::uint64_t hash = Hash{}(value);
	return hashed<T>{
		.value = std::move(value), 
		.hash = hash
	};
}

template<std::size_t N>
struct hashed<std::array<char, N>>
{
	std::array<char, N> value;
	std::uint64_t hash;

	constexpr std::string_view view() const
	{
		return std::string_view(
			std::data(hashed<std::array<char, N>>::value), 
			std::size(hashed<std::array<char, N>>::value)
		);
	}
};

template<std::size_t N>
using hashed_string = hashed<std::array<char, N>>;

template<std::size_t N>
constexpr static hashed_string<N> make_hashed_string(const std::array<char, N>& arr)
{
	const auto uint8_arr = util::bit_cast<std::uint8_t>(arr);
	const std::uint64_t hash = util::fnv<std::uint64_t>(std::data(uint8_arr), std::size(uint8_arr));
	return hashed_string{
		.value = arr,
		.hash = hash
	};
}

template<std::size_t N>
std::ostream& operator<<(std::ostream& os, const hashed_string<N>& hs)
{
	return os << std::string_view(std::data(hs.value), std::size(hs.value));
}

template<std::size_t N>
constexpr static hashed_string<N - 1> make_hashed_string(const char(&arr)[N])
{
	return make_hashed_string(util::to_array(arr));
}

template<typename T, typename Hash, typename Equals>
class interner
{
public:
	interner(pool_t<T>& pool) :
		data_(0, hash_{ .pool = pool }, equal_to_{ .pool = pool }),
		pool_(pool)
	{}

	id<T> intern(const T& v)
	{
		const auto maybe_res = data_.find(v);
		if (maybe_res != std::end(data_))
		{
			return maybe_res->value;
		}

		const id<T> res = pool_.emplace(v);
		const std::uint64_t hash = Hash{}(v);
		data_.insert(hashed<id<T>>{.value = res, .hash = hash});
		return res;
	}

	id<T> get(const T& v) const
	{
		const auto maybe_res = data_.find(v);
		return maybe_res->value;
	}

private:

	struct hash_
	{
		const pool_t<T>& pool;

		std::uint64_t operator()(const hashed<id<T>>& v) const
		{
			return v.hash;
		}

		std::uint64_t operator()(const T& v) const
		{
			return Hash{}(v);
		}
	};

	struct equal_to_
	{
		const pool_t<T>& pool;

		bool operator()(const T& l, const hashed<id<T>>& r) const
		{
			const T& r_elem = pool.get(r.value);
			return Equals{}(l, r_elem);
		}

		// only called on insert, and only inserts unique objects, so should always return false
		bool operator()(const hashed<id<T>>& l, const hashed<id<T>>& r) const
		{
			/*
			const T& l_elem = pool.get(l.value);
			const T& r_elem = pool.get(r.value);
			return Equals{}(l_elem, r_elem);
			*/
			return false;
		}
	};

	using allocator = std::allocator<hashed<id<T>>>;

	using intern_map = ska::detailv8::sherwood_v8_table<
		hashed<id<T>>,
		T,
		hash_,
		ska::detailv8::functor_storage<size_t, hash_>,
		equal_to_,
		ska::detailv8::functor_storage<bool, equal_to_>,
		allocator,
		typename std::allocator_traits<allocator>::template rebind_alloc<unsigned char>,
		ska::detailv8::CalculateBytellBlockSize<T>::value
	>;

	intern_map data_;
	pool_t<T>& pool_;
};

template<typename T>
struct hash;

template<>
struct hash<std::string_view>
{
	std::uint64_t operator()(const std::string_view& sv) const
	{
		const XXH64_hash_t res = XXH64(sv.data(), sv.size(), 0);
		return res;
	}
};

template<typename T>
struct hash<id<T>>
{
	std::uint64_t operator()(const id<T>& v) const
	{
		return std::uint64_t(util::scramble(v.idx));
	}
};

template<typename T>
struct hash<hashed<T>>
{
	std::uint64_t operator()(const hashed<T>& v) const
	{
		return v.hash;
	}
};

template<typename FuncType>
class function_ref;

template<typename Ret, typename... Params>
class function_ref<Ret(Params...)>
{
private:
	template<typename Functor, typename FuncType>
	struct caller;

	template<typename Functor, typename... Params>
	struct caller<Functor, void(Params...)>
	{
		static void call(void* obj, Params&... params)
		{
			(*reinterpret_cast<Functor*>(obj))(std::forward<Params>(params)...);
		}
	};

	template<typename Functor, typename Ret, typename... Params>
	struct caller<Functor, Ret(Params...)>
	{
		static void call(void* obj, Params&... params)
		{
			(*reinterpret_cast<Functor*>(obj))(std::forward<Params>(params)...);
		}
	};

public:

	template<typename Functor>
	function_ref(Functor&& functor)
	{
		using decayed_type = std::decay_t<Functor>;

		ptr_ = reinterpret_cast<void*>(&functor);
		callable_ = &caller<decayed_type, Ret(Params...)>::call;
	}

	Ret operator()(Params... params) const
	{
		return callable_(ptr_, params...);
	}

private:

	void* ptr_;
	Ret(*callable_)(void*, Params&...);
};

}