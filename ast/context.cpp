#include <iostream>
#include <array>
#include <deque>
#include <ranges>
#include <cassert>
#include <variant>

#include "context.hpp"

namespace xs
{
namespace ast
{

template<node N, node_variant<N> T>
static void register_if_prim(builder<N>& b, ska::bytell_hash_map<string_binding, id<N>, hash<string_binding>>& prim_map)
{
    if constexpr (prim<T>)
    {
        prim_map.insert(std::make_pair(string_binding(T::name.view()), b.build(T())));
    }
}

template<node N, typename V>
struct register_prims;

template<node N, node_variant<N> T>
struct register_prims<N, std::variant<T>>
{
    template<node N>
    void operator()(builder<N>& b, ska::bytell_hash_map<string_binding, id<N>, hash<string_binding>>& prim_map)
    {
        register_if_prim<N, T>(b, prim_map);
    }
};

template<node N, node_variant<N> T, node_variant<N>... Ts>
struct register_prims<N, std::variant<T, Ts...>>
{
    template<node N>
    void operator()(builder<N>& b, ska::bytell_hash_map<string_binding, id<N>, hash<string_binding>>& prim_map)
    {
        register_if_prim<N, T>(b, prim_map);
        register_prims<N, std::variant<Ts...>>{}(b, prim_map);
    }
};

builder<sb_node>::builder(node_interner<sb_node>& interner) :
    interner_(interner), prim_map_()
{
    register_prims<sb_node, detail::node_base_t<sb_node>>{}(*this, prim_map_);
}

builder<rb_node>::builder(node_interner<rb_node>& interner) :
    interner_(interner), prim_map_()
{
    register_prims<rb_node, detail::node_base_t<rb_node>>{}(*this, prim_map_);
}

} // namespace ast
} // namespace xs
