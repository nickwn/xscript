
/*
struct spirv_writer
{
    std::deque<spv_id> data;
    std::size_t cur_instr_start;

    void start_instr(const spv::Op op, const spv::Id result_id, const spv::Id type_id)
    {
        data.push_back(op);
        data.push_back(type_id);
        data.push_back(result_id);
    }

    void start_instr_no_type(const spv::Op op, const spv::Id result_id)
    {
        data.push_back(op);
        data.push_back(result_id);
    }

    void start_instr_no_result_or_type(spv::Op op)
    {
        data.push_back(op);
    }

    void end_instr()
    {
        const std::size_t word_count = data.size() - cur_instr_start;
        const std::uint32_t first_word = (word_count << spv::WordCountShift);
        data[cur_instr_start] |= first_word;
    }

    void add_operand(const std::uint32_t operand)
    {
        data.push_back(operand);
    }

    void add_operands(const std::span<std::uint32_t>& operands)
    {
        for (std::uint32_t operand : operands)
        {
            data.push_back(operand);
        }
    }

    static std::size_t str_operand_size(const std::string_view name)
    {
        return name.empty() ? 0 : xs::div_round_up(name.size() + 1ui64, 4ui64);
    }

    void add_str_operand(const std::string_view str)
    {
        std::size_t write_itr = 0;
        std::uint32_t cur_word = 0;
        const std::size_t str_words = str_operand_size(str);
        const std::size_t write_pos = data.size();
        data.resize(data.size() + str_words);
        for (write_itr = 0; write_itr < str.size(); write_itr++)
        {
            data[write_pos + (write_itr / 4)] = str[write_itr] << (write_itr % 4);
        }

        for (; write_itr < str_words * 4; write_itr++)
        {
            data[write_pos + (write_itr / 4)] = 0;
        }
    }
};

struct type_lookup
{
    id_generator& new_id;
    spirv_writer& types_writer;

    spv_id none_type;
    std::array<spv_id, 4> int_types;
    std::array<spv_id, 4> uint_types;
    std::array<spv_id, 3> float_types;
    robin_hood::unordered_flat_map<std::uint32_t, spv_id> vec_types;
    robin_hood::unordered_flat_map<std::uint32_t, spv_id> mat_types;

    struct func_type
    {
        const ast::nodes::function& func;
        std::uint64_t hash;

        bool operator==(const func_type& other) const
        {
            const bool return_equal = func.return_type == other.func.return_type;
            const bool params_equal = std::equal(func.params.begin(), func.params.end(), 
                other.func.params.begin(), [](const ast::nodes::variable& v0, const ast::nodes::variable& v1) {
                    return v0.return_type == v1.return_type;
                }
            );
            return return_equal && params_equal;
        }
    };

    struct func_type_hash
    {
        std::uint64_t operator()(const func_type& item) const
        {
            return item.hash;
        }
    };

    robin_hood::unordered_map<func_type, spv::Id, func_type_hash> func_type_cache;


    type_lookup(id_generator& new_id_, spirv_writer& types_writer_) :
        new_id(new_id_), types_writer(types_writer_)
    {}

    spv_id operator()(const ast::types::integer& type)
    {
        spv_id* found = nullptr;
        if (type.has_sign)
        {
            switch (type.width)
            {
            case 8:
                found = &int_types[0];
            case 16:
                found = &int_types[1];
            case 32:
                found = &int_types[2];
            case 64:
                found = &int_types[3];
            }
        }
        else
        {
            switch(type.width)
            {
            case 8:
                found = &uint_types[0];
            case 16:
                found = &uint_types[1];
            case 32:
                found = &uint_types[2];
            case 64:
                found = &uint_types[3];
            }
        }

        if (*found)
        {
            return *found;
        }

        *found = new_id();
        types_writer.start_instr_no_type(spv::OpTypeInt, *found);
        types_writer.add_operand(type.width);
        types_writer.add_operand(type.has_sign ? 1 : 0);
        types_writer.end_instr();

        return *found;
    }

    spv_id operator()(const ast::types::real& type)
    {
        spv_id* found = nullptr;
        switch (type.width)
        {
        case 16:
            found = &float_types[0];
        case 32:
            found = &float_types[1];
        case 64:
            found = &float_types[2];
        }
        
        if (*found)
        {
            return *found;
        }

        *found = new_id();
        types_writer.start_instr_no_type(spv::OpTypeFloat, *found);
        types_writer.add_operand(type.width);
        types_writer.end_instr();

        return *found;
    }

    spv_id operator()(const ast::types::vector& type)
    {
        static constexpr std::size_t key_size_offset = 8;
        const spv_id comp_type_id = std::visit(*this, *type.comp_type);
        const std::uint32_t type_key = comp_type_id | (type.size << key_size_offset);
        spv_id& found = vec_types[type_key];

        if (found)
        {
            return found;
        }

        found = new_id();
        types_writer.start_instr_no_type(spv::OpTypeVector, found);
        types_writer.add_operand(comp_type_id);
        types_writer.add_operand(type.size);
        types_writer.end_instr();

        return found;
    }

    spv_id operator()(const ast::types::matrix& type)
    {
        static constexpr std::size_t key_rows_offset = 8;
        static constexpr std::size_t key_cols_offset = 16;
        const spv_id comp_type_id = std::visit(*this, *type.comp_type);
        const spv_id col_type_id = operator()(ast::types::vector(type.comp_type, type.rows));
        const std::uint32_t type_key = comp_type_id | (type.rows << key_cols_offset) | (type.cols << key_cols_offset);
        spv_id& found = mat_types[type_key];

        if (found)
        {
            return found;
        }

        found = new_id();
        types_writer.start_instr_no_type(spv::OpTypeMatrix, found);
        types_writer.add_operand(col_type_id);
        types_writer.add_operand(type.cols);
        types_writer.end_instr();

        return found;
    }

    spv_id operator()(const ast::types::none& type)
    {
        if (none_type)
        {
            return none_type;
        }

        none_type = new_id();
        types_writer.start_instr_no_type(spv::OpTypeVoid, none_type);
        types_writer.end_instr();
    }

    spv_id func(const ast::nodes::function& func, const spv_id ret_type, const std::span<spv_id>& param_types)
    {
        const std::uint64_t hash = xs::hash{}(param_types) ^ ret_type; // lazy

        const func_type tmp_ft = {
            .func = func,
            .hash = hash
        };

        spv_id& found = func_type_cache[tmp_ft];
        if (found)
        {
            return found;
        }

        found = new_id();
        types_writer.start_instr_no_type(spv::OpTypeFunction, found);
        types_writer.add_operand(ret_type);
        types_writer.add_operands(param_types);
        types_writer.end_instr();

        return found;
    }

};

struct spirv_codegen
{
    id_generator new_id;
    spirv_writer types_writer;
    spirv_writer module_writer;
    type_lookup type_id;

    static constexpr std::uint32_t magic_number = 0x07230203;
    static constexpr std::size_t num_spirv_types = spv::OpConstantTrue - spv::OpTypeVoid;

    spirv_codegen() :
        new_id(),
        types_writer(),
        type_id(new_id, types_writer)
    {}

    spv_id start_func_decl(const ast::nodes::function& node)
    {
        const spv_id ret_type_id = std::visit(type_id, *node.return_type);
        spv_id* param_ids_ptr = (spv_id*)alloca(node.params.size() * sizeof(spv_id));
        std::span<spv_id> param_type_ids = std::span<spv_id>(param_ids_ptr, node.params.size());
        std::transform(std::begin(node.params), std::end(node.params), std::begin(param_type_ids),
            [this](const ast::nodes::variable& v) {
                return std::visit(type_id, *v.return_type);
            }
        );

        const spv_id func_type_id = type_id.func(node, ret_type_id, param_type_ids);

        // TODO: handle return and param precision
        // TODO: name handling

        const spv_id result_id = new_id();
        module_writer.start_instr(spv::OpFunction, result_id, ret_type_id);
        module_writer.add_operand(spv::FunctionControlMaskNone);
        module_writer.add_operand(func_type_id);
        module_writer.end_instr();

        for (const spv_id spv_type : param_type_ids)
        {
            const spv::Id param_id = new_id();
            module_writer.start_instr(spv::OpFunctionParameter, param_id, spv_type);
            module_writer.end_instr();
        }

        return result_id;
    }

    spv_id operator()(ast::nodes::function node)
    {
        const spv_id result_id = start_func_decl(node);

        const spv::Id start_label_id = new_id();
        module_writer.start_instr_no_type(spv::OpLabel, start_label_id);
        module_writer.end_instr();

        //do

        module_writer.start_instr_no_result_or_type(spv::OpFunctionEnd);
        module_writer.end_instr();
    }
};
*/

struct tools_impl
{
    tools_impl() :
        ctx(SPV_ENV_UNIVERSAL_1_5),
        core(SPV_ENV_UNIVERSAL_1_5),
        opt(SPV_ENV_UNIVERSAL_1_5)
    {}

    spvtools::Context ctx;
    spvtools::SpirvTools core;
    spvtools::Optimizer opt;

    std::vector<std::vector<std::uint32_t>> modules;
};

script_context::script_context() :
    tools_(std::make_unique<tools_impl>())
{
    auto spv_tools_log = [this](spv_message_level_t, const char*, const spv_position_t&, const char* m) {
        std::cout << m << std::endl;
    };

    tools_->ctx.SetMessageConsumer(spv_tools_log);
    tools_->core.SetMessageConsumer(spv_tools_log);
    tools_->opt.SetMessageConsumer(spv_tools_log);
    tools_->opt.RegisterPerformancePasses();

}

script_context::~script_context() {}

std::uint64_t script_context::add(std::vector<std::uint32_t> spirv)
{
    const std::uint64_t id = tools_->modules.size();
    tools_->modules.push_back(std::move(spirv));
    return id;
}

id<ast::node> script_context::parse(const std::string_view filename)
{
   
    return invalid_id<ast::node>;
}

std::vector<std::uint32_t> script_context::compile(id<ast::node> root)
{
    std::vector<std::uint32_t> test;

    //spirv_codegen codegen = spirv_codegen();
    //std::visit(codegen, *root);

    //std::vector<std::uint32_t> result = codegen.dump();

    std::string text;
    //const bool ok = tools_->core.Validate(result.data(), result.size());
    //const bool ok = tools_->core.Disassemble(result.data(), result.size(), &text);
        //SPV_BINARY_TO_TEXT_OPTION_PRINT | SPV_BINARY_TO_TEXT_OPTION_FRIENDLY_NAMES | SPV_BINARY_TO_TEXT_OPTION_COMMENT);
    //assert(ok);

    return {};
}

std::vector<std::uint32_t> script_context::link(std::span<const std::uint64_t> module_ids, const bool optimize)
{
    std::vector<std::size_t> module_sizes(module_ids.size());
    std::vector<const std::uint32_t*> module_ptrs(module_ids.size());
    for (const std::uint64_t id : module_ids)
    {
        module_sizes.push_back(tools_->modules[id].size());
        module_ptrs.push_back(tools_->modules[id].data());
    }

    std::vector<std::uint32_t> linked;
    spvtools::Link(tools_->ctx, module_ptrs.data(), module_sizes.data(), module_ids.size(), &linked);

    if (!optimize)
    {
        return linked;
    }

    std::vector<std::uint32_t> optimized;
    tools_->opt.Run(linked.data(), linked.size(), &optimized);

    return optimized;
}

}