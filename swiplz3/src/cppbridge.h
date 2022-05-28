#ifndef CPPBRIDGE_H
#define CPPBRIDGE_H

#include "./pl2cpp/cpptypes.h"
#include "z3/z3Manager.h"

class CppBridge {
    CppBridge();
    ~CppBridge();
public:
    CppBridge(const CppBridge&) = delete;
    CppBridge& operator=(const CppBridge&) = delete;

    static CppBridge instance;

    z3Manager m_z3manager;
};

extern "C" bool cpp__z3_mk_config();
extern "C" bool cpp__z3_del_config();

extern "C" bool cpp__z3_set_param_value(pl::String param, pl::String value);

extern "C" bool cpp__z3_mk_context(int *context_id);
extern "C" bool cpp__z3_del_context(int context_id);

extern "C" bool cpp__z3_mk_solver(int context_id);
extern "C" bool cpp__z3_del_solver(int context_id);

extern "C" bool cpp__z3_push(int context_id);
extern "C" bool cpp__z3_pop(int context_id);

extern "C" bool cpp__z3_mk_datatypes(int context_id, pl::List<pl::String> tpl_vars, pl::List<pl::Datatype> datatypes);
extern "C" bool cpp__z3_mk_sort(int context_id, pl::String sort_name, pl::String base_name = "");

extern "C" bool cpp__z3_mk_var(int context_id, pl::String var_name, pl::String var_type);
extern "C" bool cpp__z3_mk_func(int context_id, pl::String func_name, pl::List<pl::String> func_args, pl::String ret_type, pl::String func_body = "", bool recursive = false);

extern "C" bool cpp__z3_assert_string(int context_id, pl::String assertion);

extern "C" bool cpp__z3_check_sat(int context_id, pl::atom *result);

extern "C" bool cpp__z3_get_model_to_string(int context_id, pl::String *model);

extern "C" bool cpp__z3_eval_model_var(int context_id, pl::String var_name, pl::Term *var_value);

#endif