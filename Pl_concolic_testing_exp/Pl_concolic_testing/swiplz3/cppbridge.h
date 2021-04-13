
#pragma once

#include "./pl2cpp/cpptypes.h"

extern "C" bool cpp__z3_mk_config();
extern "C" bool cpp__z3_del_config();

extern "C" bool cpp__z3_set_param_value(String param, String value);

extern "C" bool cpp__z3_mk_context(int *context_id);
extern "C" bool cpp__z3_del_context(int context_id);

extern "C" bool cpp__z3_mk_solver(int context_id);
extern "C" bool cpp__z3_del_solver(int context_id);

extern "C" bool cpp__z3_push(int context_id);
extern "C" bool cpp__z3_pop(int context_id);

extern "C" bool cpp__z3_mk_int_var(int context_id, String var_name);
extern "C" bool cpp__z3_mk_term_var(int context_id, String var_name);

extern "C" bool cpp__z3_mk_term_type(int context_id, List<Functor> known_terms, bool need_int, bool need_lists);

extern "C" bool cpp__z3_assert_int_string(int context_id, String assertion);
extern "C" bool cpp__z3_assert_term_string(int context_id, String assertion, bool need_int, bool need_lists);

extern "C" bool cpp__z3_check(int context_id);

extern "C" bool cpp__z3_print_model(int context_id, String *model);

extern "C" bool cpp__z3_get_model_intvar_eval(int context_id, String var_name, Integer *var_value);
extern "C" bool cpp__z3_get_model_termvar_eval(int context_id, String var_name, Term *var_value);


extern "C" bool cpp__z3_mk_datatypes(int context_id, List<String> tpl_args, List<Datatype> datatypes);

extern "C" bool cpp__z3_mk_var(int context_id, String var_name, String var_type);


//void cpp__z3_error_handler(const char *error, const char *msg);