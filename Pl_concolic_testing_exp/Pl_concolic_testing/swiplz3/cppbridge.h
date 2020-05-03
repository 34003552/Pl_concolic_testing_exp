
extern "C" void cpp__z3_mk_config();
extern "C" void cpp__z3_del_config();
extern "C" void cpp__z3_set_param_value(const char *param_id, const char *param_value);

extern "C" void cpp__z3_mk_context(int *index);
extern "C" void cpp__z3_del_context(int index);

extern "C" void cpp__z3_mk_solver(int index);
extern "C" void cpp__z3_del_solver(int index);

extern "C" void cpp__z3_push(int index);
extern "C" void cpp__z3_pop(int index);

extern "C" void cpp__z3_mk_int_var(int index, const char *varname);
extern "C" void cpp__z3_mk_term_var(int index, const char *varname);

extern "C" void cpp__z3_mk_term_type(int index, void *tlist, int need_int, int need_lists);

extern "C" int cpp__z3_assert_int_string(int index, const char *z3string);
extern "C" int cpp__z3_assert_term_string(int index, const char *z3string, int need_int, int need_lists);

extern "C" int cpp__z3_check(int index);

extern "C" void cpp__z3_print_model(int index, char **str);

extern "C" int cpp__z3_get_model_intvar_eval(int index, const char *vn, int *val);
extern "C" int cpp__z3_get_model_termvar_eval(int index, const char *vn, void **v);

extern "C" void cpp__z3_mk_term__get_app(int index, void *v, void **app);
extern "C" void cpp__z3_mk_term__get_functor(int index, void *app, char **name, int *arity);
extern "C" void cpp__z3_mk_term__get_app_arg(int index, void *app, int j, void **app_arg);
