#include "cppbridge.h"

#include "z3/z3Bundle.h"

void cpp__z3_mk_config()
{
	z3Manager::get_manager().mk_config();
}
void cpp__z3_del_config()
{
	z3Manager::get_manager().del_config();
}
void cpp__z3_set_param_value(const char *param_id, const char *param_value)
{
	z3Manager::get_manager().set_param_value(param_id, param_value);
}
void cpp__z3_mk_context(int *index)
{
	*index = z3Manager::get_manager().mk_context();
}
void cpp__z3_del_context(int index)
{
	z3Manager::get_manager().del_context(index);
}

void cpp__z3_mk_solver(int index)
{
	z3Manager::get_manager().get_bundle(index).mk_solver();
}
void cpp__z3_del_solver(int index)
{
	z3Manager::get_manager().get_bundle(index).del_solver();
}

void cpp__z3_push(int index)
{
	z3Manager::get_manager().get_bundle(index).push();
}
void cpp__z3_pop(int index)
{
	z3Manager::get_manager().get_bundle(index).pop();
}

void cpp__z3_mk_int_var(int index, const char *varname)
{
	z3Manager::get_manager().get_bundle(index).mk_int_var(varname);
}
void cpp__z3_mk_term_var(int index, const char *varname)
{
	z3Manager::get_manager().get_bundle(index).mk_term_var(varname);
}

void cpp__z3_mk_term_type(int index, void *tlist, int need_int, int need_lists)
{
	z3Manager::get_manager().get_bundle(index).mk_term_type((TList*)tlist, need_int, need_lists);
}

int cpp__z3_assert_int_string(int index, const char *z3string)
{
	return z3Manager::get_manager().get_bundle(index).assert_int_string(z3string);
}
int cpp__z3_assert_term_string(int index, const char *z3string, int need_int, int need_lists)
{
	return z3Manager::get_manager().get_bundle(index).assert_term_string(z3string, need_int, need_lists);
}

int cpp__z3_check(int index)
{
	return z3Manager::get_manager().get_bundle(index).check();
}

void cpp__z3_print_model(int index, char **str)
{
	*str = z3Manager::get_manager().get_bundle(index).print_model();
}

int cpp__z3_get_model_intvar_eval(int index, const char *vn, int *val)
{
	return z3Manager::get_manager().get_bundle(index).get_model_intvar_eval(vn, val);
}
int cpp__z3_get_model_termvar_eval(int index, const char *vn, void **v)
{
	return z3Manager::get_manager().get_bundle(index).get_model_termvar_eval(vn, (z3term_t*)v);
}

void cpp__z3_mk_term__get_app(int index, void *v, void **app)
{
	*app = (void*)z3Manager::get_manager().get_bundle(index).mk_term__get_app((Z3_ast)v);
}
void cpp__z3_mk_term__get_functor(int index, void *app, char **name, int *arity)
{
	z3Manager::get_manager().get_bundle(index).mk_term__get_functor((Z3_app)app, name, arity);
}
void cpp__z3_mk_term__get_app_arg(int index, void *app, int j, void **app_arg)
{
	*app_arg = (void*)z3Manager::get_manager().get_bundle(index).mk_term__get_app_arg((Z3_app)app, j);
}