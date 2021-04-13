#include "cppbridge.h"

#include <sstream>

#include "z3/z3Bundle.h"

using namespace std;

extern "C" void pl_error_handler(const char *error, const char *msg);
void cpp__crash_handler(const char *funcname, const char *error, const char *msg)
{
	stringstream ss;
	ss << "An exception occurred while calling '" << funcname << "'\n";
	ss << "Error: " << error << "\nDetails: " << msg << endl;
	pl_error_handler(error, ss.str().c_str());
}

bool cpp__z3_mk_config()
{
	bool ret = false;
	try {
		z3Manager::get_manager().mk_config();
		ret = true;
	}
	catch (exception& e) { cpp__crash_handler(__func__, typeid(e).name(), e.what()); }
	return ret;
}
bool cpp__z3_del_config()
{
	bool ret = false;
	try {
		z3Manager::get_manager().del_config();
		ret = true;
	}
	catch (exception& e) { cpp__crash_handler(__func__, typeid(e).name(), e.what()); }
	return ret;
}

bool cpp__z3_set_param_value(String param, String value)
{
	bool ret = false;
	try {
		z3Manager::get_manager().set_param_value(param, value);
		ret = true;
	}
	catch (exception& e) { cpp__crash_handler(__func__, typeid(e).name(), e.what()); }
	return ret;
}

bool cpp__z3_mk_context(int *context_id)
{
	bool ret = false;
	try {
		*context_id = z3Manager::get_manager().mk_context();
		ret = true;
	}
	catch (exception& e) { cpp__crash_handler(__func__, typeid(e).name(), e.what()); }
	return ret;
}
bool cpp__z3_del_context(int context_id)
{
	bool ret = false;
	try {
		z3Manager::get_manager().del_context(context_id);
		ret = true;
	}
	catch (exception& e) { cpp__crash_handler(__func__, typeid(e).name(), e.what()); }
	return ret;
}

bool cpp__z3_mk_solver(int context_id)
{
	bool ret = false;
	try {
		z3Manager::get_bundle(context_id).mk_solver();
		ret = true;
	}
	catch (exception& e) { cpp__crash_handler(__func__, typeid(e).name(), e.what()); }
	return ret;
}
bool cpp__z3_del_solver(int context_id)
{
	bool ret = false;
	try {
		z3Manager::get_bundle(context_id).del_solver();
		ret = true;
	}
	catch (exception& e) { cpp__crash_handler(__func__, typeid(e).name(), e.what()); }
	return ret;
}

bool cpp__z3_push(int context_id)
{
	bool ret = false;
	try {
		z3Manager::get_bundle(context_id).push();
		ret = true;
	}
	catch (exception& e) { cpp__crash_handler(__func__, typeid(e).name(), e.what()); }
	return ret;
}
bool cpp__z3_pop(int context_id)
{
	bool ret = false;
	try {
		z3Manager::get_bundle(context_id).pop();
		ret = true;
	}
	catch (exception& e) { cpp__crash_handler(__func__, typeid(e).name(), e.what()); }
	return ret;
}

bool cpp__z3_mk_int_var(int context_id, String var_name)
{
	bool ret = false;
	try {
		z3Manager::get_bundle(context_id).mk_int_var(var_name);
		ret = true;
	}
	catch (exception& e) { cpp__crash_handler(__func__, typeid(e).name(), e.what()); }
	return ret;
}
bool cpp__z3_mk_term_var(int context_id, String var_name)
{
	bool ret = false;
	try {
		z3Manager::get_bundle(context_id).mk_term_var(var_name);
		ret = true;
	}
	catch (exception& e) { cpp__crash_handler(__func__, typeid(e).name(), e.what()); }
	return ret;
}

bool cpp__z3_mk_term_type(int context_id, List<Functor> known_terms, bool need_int, bool need_lists)
{
	bool ret = false;
	try {
		z3Manager::get_bundle(context_id).mk_term_type(known_terms, need_int, need_lists);
		ret = true;
	}
	catch (exception& e) { cpp__crash_handler(__func__, typeid(e).name(), e.what()); }
	return ret;
}

bool cpp__z3_assert_int_string(int context_id, String assertion)
{
	bool ret = false;
	try {
		z3Manager::get_bundle(context_id).assert_int_string(assertion);
		ret = true;
	}
	catch (exception& e) { cpp__crash_handler(__func__, typeid(e).name(), e.what()); }
	return ret;
}
bool cpp__z3_assert_term_string(int context_id, String assertion, bool need_int, bool need_lists)
{
	bool ret = false;
	try {
		z3Manager::get_bundle(context_id).assert_term_string(assertion, need_int, need_lists);
		ret = true;
	}
	catch (z3Exception& e) {
		string error = typeid(e).name();
		string msg = string(e.msg()) + "\n"s + assertion + "\n"s;
		switch (e.error_code()) {
			case Z3_PARSER_ERROR:
				error += " -> parser error";
				//msg += assertion + "\n"s;
				break;
		}
		cpp__crash_handler(__func__, error.c_str(), msg.c_str());
	}
	catch (exception& e) { cpp__crash_handler(__func__, typeid(e).name(), e.what()); }
	return ret;
}

bool cpp__z3_check(int context_id)
{
	bool ret = false;
	try {
		ret = z3Manager::get_bundle(context_id).check();
	}
	catch (exception& e) { cpp__crash_handler(__func__, typeid(e).name(), e.what()); }
	return ret;
}

bool cpp__z3_print_model(int context_id, String *model)
{
	bool ret = false;
	try {
		*model = z3Manager::get_bundle(context_id).print_model();
		ret = true;
	}
	catch (exception& e) { cpp__crash_handler(__func__, typeid(e).name(), e.what()); }
	return ret;
}

bool cpp__z3_get_model_intvar_eval(int context_id, String var_name, Integer *var_value)
{
	bool ret = false;
	try {
		*var_value = z3Manager::get_bundle(context_id).get_model_intvar_eval(var_name);
		ret = true;
	}
	catch (exception& e) { cpp__crash_handler(__func__, typeid(e).name(), e.what()); }
	return ret;
}
bool cpp__z3_get_model_termvar_eval(int context_id, String var_name, Term *var_value)
{
	bool ret = false;
	try {
		*var_value = z3Manager::get_bundle(context_id).get_model_termvar_eval(var_name);
		ret = true;
	}
	catch (exception& e) { cpp__crash_handler(__func__, typeid(e).name(), e.what()); }
	return ret;
}

bool cpp__z3_mk_datatypes(int context_id, List<String> tpl_args, List<Datatype> datatypes)
{
	bool ret = false;
	try {
		z3Manager::get_bundle(context_id).mk_datatypes(tpl_args, datatypes);
		ret = true;
	}
	catch (exception& e) { cpp__crash_handler(__func__, typeid(e).name(), e.what()); }
	return ret;
}
bool cpp__z3_mk_var(int context_id, String var_name, String var_type)
{
	bool ret = false;
	try {
		z3Manager::get_bundle(context_id).mk_var(var_name, var_type);
		ret = true;
	}
	catch (exception& e) { cpp__crash_handler(__func__, typeid(e).name(), e.what()); }
	return ret;
}

/*void cpp__z3_error_handler(const char *error, const char *msg)
{
	//cerr << "\nAn exception occurred while calling " << (current_callname ? current_callname : "<anonymous>") << "\n";
	cerr << "Error: " << error << "\nDetails: " << msg << endl;
	return pl_error_handler(error, msg);
}*/