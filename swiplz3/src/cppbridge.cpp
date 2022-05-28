#include "cppbridge.h"

#include "z3/z3Tools.h"

using namespace std;

extern "C" void pl_iolog__init();
CppBridge::CppBridge() :
m_z3manager() {
    pl_iolog__init();
}

extern "C" void pl_iolog__del();
CppBridge::~CppBridge() {
    pl_iolog__del();
}

CppBridge CppBridge::instance;


extern "C" int pl_error_handler(const char *funcname, const char *error, const char *msg);
int (*cpp__crash_handler)(const char *funcname, const char *error, const char *msg) = pl_error_handler;

#define __np_func__ (__func__+5) // function name without the 5 characters prefix

bool cpp__z3_mk_config() {
    bool ret = false;
    try {
        CppBridge::instance.m_z3manager.mk_config();
        ret = true;
    }
    catch (exception& e) { cpp__crash_handler(__np_func__, typeid(e).name(), e.what()); }
    return ret;
}
bool cpp__z3_del_config() {
    bool ret = false;
    try {
        CppBridge::instance.m_z3manager.del_config();
        ret = true;
    }
    catch (exception& e) { cpp__crash_handler(__np_func__, typeid(e).name(), e.what()); }
    return ret;
}

bool cpp__z3_set_param_value(pl::String param, pl::String value) {
    bool ret = false;
    try {
        CppBridge::instance.m_z3manager.set_param_value(param, value);
        ret = true;
    }
    catch (exception& e) { cpp__crash_handler(__np_func__, typeid(e).name(), e.what()); }
    return ret;
}

bool cpp__z3_mk_context(int *context_id) {
    bool ret = false;
    try {
        *context_id = CppBridge::instance.m_z3manager.mk_context();
        ret = true;
    }
    catch (exception& e) { cpp__crash_handler(__np_func__, typeid(e).name(), e.what()); }
    return ret;
}
bool cpp__z3_del_context(int context_id) {
    bool ret = false;
    try {
        CppBridge::instance.m_z3manager.del_context(context_id);
        ret = true;
    }
    catch (exception& e) { cpp__crash_handler(__np_func__, typeid(e).name(), e.what()); }
    return ret;
}

bool cpp__z3_mk_solver(int context_id) {
    bool ret = false;
    try {
        CppBridge::instance.m_z3manager.get_bundle(context_id).mk_solver();
        ret = true;
    }
    catch (exception& e) { cpp__crash_handler(__np_func__, typeid(e).name(), e.what()); }
    return ret;
}
bool cpp__z3_del_solver(int context_id) {
    bool ret = false;
    try {
        CppBridge::instance.m_z3manager.get_bundle(context_id).del_solver();
        ret = true;
    }
    catch (exception& e) { cpp__crash_handler(__np_func__, typeid(e).name(), e.what()); }
    return ret;
}

bool cpp__z3_push(int context_id) {
    bool ret = false;
    try {
        CppBridge::instance.m_z3manager.get_bundle(context_id).push();
        ret = true;
    }
    catch (exception& e) { cpp__crash_handler(__np_func__, typeid(e).name(), e.what()); }
    return ret;
}
bool cpp__z3_pop(int context_id) {
    bool ret = false;
    try {
        CppBridge::instance.m_z3manager.get_bundle(context_id).pop();
        ret = true;
    }
    catch (exception& e) { cpp__crash_handler(__np_func__, typeid(e).name(), e.what()); }
    return ret;
}

bool cpp__z3_mk_datatypes(int context_id, pl::List<pl::String> tpl_vars, pl::List<pl::Datatype> datatypes) {
    bool ret = false;
    try {
        CppBridge::instance.m_z3manager.get_bundle(context_id).mk_datatypes(tpl_vars, datatypes);
        ret = true;
    }
    catch (exception& e) { cpp__crash_handler(__np_func__, typeid(e).name(), e.what()); }
    return ret;
}
bool cpp__z3_mk_sort(int context_id, pl::String sort_name, pl::String base_name) {
    bool ret = false;
    try {
        CppBridge::instance.m_z3manager.get_bundle(context_id).mk_sort(sort_name, base_name);
        ret = true;
    }
    catch (exception& e) { cpp__crash_handler(__np_func__, typeid(e).name(), e.what()); }
    return ret;
}

bool cpp__z3_mk_var(int context_id, pl::String var_name, pl::String var_type) {
    bool ret = false;
    try {
        CppBridge::instance.m_z3manager.get_bundle(context_id).mk_var(var_name, var_type);
        ret = true;
    }
    catch (exception& e) { cpp__crash_handler(__np_func__, typeid(e).name(), e.what()); }
    return ret;
}

bool cpp__z3_mk_func(int context_id, pl::String func_name, pl::List<pl::String> func_args, pl::String ret_type, pl::String func_body, bool recursive) {
    bool ret = false;
    try {
        CppBridge::instance.m_z3manager.get_bundle(context_id).mk_func(func_name, func_args, ret_type, func_body, recursive);
        ret = true;
    }
    catch (exception& e) { cpp__crash_handler(__np_func__, typeid(e).name(), e.what()); }
    return ret;
}

bool cpp__z3_assert_string(int context_id, pl::String assertion) {
    bool ret = false;
    try {
        CppBridge::instance.m_z3manager.get_bundle(context_id).assert_string(assertion);
        ret = true;
    }
    catch (z3Exception& e) {
        string error = typeid(e).name();
        string msg = string(e.msg());
        switch (e.error_code()) {
            case Z3_PARSER_ERROR:
                error += "::PARSER_ERROR";
                //msg += " in "s + assertion;
                break;
        }
        cpp__crash_handler(__np_func__, error.c_str(), msg.c_str());
    }
    catch (exception& e) { cpp__crash_handler(__np_func__, typeid(e).name(), e.what()); }
    return ret;
}

bool cpp__z3_check_sat(int context_id, pl::atom *result) {
    bool ret = false;
    try {
        *result = CppBridge::instance.m_z3manager.get_bundle(context_id).check_sat();
        ret = true;
    }
    catch (exception& e) { cpp__crash_handler(__np_func__, typeid(e).name(), e.what()); }
    return ret;
}

bool cpp__z3_get_model_to_string(int context_id, pl::String *model) {
    bool ret = false;
    try {
        *model = CppBridge::instance.m_z3manager.get_bundle(context_id).get_model_to_string();
        ret = true;
    }
    catch (exception& e) { cpp__crash_handler(__np_func__, typeid(e).name(), e.what()); }
    return ret;
}

bool cpp__z3_eval_model_var(int context_id, pl::String var_name, pl::Term *var_value) {
    bool ret = false;
    try {
        *var_value = CppBridge::instance.m_z3manager.get_bundle(context_id).eval_model_var(var_name);
        ret = true;
    }
    catch (exception& e) { cpp__crash_handler(__np_func__, typeid(e).name(), e.what()); }
    return ret;
}