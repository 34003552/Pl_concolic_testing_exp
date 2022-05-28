#include "z3FuncDeclHolder.h"

using namespace std;

z3FuncDeclHolder::z3FuncDeclHolder(z3::context *ctx) : m_ctx(ctx) {}

void z3FuncDeclHolder::add_func(z3::func_decl f) {
    m_funcs.push_back(f);
}

z3::expr z3FuncDeclHolder::eval_const(const string& const_name, const z3::model& m) const {
    for (auto& func : m_funcs) {
        if (func.is_const() && func.name().str() == const_name) return m.eval(func(), true);
    }
    throw runtime_error(const_name + " is a missing constant!");
}

void z3FuncDeclHolder::fill_func_decl_vector(z3::func_decl_vector& fvec) const {
    for (auto& func : m_funcs) {
        fvec.push_back(func);
    }
}

void z3FuncDeclHolder::backup_scope() {
    m_funcs.push_scope();
}
void z3FuncDeclHolder::restore_scope(unsigned s) {
    m_funcs.pop_scope(s);
}