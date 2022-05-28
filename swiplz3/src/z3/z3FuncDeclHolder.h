#ifndef Z3FUNCDECLHOLDER_H
#define Z3FUNCDECLHOLDER_H

#include <deque>
#include <string>
#include <z3++.h>

#include "../ScopedContainer.h"

class z3FuncDeclHolder {
    z3::context *m_ctx;

    ScopedContainer<std::deque<z3::func_decl>> m_funcs;
public:
    z3FuncDeclHolder(z3::context *ctx);

    void add_func(z3::func_decl f);
    
    z3::expr eval_const(const std::string& const_name, const z3::model& m) const;
    
    void fill_func_decl_vector(z3::func_decl_vector& fvec) const;

    void backup_scope();
    void restore_scope(unsigned scope);
};

#endif