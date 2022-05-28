#ifndef Z3BUNDLE_H
#define Z3BUNDLE_H

#include <string>
#include <z3++.h>

#include "../pl2cpp/cpptypes.h"
#include "z3DatatypeHolder.h"
#include "z3FuncDeclHolder.h"

class z3Manager::z3Bundle {
    z3Manager *m_manager;

    z3::context *m_ctx;
    z3::solver *m_slv;

    z3DatatypeHolder m_dth;
    z3FuncDeclHolder m_fdh;

    static pl::Term Z3_expr_to_Term(z3::expr ex);
    z3::expr_vector parse_assertion(std::string assertion, z3::sort_vector* svec = nullptr, z3::func_decl_vector* fvec = nullptr) const;
public:
    z3Bundle(z3Manager *manager, z3::context *ctx);
    z3Bundle(const z3Bundle&) = delete; // not copyable...
    z3Bundle(z3Bundle&& other); // ... but movable
    z3Bundle& operator=(const z3Bundle&) = delete;
    z3Bundle& operator=(z3Bundle&& other);
    ~z3Bundle();

    void mk_solver();
    void del_solver();

    void push();
    void pop();

    void mk_datatypes(pl::List<pl::String> tpl_vars, pl::List<pl::Datatype> datatypes);
    void mk_sort(pl::String sort_name, pl::String base_name);

    void mk_var(pl::String var_name, pl::String var_type);
    void mk_func(pl::String func_name, pl::List<pl::String> func_args, pl::String ret_type, pl::String func_body, bool recursive);

    void assert_string(pl::String assertion);

    pl::atom check_sat() const;

    pl::String get_model_to_string() const;
    pl::Term eval_model_var(pl::String var_name) const;
};

#endif