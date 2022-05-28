#include "swiplc.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "iolog.h"

/***************************************************/
/*         registered SWI Prolog predicates        */
/***************************************************/

static PL_extension predicates[] = {
    { "z3_mk_config",               0, pl_mk_config,                0 },
    { "z3_del_config",              0, pl_del_config,               0 },
    { "z3_set_param_value",         2, pl_set_param_value,          0 },
    { "z3_mk_context",              1, pl_mk_context,               0 },
    { "z3_del_context",             1, pl_del_context,              0 },
    { "z3_mk_solver",               1, pl_mk_solver,                0 },
    { "z3_del_solver",              1, pl_del_solver,               0 },
    { "z3_push",                    1, pl_push,                     0 },
    { "z3_pop",                     1, pl_pop,                      0 },
    { "z3_mk_datatypes",            3, pl_mk_datatypes,             0 },
    { "z3_mk_sort",                 3, pl_mk_sort,                  0 },
    { "z3_mk_vars",                 3, pl_mk_vars,                  0 },
    { "z3_mk_func",                 6, pl_mk_func,                  0 },
    { "z3_assert_string_",          2, pl_assert_string,            0 },
    { "z3_check_sat",               2, pl_check_sat,                0 },
    { "z3_get_model_to_string",     2, pl_get_model_to_string,      0 },
    { "z3_eval_model_var",          3, pl_eval_model_var,           0 },
    
    { NULL,                         0, NULL,                        0 }
};
enum pred_index {
    PRED_Z3_MK_CONFIG, PRED_Z3_DEL_CONFIG,
    PRED_Z3_SET_PARAM_VALUE,
    PRED_Z3_MK_CONTEXT, PRED_Z3_DEL_CONTEXT,
    PRED_Z3_MK_SOLVER, PRED_Z3_DEL_SOLVER,
    PRED_Z3_PUSH, PRED_Z3_POP,
    PRED_Z3_MK_DATATYPES, PRED_Z3_MK_SORT,
    PRED_Z3_MK_VARS,
    PRED_Z3_MK_FUNC,
    PRED_Z3_ASSERT_STRING, 
    PRED_Z3_CHECK_SAT,
    PRED_Z3_GET_MODEL_TO_STRING,
    PRED_Z3_EVAL_MODEL_VAR
};
#define PL_IOLOG_PRINT_PRED(PI) pl_iolog__PRINT_PRED(predicates[PI].predicate_name, predicates[PI].arity)
#define INSTANTIATION_FAULT(PI, ARG) PL_warning("%s/%d: instantiation fault (%s)", predicates[PI].predicate_name, predicates[PI].arity, ARG)

extern bool cpp__z3_mk_config();
static foreign_t pl_mk_config() {
    const int pi = PRED_Z3_MK_CONFIG;
    PL_IOLOG_PRINT_PRED(pi);

    bool ret = cpp__z3_mk_config();

    pl_iolog__PRINT_PRED_RET(ret);

    return ret;
}

extern bool cpp__z3_del_config();
static foreign_t pl_del_config() {
    const int pi = PRED_Z3_DEL_CONFIG;
    PL_IOLOG_PRINT_PRED(pi);

    bool ret = cpp__z3_del_config();

    pl_iolog__PRINT_PRED_RET(ret);

    return ret;
}

extern bool cpp__z3_set_param_value(String param, String value);
static foreign_t pl_set_param_value(term_t param, term_t value) {
    const int pi = PRED_Z3_SET_PARAM_VALUE;
    PL_IOLOG_PRINT_PRED(pi);

    String par;
    if (!PL_get_chars(param, (char**)&par, CVT_STRING)) {
        return INSTANTIATION_FAULT(pi, "param");
    }
    pl_iolog__PRINT_PRED_ARG(IOLOG_IN, "param", &par);

    String val;
    if (!PL_get_chars(value, (char**)&val, CVT_STRING)) {
        return INSTANTIATION_FAULT(pi, "value");
    }
    pl_iolog__PRINT_PRED_ARG(IOLOG_IN, "value", &val);

    bool ret = cpp__z3_set_param_value(par, val);

    pl_iolog__PRINT_PRED_RET(ret);

    return ret;
}

extern bool cpp__z3_mk_context(int *context_id);
static foreign_t pl_mk_context(term_t context_id) {
    const int pi = PRED_Z3_MK_CONTEXT;
    PL_IOLOG_PRINT_PRED(pi);

    int ctx_id;
    bool ret = cpp__z3_mk_context(&ctx_id);

    pl_iolog__PRINT_PRED_ARG(IOLOG_OUT, "context_id", &ctx_id);

    ret &= PL_unify_integer(context_id, ctx_id);

    pl_iolog__PRINT_PRED_RET(ret);

    return ret;
}

extern bool cpp__z3_del_context(int context_id);
static foreign_t pl_del_context(term_t context_id) {
    const int pi = PRED_Z3_DEL_CONTEXT;
    PL_IOLOG_PRINT_PRED(pi);

    int ctx_id;
    if (!PL_get_integer(context_id, &ctx_id)) {
        return INSTANTIATION_FAULT(pi, "context_id");
    }
    pl_iolog__PRINT_PRED_ARG(IOLOG_IN, "context_id", &ctx_id);

    bool ret = cpp__z3_del_context(ctx_id);
    
    pl_iolog__PRINT_PRED_RET(ret);

    return ret;
}

extern bool cpp__z3_mk_solver(int context_id);
static foreign_t pl_mk_solver(term_t context_id) {
    const int pi = PRED_Z3_MK_SOLVER;
    PL_IOLOG_PRINT_PRED(pi);

    int ctx_id;
    if (!PL_get_integer(context_id, &ctx_id)) {
        return INSTANTIATION_FAULT(pi, "context_id");
    }
    pl_iolog__PRINT_PRED_ARG(IOLOG_IN, "context_id", &ctx_id);

    bool ret = cpp__z3_mk_solver(ctx_id);

    pl_iolog__PRINT_PRED_RET(ret);

    return ret;
}

extern bool cpp__z3_del_solver(int context_id);
static foreign_t pl_del_solver(term_t context_id) {
    const int pi = PRED_Z3_DEL_SOLVER;
    PL_IOLOG_PRINT_PRED(pi);

    int ctx_id;
    if (!PL_get_integer(context_id, &ctx_id)) {
        return INSTANTIATION_FAULT(pi, "context_id");
    }
    pl_iolog__PRINT_PRED_ARG(IOLOG_IN, "context_id", &ctx_id);

    bool ret = cpp__z3_del_solver(ctx_id);

    pl_iolog__PRINT_PRED_RET(ret);

    return ret;
}

extern bool cpp__z3_push(int context_id);
static foreign_t pl_push(term_t context_id) {
    const int pi = PRED_Z3_PUSH;
    PL_IOLOG_PRINT_PRED(pi);

    int ctx_id;
    if (!PL_get_integer(context_id, &ctx_id)) {
        return INSTANTIATION_FAULT(pi, "context_id");
    }
    pl_iolog__PRINT_PRED_ARG(IOLOG_IN, "context_id", &ctx_id);

    bool ret = cpp__z3_push(ctx_id);

    pl_iolog__PRINT_PRED_RET(ret);

    return ret;
}

extern bool cpp__z3_pop(int context_id);
static foreign_t pl_pop(term_t context_id) {
    const int pi = PRED_Z3_POP;
    PL_IOLOG_PRINT_PRED(pi);

    int ctx_id;
    if (!PL_get_integer(context_id, &ctx_id)) {
        return INSTANTIATION_FAULT(pi, "context_id");
    }
    pl_iolog__PRINT_PRED_ARG(IOLOG_IN, "context_id", &ctx_id);

    bool ret = cpp__z3_pop(ctx_id);

    pl_iolog__PRINT_PRED_RET(ret);

    return ret;
}

extern bool cpp__z3_mk_datatypes(int context_id, StringList tpl_vars, DatatypeList datatypes);
static foreign_t pl_mk_datatypes(term_t context_id, term_t tpl_vars, term_t datatypes) {
    const int pi = PRED_Z3_MK_DATATYPES;
    PL_IOLOG_PRINT_PRED(pi);

    int ctx_id;
    if (!PL_get_integer(context_id, &ctx_id)) {
        return INSTANTIATION_FAULT(pi, "context_id");
    }
    pl_iolog__PRINT_PRED_ARG(IOLOG_IN, "context_id", &ctx_id);

    term_t tp = PL_new_term_ref();
    term_t tplist = PL_copy_term_ref(tpl_vars);

    StringList tpl = StringList__nil;
    StringListIterator tpli = StringList__begin(&tpl);
    while (PL_get_list(tplist, tp, tplist)) {
        String tpname;
        if (!PL_get_chars(tp, (char**)&tpname, CVT_STRING)) {
            return INSTANTIATION_FAULT(pi, "tpl_vars");
        }
        tpname = strdup(tpname);
        StringList__insert(&tpl, &tpli, tpname);
    }
    pl_iolog__PRINT_PRED_ARG(IOLOG_IN, "tpl_vars", &tpl);

    int dummy;

    term_t dt = PL_new_term_ref();
    term_t dtlist = PL_copy_term_ref(datatypes);

    DatatypeList dtl = DatatypeList__nil;
    DatatypeListIterator dtli = DatatypeList__begin(&dtl);
    while (PL_get_list(dtlist, dt, dtlist)) {
        term_t dtname_ = PL_new_term_ref();
        term_t ctlist = PL_new_term_ref();

        dummy = PL_get_list(dt, dtname_, ctlist);

        String dtname;
        if (!PL_get_chars(dtname_, (char**)&dtname, CVT_STRING)) {
            return INSTANTIATION_FAULT(pi, "datatypes::name");
        }
        dtname = strdup(dtname);
        //printf("%s \n", dtname);

        term_t ct = PL_new_term_ref();

        Datatype__ConstructorList ctl = Datatype__ConstructorList__nil;
        Datatype__ConstructorListIterator ctli = Datatype__ConstructorList__begin(&ctl);
        while (PL_get_list(ctlist, ct, ctlist)) {
            term_t ctname_ = PL_new_term_ref();
            term_t arglist = PL_new_term_ref();

            if (!PL_is_list(ct)) ctname_ = ct;
            else dummy = PL_get_list(ct, ctname_, arglist);

            String ctname;
            if (!PL_get_chars(ctname_, (char**)&ctname, CVT_STRING)) {
                return INSTANTIATION_FAULT(pi, "datatypes::ctors::name");
            }
            ctname = strdup(ctname);
            
            if (PL_is_variable(arglist)) {
                //printf("\t%s \n", ctname);

                Datatype__ConstructorList__insert(&ctl, &ctli, (Datatype__Constructor) { .name = ctname, .accs = 0 });
            } else {
                //printf("\t%s(", ctname);

                term_t arg = PL_new_term_ref();

                Datatype__Constructor__AccessorList acl = Datatype__Constructor__AccessorList__nil;
                Datatype__Constructor__AccessorListIterator acli = Datatype__Constructor__AccessorList__begin(&acl);
                while (PL_get_list(arglist, arg, arglist)) {
                    term_t argname_ = PL_new_term_ref();
                    term_t argtype_ = PL_new_term_ref();
                    dummy = PL_get_head(arg, argname_);
                    dummy = PL_get_tail(arg, arg);
                    dummy = PL_get_head(arg, argtype_);

                    String argname;
                    if (!PL_get_chars(argname_, (char**)&argname, CVT_STRING)) {
                        return INSTANTIATION_FAULT(pi, "datatypes::ctors::accs::name");
                    }
                    argname = strdup(argname);

                    String argtype;
                    if (!PL_get_chars(argtype_, (char**)&argtype, CVT_STRING)) {
                        return INSTANTIATION_FAULT(pi, "datatypes::ctors::accs::type");
                    }
                    argtype = strdup(argtype);

                    //printf("%s: %s", argname, argtype);
                    //if (!PL_get_nil(arglist)) printf(", ");

                    Datatype__Constructor__AccessorList__insert(&acl, &acli, (Datatype__Constructor__Accessor) { .name = argname, .type = argtype });
                }

                //printf(") \n");

                Datatype__ConstructorList__insert(&ctl, &ctli, (Datatype__Constructor) { .name = ctname, .accs = acl });
            }
        }
        DatatypeList__insert(&dtl, &dtli, (Datatype) { .name = dtname, .ctors = ctl });
    }
    //printf("\n");

    //printf("%s\n", DatatypeList__to_chars(&dtl));

    pl_iolog__PRINT_PRED_ARG(IOLOG_IN, "datatypes", &dtl);

    bool ret = cpp__z3_mk_datatypes(ctx_id, tpl, dtl);

    pl_iolog__PRINT_PRED_RET(ret);

    StringList__clear(&tpl);
    DatatypeList__clear(&dtl);

    return ret;
}

extern bool cpp__z3_mk_sort(int context_id, String sort_name, String base_name);
static foreign_t pl_mk_sort(term_t context_id, term_t sort_name, term_t base_name) {
    const int pi = PRED_Z3_MK_SORT;
    PL_IOLOG_PRINT_PRED(pi);

    int ctx_id;
    if (!PL_get_integer(context_id, &ctx_id)) {
        return INSTANTIATION_FAULT(pi, "context_id");
    }
    pl_iolog__PRINT_PRED_ARG(IOLOG_IN, "context_id", &ctx_id);

    String sortname;
    if (!PL_get_chars(sort_name, (char**)&sortname, CVT_STRING)) {
        return INSTANTIATION_FAULT(pi, "sort_name");
    }
    pl_iolog__PRINT_PRED_ARG(IOLOG_IN, "sort_name", &sortname);

    String basename;
    if (!PL_get_chars(base_name, (char**)&basename, CVT_STRING)) {
        return INSTANTIATION_FAULT(pi, "base_name");
    }
    pl_iolog__PRINT_PRED_ARG(IOLOG_IN, "base_name", &basename);

    bool ret = cpp__z3_mk_sort(ctx_id, sortname, basename);

    pl_iolog__PRINT_PRED_RET(ret);

    return ret;
}

extern bool cpp__z3_mk_var(int context_id, String var_name, String var_type);
static foreign_t pl_mk_vars(term_t context_id, term_t var_names, term_t var_type) {
    const int pi = PRED_Z3_MK_VARS;
    PL_IOLOG_PRINT_PRED(pi);

    int ctx_id;
    if (!PL_get_integer(context_id, &ctx_id)) {
        return INSTANTIATION_FAULT(pi, "context_id");
    }
    pl_iolog__PRINT_PRED_ARG(IOLOG_IN, "context_id", &ctx_id);

    String vartype;
    if (!PL_get_chars(var_type, (char**)&vartype, CVT_STRING)) {
        return INSTANTIATION_FAULT(pi, "var_type");
    }
    vartype = strdup(vartype); // prevents a possible replacement on the stack
    
    bool ret = true;

    StringList varnames = StringList__nil;
    StringListIterator vni = StringList__begin(&varnames);

    term_t vname = PL_new_term_ref();
    term_t vlist = PL_copy_term_ref(var_names);
    while (PL_get_list(vlist, vname, vlist)) {
        String varname;
        if (!PL_get_chars(vname, (char**)&varname, CVT_STRING)) {
            return INSTANTIATION_FAULT(pi, "var_names");
        }
        varname = strdup(varname);
        StringList__insert(&varnames, &vni, varname);

        ret &= cpp__z3_mk_var(ctx_id, varname, vartype);
    }
    ret &= PL_get_nil(vlist);
    pl_iolog__PRINT_PRED_ARG(IOLOG_IN, "var_names", &varnames);
    pl_iolog__PRINT_PRED_ARG(IOLOG_IN, "var_type", &vartype);

    pl_iolog__PRINT_PRED_RET(ret);

    StringList__clear(&varnames);
    String__drain(&vartype);

    return ret;
}

extern bool cpp__z3_mk_func(int context_id, String func_name, StringList func_args, String ret_type, String func_body, bool recursive);
static foreign_t pl_mk_func(term_t context_id, term_t func_name, term_t func_args, term_t ret_type, term_t func_body, term_t recursive) {
    const int pi = PRED_Z3_MK_FUNC;
    PL_IOLOG_PRINT_PRED(pi);

    int ctx_id;
    if (!PL_get_integer(context_id, &ctx_id)) {
        return INSTANTIATION_FAULT(pi, "context_id");
    }
    pl_iolog__PRINT_PRED_ARG(IOLOG_IN, "context_id", &ctx_id);

    String fname;
    if (!PL_get_chars(func_name, (char**)&fname, CVT_STRING)) {
        return INSTANTIATION_FAULT(pi, "func_name");
    }
    pl_iolog__PRINT_PRED_ARG(IOLOG_IN, "func_name", &fname);

    StringList fargs = StringList__nil;
    StringListIterator fai = StringList__begin(&fargs);

    term_t farg = PL_new_term_ref();
    term_t falist = PL_copy_term_ref(func_args);
    while (PL_get_list(falist, farg, falist)) {
        String func_arg;
        if (!PL_get_chars(farg, (char**)&func_arg, CVT_STRING)) {
            return INSTANTIATION_FAULT(pi, "func_args");
        }
        func_arg = strdup(func_arg);
        StringList__insert(&fargs, &fai, func_arg);
    }
    pl_iolog__PRINT_PRED_ARG(IOLOG_IN, "func_args", &fargs);

    String fret;
    if (!PL_get_chars(ret_type, (char**)&fret, CVT_STRING)) {
        return INSTANTIATION_FAULT(pi, "ret_type");
    }
    pl_iolog__PRINT_PRED_ARG(IOLOG_IN, "ret_type", &fret);

    String fbody;
    if (!PL_get_chars(func_body, (char**)&fbody, CVT_STRING)) {
        return INSTANTIATION_FAULT(pi, "func_body");
    }
    pl_iolog__PRINT_PRED_ARG(IOLOG_IN, "func_body", &fbody);

    bool rec;
    if (!PL_get_bool(recursive, (int*)&rec)) {
        return INSTANTIATION_FAULT(pi, "recursive");
    }
    pl_iolog__PRINT_PRED_ARG(IOLOG_IN, "recursive", &rec);

    bool ret = cpp__z3_mk_func(ctx_id, fname, fargs, fret, fbody, rec);
    
    pl_iolog__PRINT_PRED_RET(ret);

    StringList__clear(&fargs);

    return ret;
}

extern bool cpp__z3_assert_string(int context_id, String assertion);
static foreign_t pl_assert_string(term_t context_id, term_t assertion) {
    const int pi = PRED_Z3_ASSERT_STRING;
    PL_IOLOG_PRINT_PRED(pi);

    int ctx_id;
    if (!PL_get_integer(context_id, &ctx_id)) {
        return INSTANTIATION_FAULT(pi, "context_id");
    }
    pl_iolog__PRINT_PRED_ARG(IOLOG_IN, "context_id", &ctx_id);

    String a;
    if (!PL_get_chars(assertion, (char**)&a, CVT_STRING)) {
        return INSTANTIATION_FAULT(pi, "assertion");
    }
    pl_iolog__PRINT_PRED_ARG(IOLOG_IN, "assertion", &a);

    bool ret = cpp__z3_assert_string(ctx_id, a);
    
    pl_iolog__PRINT_PRED_RET(ret);

    return ret;
}

extern bool cpp__z3_check_sat(int context_id, atom *result);
static foreign_t pl_check_sat(term_t context_id, term_t result) {
    const int pi = PRED_Z3_CHECK_SAT;
    PL_IOLOG_PRINT_PRED(pi);

    int ctx_id;
    if (!PL_get_integer(context_id, &ctx_id)) {
        return INSTANTIATION_FAULT(pi, "context_id");
    }
    pl_iolog__PRINT_PRED_ARG(IOLOG_IN, "context_id", &ctx_id);

    atom res;
    bool ret = cpp__z3_check_sat(ctx_id, &res);

    pl_iolog__PRINT_PRED_ARG(IOLOG_OUT, "result", &res);

    ret &= PL_unify_atom_chars(result, res.name);

    pl_iolog__PRINT_PRED_RET(ret);

    return ret;
}

extern bool cpp__z3_get_model_to_string(int context_id, String *model);
static foreign_t pl_get_model_to_string(term_t context_id, term_t model) {
    const int pi = PRED_Z3_GET_MODEL_TO_STRING;
    PL_IOLOG_PRINT_PRED(pi);

    int ctx_id;
    if (!PL_get_integer(context_id, &ctx_id)) {
        return INSTANTIATION_FAULT(pi, "context_id");
    }
    pl_iolog__PRINT_PRED_ARG(IOLOG_IN, "context_id", &ctx_id);

    String mdl;
    bool ret = cpp__z3_get_model_to_string(ctx_id, &mdl);

    pl_iolog__PRINT_PRED_ARG(IOLOG_OUT, "model", &mdl);

    ret &= PL_unify_string_chars(model, mdl);

    pl_iolog__PRINT_PRED_RET(ret);

    return ret;
}

extern bool cpp__z3_eval_model_var(int context_id, String var_name, Term *var_value);
static foreign_t pl_eval_model_var(term_t context_id, term_t var_name, term_t var_value) {
    const int pi = PRED_Z3_EVAL_MODEL_VAR;
    PL_IOLOG_PRINT_PRED(pi);

    int ctx_id;
    if (!PL_get_integer(context_id, &ctx_id)) {
        return INSTANTIATION_FAULT(pi, "context_id");
    }
    pl_iolog__PRINT_PRED_ARG(IOLOG_IN, "context_id", &ctx_id);

    String varname;
    if (!PL_get_chars(var_name, (char**)&varname, CVT_STRING)) {
        return INSTANTIATION_FAULT(pi, "var_name");
    }
    pl_iolog__PRINT_PRED_ARG(IOLOG_IN, "var_name", &varname);

    Term v;
    bool ret = cpp__z3_eval_model_var(ctx_id, varname, &v);

    pl_iolog__PRINT_PRED_ARG(IOLOG_OUT, "var_value", &v);

    term_t varvalue = mk_term(v); //res ? mk_term(v) : PL_new_term_ref();
    ret &= PL_unify(var_value, varvalue);

    pl_iolog__PRINT_PRED_RET(ret);

    Term__drain(&v);
    
    return ret;
}

install_t install() {
    PL_register_extensions(predicates);
}
install_t uninstall() {}


term_t mk_term(Term v) {
    //printf("%s\n", Term__to_chars(&v));

    Functor f = v.functor;

    int dummy;

    atom_t fname = strcmp(f.name, "[]") == 0 ? ATOM_nil : PL_new_atom(f.name);
    functor_t functor = PL_new_functor(fname, f.arity);
    term_t term = PL_new_term_ref();

    if (f.arity == 0) {
        dummy = PL_cons_functor(term, functor);
    } else {
        term_t args = PL_new_term_refs(f.arity);
        TermListIterator tlit = TermList__begin(&v.args);
        for (unsigned j = 0; j < f.arity; ++j) {
            Term arg = *tlit.m_current->m_elem;
            PL_put_term(args + j, mk_term(arg));
            tlit = (TermListIterator) { .m_current = tlit.m_current->m_next };
        }

        dummy = PL_cons_functor_v(term, functor, args);
    }

    return term;
}

Functor get_term_functor(term_t t) {
    int dummy;

    char *name;
    int arity;

    term_t n = PL_new_term_ref();
    dummy = PL_get_arg(1, t, n);
    dummy = PL_get_chars(n, &name, CVT_ALL);

    term_t a = PL_new_term_ref();
    dummy = PL_get_arg(2, t, a);
    dummy = PL_get_integer(a, &arity);

    return (Functor) { .name = name, .arity = arity };
}

int pl_error_handler(const char* funcname, const char *error, const char *msg) {
    pl_iolog__printf("\t...\n\tthrew swiplz3_error(\"%s\", \"%s\", \"%s\")\n", funcname, error, msg);

    int dummy;

    static predicate_t p = 0;
    if (!p) p = PL_predicate("throw", 1, "system");

    atom_t atom = PL_new_atom("swiplz3_error");
    functor_t functor = PL_new_functor(atom, 3);

    term_t a0 = PL_new_term_ref();
    dummy = PL_put_string_chars(a0, funcname);
    term_t a1 = PL_new_term_ref();
    dummy = PL_put_string_chars(a1, error);
    term_t a2 = PL_new_term_ref();
    dummy = PL_put_string_chars(a2, msg);

    term_t f = PL_new_term_ref();
    dummy = PL_cons_functor(f, functor, a0, a1, a2);
    int res = PL_call_predicate(NULL, PL_Q_PASS_EXCEPTION, p, f);
    if (!res) {
        term_t exc;
        if ((exc=PL_exception(0))) res = PL_throw(exc);
    }
    return res;
}