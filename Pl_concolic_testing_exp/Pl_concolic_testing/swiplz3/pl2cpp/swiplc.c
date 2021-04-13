#include "swiplc.h"
#include "iolog.h"

extern bool cpp__z3_mk_config();
static foreign_t pl_mk_config()
{
    bool ret = cpp__z3_mk_config();

    pl_iolog__PRINT_PREDICATE(ret);

    return ret; //PL_succeed;
}

extern bool cpp__z3_del_config();
static foreign_t pl_del_config()
{
    bool ret = cpp__z3_del_config();

    pl_iolog__PRINT_PREDICATE(ret);

    return ret; //PL_succeed;
}

extern bool cpp__z3_set_param_value(String param, String value);
static foreign_t pl_set_param_value(term_t param, term_t value)
{
    String par, val;
    if (!PL_get_chars(param, (char**)&par, CVT_STRING))
        return PL_warning("z3_set_param_value/2: instantiation fault (param)");
    if (!PL_get_chars(value, (char**)&val, CVT_STRING))
        return PL_warning("z3_set_param_value/2: instantiation fault (value)");

    bool ret = cpp__z3_set_param_value(par, val);

    pl_iolog__PRINT_PREDICATE(ret);
    pl_iolog__PRINT_PRED_ARG(IOLOG_IN, "param", &par);
    pl_iolog__PRINT_PRED_ARG(IOLOG_IN, "value", &val);

    return ret; //PL_succeed;
}

extern bool cpp__z3_mk_context(int *context_id);
static foreign_t pl_mk_context(term_t context_id)
{
    int ctx_id;

    bool ret = cpp__z3_mk_context(&ctx_id);
    ret &= PL_unify_integer(context_id, ctx_id);

    pl_iolog__PRINT_PREDICATE(ret);
    pl_iolog__PRINT_PRED_ARG(IOLOG_OUT, "context_id", &ctx_id);

    return ret;
}

extern bool cpp__z3_mk_solver(int context_id);
static foreign_t pl_mk_solver(term_t context_id)
{
    int ctx_id;
    if (!PL_get_integer(context_id, &ctx_id))
        return PL_warning("z3_mk_solver/1: instantiation fault (context_id)");

    bool ret = cpp__z3_mk_solver(ctx_id);

    pl_iolog__PRINT_PREDICATE(ret);
    pl_iolog__PRINT_PRED_ARG(IOLOG_IN, "context_id", &ctx_id);

    return ret; //PL_succeed;
}

extern bool cpp__z3_del_solver(int context_id);
static foreign_t pl_del_solver(term_t context_id)
{
    int ctx_id;
    if (!PL_get_integer(context_id, &ctx_id))
        return PL_warning("z3_del_solver/1: instantiation fault (context_id)");

    bool ret = cpp__z3_del_solver(ctx_id);

    pl_iolog__PRINT_PREDICATE(ret);
    pl_iolog__PRINT_PRED_ARG(IOLOG_IN, "context_id", &ctx_id);

    return ret; //PL_succeed;
}

extern bool cpp__z3_del_context(int context_id);
static foreign_t pl_del_context(term_t context_id)
{
    int ctx_id;
    if (!PL_get_integer(context_id, &ctx_id))
        return PL_warning("z3_del_context/1: instantiation fault (context_id)");

    bool ret = cpp__z3_del_context(ctx_id);

    pl_iolog__PRINT_PREDICATE(ret);
    pl_iolog__PRINT_PRED_ARG(IOLOG_IN, "context_id", &ctx_id);

    return ret; //PL_succeed;
}

extern bool cpp__z3_push(int context_id);
static foreign_t pl_push(term_t context_id)
{
    int ctx_id;
    if (!PL_get_integer(context_id, &ctx_id))
        return PL_warning("z3_push/1: instantiation fault (context_id)");

    bool ret = cpp__z3_push(ctx_id);

    pl_iolog__PRINT_PREDICATE(ret);
    pl_iolog__PRINT_PRED_ARG(IOLOG_IN, "context_id", &ctx_id);

    return ret; //PL_succeed;
}

extern bool cpp__z3_pop(int context_id);
static foreign_t pl_pop(term_t context_id)
{
    int ctx_id;
    if (!PL_get_integer(context_id, &ctx_id))
        return PL_warning("z3_pop/1: instantiation fault (context_id)");

    bool ret = cpp__z3_pop(ctx_id);

    pl_iolog__PRINT_PREDICATE(ret);
    pl_iolog__PRINT_PRED_ARG(IOLOG_IN, "context_id", &ctx_id);

    return ret; //PL_succeed;
}

extern bool cpp__z3_mk_int_var(int context_id, String var_name);
static foreign_t pl_mk_int_vars(term_t context_id, term_t var_names)
{
    int ctx_id;
    if (!PL_get_integer(context_id, &ctx_id))
        return PL_warning("z3_mk_int_vars/2: instantiation fault (context_id)");

    bool ret = true;

    StringList varnames = StringList__nil;
    StringListIterator vni = StringList__begin(&varnames);

    term_t vname = PL_new_term_ref();
    term_t vlist = PL_copy_term_ref(var_names);
    while (PL_get_list(vlist, vname, vlist))
    {
        String varname;
        if (!PL_get_chars(vname, (char**)&varname, CVT_STRING))
            return PL_warning("z3_mk_int_vars/2: instantiation fault (var_names)");

        StringList__insert(&varnames, &vni, varname);

        ret &= cpp__z3_mk_int_var(ctx_id, varname);
    }
    ret &= PL_get_nil(vlist);

    pl_iolog__PRINT_PREDICATE(ret);
    pl_iolog__PRINT_PRED_ARG(IOLOG_IN, "context_id", &ctx_id);
    pl_iolog__PRINT_PRED_ARG(IOLOG_IN, "var_names", &varnames);

    StringList__clear(&varnames);

    return ret;
}

extern bool cpp__z3_mk_term_type(int context_id, FunctorList known_terms, bool need_int, bool need_lists);
static foreign_t pl_mk_term_type(term_t context_id, term_t known_terms, term_t need_int, term_t need_lists)
{
    int ctx_id;
    bool use_int, use_lists;
    if (!PL_get_integer(context_id, &ctx_id))
        return PL_warning("z3_mk_term_type/4: instantiation fault (context_id)");
    if (!PL_get_bool(need_int, (int*)&use_int))
        return PL_warning("z3_mk_term_type/4: instantiation fault (need_int)");
    if (!PL_get_bool(need_lists, (int*)&use_lists))
        return PL_warning("z3_mk_term_type/4: instantiation fault (need_lists)");

    FunctorList fl = FunctorList__nil;
    FunctorListIterator fli = FunctorList__begin(&fl);//{ .m_current = tlist.m_first };

    term_t ftor = PL_new_term_ref();
    term_t flist = PL_copy_term_ref(known_terms);
    while (PL_get_list(flist, ftor, flist))
    {
        FunctorList__insert(&fl, &fli, get_term_functor(ftor));
    }

    bool ret = cpp__z3_mk_term_type(ctx_id, fl, use_int, use_lists);
    ret &= PL_get_nil(flist);

    pl_iolog__PRINT_PREDICATE(ret);
    pl_iolog__PRINT_PRED_ARG(IOLOG_IN, "context_id", &ctx_id);
    pl_iolog__PRINT_PRED_ARG(IOLOG_IN, "known_terms", &fl);
    pl_iolog__PRINT_PRED_ARG(IOLOG_IN, "need_int", &use_int);
    pl_iolog__PRINT_PRED_ARG(IOLOG_IN, "need_lists", &use_lists);

    FunctorList__clear(&fl);

    return ret;
}

extern bool cpp__z3_mk_term_var(int context_id, String var_name);
static foreign_t pl_mk_term_vars(term_t context_id, term_t var_names)
{
    int ctx_id;
    if (!PL_get_integer(context_id, &ctx_id))
        return PL_warning("z3_mk_term_vars/2: instantiation fault (context_id)");

    bool ret = true;

    StringList varnames = StringList__nil;
    StringListIterator vli = StringList__begin(&varnames);//{ .m_current = varnames.m_first };

    term_t vname = PL_new_term_ref();
    term_t vlist = PL_copy_term_ref(var_names);
    while (PL_get_list(vlist, vname, vlist))
    {
        String varname;
        if (!PL_get_chars(vname, (char**)&varname, CVT_STRING))
            return PL_warning("z3_mk_term_vars/2: instantiation fault (var_names)");

        StringList__insert(&varnames, &vli, varname);

        ret &= cpp__z3_mk_term_var(ctx_id, varname);
    }
    ret &= PL_get_nil(vlist);

    pl_iolog__PRINT_PREDICATE(ret);
    pl_iolog__PRINT_PRED_ARG(IOLOG_IN, "context_id", &ctx_id);
    pl_iolog__PRINT_PRED_ARG(IOLOG_IN, "var_names", &varnames);

    StringList__clear(&varnames);

    return ret;
}

extern bool cpp__z3_assert_int_string(int context_id, String assertion);
static foreign_t pl_assert_int_string(term_t context_id, term_t assertion)
{
    int ctx_id;
    String a;
    if (!PL_get_integer(context_id, &ctx_id))
        return PL_warning("z3_assert_int_string/2: instantiation fault (context_id)");
    if (!PL_get_chars(assertion, (char**)&a, CVT_STRING))
        return PL_warning("z3_assert_int_string/2: instantiation fault (assertion)");

    bool ret = cpp__z3_assert_int_string(ctx_id, a);

    pl_iolog__PRINT_PREDICATE(ret);
    pl_iolog__PRINT_PRED_ARG(IOLOG_IN, "context_id", &ctx_id);
    pl_iolog__PRINT_PRED_ARG(IOLOG_IN, "assertion", &a);

    return ret; //if(ret == false) PL_fail; else PL_succeed;
}

extern bool cpp__z3_assert_term_string(int context_id, String assertion, bool need_int, bool need_lists);
static foreign_t pl_assert_term_string(term_t context_id, term_t assertion, term_t need_int, term_t need_lists)
{
    int ctx_id;
    String a;
    bool use_int, use_lists;
    if (!PL_get_integer(context_id, &ctx_id))
        return PL_warning("z3_assert_term_string/4: instantiation fault (context_id)");
    if (!PL_get_chars(assertion, (char**)&a, CVT_STRING))
        return PL_warning("z3_assert_term_string/4: instantiation fault (assertion)");
    if (!PL_get_bool(need_int, (int*)&use_int))
        return PL_warning("z3_assert_term_string/4: instantiation fault (need_int)");
    if (!PL_get_bool(need_lists, (int*)&use_lists))
        return PL_warning("z3_assert_term_string/4: instantiation fault (need_lists)");

    bool ret = cpp__z3_assert_term_string(ctx_id, a, use_int, use_lists);

    pl_iolog__PRINT_PREDICATE(ret);
    pl_iolog__PRINT_PRED_ARG(IOLOG_IN, "context_id", &ctx_id);
    pl_iolog__PRINT_PRED_ARG(IOLOG_IN, "assertion", &a);
    pl_iolog__PRINT_PRED_ARG(IOLOG_IN, "need_int", &use_int);
    pl_iolog__PRINT_PRED_ARG(IOLOG_IN, "need_lists", &use_lists);

    return ret; //if(ret == false) PL_fail; else PL_succeed;
}

extern bool cpp__z3_check(int context_id);
static foreign_t pl_check(term_t context_id)
{
    int ctx_id;
    if (!PL_get_integer(context_id, &ctx_id))
        return PL_warning("z3_check/1: instantiation fault (context_id)");

    bool ret = cpp__z3_check(ctx_id);

    pl_iolog__PRINT_PREDICATE(ret);
    pl_iolog__PRINT_PRED_ARG(IOLOG_IN, "context_id", &ctx_id);

    return ret; //if(ret == 0) PL_fail; else PL_succeed;
}

extern bool cpp__z3_print_model(int context_id, String *model);
static foreign_t pl_print_model(term_t context_id, term_t model)
{
    int ctx_id;
    if (!PL_get_integer(context_id, &ctx_id))
        return PL_warning("z3_print_model/2: instantiation fault (context_id)");

    String mdl;
    bool ret = cpp__z3_print_model(ctx_id, &mdl);
    ret &= PL_unify_string_chars(model, mdl);

    pl_iolog__PRINT_PREDICATE(ret);
    pl_iolog__PRINT_PRED_ARG(IOLOG_IN, "context_id", &ctx_id);
    pl_iolog__PRINT_PRED_ARG(IOLOG_OUT, "model", &mdl);

    return ret;
}

extern bool cpp__z3_get_model_intvar_eval(int context_id, String var_name, Integer *var_value);
static foreign_t pl_get_model_intvar_eval(term_t context_id, term_t var_name, term_t var_value)
{
    int ctx_id;
    String varname;
    if (!PL_get_integer(context_id, &ctx_id))
        return PL_warning("z3_get_model_intvar_eval/3: instantiation fault (context_id)");
    if (!PL_get_chars(var_name, (char**)&varname, CVT_STRING))
        return PL_warning("z3_get_model_intvar_eval/3: instantiation fault (var_name)");

    Integer varvalue;
    bool ret = cpp__z3_get_model_intvar_eval(ctx_id, varname, &varvalue);

    term_t num = PL_new_term_ref();
    PL_chars_to_term(varvalue.value, num);
    ret &= PL_unify(var_value, num);

    pl_iolog__PRINT_PREDICATE(ret);
    pl_iolog__PRINT_PRED_ARG(IOLOG_IN, "context_id", &ctx_id);
    pl_iolog__PRINT_PRED_ARG(IOLOG_IN, "var_name", &varname);
    pl_iolog__PRINT_PRED_ARG(IOLOG_OUT, "var_value", &varvalue);

    return ret;
}

extern bool cpp__z3_get_model_termvar_eval(int context_id, String var_name, Term *var_value);
static foreign_t pl_get_model_termvar_eval(term_t context_id, term_t var_name, term_t var_value)
{
    int ctx_id;
    String varname;
    if (!PL_get_integer(context_id, &ctx_id))
        return PL_warning("z3_get_model_termvar_eval/3: instantiation fault (context_id)");
    if (!PL_get_chars(var_name, (char**)&varname, CVT_STRING))
        return PL_warning("z3_get_model_termvar_eval/3: instantiation fault (var_name)");

    Term v;
    bool ret = cpp__z3_get_model_termvar_eval(ctx_id, varname, &v); // int res = ...

    term_t varvalue = mk_term(v); //res ? mk_term(v) : PL_new_term_ref();
    ret &= PL_unify(var_value, varvalue);

    pl_iolog__PRINT_PREDICATE(ret);
    pl_iolog__PRINT_PRED_ARG(IOLOG_IN, "context_id", &ctx_id);
    pl_iolog__PRINT_PRED_ARG(IOLOG_IN, "var_name", &varname);
    pl_iolog__PRINT_PRED_ARG(IOLOG_OUT, "var_value", &v);
    
    return ret;
}

extern bool cpp__z3_mk_datatypes(int context_id, StringList tpl_args, DatatypeList datatypes);
static foreign_t pl_mk_datatypes(term_t context_id, term_t tpl_args, term_t datatypes)
{
    int ctx_id;
    if (!PL_get_integer(context_id, &ctx_id))
        return PL_warning("z3_mk_datatypes/3: instantiation fault (context_id)");

    StringList optlist = StringList__nil;

    int dummy;

    term_t dt = PL_new_term_ref();
    term_t dtlist = PL_copy_term_ref(datatypes);

    DatatypeList dtl = DatatypeList__nil;
    DatatypeListIterator dtli = DatatypeList__begin(&dtl);//{ .m_current = dtl.m_first };
    while (PL_get_list(dtlist, dt, dtlist))
    {
        term_t dtname_ = PL_new_term_ref();
        term_t ctlist = PL_new_term_ref();

        dummy = PL_get_list(dt, dtname_, ctlist);

        String dtnameT, dtname;
        if (!PL_get_chars(dtname_, (char**)&dtnameT, CVT_STRING))
            return PL_warning("z3_mk_datatypes/3: instantiation fault (datatypes::name)");
        dtname = strdup(dtnameT);
        //printf("%s \n", dtname);

        term_t ct = PL_new_term_ref();

        Datatype__ConstructorList ctl = Datatype__ConstructorList__nil;
        Datatype__ConstructorListIterator ctli = Datatype__ConstructorList__begin(&ctl);//{ .m_current = ctl.m_first };
        while (PL_get_list(ctlist, ct, ctlist))
        {
            term_t ctname_ = PL_new_term_ref();
            term_t arglist = PL_new_term_ref();

            if (!PL_is_list(ct))
                ctname_ = ct;
            else
                dummy = PL_get_list(ct, ctname_, arglist);

            String ctnameT, ctname;
            if (!PL_get_chars(ctname_, (char**)&ctnameT, CVT_STRING))
                return PL_warning("z3_mk_datatypes/3: instantiation fault (datatypes::ctors::name)");
            ctname = strdup(ctnameT);
            
            if (PL_is_variable(arglist)) {
                //printf("\t%s \n", ctname);

                Datatype__ConstructorList__insert(&ctl, &ctli, (Datatype__Constructor) { .name = ctname, .accs = 0 });
            }
            else
            {
                //printf("\t%s(", ctname);

                term_t arg = PL_new_term_ref();

                Datatype__Constructor__AccessorList acl = Datatype__Constructor__AccessorList__nil;
                Datatype__Constructor__AccessorListIterator acli = Datatype__Constructor__AccessorList__begin(&acl);//{ .m_current = acl.m_first };
                while (PL_get_list(arglist, arg, arglist))
                {
                    term_t argname_ = PL_new_term_ref();
                    term_t argtype_ = PL_new_term_ref();
                    dummy = PL_get_head(arg, argname_);
                    dummy = PL_get_tail(arg, arg);
                    dummy = PL_get_head(arg, argtype_);

                    String argnameT, argname;
                    if (!PL_get_chars(argname_, (char**)&argnameT, CVT_STRING))
                        return PL_warning("z3_mk_datatypes/3: instantiation fault (datatypes::ctors::accs::name)");
                    argname = strdup(argnameT);

                    String argtypeT, argtype;
                    if (!PL_get_chars(argtype_, (char**)&argtypeT, CVT_STRING))
                        return PL_warning("z3_mk_datatypes/3: instantiation fault (datatypes::ctors::accs::type)");
                    argtype = strdup(argtypeT);

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
    bool ret = cpp__z3_mk_datatypes(ctx_id, optlist, dtl);

    pl_iolog__PRINT_PREDICATE(ret);
    pl_iolog__PRINT_PRED_ARG(IOLOG_IN, "context_id", &ctx_id);
    pl_iolog__PRINT_PRED_ARG(IOLOG_IN, "tpl_args", &optlist);
    pl_iolog__PRINT_PRED_ARG(IOLOG_IN, "datatypes", &dtl);

    StringList__clear(&optlist);
    DatatypeList__clear(&dtl);

    return ret; //PL_succeed;
}

extern bool cpp__z3_mk_var(int context_id, String var_name, String var_type);
static foreign_t pl_mk_vars(term_t context_id, term_t var_names, term_t var_type)
{
    int ctx_id;
    if (!PL_get_integer(context_id, &ctx_id))
        return PL_warning("z3_mk_vars/2: instantiation fault (context_id)");

    String vartype;
    if (!PL_get_chars(var_type, (char**)&vartype, CVT_STRING))
        return PL_warning("z3_mk_vars/2: instantiation fault (var_type)");

    bool ret = true;

    StringList varnames = StringList__nil;
    StringListIterator vni = StringList__begin(&varnames);

    term_t vname = PL_new_term_ref();
    term_t vlist = PL_copy_term_ref(var_names);
    while (PL_get_list(vlist, vname, vlist))
    {
        String varname;
        if (!PL_get_chars(vname, (char**)&varname, CVT_STRING))
            return PL_warning("z3_mk_vars/2: instantiation fault (var_names)");

        StringList__insert(&varnames, &vni, varname);

        ret &= cpp__z3_mk_var(ctx_id, varname, vartype);
    }
    ret &= PL_get_nil(vlist);

    pl_iolog__PRINT_PREDICATE(ret);
    pl_iolog__PRINT_PRED_ARG(IOLOG_IN, "context_id", &ctx_id);
    pl_iolog__PRINT_PRED_ARG(IOLOG_IN, "var_names", &varnames);
    pl_iolog__PRINT_PRED_ARG(IOLOG_IN, "var_type", &vartype);

    StringList__clear(&varnames);

    return ret;
}

/***************************************************/
/*         registered SWI Prolog predicates        */
/***************************************************/

install_t install()
{
    PL_register_foreign("z3_mk_config", 0, pl_mk_config, 0);
    PL_register_foreign("z3_del_config", 0, pl_del_config, 0);
    PL_register_foreign("z3_set_param_value", 2, pl_set_param_value, 0);
    PL_register_foreign("z3_mk_new_context", 1, pl_mk_context, 0);
    PL_register_foreign("z3_del_context", 1, pl_del_context, 0);
    PL_register_foreign("z3_mk_solver", 1, pl_mk_solver, 0);
    PL_register_foreign("z3_del_solver", 1, pl_del_solver, 0);
    PL_register_foreign("z3_push", 1, pl_push, 0);
    PL_register_foreign("z3_pop", 1, pl_pop, 0);
    PL_register_foreign("z3_mk_int_vars", 2, pl_mk_int_vars, 0);
    PL_register_foreign("z3_mk_term_type", 4, pl_mk_term_type, 0);
    PL_register_foreign("z3_mk_term_vars", 2, pl_mk_term_vars, 0);
    PL_register_foreign("z3_assert_int_string_", 2, pl_assert_int_string, 0);
    PL_register_foreign("z3_assert_term_string_", 4, pl_assert_term_string, 0);
    PL_register_foreign("z3_check", 1, pl_check, 0);
    PL_register_foreign("z3_print_model", 2, pl_print_model, 0);
    PL_register_foreign("z3_get_model_intvar_eval", 3, pl_get_model_intvar_eval, 0);
    PL_register_foreign("z3_get_model_termvar_eval", 3, pl_get_model_termvar_eval, 0);

    PL_register_foreign("z3_mk_datatypes_", 3, pl_mk_datatypes, 0);
    PL_register_foreign("z3_mk_vars_", 3, pl_mk_vars, 0);
}


term_t mk_term(Term v)
{
    //printf("%s\n", Term__to_chars(&v));

    Functor f = v.functor;

    int dummy;

    functor_t functor = PL_new_functor(PL_new_atom(f.name), f.arity);
    term_t term = PL_new_term_ref();

    if (f.arity == 0)
        dummy = PL_cons_functor(term, functor);
    else
    {
        term_t args = PL_new_term_refs(f.arity);
        TermListIterator tlit = TermList__begin(&v.args);//{ .m_current = v->args.m_first };
        for (unsigned j = 0; j < f.arity; ++j)
        {
            Term arg = *tlit.m_current->m_elem;
            PL_put_term(args + j, mk_term(arg));
            tlit = (TermListIterator) { .m_current = tlit.m_current->m_next };
        }

        dummy = PL_cons_functor_v(term, functor, args);
    }

    return term;
}

Functor get_term_functor(term_t t)
{
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

void pl_error_handler(const char *error, const char *msg)
{
    fprintf(stderr, "\nThe swiplz3 module has crashed!\n");
    fputs(msg, stderr);
    term_t t1 = PL_new_functor(PL_new_atom(error), 0);
    PL_raise_exception(t1);
}