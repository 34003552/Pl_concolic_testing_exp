#include "swiplc.h"
#include <stdio.h>
#include <string.h>

#define IOLOG_MODE

#define BOOL_FMT(x) ((x) ? "true" : "false")

extern void cpp__z3_mk_config();
static foreign_t pl_mk_config()
{
    cpp__z3_mk_config();

    pl_iolog__printf("%s\n", __func__);

    PL_succeed;
}

extern void cpp__z3_del_config();
static foreign_t pl_del_config()
{
    cpp__z3_del_config();

    pl_iolog__printf("%s\n", __func__);

    PL_succeed;
}

extern void cpp__z3_set_param_value(const char *param_id, const char *param_value);
static foreign_t pl_set_param_value(term_t param, term_t value)
{
    char *par = NULL, *val = NULL;
    if (!PL_get_chars(param, &par, CVT_STRING))
        return PL_warning("z3_set_parameter_value/1: instantiation fault (parameter)");
    if (!PL_get_chars(value, &val, CVT_STRING))
        return PL_warning("z3_set_parameter_value/1: instantiation fault (value)");

    cpp__z3_set_param_value(par, val);

    pl_iolog__printf("%s\n", __func__);
    pl_iolog__printf("\tin param: string = \"%s\"\n", par);
    pl_iolog__printf("\tin value: string = \"%s\"\n", val);

    PL_succeed;
}

extern void cpp__z3_mk_context(int *index);
static foreign_t pl_mk_context(term_t ind)
{
    int i;
    cpp__z3_mk_context(&i);

    pl_iolog__printf("%s\n", __func__);
    pl_iolog__printf("\tout context_id: int = %d\n", i);

    return PL_unify_integer(ind, i);
}

extern void cpp__z3_mk_solver(int index);
static foreign_t pl_mk_solver(term_t ind)
{
    int i;
    if (!PL_get_integer(ind, &i))
        return PL_warning("z3_mk_solver/2: instantiation fault");

    cpp__z3_mk_solver(i);

    pl_iolog__printf("%s\n", __func__);
    pl_iolog__printf("\tin context_id: int = %d\n", i);

    PL_succeed;
}

extern void cpp__z3_del_solver(int index);
static foreign_t pl_del_solver(term_t ind)
{
    int i;
    if (!PL_get_integer(ind, &i))
        return PL_warning("z3_del_solver/2: instantiation fault");

    cpp__z3_del_solver(i);

    pl_iolog__printf("%s\n", __func__);
    pl_iolog__printf("\tin context_id: int = %d\n", i);

    PL_succeed;
}

extern void cpp__z3_del_context(int index);
static foreign_t pl_del_context(term_t ind)
{
    int i;
    if (!PL_get_integer(ind, &i))
        return PL_warning("z3_del_context/1: instantiation fault");

    cpp__z3_del_context(i);

    pl_iolog__printf("%s\n", __func__);
    pl_iolog__printf("\tin context_id: int = %d\n", i);

    PL_succeed;
}

extern void cpp__z3_push(int index);
static foreign_t pl_push(term_t ind)
{
    int i;
    if (!PL_get_integer(ind, &i))
        return PL_warning("z3_push/1: instantiation fault");

    cpp__z3_push(i);

    pl_iolog__printf("%s\n", __func__);
    pl_iolog__printf("\tin context_id: int = %d\n", i);

    PL_succeed;
}

extern void cpp__z3_pop(int index);
static foreign_t pl_pop(term_t ind)
{
    int i;
    if (!PL_get_integer(ind, &i))
        return PL_warning("z3_pop/1: instantiation fault");

    cpp__z3_pop(i);

    pl_iolog__printf("%s\n", __func__);
    pl_iolog__printf("\tin context_id: int = %d\n", i);

    PL_succeed;
}

extern void cpp__z3_mk_int_var(int index, const char *varname);
static foreign_t pl_mk_int_vars(term_t ind, term_t varlist)
{
    int i;
    if (!PL_get_integer(ind, &i))
        return PL_warning("z3_parse_string/2: instantiation fault (context)");

    term_t plvar = PL_new_term_ref();   // the elements
    term_t list = PL_copy_term_ref(varlist); // copy (we modify list)

    char *varnames = NULL;
    while (PL_get_list(list, plvar, list))
    {
        char *varname = NULL;
        if (!PL_get_chars(plvar, &varname, CVT_STRING))
            return PL_warning("z3_mk_int_vars/2: instantiation fault");

        cpp__z3_mk_int_var(i, varname);

        if (varnames == NULL)
        {
            varnames = malloc(strlen(varname) + 1); // null-terminated
            strcpy(varnames, varname);
        }
        else
        {
            varnames = realloc(varnames, strlen(varnames) + 2 + strlen(varname) + 1); // null-terminated
            strcat(varnames, ", ");
            strcat(varnames, varname);
        }
    }

    pl_iolog__printf("%s\n", __func__);
    pl_iolog__printf("\tin context_id: int = %d\n", i);
    pl_iolog__printf("\tin var_names: string[] = { %s }\n", varnames);

    free(varnames);

    return PL_get_nil(list);
}

extern void cpp__z3_mk_term_type(int index, void *tlist, int need_int, int need_lists);
static foreign_t pl_mk_term_type(term_t ind, term_t termlist, term_t exists_integers, term_t exists_lists)
{
    int i, need_int, need_lists;
    if (!PL_get_integer(ind, &i))
        return PL_warning("z3_parse_string/2: instantiation fault (context)");
    if (!PL_get_bool(exists_integers, &need_int))
        return PL_warning("z3_parse_string/2: instantiation fault (exists_integers)");
    if (!PL_get_bool(exists_lists, &need_lists))
        return PL_warning("z3_parse_string/2: instantiation fault (exists_lists)");

    typedef struct TList TList;
    struct TList
    {
        char *name;
        int arity;
        TList *next;
    };
    TList *tlist = NULL;
    TList *itlist;

    term_t head = PL_new_term_ref();
    term_t list = PL_copy_term_ref(termlist);

    while (PL_get_list(list, head, list))
    {
        if (tlist == NULL)
        {
            tlist = malloc(sizeof(TList));
            itlist = tlist;
        }
        else
        {
            itlist->next = malloc(sizeof(TList));
            itlist = itlist->next;
        }
        itlist->name = get_term_name(head);
        itlist->arity = get_term_arity(head);
        itlist->next = NULL;
    }

    cpp__z3_mk_term_type(i, tlist, need_int, need_lists);

    char *terms = NULL;
    TList *ttlist;
    for (itlist = tlist; itlist != NULL; itlist = ttlist)
    {
        if (terms == NULL)
        {
            terms = malloc(strlen(itlist->name) + 1 + 2 + 1); // null-terminated
            sprintf(terms, "%s/%d", itlist->name, itlist->arity);
        }
        else
        {
            terms = realloc(terms, strlen(terms) + 2 + strlen(itlist->name) + 1 + 2 + 1); // null-terminated
            strcat(terms, ", ");
            sprintf(terms+strlen(terms), "%s/%d", itlist->name, itlist->arity);
        }

        ttlist = itlist->next;
        free(itlist);
    }

    pl_iolog__printf("%s\n", __func__);
    pl_iolog__printf("\tin context_id: int = %d\n", i);
    pl_iolog__printf("\tin known_terms: term[] = { %s }\n", terms);
    pl_iolog__printf("\tin need_int: bool = %s\n", BOOL_FMT(need_int));
    pl_iolog__printf("\tin need_lists: bool = %s\n", BOOL_FMT(need_lists));

    free(terms);

    return PL_get_nil(list);
}

extern void cpp__z3_mk_term_var(int index, const char *varname);
static foreign_t pl_mk_term_vars(term_t ind, term_t varlist)
{
    int i;
    if (!PL_get_integer(ind, &i))
        return PL_warning("z3_parse_string/2: instantiation fault (context)");

    term_t plvar = PL_new_term_ref();   // the elements
    term_t list = PL_copy_term_ref(varlist); // copy (we modify list)

    char *varnames = NULL;
    while (PL_get_list(list, plvar, list))
    {
        char *varname = NULL;
        if (!PL_get_chars(plvar, &varname, CVT_STRING))
            return PL_warning("z3_mk_pred_vars/2: instantiation fault");

        cpp__z3_mk_term_var(i, varname);

        if (varnames == NULL)
        {
            varnames = malloc(1 + strlen(varname) + 1 + 1); // null-terminated
            sprintf(varnames, "\"%s\"", varname);
        }
        else
        {
            varnames = realloc(varnames, strlen(varnames) + 2 + 1 + strlen(varname) + 1 + 1); // null-terminated
            sprintf(varnames + strlen(varnames), ", \"%s\"", varname);
        }
    }

    pl_iolog__printf("%s\n", __func__);
    pl_iolog__printf("\tin context_id: int = %d\n", i);
    pl_iolog__printf("\tin var_names: string[] = { %s }\n", varnames);

    free(varnames);

    return PL_get_nil(list);
}

extern int cpp__z3_assert_int_string(int index, const char *z3string);
static foreign_t pl_assert_int_string(term_t ind, term_t plstr)
{
    int i;
    char *z3string = NULL;
    if (!PL_get_integer(ind, &i))
        return PL_warning("z3_assert_int_string/2: instantiation fault (context)");
    if (!PL_get_chars(plstr, &z3string, CVT_STRING))
        return PL_warning("z3_assert_int_string/2: instantiation fault (string)");

    int ret = cpp__z3_assert_int_string(i, z3string);

    pl_iolog__printf("%s\n", __func__);
    pl_iolog__printf("\tin context_id: int = %d\n", i);
    pl_iolog__printf("\tin assertion: string = \"%s\"\n", z3string);

    if(ret == 0) PL_fail;
    else PL_succeed;
}

extern int cpp__z3_assert_term_string(int index, const char *z3string, int need_int, int need_lists);
static foreign_t pl_assert_term_string(term_t ind, term_t plstr, term_t exists_integers, term_t exists_lists)
{
    int i, need_int, need_lists;
    char *z3string = NULL;
    if (!PL_get_integer(ind, &i))
        return PL_warning("z3_parse_string/2: instantiation fault (context)");
    if (!PL_get_bool(exists_integers, &need_int))
        return PL_warning("z3_parse_string/2: instantiation fault (exists_integers)");
    if (!PL_get_bool(exists_lists, &need_lists))
        return PL_warning("z3_parse_string/2: instantiation fault (exists_lists)");
    if (!PL_get_chars(plstr, &z3string, CVT_STRING))
        return PL_warning("z3_assert_term_string/2: instantiation fault (string)");

    int ret = cpp__z3_assert_term_string(i, z3string, need_int, need_lists);

    pl_iolog__printf("%s\n", __func__);
    pl_iolog__printf("\tin context_id: int = %d\n", i);
    pl_iolog__printf("\tin assertion: string = \"%s\"\n", z3string);
    pl_iolog__printf("\tin need_int: bool = %s\n", BOOL_FMT(need_int));
    pl_iolog__printf("\tin need_lists: bool = %s\n", BOOL_FMT(need_lists));

    if(ret == 0) PL_fail;
    else PL_succeed;
}

extern int cpp__z3_check(int index);
static foreign_t pl_check(term_t ind)
{
    int i;
    if (!PL_get_integer(ind, &i))
        return PL_warning("z3_check/1: instantiation fault");

    int ret = cpp__z3_check(i);

    pl_iolog__printf("%s\n", __func__);
    pl_iolog__printf("\tin context_id: int = %d\n", i);

    if(ret == 0) PL_fail;
    else PL_succeed;
}

extern void cpp__z3_print_model(int index, char **str);
static foreign_t pl_print_model(term_t ind, term_t t)
{
    int i;
    if (!PL_get_integer(ind, &i))
        return PL_warning("z3_get_model/1: instantiation fault");

    char *str = NULL;
    cpp__z3_print_model(i, &str);

    pl_iolog__printf("%s\n", __func__);
    pl_iolog__printf("\tin context_id: int = %d\n", i);
    pl_iolog__printf("\tout model: string = \"%s\"\n", strndup(str, strlen(str) - 1));

    return PL_unify_string_chars(t, str);
}

extern int cpp__z3_get_model_intvar_eval(int index, const char *vn, int *val);
static foreign_t pl_get_model_intvar_eval(term_t ind, term_t varname, term_t varval)
{
    int i;
    char *vn = NULL;
    if (!PL_get_integer(ind, &i))
        return PL_warning("z3_get_model_intvar_eval/3: instantiation fault (context)");
    if (!PL_get_chars(varname, &vn, CVT_STRING))
        return PL_warning("z3_get_model_intvar_eval/3: instantiation fault (varname)");

    int val;
    cpp__z3_get_model_intvar_eval(i, vn, &val);

    pl_iolog__printf("%s\n", __func__);
    pl_iolog__printf("\tin context_id: int = %d\n", i);
    pl_iolog__printf("\tin var_name: string = \"%s\"\n", varname);
    pl_iolog__printf("\tout var_value: int = %d\n", val);

    return PL_unify_integer(varval, val);
}

extern int cpp__z3_get_model_termvar_eval(int index, const char *vn, void **v);
static foreign_t pl_get_model_termvar_eval(term_t ind, term_t varname, term_t varval)
{
    int i;
    char *vn = NULL;
    if (!PL_get_integer(ind, &i))
        return PL_warning("z3_get_model_intvar_eval/3: instantiation fault (context)");
    if (!PL_get_chars(varname, &vn, CVT_STRING))
        return PL_warning("z3_get_model_intvar_eval/3: instantiation fault (varname)");

    void *v;
    int res = cpp__z3_get_model_termvar_eval(i, vn, &v);

    term_t val;
    
    if (res) val = mk_term(i, v);
    else val = PL_new_term_ref();

    char *tval = NULL;
    int dummy = PL_get_chars(val, &tval, CVT_WRITE | CVT_EXCEPTION);

    pl_iolog__printf("%s\n", __func__);
    pl_iolog__printf("\tin context_id: int = %d\n", i);
    pl_iolog__printf("\tin var_name: string = \"%s\"\n", vn);
    pl_iolog__printf("\tout var_value: term = %s\n", tval);
    
    return PL_unify(varval, val);
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
}

extern void cpp__z3_mk_term__get_app(int index, void *v, void **app);
extern void cpp__z3_mk_term__get_functor(int index, void *app, char **name, int *arity);
extern void cpp__z3_mk_term__get_app_arg(int index, void *app, int j, void **app_arg);
term_t mk_term(int index, void *v)
{
    void *app;
    char *name;
    int arity;

    cpp__z3_mk_term__get_app(index, v, &app);
    cpp__z3_mk_term__get_functor(index, app, &name, &arity);

    functor_t functor = PL_new_functor(PL_new_atom(name), arity);
    term_t term = PL_new_term_ref();

    int dummy;
    if (arity == 0)
        dummy = PL_cons_functor(term, functor);
    else
    {
        term_t args = PL_new_term_refs(arity);
        for (unsigned j = 0; j < arity; ++j)
        {
            void *arg;
            cpp__z3_mk_term__get_app_arg(index, app, j, &arg);
            PL_put_term(args + j, mk_term(index, arg));
        }

        dummy = PL_cons_functor_v(term, functor, args);
    }

    return term;
}

char *get_term_name(term_t functor)
{
    term_t a = PL_new_term_ref();

    char *termstr;

    int dummy;
    dummy = PL_get_arg(1, functor, a);
    dummy = PL_get_chars(a, &termstr, CVT_ALL);

    return termstr;
}

int get_term_arity(term_t functor)
{
    term_t a = PL_new_term_ref();

    char *s;

    int dummy;
    dummy = PL_get_arg(2, functor, a);
    dummy = PL_get_chars(a, &s, CVT_ALL);

    return atoi(s);
}

void pl_error_handler(const char *error)
{
    term_t t1 = PL_new_functor(PL_new_atom(error), 0);
    PL_raise_exception(t1);
}

#ifndef IOLOG_MODE
void pl_iolog__init() {}
void pl_iolog__printf(const char *fmt, ...) {}
void pl_iolog__del() {}
#else
#include <time.h>
#include <stdarg.h>
void pl_iolog__init()
{
    iolog_fd = fopen("swiplz3.log", "w");
    fprintf(iolog_fd, "---------------------------------------\n");
    fprintf(iolog_fd, "swiplz3 I/O logfile\n");
    fprintf(iolog_fd, "---------------------------------------\n");

    time_t now; time(&now);
    fprintf(iolog_fd, "Execution started on %s", ctime(&now));
}
void pl_iolog__printf(const char *fmt, ...)
{
    va_list args;
    va_start(args, fmt);
    vfprintf(iolog_fd, fmt, args);
    va_end(args);
}
void pl_iolog__del()
{
    time_t now; time(&now);
    fprintf(iolog_fd, "Execution stopped on %s", ctime(&now));
    fclose(iolog_fd);
}
#endif