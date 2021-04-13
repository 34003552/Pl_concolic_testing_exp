#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <memory.h>
#include <setjmp.h>
#include <z3.h>

#include "z3Tools.h"

z3Exception::z3Exception(Z3_error_code ec, const char *msg) : z3::exception(msg), m_ec(ec) {}
Z3_error_code z3Exception::error_code() const { return m_ec; }

void exitf(const char *message)
{
    fprintf(stderr,"BUG: %s.\n", message);
    exit(1);
}

void unreachable()
{
    exitf("unreachable code was reached");
}

char *itoa(int val, int base)
{
    static char buffer[32] = {0};
    snprintf(buffer, base, "%d", val);
    return buffer;
}

int array_sum(const int *array, int size)
{
    int i;
    int result = 0;

    for(i = 0; i < size; i++)
        result += array[i];

    return result;
}

void z3Tools::display_symbol(Z3_context c, FILE *out, Z3_symbol s)
{
    switch (Z3_get_symbol_kind(c, s)) {
        case Z3_INT_SYMBOL:
            fprintf(out, "#%d", Z3_get_symbol_int(c, s));
            break;
        case Z3_STRING_SYMBOL:
            fprintf(out, "%s", Z3_get_symbol_string(c, s));
            break;
        default:
            unreachable();
    }
}

void z3Tools::display_sort(Z3_context c, FILE *out, Z3_sort ty)
{
    switch (Z3_get_sort_kind(c, ty)) {
        case Z3_UNINTERPRETED_SORT:
            display_symbol(c, out, Z3_get_sort_name(c, ty));
            break;
        case Z3_BOOL_SORT:
            fprintf(out, "bool");
            break;
        case Z3_INT_SORT:
            fprintf(out, "int");
            break;
        case Z3_REAL_SORT:
            fprintf(out, "real");
            break;
        case Z3_BV_SORT:
            fprintf(out, "bv%d", Z3_get_bv_sort_size(c, ty));
            break;
        case Z3_ARRAY_SORT:
            fprintf(out, "[");
            display_sort(c, out, Z3_get_array_sort_domain(c, ty));
            fprintf(out, "->");
            display_sort(c, out, Z3_get_array_sort_range(c, ty));
            fprintf(out, "]");
            break;
        case Z3_DATATYPE_SORT:
   	        if (Z3_get_datatype_sort_num_constructors(c, ty) != 1)
            {
                fprintf(out, "%s", Z3_sort_to_string(c,ty));
                break;
            }
            {
                unsigned num_fields = Z3_get_tuple_sort_num_fields(c, ty);
                unsigned i;
                fprintf(out, "(");
                for (i = 0; i < num_fields; i++)
                {
                    Z3_func_decl field = Z3_get_tuple_sort_field_decl(c, ty, i);
                    if (i > 0) fprintf(out, ", ");
                    display_sort(c, out, Z3_get_range(c, field));
                }
                fprintf(out, ")");
                break;
            }
        default:
            fprintf(out, "unknown[");
            display_symbol(c, out, Z3_get_sort_name(c, ty));
            fprintf(out, "]");
            break;
    }
}

void z3Tools::display_ast(Z3_context c, FILE *out, Z3_ast v)
{
    switch (Z3_get_ast_kind(c, v)) {
        case Z3_NUMERAL_AST: {
            Z3_sort t;
            fprintf(out, "%s", Z3_get_numeral_string(c, v));
            t = Z3_get_sort(c, v);
            fprintf(out, ":");
            display_sort(c, out, t);
            break;
        }
        case Z3_APP_AST: {
            unsigned i;
            Z3_app app = Z3_to_app(c, v);
            unsigned num_fields = Z3_get_app_num_args(c, app);
            Z3_func_decl d = Z3_get_app_decl(c, app);
            fprintf(out, "%s", Z3_func_decl_to_string(c, d));
            if (num_fields > 0)
            {
                fprintf(out, "[");
                for (i = 0; i < num_fields; i++)
                {
                    if (i > 0) fprintf(out, ", ");
                    display_ast(c, out, Z3_get_app_arg(c, app, i));
                }
                fprintf(out, "]");
            }
            break;
        }
        case Z3_QUANTIFIER_AST: {
            fprintf(out, "quantifier");
            ;
        }
        default:
            fprintf(out, "#unknown");
    }
}

void z3Tools::display_function_interpretations(Z3_context c, FILE *out, Z3_model m)
{
    unsigned num_functions, i;

    fprintf(out, "function interpretations:\n");

    num_functions = Z3_model_get_num_funcs(c, m);
    for (i = 0; i < num_functions; i++)
    {
        Z3_func_decl fdecl;
        Z3_symbol name;
        Z3_ast func_else;
        unsigned num_entries = 0, j;
        Z3_func_interp_opt finterp;

        fdecl = Z3_model_get_func_decl(c, m, i);
        finterp = Z3_model_get_func_interp(c, m, fdecl);
        Z3_func_interp_inc_ref(c, finterp);
        name = Z3_get_decl_name(c, fdecl);
        display_symbol(c, out, name);
        fprintf(out, " = {");
        if (finterp)
        num_entries = Z3_func_interp_get_num_entries(c, finterp);
        for (j = 0; j < num_entries; j++)
        {
            unsigned num_args, k;
            Z3_func_entry fentry = Z3_func_interp_get_entry(c, finterp, j);
            Z3_func_entry_inc_ref(c, fentry);

            if (j > 0) fprintf(out, ", ");

            num_args = Z3_func_entry_get_num_args(c, fentry);
            fprintf(out, "(");
            for (k = 0; k < num_args; k++)
            {
                if (k > 0) fprintf(out, ", ");
                display_ast(c, out, Z3_func_entry_get_arg(c, fentry, k));
            }
            fprintf(out, "|->");
            display_ast(c, out, Z3_func_entry_get_value(c, fentry));
            fprintf(out, ")");
            Z3_func_entry_dec_ref(c, fentry);
        }
        if (num_entries > 0) fprintf(out, ", ");

        fprintf(out, "(else|->");
        func_else = Z3_func_interp_get_else(c, finterp);
        display_ast(c, out, func_else);
        fprintf(out, ")}\n");
        Z3_func_interp_dec_ref(c, finterp);
    }
}

void z3Tools::display_model(Z3_context c, FILE *out, Z3_model m)
{
    unsigned num_constants;
    unsigned i;

    if (!m) return;

    num_constants = Z3_model_get_num_consts(c, m);
    for (i = 0; i < num_constants; i++)
    {
        Z3_symbol name;
        Z3_func_decl cnst = Z3_model_get_const_decl(c, m, i);
        Z3_ast a, v;
        Z3_bool ok;
        name = Z3_get_decl_name(c, cnst);
        display_symbol(c, out, name);
        fprintf(out, " = ");
        a = Z3_mk_app(c, cnst, 0, 0);
        v = a;
        ok = Z3_model_eval(c, m, a, 1, &v);
        display_ast(c, out, v);
        fprintf(out, "\n");
    }
    display_function_interpretations(c, out, m);
}

Z3_ast z3Tools::mk_var(Z3_context ctx, const char *name, Z3_sort ty)
{
    Z3_symbol s  = Z3_mk_string_symbol(ctx, name);
    return Z3_mk_const(ctx, s, ty);
}

Z3_ast z3Tools::mk_int_var(Z3_context ctx, const char *name)
{
    Z3_sort ty = Z3_mk_int_sort(ctx);
    return mk_var(ctx, name, ty);
}

Z3_ast z3Tools::mk_real_var(Z3_context ctx, const char *name)
{
    Z3_sort ty = Z3_mk_real_sort(ctx);
    return mk_var(ctx, name, ty);
}