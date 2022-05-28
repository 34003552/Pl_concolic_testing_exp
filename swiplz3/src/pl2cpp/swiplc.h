#ifndef SWIPLC_H
#define SWIPLC_H

#include <SWI-Prolog.h>
#include "ctypes.h"

/**
 Create a configuration
 */
static foreign_t pl_mk_config();

/**
 Delete a configuration
 */
static foreign_t pl_del_config();

/**
 Set a configuration parameter
 */
static foreign_t pl_set_param_value(term_t param, term_t value);

/**
 Create a context using the current configuration
 Enable tracing to stderr and register standard error handler
 */
static foreign_t pl_mk_context(term_t context_id);

/**
 Create a solver in a context
 */
static foreign_t pl_mk_solver(term_t context_id);

/**
 Delete a solver of a context
 */
static foreign_t pl_del_solver(term_t context_id);

/**
 Delete a context
 */
static foreign_t pl_del_context(term_t context_id);

/**
 Create a backtracking point in a context
 */
static foreign_t pl_push(term_t context_id);

/**
 Backtrack one point in a context
 */
static foreign_t pl_pop(term_t context_id);

/**
 Declare datatypes in a context
 */
static foreign_t pl_mk_datatypes(term_t context_id, term_t tpl_vars, term_t datatypes);

/**
 Declare a sort in a context
 */
static foreign_t pl_mk_sort(term_t context_id, term_t sort_name, term_t base_name);

/**
 Declare multiple variables which have the same type in a context
 */
static foreign_t pl_mk_vars(term_t context_id, term_t var_names, term_t var_type);

/**
 Declare/define a function in a context
 */
static foreign_t pl_mk_func(term_t context_id, term_t func_name, term_t func_args, term_t ret_type, term_t func_body, term_t recursive);

/**
 Assert a formula in a context
 */
static foreign_t pl_assert_string(term_t context_id, term_t assertion);

/**
 Check the satisfiability of a context
 */
static foreign_t pl_check_sat(term_t context_id, term_t result);

/**
 Show the computed model for a context
 */
static foreign_t pl_get_model_to_string(term_t context_id, term_t model);

/**
 Extract a variable from the computed model for a context
 */
static foreign_t pl_eval_model_var(term_t context_id, term_t var_name, term_t var_value);


install_t install();
install_t uninstall();

term_t mk_term(Term v);

/**
 Get the functor from the Prolog pair (functor, arity)
 */
Functor get_term_functor(term_t functor);

int pl_error_handler(const char *funcname, const char *error, const char *msg);

#endif