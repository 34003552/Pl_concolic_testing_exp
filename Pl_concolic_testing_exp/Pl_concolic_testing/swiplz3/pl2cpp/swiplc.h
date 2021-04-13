#include <SWI-Prolog.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
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
 Create a context with index context_id using the current configuration
 Enable tracing to stderr and register standard error handler.
 */
static foreign_t pl_mk_context(term_t context_id);

/**
 Create a solver associated to the context with index context_id
 */
static foreign_t pl_mk_solver(term_t context_id);

/**
 Delete a solver associated to the context with index context_id
 */
static foreign_t pl_del_solver(term_t context_id);

/**
 Delete a context with index context_id
 */
static foreign_t pl_del_context(term_t context_id);

/**
 Create a backtracking point in the context with index context_id
 */
static foreign_t pl_push(term_t context_id);

/**
 Backtrack one point in the context with index context_id
 */
static foreign_t pl_pop(term_t context_id);



static foreign_t pl_mk_int_vars(term_t context_id, term_t var_names);


/**
 Declares the Term datatype
 */
static foreign_t pl_mk_term_type(term_t context_id, term_t known_terms, term_t need_int, term_t need_lists);

static foreign_t pl_mk_term_vars(term_t context_id, term_t var_names);

/**
 Declares a new integer variable intvarname in context context_id
 */
static foreign_t pl_assert_int_string(term_t context_id, term_t assertion);

/**
 Declares a new term variable termvarname in context context_id
 */
static foreign_t pl_assert_term_string(term_t context_id, term_t assertion, term_t need_int, term_t need_lists);

/**
    Check the satisfiability of a context with index context_id
 */
static foreign_t pl_check(term_t context_id);

/**
 Show the computed model for a context context_id
 */
static foreign_t pl_print_model(term_t context_id, term_t model);

/**
 Show the computed model for an integer variable
 */
static foreign_t pl_get_model_intvar_eval(term_t context_id, term_t var_name, term_t var_value);

/**
 Show the computed model for a term variable
 */
static foreign_t pl_get_model_termvar_eval(term_t context_id, term_t var_name, term_t var_value);


static foreign_t pl_mk_datatypes(term_t context_id, term_t tpl_args, term_t datatypes);

static foreign_t pl_mk_vars(term_t context_id, term_t var_names, term_t var_type);


install_t install();

term_t mk_term(Term v);

/**
 Gets the functor from the Prolog pair: (functor, arity)
 */
Functor get_term_functor(term_t functor);

void pl_error_handler(const char *error, const char *msg);
