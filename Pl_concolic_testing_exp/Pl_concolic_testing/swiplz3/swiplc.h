#include <SWI-Prolog.h>
#include <stdio.h>

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
 Create a context with index ind using the current configuration
 Enable tracing to stderr and register standard error handler.
 */
static foreign_t pl_mk_context(term_t ind);

/**
 Create a solver associated to the context with index ind
 */
static foreign_t pl_mk_solver(term_t ind);

/**
 Delete a solver associated to the context with index ind
 */
static foreign_t pl_del_solver(term_t ind);

/**
 Delete a context with index ind
 */
static foreign_t pl_del_context(term_t ind);

/**
 Create a backtracking point in the context with index ind
 */
static foreign_t pl_push(term_t ind);

/**
 Backtrack one point in the context with index ind
 */
static foreign_t pl_pop(term_t ind);



static foreign_t pl_mk_int_vars(term_t ind, term_t varlist);


/**
 Declares the Term datatype
 */
static foreign_t pl_mk_term_type(term_t ind, term_t termlist, term_t exists_integers, term_t exists_lists);

static foreign_t pl_mk_term_vars(term_t ind, term_t varlist);

/**
 Declares a new integer variable intvarname in context ind
 */
static foreign_t pl_assert_int_string(term_t ind, term_t plstr);

/**
 Declares a new term variable termvarname in context ind
 */
static foreign_t pl_assert_term_string(term_t ind, term_t plstr, term_t exists_integers, term_t exists_lists);

/**
    Check the satisfiability of a context with index ind
 */
static foreign_t pl_check(term_t ind);

/**
 Show the computed model for a context ind
 */
static foreign_t pl_print_model(term_t ind, term_t t);

/**
 Show the computed model for an integer variable
 */
static foreign_t pl_get_model_intvar_eval(term_t ind, term_t varname, term_t varval);

/**
 Show the computed model for a term variable
 */
static foreign_t pl_get_model_termvar_eval(term_t ind, term_t varname, term_t varval);

install_t install();



term_t mk_term(int index, void *v);
/**
 Gets the name of the functor from the Prolog pair: (functor, arity)
 */
char* get_term_name(term_t functor);

/**
 Gets the arity of the functor from the Prolog pair: (functor, arity)
 */
int get_term_arity(term_t functor);


void pl_error_handler(const char *error);

FILE *iolog_fd;
void pl_iolog__init();
void pl_iolog__printf(const char *fmt, ...);
void pl_iolog__del();