
#ifndef Z3TOOLS_H
#define Z3TOOLS_H

#include <z3++.h>

class z3Exception : public z3::exception {
	Z3_error_code m_ec;
public:
	z3Exception(Z3_error_code ec, const char *msg);
	Z3_error_code error_code() const;
};

#define LOG_Z3_CALLS

#ifdef LOG_Z3_CALLS
#define LOG_MSG(msg) Z3_append_log(msg)
#else
#define LOG_MSG(msg) ((void)0)
#endif

//typedef Z3_ast z3term_t;

/**
 \brief exit gracefully in case of error.
 */
void exitf(const char *message);

/**
 \brief exit if unreachable code was reached.
 */
void unreachable();

/**
 \brief Return a char* containing an int given in a certain base.
 */
char *itoa(int val, int base);

int array_sum(const int *array, int size);

/***************************************************/
/*               some pretty printing              */
/***************************************************/

namespace z3Tools
{
	/**
	 \brief Display a symbol in the given output stream.
	 */
	void display_symbol(Z3_context c, FILE *out, Z3_symbol s);

	/**
	 \brief Display the given type.
	 */
	void display_sort(Z3_context c, FILE *out, Z3_sort ty);

	/**
	 \brief Custom ast pretty printer.

	 This function demonstrates how to use the API to navigate terms.
	 */
	void display_ast(Z3_context c, FILE *out, Z3_ast v);

	/**
	 \brief Custom function interpretations pretty printer.
	 */
	void display_function_interpretations(Z3_context c, FILE *out, Z3_model m);

	/**
	 \brief Custom model pretty printer.
	 */
	void display_model(Z3_context c, FILE *out, Z3_model m);



	/**
	 \brief Create a variable using the given name and type.
	 */
	Z3_ast mk_var(Z3_context ctx, const char *name, Z3_sort ty);

	/**
	 \brief Create an integer variable using the given name.
	 */
	Z3_ast mk_int_var(Z3_context ctx, const char *name);

	/**
	 \brief Create a real variable using the given name.
	 */
	Z3_ast mk_real_var(Z3_context ctx, const char *name);
};

#endif