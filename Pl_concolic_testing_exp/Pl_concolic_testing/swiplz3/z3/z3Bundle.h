
#ifndef Z3BUNDLE_H
#define Z3BUNDLE_H

#include <vector>
#include <map>
#include <set>
#include <z3.h>

#include "../pl2cpp/cpptypes.h"

#include "z3Tools.h"
#include "z3Manager.h"
#include "./datatypes/z3Datatype.h"
#include "./datatypes/z3DatatypeHolder.h"

class z3Manager::z3Bundle
{
	friend class z3Manager;
	
	z3Manager *m_manager;

	z3::context *m_ctx;
	z3::solver *m_z3s;

	z3DatatypeHolder m_dtm;

	struct z3NumVars
	{
		unsigned numint;
		unsigned numterm;
	};
	std::vector<z3NumVars> m_numparentvars;

	std::vector<z3::expr> m_intvars;
	std::vector<z3::func_decl> m_termvars;

	Term Z3_ast_to_Term(Z3_ast w);
	
public:
	z3Bundle(z3Manager *manager, z3::context *ctx);
	~z3Bundle();

	void mk_solver();
	void del_solver();

	void push();
	void pop();

	void mk_term_type(List<Functor> known_terms, bool need_int, bool need_lists);
	void mk_datatypes(List<const char*> tpl_args, List<Datatype> datatypes);

	void mk_int_var(const char *var_name);
	void mk_term_var(const char *var_name);
	void mk_var(const char* var_name, const char* var_type);

	void assert_int_string(const char *assertion);
	void assert_term_string(const char *assertion, bool need_int, bool need_lists);

	bool check();

	char *print_model();

	Integer get_model_intvar_eval(const char *var_name);
	Term get_model_termvar_eval(const char *var_name);
};

#endif