#include <vector>
#include <map>
#include "z3.h"
#include "z3Tools.h"
#include "z3Manager.h"

class z3Manager::z3Bundle
{
	z3Manager *m_manager;

	Z3_context m_ctx;
	Z3_solver m_z3s;

	struct z3Datatype
	{
		Z3_symbol name;
		Z3_sort sort;

		struct z3Constructor
		{
			Z3_symbol name;
			Z3_func_decl ctor;

			struct z3Accessor
			{
				Z3_symbol name;
				Z3_func_decl acc;
			};
			std::vector<z3Accessor> accs;
		};
		std::vector<z3Constructor> ctors;
	};
	std::map<const char*, z3Datatype> m_types;

	struct z3NumVars
	{
		unsigned numint;
		unsigned numterm;
	};
	std::vector<z3NumVars> m_numparentvars;

	struct z3Variable
	{
		Z3_symbol name;
		Z3_func_decl decl;
	};
	std::vector<z3Variable> m_intvars;
	std::vector<z3Variable> m_termvars;
	
public:
	z3Bundle(z3Manager *manager, Z3_context ctx);

	Z3_context get_context();

	void mk_solver();
	void del_solver();

	void push();
	void pop();

	void mk_int_var(const char *varname);
	void mk_term_var(const char *varname);

	void mk_term_type(TList *tlist, bool need_int, bool need_lists);

	bool assert_int_string(const char *z3string);
	bool assert_term_string(const char *z3string, bool need_int, bool need_lists);

	bool check();

	char *print_model();

	bool get_model_intvar_eval(const char *vn, int *val);
	bool get_model_termvar_eval(const char *vn, z3term_t *v);

	Z3_app mk_term__get_app(Z3_ast v);
	void mk_term__get_functor(Z3_app app, char **name, int *arity);
	Z3_ast mk_term__get_app_arg(Z3_app app, int j);
};
