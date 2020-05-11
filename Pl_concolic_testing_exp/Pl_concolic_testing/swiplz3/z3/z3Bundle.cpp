#include "z3Bundle.h"

using namespace std;

enum idtypes { ID_TERM, ID_TERMLIST };

z3Manager::z3Bundle::z3Bundle(z3Manager *manager, Z3_context ctx) :
m_manager(manager), m_ctx(ctx), m_z3s()
{
    
}
Z3_context z3Manager::z3Bundle::get_context()
{
    return m_ctx;
}

void z3Manager::z3Bundle::mk_solver()
{
	m_z3s = Z3_mk_solver(m_ctx);
	Z3_solver_inc_ref(m_ctx, m_z3s);
}
void z3Manager::z3Bundle::del_solver()
{
	Z3_solver_dec_ref(m_ctx, m_z3s);
}
void z3Manager::z3Bundle::push()
{
	//int s = Z3_solver_get_num_scopes(m_ctx, m_z3s);

    m_numparentvars.push_back({ (unsigned)m_intvars.size(), (unsigned)m_termvars.size() });

	Z3_solver_push(m_ctx, m_z3s);
}
void z3Manager::z3Bundle::pop()
{
	Z3_solver_pop(m_ctx, m_z3s, 1);

	int s = Z3_solver_get_num_scopes(m_ctx, m_z3s);

    int numintvar = m_intvars.size();
    for (unsigned i = m_numparentvars[s].numint; i < numintvar; i++) m_intvars.pop_back();

    int numtermvar = m_termvars.size();
    for (unsigned i = m_numparentvars[s].numterm; i < numtermvar; i++) m_termvars.pop_back();
}
void z3Manager::z3Bundle::mk_int_var(const char *varname)
{
	Z3_ast v = z3Tools::mk_int_var(m_ctx, varname);
	m_intvars.push_back({
        Z3_mk_string_symbol(m_ctx, varname),
        Z3_get_app_decl(m_ctx, Z3_to_app(m_ctx, v))
    });
}
void z3Manager::z3Bundle::mk_term_var(const char *varname)
{
	Z3_ast v = z3Tools::mk_var(m_ctx, varname, m_types["Term"].sort); // ???
    m_termvars.push_back({
        Z3_mk_string_symbol(m_ctx, varname),
        Z3_get_app_decl(m_ctx, Z3_to_app(m_ctx, v))
    });
}
void z3Manager::z3Bundle::mk_term_type(TList *tlist, bool need_int, bool need_lists)
{
    //need_int = true; need_lists = true;

    unsigned numtype = 1 + need_lists;
    Z3_symbol sort_names[numtype];
    sort_names[ID_TERM] = Z3_mk_string_symbol(m_ctx, "Term");
    if (need_lists)
        sort_names[ID_TERMLIST] = Z3_mk_string_symbol(m_ctx, "TermList");

    Z3_sort sorts[numtype];
    for (unsigned i = 0; i < numtype; i++) sorts[i] = 0;

    vector<Z3_symbol> consnames__term;
    vector<Z3_constructor> cons__term;

    vector<Z3_symbol> consnames__termlist;
    vector<Z3_constructor> cons__termlist;

    Z3_symbol accnames__term_from_int[1], accnames__cons[1];
    Z3_symbol accnames__insert[2];

    unsigned id__term_from_int, id__cons, id__tlist;
    unsigned id__nil, id__insert;

    if (need_int)
    {
        Z3_sort int_sort = Z3_mk_int_sort(m_ctx);
        Z3_sort node_accessor_sorts[1] = { int_sort };
        unsigned sort_refs[1] = { 0 };

        id__term_from_int = cons__term.size();
        consnames__term.push_back(Z3_mk_string_symbol(m_ctx, "term_from_int"));
        accnames__term_from_int[0] = Z3_mk_string_symbol(m_ctx, "term_as_int");
        cons__term.push_back(Z3_mk_constructor(m_ctx, consnames__term[id__term_from_int],
            Z3_mk_string_symbol(m_ctx, "is_int"), 1, accnames__term_from_int, node_accessor_sorts, sort_refs));
    }

    if (need_lists)
    {
        unsigned sort_refs[2];

        id__nil = cons__termlist.size();
        consnames__termlist.push_back(Z3_mk_string_symbol(m_ctx, "nil"));
        cons__termlist.push_back(Z3_mk_constructor(m_ctx, consnames__termlist[id__nil],
            Z3_mk_string_symbol(m_ctx, "is_nil"), 0, 0, 0, 0));
        

        sort_refs[0] = 0; // the insert of a list is a term
        sort_refs[1] = 1;
    
        id__insert = cons__termlist.size();
        consnames__termlist.push_back(Z3_mk_string_symbol(m_ctx, "insert"));
        accnames__insert[0] = Z3_mk_string_symbol(m_ctx, "head");
        accnames__insert[1] = Z3_mk_string_symbol(m_ctx, "tail");
        cons__termlist.push_back(Z3_mk_constructor(m_ctx, consnames__termlist[id__insert],
            Z3_mk_string_symbol(m_ctx, "is_insert"), 2, accnames__insert, sorts, sort_refs));
        


        sort_refs[0] = 1; // the cons of a Term is a TermList

        id__cons = cons__term.size();
        consnames__term.push_back(Z3_mk_string_symbol(m_ctx, "cons"));
        accnames__cons[0] = Z3_mk_string_symbol(m_ctx, "list");
        cons__term.push_back(Z3_mk_constructor(m_ctx, consnames__term[id__cons],
            Z3_mk_string_symbol(m_ctx, "is_cons"), 1, accnames__cons, sorts, sort_refs));

    }

    id__tlist = cons__term.size();
    unsigned tlist_size = 0;
    for (TList *itlist = tlist; itlist != nullptr; itlist = itlist->next)
    {
        const char *name = itlist->name;
        const int arity = itlist->arity;

        Z3_symbol sym = Z3_mk_string_symbol(m_ctx, name);
        Z3_symbol is_sym = z3Tools::get_accessor_name(m_ctx, name);

        if (arity == 0) cons__term.push_back(Z3_mk_constructor(m_ctx, sym, is_sym, 0,0,0,0));
        else
        {
            Z3_symbol node_acc_names[arity];
            Z3_sort node_acc_sorts[arity];
            unsigned node_acc_sort_refs[arity];
            for (unsigned j = 0; j < arity; ++j)
            {
                node_acc_names[j] = z3Tools::get_node_accessor_name(m_ctx, name, j);
                node_acc_sorts[j] = 0;
                node_acc_sort_refs[j] = 0;
            }
            
            cons__term.push_back(Z3_mk_constructor(m_ctx, sym, is_sym, arity, node_acc_names, node_acc_sorts, node_acc_sort_refs));
        }

        consnames__term.push_back(sym);
        tlist_size++;
    }

    Z3_constructor_list constructors[numtype];
    constructors[ID_TERM] = Z3_mk_constructor_list(m_ctx, cons__term.size(), cons__term.data());
    if (need_lists)
        constructors[ID_TERMLIST] = Z3_mk_constructor_list(m_ctx, 2, cons__termlist.data());

    Z3_mk_datatypes(m_ctx, numtype, sort_names, sorts, constructors);

    for (unsigned i = 0; i < numtype; i++)
    {
        Z3_del_constructor_list(m_ctx, constructors[i]);
    }


    m_types.emplace("Term", z3Datatype { sort_names[ID_TERM], sorts[ID_TERM], {} });

    for (unsigned i = id__tlist; i < id__tlist + tlist_size; i++)
    {
        m_types["Term"].ctors.push_back({ consnames__term[i] });
    }

    if (need_lists)
    {
        m_types.emplace("TermList", z3Datatype { sort_names[ID_TERMLIST], sorts[ID_TERMLIST], {} });
    }

    if (need_int)
    {
        Z3_sort int_sort = Z3_mk_int_sort(m_ctx);

        Z3_func_decl ctors[1];
        Z3_func_decl tsts[1];
        Z3_func_decl accs[1];
        Z3_query_constructor(m_ctx, cons__term[id__term_from_int], 1, ctors, tsts, accs);
        m_types["Term"].ctors.push_back({
            consnames__term[id__term_from_int], // term_from_int
            ctors[0],//Z3_mk_func_decl(m_ctx, consnames__term[id__term_from_int], 1, &int_sort, sorts[ID_TERM]), // term_from_int : Int -> Term
            {}
        });
        m_types["Term"].ctors.back().accs.push_back({
            accnames__term_from_int[0], // term_as_int
            accs[0]//Z3_mk_func_decl(m_ctx, accnames__term_from_int[0], 1, &sorts[ID_TERM], int_sort) // term_as_int : Term -> Int
        });
    }

    if (need_lists)
    {
        m_types["TermList"].ctors.push_back({
            consnames__termlist[id__nil], // nil
            Z3_mk_func_decl(m_ctx, consnames__termlist[id__nil], 0, 0, sorts[ID_TERMLIST]), // nil : TermList
            {}
        });

        m_types["TermList"].ctors.push_back({
            consnames__termlist[id__insert], // insert
            Z3_mk_func_decl(m_ctx, consnames__termlist[id__insert], 2, sorts, sorts[ID_TERMLIST]), // insert : (Term, TermList) -> TermList
            {}
        });
        m_types["TermList"].ctors.back().accs.push_back({
            accnames__insert[0], // head
            Z3_mk_func_decl(m_ctx, accnames__insert[0], 1, &sorts[ID_TERMLIST], sorts[ID_TERM]) // head : TermList -> Term
        });
        m_types["TermList"].ctors.back().accs.push_back({
            accnames__insert[1], // tail
            Z3_mk_func_decl(m_ctx, accnames__insert[1], 1, &sorts[ID_TERMLIST], sorts[ID_TERMLIST]) // tail : TermList -> TermList
        });


        m_types["Term"].ctors.push_back({
            consnames__term[id__cons],
            Z3_mk_func_decl(m_ctx, consnames__term[id__cons], 1, &sorts[ID_TERMLIST], sorts[ID_TERM]), // cons : TermList -> Term
            {}
        });
        m_types["TermList"].ctors.back().accs.push_back({
            accnames__cons[0], // list
            Z3_mk_func_decl(m_ctx, accnames__cons[0], 1, &sorts[ID_TERM], sorts[ID_TERMLIST]) // list : Term -> TermList
        });
    }
}

bool z3Manager::z3Bundle::assert_int_string(const char *z3string)
{
    unsigned numintvar = m_intvars.size();

    Z3_symbol intvar_names[numintvar];
    Z3_func_decl intvar_decls[numintvar];
    for (unsigned i = 0; i < numintvar; i++)
    {
        intvar_names[i] = m_intvars[i].name;
        intvar_decls[i] = m_intvars[i].decl;
    }

	Z3_ast_vector fs = (Z3_ast_vector)Z3_parse_smtlib2_string(m_ctx, z3string, 0,0,0, numintvar, intvar_names, intvar_decls);

	Z3_error_code e = Z3_get_error_code(m_ctx);
	if (e != Z3_OK)
	{
		printf("Z3 error: %s.\n", Z3_get_error_msg(m_ctx, e));
		return false;
	}

	for (unsigned j = 0; j < Z3_ast_vector_size(m_ctx, fs); ++j)
		Z3_solver_assert(m_ctx, m_z3s, Z3_ast_vector_get(m_ctx, fs, j));

	return true;
}
bool z3Manager::z3Bundle::assert_term_string(const char *z3string, bool need_int, bool need_lists)
{
    //need_int = true; need_lists = true;

    unsigned numtermvar = m_termvars.size();

    unsigned numtype = m_types.size(); // ???
    int numconsts[numtype];
    int numacc[numtype];
    Z3_symbol sort_names[numtype];
    Z3_sort sorts[numtype];
    auto types_it = m_types.begin();
    for (unsigned i = 0; i < numtype; i++)
    {
        numconsts[i] = types_it->second.ctors.size();
        numacc[i] = 0;
        for (unsigned j = 0; j < numtype; j++)
        {
            numacc[i] += types_it->second.ctors[j].accs.size();
        }
        sort_names[i] = types_it->second.name;
        sorts[i] = types_it->second.sort;
        ++types_it;
    }

	unsigned l, f, d;
    unsigned m = array_sum(numconsts, numtype);
    unsigned n = array_sum(numacc, numtype);
    unsigned k = numtermvar + m + n;
    Z3_symbol names[k];
    Z3_func_decl decls[k];

    Z3_string test;

    k = numtermvar + n;

    for (unsigned j = 0; j < numtermvar; ++j) // Variable declaration
    {
        names[j] = m_termvars[j].name;
        decls[j] = m_termvars[j].decl;
    }

    if (need_int && !need_lists) // Declaration of the constructors for Int Type
    { 
        //names[numtermvar] = m_types["Term"].ctors.back().name;
        //decls[numtermvar] = m_types["Term"].ctors.back().ctor;
        names[numtermvar] = Z3_mk_string_symbol(m_ctx, "dummy_term_from_int");
        decls[numtermvar] = m_types["Term"].ctors.back().ctor;
        k = numtermvar + 1;
    }

    if (need_lists) // Declaration of the constructors for List Type
    {
    	//printf("hello C\n");
        names[numtermvar] = m_types["Term"].ctors.back().name; // ???
        decls[numtermvar] = m_types["Term"].ctors.back().ctor; // ???

        d = numtermvar + 1; // ???
        f = d + m_types["TermList"].ctors.size(); // ???
        for (unsigned j = d; j < f; ++j)
        {
            names[j] = m_types["TermList"].ctors[j - d].name; // ???
            decls[j] = m_types["TermList"].ctors[j - d].ctor; // ???
        }

        int acc = 0;
        //for (l = 0; l < numtype; ++l) // ???
        for (auto ptype : m_types)
        {
            auto type = ptype.second;
            for (unsigned u = 0; u < type.ctors.size(); ++u)
            {
                for (unsigned j = 0; j < type.ctors[u].accs.size(); ++j) // ???
                {
                    names[j + f + acc] = type.ctors[u].accs[j].name; // ???
                    decls[j + f + acc] = type.ctors[u].accs[j].acc; // ???
                }
                acc += type.ctors[u].accs.size(); // ???
            }
        }
        k = f + acc;
    }

    /*printf("DFG: %d\n", numtermvar);
    printf("%s\n\n", z3string);
    printf("%d\n", numtype);
    for(unsigned aaa = 0; aaa < numtype; ++aaa) {
        printf("%s\n", (char*)sort_names[aaa]);
        printf("%s\n", Z3_sort_to_string(m_ctx, sorts[aaa]));
    }
    printf("%d\n", k);
    for(unsigned aaa = 0; aaa < k; ++aaa) {
        //printf("%s\n", (char*)names[aaa]);
        printf("%s\n", Z3_func_decl_to_string(m_ctx, decls[aaa]));
    }*/

    Z3_ast_vector fs = (Z3_ast_vector)Z3_parse_smtlib2_string(m_ctx, z3string, numtype, sort_names, sorts, k, names, decls);

    Z3_error_code e = Z3_get_error_code(m_ctx);
    if (e != Z3_OK)
    {
    	printf("Z3 error: %s.\n", Z3_get_error_msg(m_ctx, e));
    	return false;
    }

    for (unsigned j = 0; j < Z3_ast_vector_size(m_ctx, fs); ++j)
        Z3_solver_assert(m_ctx, m_z3s, Z3_ast_vector_get(m_ctx, fs, j));

    return true;
}

bool z3Manager::z3Bundle::check()
{
	Z3_lbool result = Z3_solver_check(m_ctx, m_z3s);

    int rval = 1;
    switch (result)
    {
    case Z3_L_FALSE:
		rval = 0;
		break;
	case Z3_L_TRUE:
		break;
	case Z3_L_UNDEF:
		break;
    }
    return rval;
}

char *z3Manager::z3Bundle::print_model()
{
	Z3_model m = 0;

    m = Z3_solver_get_model(m_ctx, m_z3s);
    if (m) Z3_model_inc_ref(m_ctx, m);

    return (char*)Z3_model_to_string(m_ctx, m);
}

bool z3Manager::z3Bundle::get_model_intvar_eval(const char *vn, int *val)
{
	Z3_model m = 0;

    m = Z3_solver_get_model(m_ctx, m_z3s);
    if (m) Z3_model_inc_ref(m_ctx, m);

    Z3_ast n = z3Tools::mk_int_var(m_ctx, vn);
    Z3_ast v;

    if (Z3_model_eval(m_ctx, m, n, 1, &v))
    	Z3_get_numeral_int(m_ctx, v, val);
    else
    	exitf("failed to evaluate the variable");

    return true;
}
bool z3Manager::z3Bundle::get_model_termvar_eval(const char *vn, z3term_t *v)
{
	Z3_model m = 0;

    m = Z3_solver_get_model(m_ctx, m_z3s);
    if (m) Z3_model_inc_ref(m_ctx, m);

    Z3_ast n = z3Tools::mk_var(m_ctx, vn, m_types["Term"].sort);

    bool res = Z3_model_eval(m_ctx, m, n, 1, (Z3_ast*)v);
    if (!res)
    	exitf("failed to evaluate the variable");
    
    return res;
}

Z3_app z3Manager::z3Bundle::mk_term__get_app(Z3_ast v)
{
	return Z3_to_app(m_ctx, v);
}
void z3Manager::z3Bundle::mk_term__get_functor(Z3_app app, char **name, int *arity)
{
    Z3_func_decl f = Z3_get_app_decl(m_ctx, app);
    Z3_symbol sym = Z3_get_decl_name(m_ctx, f);
    *name = (char*)Z3_get_symbol_string(m_ctx, sym);
    *arity = Z3_get_arity(m_ctx, f);
}
int z3Manager::z3Bundle::mk_term__get_app_arg_as_int(Z3_app app, int j)
{
    Z3_ast app_arg = Z3_get_app_arg(m_ctx, app, j);

    int v;
    Z3_get_numeral_int(m_ctx, app_arg, &v);
    return v;
}
Z3_ast z3Manager::z3Bundle::mk_term__get_app_arg(Z3_app app, int j)
{
    //printf("Test: %s\n", Z3_ast_to_string(m_ctx, Z3_app_to_ast(m_ctx, app)));
	return Z3_get_app_arg(m_ctx, app, j);
}
