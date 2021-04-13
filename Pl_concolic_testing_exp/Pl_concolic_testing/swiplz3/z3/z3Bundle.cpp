#include "z3Bundle.h"
#include <sstream>

#include "./datatypes/z3DatatypeFactory.h"

using namespace std;

Term z3Manager::z3Bundle::Z3_ast_to_Term(Z3_ast w)
{
    z3::expr ex (*m_ctx, w);
    z3::func_decl fd = ex.decl();
    z3::symbol sym = fd.name();
    string sname = sym.str();

    Functor functor;
    if (sname == "term_from_int") {
        char *qq = strdup(ex.arg(0).get_decimal_string(0).c_str()); 
        functor = { .name = qq, .arity = 0 };
    }
    else if (sname == "Int") {
        char *qq = strdup(ex.get_decimal_string(0).c_str());
        functor = { .name = qq, .arity = 0 };
    }
    else if (sname == "Real") {
        char *qq = strdup(ex.get_decimal_string(-1).c_str());
        functor = { .name = qq, .arity = 0 };
    }
    else if (sname == "cons") {
        return Z3_ast_to_Term(ex.arg(0));
    }
    else if (sname == "nil") {
        functor = { .name = "[]", .arity = 0 };
    }
    else if (sname == "insert") {
        functor = { .name = "[|]", .arity = 2 };
    }
    else if (sname == "mk-pair") {
        functor = { .name = "-", .arity = 2 };
    }
    else {
        functor = { .name = strdup(sname.c_str()), .arity = (int)fd.arity() };
    }

    List<Term> args = List<Term>::nil;
    List<Term>::iterator argsit = args.begin();
    for (unsigned i = 0; i < functor.arity; ++i)
    {
        args.insert(argsit, Z3_ast_to_Term(ex.arg(i)));
    }

    return { functor, args };
}

z3Manager::z3Bundle::z3Bundle(z3Manager* manager, z3::context* ctx) :
m_manager(manager), m_ctx(ctx), m_z3s(nullptr), m_dtm(z3DatatypeHolder(ctx))
{
    
}
z3Manager::z3Bundle::~z3Bundle()
{

}

void z3Manager::z3Bundle::mk_solver()
{
    m_z3s = new z3::solver(*m_ctx);
}
void z3Manager::z3Bundle::del_solver()
{
    delete m_z3s;
}
void z3Manager::z3Bundle::push()
{
    m_numparentvars.push_back({ (unsigned)m_intvars.size(), (unsigned)m_termvars.size() });

	m_z3s->push();
}
void z3Manager::z3Bundle::pop()
{
	m_z3s->pop();

	int s = Z3_solver_get_num_scopes(*m_ctx, (Z3_solver)*m_z3s);

    const int numintvar = m_intvars.size();
    for (unsigned i = m_numparentvars[s].numint; i < numintvar; i++) m_intvars.pop_back();

    const int numtermvar = m_termvars.size();
    for (unsigned i = m_numparentvars[s].numterm; i < numtermvar; i++) m_termvars.pop_back();
}
void z3Manager::z3Bundle::mk_term_type(List<Functor> known_terms, bool need_int, bool need_lists)
{
    //need_int = true; need_lists = true;

    z3DatatypeFactory df(m_dtm);

    df.add_datatype("Term");
    if (need_lists) df.add_datatype("TermList");

    if (need_int)
    {
        df["Term"].add_constructor("term_from_int", { "term_as_int" }, { "Int" }, "is_int");
    }

    if (need_lists)
    {
        df["TermList"].add_constructor("nil", {}, {});
        df["TermList"].add_constructor("insert", { "head", "tail" }, { "Term", "TermList" });

        df["Term"].add_constructor("cons", { "list" }, { "TermList" });
    }

    bool add_s = false;

    //unsigned tlist_size = 0;
    for (Functor f : known_terms)
    {
        /*if (strcmp(f.name, "s") == 0 && f.arity == 1)
        {
            add_s = true;
            continue;
        }*/
        vector<string> node_acc_names;
        for (unsigned j = 0; j < f.arity; ++j)
        {
            stringstream node_acc_name_ss;
            node_acc_name_ss << f.name << "_arg_" << j;
            node_acc_names.push_back(node_acc_name_ss.str().c_str());
        }

        vector<string> node_acc_types(f.arity, "Term");

        df["Term"].add_constructor(f.name, node_acc_names, node_acc_types);

        //tlist_size++;
    }

    df.mk_datatypes();
}
void z3Manager::z3Bundle::mk_datatypes(List<const char*> tpl_args, List<Datatype> datatypes)
{
    z3DatatypeFactory df(m_dtm);

    //if (datatypes.empty()) return;
    for (Datatype& dt : datatypes)
    {
        //printf("%s\n", dt.name);
        df.add_datatype(dt.name);
        //if (dt.ctors.empty()) continue;
        for (Datatype::Constructor& ct : dt.ctors)
        {
            //printf("\t%s\n", ct.name);
            //if (ct.accs.empty()) continue;
            vector<string> argnames;
            vector<string> argtypes;
            for (Datatype::Constructor::Accessor& acc : ct.accs)
            {
                //printf("\t\t%s: %s\n", acc.name, acc.type);
                argnames.push_back(acc.name);
                argtypes.push_back(acc.type);
            }
            df[dt.name].add_constructor(ct.name, argnames, argtypes);
        }
    }

    //cout << "etr flag" << endl;

    df.mk_datatypes();
}
void z3Manager::z3Bundle::mk_int_var(const char *var_name)
{
    z3::expr v = m_ctx->constant(var_name, m_ctx->int_sort());
    m_intvars.push_back(v);
}
void z3Manager::z3Bundle::mk_term_var(const char *var_name)
{
    z3::expr v = m_ctx->constant(var_name, m_dtm.get_datatype("Term")); // m_usertypes.at("Term").sort
    m_termvars.push_back(v.decl());
}
void z3Manager::z3Bundle::mk_var(const char* var_name, const char* var_type)
{
    z3::expr v = m_ctx->constant(var_name, m_dtm.get_datatype(var_type));
    m_termvars.push_back(v.decl());
}
void z3Manager::z3Bundle::assert_int_string(const char *assertion)
{
    unsigned numintvar = m_intvars.size();

    z3::sort_vector svec (*m_ctx);

    z3::func_decl_vector fvec (*m_ctx);
    for (auto& intvar : m_intvars) // Variable declaration
    {
        fvec.push_back(intvar.decl());
    }
    printf("%s\n", assertion);

    z3::expr_vector exvec = m_ctx->parse_string(assertion, svec, fvec);

    /*Z3_error_code e = Z3_get_error_code(*m_ctx);
    if (e != Z3_OK)
    {
        printf("Z3 error: %s.\n", Z3_get_error_msg(*m_ctx, e));
        return false;
    }*/

    for (z3::expr ex : exvec) m_z3s->add(ex);

    //return true;
}
void z3Manager::z3Bundle::assert_term_string(const char *assertion, bool need_int, bool need_lists)
{
    //need_int = true; need_lists = true;

    unsigned numtermvar = m_termvars.size();

    z3::sort_vector svec = m_dtm.get_sort_vector();

    z3::func_decl_vector fvec (*m_ctx);
    for (auto& termvar : m_termvars) // Variable declaration
    {
        fvec.push_back(termvar);
    }

    //cout << "assert: " << assertion << endl;

    z3::expr_vector exvec (*m_ctx);
    try {
        exvec = m_ctx->parse_string(assertion, svec, fvec);
    }
    catch (z3::exception& e) { throw z3Exception(Z3_get_error_code(*m_ctx), e.msg()); }

    /*Z3_error_code e = Z3_get_error_code(*m_ctx);
    if (e != Z3_OK)
    {
    	printf("Z3 error: %s.\n", Z3_get_error_msg(*m_ctx, e));
    	return false;
    }*/

    for (z3::expr ex : exvec) m_z3s->add(ex);

    //return true;
}

bool z3Manager::z3Bundle::check()
{
    z3::check_result result = m_z3s->check();

    int rval;
    switch (result)
    {
    case z3::unsat:
        rval = 0;
        break;
    case z3::sat:
    case z3::unknown:
        rval = 1;
        break;
    }
    return rval;
}

char *z3Manager::z3Bundle::print_model()
{
    z3::model m = m_z3s->get_model();
    return (char*)Z3_model_to_string(*m_ctx, m);
}

Integer z3Manager::z3Bundle::get_model_intvar_eval(const char *var_name)
{
    z3::model m = m_z3s->get_model();
    z3::expr n = m_ctx->int_const(var_name);

    z3::expr w = m.eval(n, true);
    return { .value = strdup(w.get_decimal_string(0).c_str()) };
    /*try {
        z3::expr w = m.eval(n, true);
        var_value->value = strdup(w.get_decimal_string(0).c_str());
        return true;
    }
    catch (exception e) {
        exitf("failed to evaluate the variable");
        return false;
    }*/
}
Term z3Manager::z3Bundle::get_model_termvar_eval(const char *var_name)
{
    z3::model m = m_z3s->get_model();
    z3::expr n (*m_ctx); //z3::expr n = m_ctx->constant(var_name, m_usertypes.at("Term").sort);
    for (z3::func_decl termvar : m_termvars)
        if (termvar.name().str() == var_name)
            n = termvar();

    z3::expr w = m.eval(n, true);
    return Z3_ast_to_Term(w);
    /*try {
        z3::expr w = m.eval(n, true);
        *var_value = Z3_ast_to_Term(w);
        return true;
    }
    catch (exception e) {
        exitf("failed to evaluate the variable");
        return false;
    }*/
}