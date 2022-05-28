
#include <vector>
#include <sstream>
#include <regex>

#include "z3Tools.h"

pl::Term z3Manager::z3Bundle::Z3_expr_to_Term(z3::expr ex) {
    z3::func_decl fd = ex.decl();
    string fname = fd.name().str();

    pl::Functor functor;
    if (fname == "Int") {
        char *qq = strdup(ex.get_decimal_string(0).c_str());
        functor = { .name = qq, .arity = 0 };
    }
    else if (fname == "Real") {
        char *qq = strdup(ex.get_decimal_string(-1).c_str());
        functor = { .name = qq, .arity = 0 };
    }
    else {
        functor = { .name = strdup(fname.c_str()), .arity = (int)fd.arity() };
    }

    pl::List<pl::Term> args = pl::List<pl::Term>::nil;
    pl::List<pl::Term>::iterator argsit = args.begin();
    for (unsigned i = 0; i < functor.arity; ++i) {
        args.insert(argsit, Z3_expr_to_Term(ex.arg(i)));
    }

    return { functor, args };
}
z3::expr_vector z3Manager::z3Bundle::parse_assertion(string assertion, z3::sort_vector* svec_, z3::func_decl_vector* fvec_) const {
    z3::sort_vector _svec (*m_ctx);
    z3::sort_vector& svec = svec_ ? *svec_ : _svec;
    z3::func_decl_vector _fvec (*m_ctx);
    z3::func_decl_vector& fvec = fvec_ ? *fvec_ : _fvec;

    m_dth.fill_sort_vector(svec);
    m_fdh.fill_func_decl_vector(fvec);

    // required to parse type aliases
    string& s = assertion;
    const regex rx("[0-9a-zA-Z_]+(<[0-9a-zA-Z_<,>]+>)?");
    stringstream ss;
    unsigned ppos = 0;
    for (std::sregex_iterator it(s.begin(), s.end(), rx); it != std::sregex_iterator(); ++it) {
        smatch sm = *it;
        string u = sm[0];
        if (m_dth.match_type_alias(u)) {
            unsigned npos = sm.position(0);
            ss << s.substr(ppos, npos - ppos) << u;
            ppos = npos + sm.length(0);
        }
    }
    ss << s.substr(ppos);
    assertion = ss.str();
    //cout << assertion << endl;

    z3::expr_vector exvec (*m_ctx);
    try {
        exvec = m_ctx->parse_string(assertion.c_str(), svec, fvec);
    }
    catch (z3::exception& e) { throw z3Exception(Z3_get_error_code(*m_ctx), e.msg()); }
    return exvec;
}

z3Manager::z3Bundle::z3Bundle(z3Manager* manager, z3::context* ctx) :
m_manager(manager), m_ctx(ctx), m_slv(nullptr), m_dth(z3DatatypeHolder(ctx)), m_fdh(z3FuncDeclHolder(ctx)) {}
z3Manager::z3Bundle::z3Bundle(z3Bundle&& other) :
m_manager(other.m_manager), m_ctx(other.m_ctx), m_slv(other.m_slv), m_dth(std::move(other.m_dth)), m_fdh(std::move(other.m_fdh)) {
    other.m_ctx = nullptr;
}
z3Manager::z3Bundle& z3Manager::z3Bundle::operator=(z3Bundle&& other) {
    if (this != &other) {
        delete m_ctx;

        m_manager = other.m_manager;
        m_ctx = other.m_ctx;
        m_slv = other.m_slv;
        m_dth = std::move(other.m_dth);
        m_fdh = std::move(other.m_fdh);

        other.m_ctx = nullptr;
    }
    return *this;
}
z3Manager::z3Bundle::~z3Bundle() {
    if (m_ctx != nullptr) delete m_ctx;
}

void z3Manager::z3Bundle::mk_solver() {
    m_slv = new z3::solver(*m_ctx);
}
void z3Manager::z3Bundle::del_solver() {
    delete m_slv;
}
void z3Manager::z3Bundle::push() {
    //unsigned s = Z3_solver_get_num_scopes(*m_ctx, (Z3_solver)*m_slv);
    m_dth.backup_scope();
    m_fdh.backup_scope();

    m_slv->push();
}
void z3Manager::z3Bundle::pop() {
    m_slv->pop();

    unsigned s = Z3_solver_get_num_scopes(*m_ctx, (Z3_solver)*m_slv);
    m_dth.restore_scope(s);
    m_fdh.restore_scope(s);
}

void z3Manager::z3Bundle::mk_datatypes(pl::List<pl::String> tpl_vars, pl::List<pl::Datatype> datatypes) {
    vector<string> tplvars (tpl_vars.begin(), tpl_vars.end());
    z3DatatypeHolder::z3DatatypeFactory df(&m_dth, tplvars);

    //if (datatypes.empty()) return;
    for (pl::Datatype& dt : datatypes) {
        //printf("%s\n", dt.name);
        df.add_datatype(dt.name);
        //if (dt.ctors.empty()) continue;
        for (pl::Datatype::Constructor& ct : dt.ctors) {
            //printf("\t%s\n", ct.name);
            //if (ct.accs.empty()) continue;
            vector<string> argnames;
            vector<string> argtypes;
            for (pl::Datatype::Constructor::Accessor& acc : ct.accs) {
                //printf("\t\t%s: %s\n", acc.name, acc.type);
                argnames.push_back(acc.name);
                argtypes.push_back(acc.type);
            }
            df[dt.name].add_constructor(ct.name, std::move(argnames), std::move(argtypes));
        }
    }

    auto& rdf = m_dth.register_factory(std::move(df));

    if (tplvars.empty()) rdf.mk_datatypes({}); // instantiates asap!
}

void z3Manager::z3Bundle::mk_sort(pl::String sort_name, pl::String base_name) {
    if (strcmp(base_name, "") == 0) {
        m_dth.mk_uninterpreted_type(sort_name);
    }
    else {
        m_dth.mk_type_alias(sort_name, base_name);
    }
}

void z3Manager::z3Bundle::mk_var(pl::String var_name, pl::String var_type) {
    z3::expr v = m_ctx->constant(var_name, m_dth.get_datatype(var_type));
    m_fdh.add_func(v.decl());
}

void z3Manager::z3Bundle::mk_func(pl::String func_name, pl::List<pl::String> func_args, pl::String ret_type, pl::String func_body, bool recursive) {
    vector<z3::sort> arg_types;
    for (string func_arg : func_args) {
        arg_types.push_back(m_dth.get_datatype(func_arg));
    }

    z3::sort ret = m_dth.get_datatype(ret_type);

    z3::func_decl f (*m_ctx);
    if (recursive) {
        f = m_ctx->recfun(func_name, arg_types.size(), arg_types.data(), ret);
    }
    else {
        f = m_ctx->function(func_name, arg_types.size(), arg_types.data(), ret);
    }
    m_fdh.add_func(f);

    if (strcmp(func_body, "") == 0) return;

    vector<string> arg_names; arg_names.reserve(arg_types.size());
    for (int i = 0; i < arg_types.size(); i++) {
        arg_names.push_back("x!" + to_string(i));
    }

    z3::sort_vector svec (*m_ctx);
    z3::func_decl_vector fvec (*m_ctx);
    if (!recursive) {
        stringstream ss;
        ss << "(assert (forall (";
        for (int i = 0; i < arg_types.size(); i++) ss << "(" << arg_names[i] << " " << arg_types[i] << ")";
        ss << ") (= (|" << func_name << "|";
        for (string& arg_name : arg_names) ss << " " << arg_name;
        ss << ") " << func_body << ")))";
        string assertion = ss.str();

        z3::expr_vector exvec = parse_assertion(assertion, &svec, &fvec);
        
        assert(exvec.size() == 1);
        z3::expr ex = exvec[0];
        //cout << ex << endl;

        m_slv->add(ex);
    }
    else {
        z3::expr_vector args (*m_ctx);
        for (int i = 0; i < arg_types.size(); i++) {
            z3::expr v = m_ctx->constant(arg_names[i].c_str(), arg_types[i]);
            args.push_back(v);
            fvec.push_back(v.decl());
        }
        
        stringstream ss;
        ss << "(assert (=" << func_body << " " << func_body << "))"; // an assertion must be a Boolean expression!
        string assertion = ss.str();

        z3::expr_vector exvec = parse_assertion(assertion, &svec, &fvec);
        
        assert(exvec.size() == 1);
        z3::expr ex = exvec[0].arg(0);
        //cout << ex << endl;

        m_ctx->recdef(f, args, ex);
    }
}

void z3Manager::z3Bundle::assert_string(pl::String assertion) {
    z3::expr_vector exvec = parse_assertion(assertion);
    for (z3::expr ex : exvec) m_slv->add(ex);
}

pl::atom z3Manager::z3Bundle::check_sat() const {
    z3::check_result result = m_slv->check();

    pl::atom ret;
    switch (result) {
    case z3::unsat:
        ret.name = "unsat";
        break;
    case z3::sat:
        ret.name = "sat";
        break;
    case z3::unknown:
        ret.name = "unknown";
        break;
    }
    return ret;
}

pl::String z3Manager::z3Bundle::get_model_to_string() const {
    z3::model m = m_slv->get_model();
    return (char*)Z3_model_to_string(*m_ctx, m);
}

pl::Term z3Manager::z3Bundle::eval_model_var(pl::String var_name) const {
    z3::model m = m_slv->get_model();
    z3::expr ex = m_fdh.eval_const(var_name, m);
    return Z3_expr_to_Term(ex);
}