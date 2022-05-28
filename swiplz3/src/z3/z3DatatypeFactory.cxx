
#include <sstream>
#include <set>

void z3DatatypeHolder::z3DatatypeFactory::Datatype::add_constructor(string name, vector<string> accnames, vector<string> acctypes, string tname) {
    if (tname == "") tname = "is_"s + name;
    ctors.push_back({ name, tname, accnames, acctypes });
}
bool z3DatatypeHolder::z3DatatypeFactory::add_system_type(const string& name) {
    if (splitted_typename_t names; m_dth->split_typename(name, names)) {
        auto& [dtname, dtargs] = names;

        auto maker = m_dth->get_systype_maker(dtname, dtargs.size());
        if(!maker.has_value()) return false;

        (*maker)(*this, name, dtargs);
        return true;
    }
    return false;
}
string z3DatatypeHolder::z3DatatypeFactory::substitute_template(const string& name, const vector<string>& tplnames) const {
    if (splitted_typename_t names; m_dth->split_typename(name, names)) {
        auto& [dtname, dtargs] = names;

        if (!dtargs.empty()) {
            stringstream ss;
            ss << dtname << "<" << substitute_template(dtargs[0], tplnames);
            for (auto it = dtargs.begin() + 1; it != dtargs.end(); ++it) {
                ss << "," << substitute_template(*it, tplnames);
            }
            ss << ">";
            return ss.str();
        }
    }

    for (int i = 0; i < m_tplvars.size(); ++i) if (name == m_tplvars[i]) return tplnames[i];

    return name;
}
pair<vector<Z3_sort>, vector<unsigned>> z3DatatypeHolder::z3DatatypeFactory::get_types_desc(const vector<string>& names, const vector<string>& tplnames) {
    size_t vlen = names.size();
    vector<Z3_sort> sorts; sorts.reserve(vlen);
    vector<unsigned> refs; sorts.reserve(vlen);

    for (string name : names) {
        bool ok = false;

        // existing template types
        for (int i = 0; i < m_tplvars.size(); ++i) if (name == m_tplvars[i]) name = tplnames[i];
        
        // existing local types
        for (int i = 0; i < m_dts.size(); ++i) {
            if (name == m_dts[i].name) {
                sorts.push_back(0);
                refs.push_back(i);
                ok = true;
                break;
            }
        }
        if (ok) continue;

        // existing global types
        if (auto dt = m_dth->find_datatype(name); dt.has_value()) {
            sorts.emplace_back(dt.value());
            refs.emplace_back(0);
            continue;
        }

        // constructible system types
        if (!has_datatype(name) && add_system_type(name)) {
            sorts.push_back(0);
            refs.push_back(m_dts.size() - 1);
            continue;
        }

        // constructible user types
        static set<string> b_dts;
        if (b_dts.find(name) != b_dts.end()) {
            throw runtime_error(name + " is not a recursively constructible datatype!");
        }
        b_dts.insert(name);
        if (auto dt = m_dth->build_datatype(name); dt.has_value()) {
            sorts.emplace_back(dt.value());
            refs.emplace_back(0);
            ok = true;
        }
        b_dts.erase(name);
        if (ok) continue;

        // unrecognized types
        throw runtime_error(name + " is not a recognized datatype!");
    }
    return { sorts, refs };
}
z3DatatypeHolder::z3DatatypeFactory::z3DatatypeFactory(z3DatatypeHolder* dth, vector<string> tplvars) :
m_dth(dth), m_tplvars(tplvars), m_dts() {}
void z3DatatypeHolder::z3DatatypeFactory::add_datatype(string name) {
    m_dts.push_back({ this, name, {} });
}
bool z3DatatypeHolder::z3DatatypeFactory::has_datatype(const string& name) const {
    for (auto &dt : m_dts) if (name == dt.name) return true;
    return false;
}
z3DatatypeHolder::z3DatatypeFactory::Datatype& z3DatatypeHolder::z3DatatypeFactory::operator[](const string& name) {
    for (auto &dt : m_dts) if (name == dt.name) return dt;
    throw runtime_error(name + " is an unexpected typename!");
}
vector<z3Datatype> z3DatatypeHolder::z3DatatypeFactory::mk_datatypes(const vector<string>& tpltypes) {
    /*
    for (auto& dt : m_dts) {
        cout << "dt: " << dt.name << endl;
        for (auto& ct : dt.ctors) {
            cout << "ct: " << ct.name << endl;
            for (unsigned i = 0; i < ct.accnames.size(); i++) {
                cout << "acc: " << ct.accnames[i] << " -> " << ct.acctypes[i] << endl;
            }
        }
    }
    */

    z3::context& ctx = *m_dth->m_ctx;

    unsigned numtype = m_dts.size();

    vector<Z3_symbol> sort_names; sort_names.reserve(numtype);
    vector<vector<Z3_constructor>> consvs; consvs.reserve(numtype);
    vector<Z3_constructor_list> constructors; constructors.reserve(numtype);

    for (auto dti = m_dts.begin() ; dti != m_dts.end() ; ++dti) {
        z3DatatypeFactory::Datatype& dt = *dti;

        string dtname = dt.name;
        if (dtname.find('<') != string::npos) {
            dtname = substitute_template(dtname, tpltypes);
        }
        else if (!tpltypes.empty()) {
            stringstream ss;
            ss << dtname << '<' << tpltypes[0];
            for (auto it = tpltypes.begin() + 1; it != tpltypes.end(); ++it) {
                ss << ',' << *it;
            }
            ss << '>';
            dtname = ss.str();
        }
        sort_names.push_back(Z3_mk_string_symbol(ctx, dtname.c_str()));

        if (dt.ctors.size() == 0) throw z3::exception("Type domain must not be empty!");
        
        vector<Z3_constructor> consv; consv.reserve(dt.ctors.size());
        for (z3DatatypeFactory::Datatype::Constructor& ct : dt.ctors) {
            auto [sorts, sort_refs] = get_types_desc(ct.acctypes, tpltypes);

            Z3_symbol zname = Z3_mk_string_symbol(ctx, ct.name.c_str());
            Z3_symbol ztname = Z3_mk_string_symbol(ctx, ct.tname.c_str());
            int numaccs = ct.accnames.size();

            vector<Z3_symbol> zaccnames; zaccnames.reserve(ct.accnames.size());
            for (string& accname : ct.accnames) zaccnames.push_back(Z3_mk_string_symbol(ctx, accname.c_str()));

            Z3_constructor cons;
            if (numaccs == 0) cons = Z3_mk_constructor(ctx, zname, ztname, numaccs, 0, 0, 0);
            else cons = Z3_mk_constructor(ctx, zname, ztname, numaccs, zaccnames.data(), sorts.data(), sort_refs.data());
            consv.push_back(cons);
        }
        constructors.push_back(Z3_mk_constructor_list(ctx, consv.size(), consv.data()));
        consvs.push_back(consv);
    }
    numtype = m_dts.size(); // refresh!

    vector<Z3_sort> sorts; sorts.reserve(numtype);
    Z3_mk_datatypes(ctx, numtype, sort_names.data(), sorts.data(), constructors.data());

    for (Z3_constructor_list ctlist : constructors) Z3_del_constructor_list(ctx, ctlist);

    vector<z3Datatype> rsorts; rsorts.reserve(m_dts.size());
    
    unsigned i = 0;
    for (z3DatatypeFactory::Datatype& dt : m_dts) {
        vector<z3Datatype::z3Constructor> ctors; ctors.reserve(dt.ctors.size());

        unsigned j = 0;
        for (z3DatatypeFactory::Datatype::Constructor& ct : dt.ctors) {
            unsigned numaccs = ct.accnames.size();

            Z3_func_decl ctor;
            Z3_func_decl tst;
            Z3_func_decl accs[numaccs];

            Z3_query_constructor(ctx, consvs[i].data()[j], numaccs, &ctor, &tst, accs);

            vector<z3Datatype::z3Constructor::z3Accessor> waccs; waccs.reserve(numaccs);
            for (Z3_func_decl acc : accs) waccs.push_back({ z3::func_decl(ctx, acc) });

            ctors.push_back({ z3::func_decl(ctx, ctor), z3::func_decl(ctx, tst), waccs });
            j++;
        }
        rsorts.push_back({ z3::sort(ctx, sorts[i]), ctors });
        i++;
    }

    for (z3Datatype& rsort : rsorts) {
        string rname = rsort.name().str();
        m_dth->m_knowntypes.emplace(rname, rsort);
    }

    /*
    cout << "new available datatypes: ";
    for (z3Datatype rsort : rsorts) {
        string rname = rsort.sort.name().str();
        cout << rname << " ";
    }
    cout << endl;
    */

    return rsorts;
}