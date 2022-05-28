#include "z3DatatypeHolder.h"

#include <sstream>
#include <regex>

using namespace std;

#include "z3DatatypeFactory.cxx"

bool z3DatatypeHolder::split_typename(const string& name, splitted_typename_t& names) {
    const auto rx = regex("^([0-9a-zA-Z_]+)(<([0-9a-zA-Z_<,>]+)>)?$");
    if (smatch sm; regex_match(name, sm, rx)) { 
        string dtname = sm[1];
        vector<string> dtargs;

        if (sm.size() >= 4 && sm[3] != "") {
            string dtarglist = sm[3];
            
            int ab = 0;
            for (int i = 0, j = 0; i <= dtarglist.size(); i++) {
                if ((dtarglist[i] == ',' || dtarglist[i] == 0) && ab == 0) {
                    dtargs.push_back(dtarglist.substr(j, i - j));
                    j = i + 1;
                }
                else if (dtarglist[i] == '<') ab++;
                else if (dtarglist[i] == '>') ab--;
            }
        }
        names = { dtname, dtargs };
        return true;
    }
    return false;
}
optional<z3DatatypeHolder::systype_maker_t> z3DatatypeHolder::get_systype_maker(const string& name, unsigned arity) {
    optional<z3DatatypeHolder::systype_maker_t> m;
    if (name == "List" && arity == 1) {
        m = [](z3DatatypeHolder::z3DatatypeFactory& df, string dtname, vector<string> dtargs) {
            df.add_datatype(dtname);
            df[dtname].add_constructor("nil", {}, {});
            df[dtname].add_constructor("insert", { "head", "tail" }, { dtargs[0], dtname });
        };
    }
    else if (name == "Pair" && arity == 2) {
        m = [](z3DatatypeHolder::z3DatatypeFactory& df, string dtname, vector<string> dtargs) {
            df.add_datatype(dtname);
            df[dtname].add_constructor("mk-pair", { "first", "second" }, { dtargs[0], dtargs[1] });
        };
    }
    else m = nullopt;
    return m;
}

z3DatatypeHolder::z3DatatypeHolder(z3::context *ctx) : m_ctx(ctx) {}

optional<z3::sort> z3DatatypeHolder::get_builtin_sort(const string& name) const {
    if (name == "Int") {
        return m_ctx->int_sort();
    }
    else if (name == "Real") {
        return m_ctx->real_sort();
    }
    else if (name == "Bool") {
        return m_ctx->bool_sort();
    }
    /*else if (name == "Char") {
        return m_ctx->char_sort();
    }*/
    else if (name == "String") {
        return m_ctx->string_sort();
    }
    return nullopt;
}

bool z3DatatypeHolder::mk_system_type(const string& name) {
    if (splitted_typename_t names; split_typename(name, names)) {
        auto& [dtname, dtargs] = names;
        
        auto maker = get_systype_maker(dtname, dtargs.size());
        if (!maker.has_value()) return false;
        
        for (auto& dtarg : dtargs) if (auto dt = find_datatype(dtarg); !dt.has_value()) return false;

        z3DatatypeFactory *odf = nullptr;
        for (auto& factory : m_dtfactories) if (factory.has_datatype(dtname)) odf = &factory;

        if (odf == nullptr) {
            vector<string> tplvars; tplvars.reserve(dtargs.size());
            for (int i = 0; i < dtargs.size(); i++) {
                tplvars.push_back("T"s + to_string(i));
            }
            z3DatatypeFactory df(this, tplvars);
            (*maker)(df, dtname, tplvars);
            odf = &register_factory(std::move(df));
        }

        odf->mk_datatypes(dtargs);

        return true;
    }
    return false;
}
bool z3DatatypeHolder::match_type_alias(string& name) const {
    if (m_knowntypealiases.empty()) return false;

    splitted_typename_t names;
    if (!split_typename(name, names)) return false;

    for (auto& [vname,rname] : m_knowntypealiases) {
        splitted_typename_t vnames;
        if (!split_typename(vname, vnames)) return false;

        if (names.first == vnames.first && names.second.size() == vnames.second.size()) {
            string s = rname;
            for (int i = 0; i < vnames.second.size(); i++) {
                stringstream rxss;
                rxss << "\\b" << vnames.second[i] << "\\b";
                s = regex_replace(s, regex(rxss.str()), names.second[i]);
            }
            match_type_alias(s);
            name = s;
            return true;
        }
    }
    return false;
}
optional<z3Datatype> z3DatatypeHolder::find_datatype(string name) {
    match_type_alias(name);
    // built-in types
    if (auto sort = get_builtin_sort(name); sort.has_value()) return z3Datatype { sort.value(), {} };
    // registered types
    else if (m_knowntypes.find(name) != m_knowntypes.end()) return m_knowntypes.at(name);

    else if (mk_system_type(name)) return m_knowntypes.at(name);
    
    return nullopt;
}
optional<z3Datatype> z3DatatypeHolder::build_datatype(string name) {
    if (splitted_typename_t names; split_typename(name, names)) {
        auto& [dtname, dtargs] = names;
        for (auto& factory : m_dtfactories) {
            if (factory.has_datatype(dtname)) {
                factory.mk_datatypes(dtargs);
                return m_knowntypes.at(name);
            }
        }
    }
    return nullopt;
}
z3Datatype z3DatatypeHolder::get_datatype(string name) {
    if (auto dt = find_datatype(name); dt.has_value()) return dt.value();
    else if (auto dt = build_datatype(name); dt.has_value()) return dt.value();
    throw runtime_error(name + " is a missing datatype!");
}

z3DatatypeHolder::z3DatatypeFactory& z3DatatypeHolder::register_factory(z3DatatypeHolder::z3DatatypeFactory&& df) {
    m_dtfactories.push_back(std::move(df));
    return m_dtfactories.back();
}

void z3DatatypeHolder::mk_uninterpreted_type(string name) {
    m_knowntypes.emplace(name, z3Datatype { m_ctx->uninterpreted_sort(name.c_str()), {} });
}
void z3DatatypeHolder::mk_type_alias(string name, string base) {
    m_knowntypealiases.emplace(name, base);
}

void z3DatatypeHolder::fill_sort_vector(z3::sort_vector& svec) const {
    for (auto& [_, dt] : m_knowntypes) {
        svec.push_back(dt);
    }
}

void z3DatatypeHolder::backup_scope() {
    m_dtfactories.push_scope();
    m_knowntypes.push_scope();
    m_knowntypealiases.push_scope();
}
void z3DatatypeHolder::restore_scope(unsigned s) {
    m_dtfactories.pop_scope(s);
    m_knowntypes.pop_scope(s);
    m_knowntypealiases.pop_scope(s);
}