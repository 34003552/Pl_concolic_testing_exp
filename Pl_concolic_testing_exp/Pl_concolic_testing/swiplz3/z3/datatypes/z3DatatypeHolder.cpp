#include "z3DatatypeHolder.h"

#include <regex>

using namespace std;

z3DatatypeHolder::z3DatatypeHolder(z3::context *ctx) : m_ctx(ctx) {}
z3::sort z3DatatypeHolder::get_builtin_sort(std::string name) {
    if (name == "Int") {
        return m_ctx->int_sort();
    }
    else if (name == "Real") {
        return m_ctx->real_sort();
    }
    else if (name == "Bool") {
        return m_ctx->bool_sort();
    }
    else throw runtime_error("not a builtin sort!");
}
z3Datatype z3DatatypeHolder::get_datatype(std::string name) {
	if (name == "Int" || name == "Real" || name == "Bool") {
        return { get_builtin_sort(name), {} };
    }
    else if (m_usertypes.find(name) != m_usertypes.end()) {
    	return m_usertypes.at(name);
    }
    else throw runtime_error("missing datatype!");
}
z3::sort_vector z3DatatypeHolder::get_sort_vector() {
	z3::sort_vector svec (*m_ctx);
    for (auto& [_, dt] : m_usertypes) {
        svec.push_back(dt.sort);
    }
    return svec;
}