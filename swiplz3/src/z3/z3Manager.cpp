#include "z3Manager.h"

using namespace std;

#include "z3Bundle.cxx"

z3Manager::z3Manager() :
m_cfg(nullptr), m_bdls() {}

z3Manager::z3Bundle& z3Manager::get_bundle(int index) {
    return m_bdls.at(index);
}

void z3Manager::mk_config() {
    m_cfg = new z3::config();
}

void z3Manager::del_config() {
    delete m_cfg;
}

void z3Manager::set_param_value(const char *param_name, const char *param_value) {
    m_cfg->set(param_name, param_value);
}

int z3Manager::mk_context() {
    z3::context* ctx = new z3::context(*m_cfg);
    //Z3_set_error_handler(*ctx, error_handler);

#ifdef MINIMALIZE_CONTEXTID
    // MODE: next available index
    int index = 0;
    for (int i = 0; i <= m_bdls.size(); i++) {
        if (m_bdls.find(i) == m_bdls.end()) {
            index = i;
            break;
        }
    }
#else
    // MODE: incremental index
    static int index = -1;
    index++;
#endif

    m_bdls.emplace(index, z3Bundle(this, ctx));
    return index;
}

void z3Manager::del_context(int index) {
    m_bdls.erase(index);
}