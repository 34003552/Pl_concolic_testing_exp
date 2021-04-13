
//#include "z3Manager.h"

//#include "z3Tools.h"
#include "../cppbridge.h"
#include "z3Bundle.h"

using namespace std;

extern "C" void pl_iolog__init();
z3Manager::z3Manager() :
m_cfg(nullptr), m_bdls()
{
	pl_iolog__init();
}

extern "C" void pl_iolog__del();
z3Manager::~z3Manager()
{
	pl_iolog__del();
}

z3Manager& z3Manager::get_manager()
{
	static z3Manager manager;
	return manager;
}

z3Manager::z3Bundle& z3Manager::get_bundle(int index)
{
	return z3Manager::get_manager().m_bdls.at(index);
}

void z3Manager::mk_config()
{
	m_cfg = new z3::config();
}

void z3Manager::del_config()
{
	delete m_cfg;
}

void z3Manager::set_param_value(const char *param_name, const char *param_value)
{
	m_cfg->set(param_name, param_value);
}

int z3Manager::mk_context()
{
	z3::context* ctx = new z3::context(*m_cfg);
    //Z3_set_error_handler(*ctx, error_handler);

    /*
    // MODE: next available index
    int index = 0;
    for (int i = 0; i <= m_bdls.size(); i++) {
    	if (m_bdls.find(i) == m_bdls.end()) {
    		index = i;
    		break;
    	}
    }
    */
    // MODE: incremental index
    static int index = -1;
    index++;

    m_bdls.emplace(index, z3Bundle(this, ctx));
    return index;
}

void z3Manager::del_context(int index)
{
	//delete m_bdls.at(index).m_ctx;

	m_bdls.erase(index);
}

/*void z3Manager::error_handler(Z3_context c, Z3_error_code e)
{
    //printf("Error: %s\n\n", Z3_get_error_msg(c, e));
    string msg = Z3_get_error_msg(c, e);

    string error;
    switch(e)
    {
        case Z3_OK:
            error = "Z3_OK";
            break;
        case Z3_SORT_ERROR:
            error = "Z3_SORT_ERROR";
            break;
        case Z3_IOB:
            error = "Z3_IOB";
            break;
        case Z3_INVALID_ARG:
            error = "Z3_INVALID_ARG";
            break;
        case Z3_PARSER_ERROR:
            error = "Z3_PARSER_ERROR";
            break;
        case Z3_NO_PARSER:
            error = "Z3_NO_PARSER";
            break;
        case Z3_INVALID_PATTERN:
            error = "Z3_INVALID_PATTERN";
            break;
        case Z3_MEMOUT_FAIL:
            error = "Z3_MEMOUT_FAIL";
            break;
        case Z3_FILE_ACCESS_ERROR:
            error = "Z3_FILE_ACCESS_ERROR";
            break;
        case Z3_INTERNAL_FATAL:
            error = "Z3_INTERNAL_FATAL";
            break;
        case Z3_INVALID_USAGE:
            error = "Z3_INVALID_USAGE";
            break;
        case Z3_DEC_REF_ERROR:
            error = "Z3_DEC_REF_ERROR";
            break;
        case Z3_EXCEPTION:
            error = "Z3_EXCEPTION";
            break;
        default:
            error = "Z3 BUG: unknown error";
    }
    cpp__z3_error_handler(error.c_str(), msg.c_str());
}*/