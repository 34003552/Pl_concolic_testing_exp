
//#include "z3Manager.h"

//#include "z3Tools.h"
#include "z3Bundle.h"

extern "C" void pl_iolog__init();
z3Manager::z3Manager() :
m_cfg(), m_bdls()
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
	return m_bdls[index];
}
void z3Manager::mk_config()
{
    m_cfg = Z3_mk_config();
}
void z3Manager::del_config()
{
    Z3_del_config(m_cfg);
}
void z3Manager::set_param_value(const char *param_id, const char *param_value)
{
    Z3_set_param_value(m_cfg, param_id, param_value);
}
/*
int z3Manager::mk_context()
{
	int index = m_bdls.count();
	for (unsigned i = 0; i < m_bdls.count(); i++) // find an unused index
	{
		if (m_bdls.find(i) != m_bdls.end())
		{
			index = i;
			break;
		}
	}
	z3Bundle bdl;
	m_bdls.emplace(index, env);
	return index;
}
*/
int z3Manager::mk_context()
{
	Z3_context ctx = Z3_mk_context(m_cfg);
    Z3_set_error_handler(ctx, error_handler);

    m_bdls.emplace_back(this, ctx);
    return m_bdls.size() - 1;
}
void z3Manager::del_context(int index)
{
	Z3_del_context(m_bdls[index].get_context());
}

extern "C" void pl_error_handler(const char *error);
void z3Manager::error_handler(Z3_context c, Z3_error_code e)
{
    printf("Error: %s\n\n", Z3_get_error_msg(c, e));

    char *error = NULL;
    switch(e)
    {
        case Z3_OK:
            error = (char*)"Z3_OK";
            break;
        case Z3_SORT_ERROR:
            error = (char*)"Z3_SORT_ERROR";
            break;
        case Z3_IOB:
            error = (char*)"Z3_IOB";
            break;
        case Z3_INVALID_ARG:
            error = (char*)"Z3_INVALID_ARG";
            break;
        case Z3_PARSER_ERROR:
            error = (char*)"Z3_PARSER_ERROR";
            break;
        case Z3_NO_PARSER:
            error = (char*)"Z3_NO_PARSER";
            break;
        case Z3_INVALID_PATTERN:
            error = (char*)"Z3_INVALID_PATTERN";
            break;
        case Z3_MEMOUT_FAIL:
            error = (char*)"Z3_MEMOUT_FAIL";
            break;
        case Z3_FILE_ACCESS_ERROR:
            error = (char*)"Z3_FILE_ACCESS_ERROR";
            break;
        case Z3_INTERNAL_FATAL:
            error = (char*)"Z3_INTERNAL_FATAL";
            break;
        case Z3_INVALID_USAGE:
            error = (char*)"Z3_INVALID_USAGE";
            break;
        case Z3_DEC_REF_ERROR:
            error = (char*)"Z3_DEC_REF_ERROR";
            break;
        case Z3_EXCEPTION:
            error = (char*)"Z3_EXCEPTION";
            break;
        default:
            error = (char*)"Z3 BUG: unknown error";
    }
    pl_error_handler(error);
}