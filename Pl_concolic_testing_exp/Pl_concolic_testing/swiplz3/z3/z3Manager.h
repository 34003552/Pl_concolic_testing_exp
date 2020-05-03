
#include <vector>
#include <z3.h>

class z3Manager
{
	class z3Bundle;

	Z3_config m_cfg;
	std::vector<z3Bundle> m_bdls;

	static void error_handler(Z3_context c, Z3_error_code e);

	z3Manager();
	//~z3Manager();
public:
	~z3Manager();
	z3Manager(const z3Manager&) = delete;
	z3Manager& operator=(const z3Manager&) = delete;

	static z3Manager& get_manager();

	z3Bundle& get_bundle(int index);

	void mk_config();
	void del_config();

	void set_param_value(const char *param_id, const char *param_value);

	int mk_context();
	void del_context(int index);
};