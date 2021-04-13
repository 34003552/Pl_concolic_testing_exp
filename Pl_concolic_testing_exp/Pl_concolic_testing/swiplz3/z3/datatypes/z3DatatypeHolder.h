
#ifndef Z3DATATYPEHOLDER_H
#define Z3DATATYPEHOLDER_H

#include <map>
#include <set>
#include <z3++.h>

#include "z3Datatype.h"

class z3DatatypeHolder;
#include "z3DatatypeFactory.h"

class z3DatatypeHolder {
	friend class z3DatatypeFactory;

	z3::context *m_ctx;

	std::map<std::string, z3Datatype> m_usertypes;
	std::set<std::string> m_systypenames;

	z3::sort get_builtin_sort(std::string name);
public:
	z3DatatypeHolder(z3::context *ctx);

	z3Datatype get_datatype(std::string name);

	z3::sort_vector get_sort_vector();
};

#endif