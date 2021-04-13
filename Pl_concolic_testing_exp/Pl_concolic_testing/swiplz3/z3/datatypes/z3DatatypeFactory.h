
#ifndef Z3DATATYPEFACTORY_H
#define Z3DATATYPEFACTORY_H

#include <cstring>
#include <z3++.h>
#include <vector>
//#include <utility>
#include <optional>
#include <deque>

//#include "z3Manager.h"
//#include "z3Bundle.h"
class z3DatatypeFactory;
#include "z3DatatypeHolder.h"

class z3DatatypeFactory
{
	struct Datatype
    {
    	z3DatatypeFactory *m_df;

        std::string name;
        struct Constructor
        {
            std::string name;
            std::string tname;
            std::vector<std::string> accnames;
            std::vector<std::string> acctypes;
        };
        std::vector<Constructor> ctors;

        void add_constructor(std::string name, std::vector<std::string> accnames, std::vector<std::string> acctypes, std::string tname = "");
    };

    z3DatatypeHolder& m_dtm;
    

    void mk_system_sort(std::string name);
	std::pair<std::vector<Z3_sort>, std::vector<unsigned>> get_types_desc(std::vector<std::string> name);
public:
	std::deque<Datatype> m_dts;
    z3DatatypeFactory(z3DatatypeHolder& dtm);

    void add_datatype(std::string name);
    Datatype& operator[](std::string name);
    std::vector<z3Datatype> mk_datatypes();
};

#endif