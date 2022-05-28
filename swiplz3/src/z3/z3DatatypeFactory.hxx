#ifndef Z3DATATYPEFACTORY_H
#define Z3DATATYPEFACTORY_H

#include <string>
#include <vector>
#include <deque>
#include <z3++.h>

class z3DatatypeHolder::z3DatatypeFactory {
    struct Datatype {
        z3DatatypeFactory *m_df;

        std::string name;
        struct Constructor {
            std::string name;
            std::string tname;
            std::vector<std::string> accnames;
            std::vector<std::string> acctypes;
        };
        std::deque<Constructor> ctors;

        void add_constructor(std::string name, std::vector<std::string> accnames, std::vector<std::string> acctypes, std::string tname = "");
    };

    z3DatatypeHolder* m_dth;
    
    std::vector<std::string> m_tplvars;
    std::deque<Datatype> m_dts;

    bool add_system_type(const std::string& name);
    std::string substitute_template(const std::string& name, const std::vector<std::string>& tplnames) const;
    std::pair<std::vector<Z3_sort>, std::vector<unsigned>> get_types_desc(const std::vector<std::string>& names, const std::vector<std::string>& tplnames);
public:
    z3DatatypeFactory(z3DatatypeHolder* dth, std::vector<std::string> tplvars);

    void add_datatype(std::string name);
    bool has_datatype(const std::string& name) const;
    Datatype& operator[](const std::string& name);

    std::vector<z3Datatype> mk_datatypes(const std::vector<std::string>& tpltypes);
};

#endif