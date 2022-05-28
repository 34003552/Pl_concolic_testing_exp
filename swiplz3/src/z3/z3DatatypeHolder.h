#ifndef Z3DATATYPEHOLDER_H
#define Z3DATATYPEHOLDER_H

#include <string>
#include <vector>
#include <deque>
#include <map>
#include <optional>
#include <functional>
#include <z3++.h>

#include "../ScopedContainer.h"

struct z3Datatype : public z3::sort {
    struct z3Constructor {
        const z3::func_decl ctor;
        const z3::func_decl tst;

        struct z3Accessor {
            const z3::func_decl acc;
        };
        const std::vector<z3Accessor> accs;
    };
    const std::vector<z3Constructor> ctors;
};

class z3DatatypeHolder {
public:
    class z3DatatypeFactory;
private:
    using splitted_typename_t = std::pair<std::string, std::vector<std::string>>;
    using systype_maker_t = std::function<void(z3DatatypeFactory&, std::string, std::vector<std::string>)>;
    
    z3::context *m_ctx;

    ScopedContainer<std::deque<z3DatatypeFactory>> m_dtfactories;
    ScopedContainer<std::map<std::string, z3Datatype>> m_knowntypes;
    ScopedContainer<std::map<std::string, std::string>> m_knowntypealiases;

    static bool split_typename(const std::string& name, splitted_typename_t& names);
    static std::optional<systype_maker_t> get_systype_maker(const std::string& name, unsigned arity);

    std::optional<z3::sort> get_builtin_sort(const std::string& name) const;
    bool mk_system_type(const std::string& name);

    std::optional<z3Datatype> find_datatype(std::string name);
    std::optional<z3Datatype> build_datatype(std::string name);
public:
    z3DatatypeHolder(z3::context *ctx);

    bool match_type_alias(std::string& name) const;
    z3Datatype get_datatype(std::string name);

    z3DatatypeFactory& register_factory(z3DatatypeFactory&& df);

    void mk_uninterpreted_type(std::string name);
    void mk_type_alias(std::string name, std::string base);

    void fill_sort_vector(z3::sort_vector& svec) const;

    void backup_scope();
    void restore_scope(unsigned s);
};

#include "z3DatatypeFactory.hxx"

#endif