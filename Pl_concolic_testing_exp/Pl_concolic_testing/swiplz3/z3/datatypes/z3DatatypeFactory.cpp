#include "z3DatatypeFactory.h"

//#include <algorithm>
#include <sstream>
#include <regex>

using namespace std;

void z3DatatypeFactory::Datatype::add_constructor(string name, vector<string> accnames, vector<string> acctypes, string tname) {
    if (tname == "")
    {
    	stringstream tss;
    	tss << "is_" << name;
    	tname = tss.str();
    }
    for (string& acctype : acctypes) {
    	m_df->mk_system_sort(acctype);
	}
    ctors.push_back({ name, tname, accnames, acctypes });
}
void z3DatatypeFactory::mk_system_sort(string name) {
	if (m_dtm.m_systypenames.find(name) != m_dtm.m_systypenames.end()) return; // existing type

	z3DatatypeFactory& df = *this;
    if (smatch tplargs; regex_match(name, tplargs, regex("^List<([a-zA-Z<,>]+)>$"))) { // regex("^\\(List ([a-zA-Z \\(\\)]+)\\)$")
        //cout << "List: " << tplargs[1] << endl;
        m_dtm.m_systypenames.insert(name);
    	
    	df.add_datatype(name);
    	df[name].add_constructor("nil", {}, {});
    	df[name].add_constructor("insert", { "head", "tail" }, { tplargs[1], name });
    }
    else if (smatch tplargs; regex_match(name, tplargs, regex("^Pair<([a-zA-Z<,>]+),([a-zA-Z<,>]+)>$"))) { //regex("^\\(Pair ([a-zA-Z \\(\\)]+) ([a-zA-Z \\(\\)]+)\\)$")
        //cout << "Pair: " << tplargs[1] << ", " << tplargs[2] << endl;
        m_dtm.m_systypenames.insert(name);

        df.add_datatype(name);
        df[name].add_constructor("mk-pair", { "first", "second" }, { tplargs[1], tplargs[2] });
    }
}
pair<vector<Z3_sort>, vector<unsigned>> z3DatatypeFactory::get_types_desc(vector<string> names)
{
    vector<Z3_sort> sorts;
    vector<unsigned> refs;

    for (string name : names) {
        try { // existing global types
        	sorts.emplace_back(m_dtm.get_datatype(name).sort);
        	refs.emplace_back(0);
        }
        catch (exception& e) { // existing local types
        	bool ok = false;
	        for (int i = 0; i < m_dts.size(); ++i)
	        {
	            if (name == m_dts[i].name)
	            {
	            	sorts.push_back(0);
	                refs.push_back(i);
	                ok = true;
	        	}
	        }
	        if (!ok) { // unrecognized types
	        	cout << "datatype: " << name << endl;
	        	throw runtime_error("unrecognized datatype!");
	        }
        }
    }
    return { sorts, refs };
}
z3DatatypeFactory::z3DatatypeFactory(z3DatatypeHolder& dtm) : m_dtm(dtm), m_dts() {}
void z3DatatypeFactory::add_datatype(string name) {
    m_dts.push_back({ this, name, {}}); //m_dts.push_back({ this, m_dtm.m_ctx->str_symbol(name.c_str()), {} });
}
z3DatatypeFactory::Datatype& z3DatatypeFactory::operator[](string name) {
    for (auto &dt : m_dts) if (name == dt.name) return dt;
    throw runtime_error("unexpected typename!");
}
vector<z3Datatype> z3DatatypeFactory::mk_datatypes() {
	/*for (auto& dt : m_dts) {
        cout << "dt: " << dt.name << endl;
        for (auto& ct : dt.ctors) {
            cout << "ct: " << ct.name << endl;
            for (unsigned i = 0; i < ct.accnames.size(); i++) {
                cout << "acc: " << ct.accnames[i] << " -> " << ct.acctypes[i] << endl;
            }
        }
    }*/

	auto ctx = m_dtm.m_ctx;
	unsigned t = 0, numtype = m_dts.size();

	Z3_symbol sort_names[numtype];
	vector<Z3_constructor> consvs[numtype];

	vector<Z3_constructor_list> constructors;
    for (z3DatatypeFactory::Datatype& dt : m_dts) {
        sort_names[t] = Z3_mk_string_symbol(*ctx, dt.name.c_str());
        for (z3DatatypeFactory::Datatype::Constructor& ct : dt.ctors) {
        	auto [sorts, sort_refs] = get_types_desc(ct.acctypes);

        	Z3_symbol zname = Z3_mk_string_symbol(*ctx, ct.name.c_str());
        	Z3_symbol ztname = Z3_mk_string_symbol(*ctx, ct.tname.c_str());
        	int numaccs = ct.accnames.size();

        	vector<Z3_symbol> zaccnames;
    		for (string accname : ct.accnames) zaccnames.push_back(Z3_mk_string_symbol(*ctx, accname.c_str()));

        	Z3_constructor cons;
    		if (numaccs == 0) cons = Z3_mk_constructor(*ctx, zname, ztname, numaccs, 0, 0, 0);
    		else cons = Z3_mk_constructor(*ctx, zname, ztname, numaccs, zaccnames.data(), sorts.data(), sort_refs.data());
            consvs[t].push_back(cons);
        }
        constructors.push_back(Z3_mk_constructor_list(*ctx, consvs[t].size(), consvs[t].data()));
        t++;
    }

    Z3_sort sorts[numtype];
    Z3_mk_datatypes(*ctx, numtype, sort_names, sorts, constructors.data());

    for (Z3_constructor_list ctlist : constructors) Z3_del_constructor_list(*ctx, ctlist);

    vector<z3Datatype> rsorts;
    unsigned az = 0;
    for (z3DatatypeFactory::Datatype& dt : m_dts) {
        vector<z3Datatype::z3Constructor> ctors;
        unsigned az2 = 0;
        for (z3DatatypeFactory::Datatype::Constructor& ct : dt.ctors) {
            unsigned numaccs = ct.accnames.size();

            Z3_func_decl ctor;
            Z3_func_decl tst;
            Z3_func_decl accs[numaccs];

            Z3_query_constructor(*ctx, consvs[az].data()[az2], numaccs, &ctor, &tst, accs);

            vector<z3Datatype::z3Constructor::z3Accessor> waccs;
            for (Z3_func_decl acc : accs) waccs.push_back({ z3::func_decl(*ctx, acc) });

            ctors.push_back({ z3::func_decl(*ctx, ctor), z3::func_decl(*ctx, tst), waccs });
            az2++;
        }
        rsorts.push_back({ z3::sort(*ctx, sorts[az++]), ctors });
    }

    //cout << "new available datatypes: ";
    for (z3Datatype rsort : rsorts) {
    	string rname = rsort.sort.name().str();
        //cout << rname << " ";
        m_dtm.m_usertypes.emplace(rname, rsort);
    }
    //cout << endl;

    return rsorts;
}