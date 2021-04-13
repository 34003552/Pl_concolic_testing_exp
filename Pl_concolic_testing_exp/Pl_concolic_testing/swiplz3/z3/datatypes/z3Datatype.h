
#ifndef Z3DATATYPE_H
#define Z3DATATYPE_H

#include <vector>
#include <z3++.h>

struct z3Datatype
{
	const z3::sort sort;

	struct z3Constructor
	{
		const z3::func_decl ctor;
		const z3::func_decl tst;

		struct z3Accessor
		{
			const z3::func_decl acc;
		};
		const std::vector<z3Accessor> accs;
	};
	const std::vector<z3Constructor> ctors;

    operator z3::sort();

	//z3Datatype(z3::sort s, std::vector<z3Constructor> cts);
};

#endif