#include "z3Datatype.h"

//z3Datatype::z3Datatype(z3::sort s, vector<z3Constructor> cts) : sort(s), ctors(cts) {}
z3Datatype::operator z3::sort() { return sort; }