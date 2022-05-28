#ifndef Z3TOOLS_H
#define Z3TOOLS_H

#include <z3++.h>

class z3Exception : public z3::exception {
    Z3_error_code m_ec;
public:
    z3Exception(Z3_error_code ec, const char *msg);

    Z3_error_code error_code() const;
};

#endif