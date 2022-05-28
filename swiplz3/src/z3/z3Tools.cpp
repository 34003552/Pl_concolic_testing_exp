#include "z3Tools.h"

z3Exception::z3Exception(Z3_error_code ec, const char *msg) : z3::exception(msg), m_ec(ec) {}

Z3_error_code z3Exception::error_code() const { return m_ec; }