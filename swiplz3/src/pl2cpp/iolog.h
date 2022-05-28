#ifndef IOLOG_H
#define IOLOG_H

#define USE_IOLOG

typedef enum { IOLOG_IN = 1, IOLOG_OUT = 2, IOLOG_INOUT = 3 } iolog_mode;

void pl_iolog__init();
void pl_iolog__printf(const char *fmt, ...);
void pl_iolog__del();

#define pl_iolog__PRINT_PRED(NAME, ARITY) pl_iolog__printf("%s/%d\n", NAME, ARITY)
#define pl_iolog__PRINT_PRED_RET(RET) pl_iolog__printf(RET ? "" : "\t=> fail\n")

char *iolog_strbuf;
#define pl_iolog__PRINT_PRED_ARG(MODE, ARGNAME, ARGVAL)\
    iolog_strbuf = to_chars(ARGVAL);\
    pl_iolog__printf("\t%s " ARGNAME ": %s = %s\n",\
        MODE == IOLOG_IN ? "+" : (MODE == IOLOG_OUT ? "-" : "?"),\
        typename(*ARGVAL), iolog_strbuf\
    );\
    free(iolog_strbuf)

#endif