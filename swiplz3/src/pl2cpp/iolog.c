#include "iolog.h"

#ifndef USE_IOLOG
void pl_iolog__init() {}
void pl_iolog__printf(const char *fmt, ...) {}
void pl_iolog__del() {}
#else
#include <time.h>
#include <stdarg.h>
#include <stdio.h>

FILE *iolog_fd;

void pl_iolog__init() {
    iolog_fd = fopen("swiplz3.log", "w");
    fprintf(iolog_fd, "---------------------------------------\n");
    fprintf(iolog_fd, "swiplz3 I/O logfile\n");
    fprintf(iolog_fd, "---------------------------------------\n");

    time_t now; time(&now);
    fprintf(iolog_fd, "Execution started on %s", ctime(&now));
}
void pl_iolog__printf(const char *fmt, ...) {
    va_list args;
    va_start(args, fmt);
    vfprintf(iolog_fd, fmt, args);
    va_end(args);
}
void pl_iolog__del() {
    time_t now; time(&now);
    fprintf(iolog_fd, "Execution stopped on %s", ctime(&now));
    fclose(iolog_fd);
}
#endif