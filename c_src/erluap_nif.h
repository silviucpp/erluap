#ifndef C_SRC_ERLUAP_NIF_H_
#define C_SRC_ERLUAP_NIF_H_

#include "erl_nif.h"

struct atoms
{
    ERL_NIF_TERM atomNull;
    ERL_NIF_TERM atomError;
    ERL_NIF_TERM atomBadArg;
    ERL_NIF_TERM atomDevice;
    ERL_NIF_TERM atomAgent;
};

extern atoms ATOMS;

#endif
