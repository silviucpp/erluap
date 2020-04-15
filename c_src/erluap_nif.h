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
    ERL_NIF_TERM atomUnknown;
    ERL_NIF_TERM atomDesktop;
    ERL_NIF_TERM atomMobile;
    ERL_NIF_TERM atomTablet;
};

extern atoms ATOMS;

#endif
