
PROJECT_NIF_NAME=erluap_nif

include nif.mk

ifeq ($(UNAME_SYS), darwin)
    YAML_CPP_ROOT := $(shell brew --prefix yaml-cpp)

    CXXFLAGS += -I$(YAML_CPP_ROOT)/include
    LDFLAGS  += -L$(YAML_CPP_ROOT)/lib

	LDFLAGS +=  -Wl,-U,_enif_inspect_binary \
				-Wl,-U,_enif_inspect_iolist_as_binary \
				-Wl,-U,_enif_is_binary \
				-Wl,-U,_enif_is_identical \
				-Wl,-U,_enif_make_atom \
				-Wl,-U,_enif_make_existing_atom \
				-Wl,-U,_enif_make_new_binary \
				-Wl,-U,_enif_make_tuple
endif

CXXFLAGS += -I ../deps/uap-cpp -DNDEBUG -g -Wextra -Werror -Wno-missing-field-initializers -std=c++11
LDFLAGS += -L ../deps/uap-cpp -luaparser_cpp -lre2 -lyaml-cpp -lstdc++
