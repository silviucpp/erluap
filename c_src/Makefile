
PROJECT_NIF_NAME=erluap_nif

include nif.mk

#$(brew --prefix openssl)

ifeq ($(UNAME_SYS), darwin)
    BOOST_ROOT := $(shell brew --prefix boost)
    YAML_CPP_ROOT := $(shell brew --prefix yaml-cpp)

    CXXFLAGS += -I$(BOOST_ROOT)/include -I$(YAML_CPP_ROOT)/include
    LDFLAGS  += -L$(BOOST_ROOT)/lib -L$(YAML_CPP_ROOT)/lib

	LDFLAGS +=  -Wl,-U,_enif_inspect_binary \
				-Wl,-U,_enif_inspect_iolist_as_binary \
				-Wl,-U,_enif_is_binary \
				-Wl,-U,_enif_is_identical \
				-Wl,-U,_enif_make_atom \
				-Wl,-U,_enif_make_existing_atom \
				-Wl,-U,_enif_make_new_binary \
				-Wl,-U,_enif_make_tuple
endif

CXXFLAGS += -DNDEBUG -g -Wextra -Werror -Wno-missing-field-initializers -fno-rtti -std=c++11
LDFLAGS += -lboost_regex -lyaml-cpp -lstdc++
