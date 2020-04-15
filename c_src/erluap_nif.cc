#include "erluap_nif.h"
#include "nif_utils.h"
#include "UaParser.h"

#define UNUSED(expr) do { (void)(expr); } while (0)

atoms ATOMS;

namespace {

constexpr const char kAtomNull[] = "null";
constexpr const char kAtomError[] = "error";
constexpr const char kAtomBadArg[] = "badarg";
constexpr const char kAtomDevice[] = "device";
constexpr const char kAtomAgent[] = "agent";
constexpr const char kAtomUnknown[] = "unknown";
constexpr const char kAtomDesktop[] = "desktop";
constexpr const char kAtomMobile[] = "mobile";
constexpr const char kAtomTablet[] = "tablet";

uap_cpp::UserAgentParser* uap_ = nullptr;

ERL_NIF_TERM devicetype2term(uap_cpp::DeviceType device_type)
{
    switch(device_type)
    {
        case uap_cpp::DeviceType::kUnknown:
            return ATOMS.atomUnknown;

        case uap_cpp::DeviceType::kDesktop:
            return ATOMS.atomDesktop;

        case uap_cpp::DeviceType::kMobile:
            return ATOMS.atomMobile;

        case uap_cpp::DeviceType::kTablet:
            return ATOMS.atomTablet;
    }
}

}

int on_nif_load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
    UNUSED(priv_data);

    ATOMS.atomNull = make_atom(env, kAtomNull);
    ATOMS.atomError = make_atom(env, kAtomError);
    ATOMS.atomBadArg = make_atom(env, kAtomBadArg);
    ATOMS.atomDevice = make_atom(env, kAtomDevice);
    ATOMS.atomAgent = make_atom(env, kAtomAgent);
    ATOMS.atomUnknown = make_atom(env, kAtomUnknown);
    ATOMS.atomDesktop = make_atom(env, kAtomDesktop);
    ATOMS.atomMobile = make_atom(env, kAtomMobile);
    ATOMS.atomTablet = make_atom(env, kAtomTablet);

    std::string regexes_path;

    if(!get_string(env, load_info, &regexes_path))
        return 1;

    try
    {
        uap_ = new uap_cpp::UserAgentParser(regexes_path);
    }
    catch (...)
    {
        return 1;
    }

    return 0;
}

ERL_NIF_TERM make_bin_or_null(ErlNifEnv* env, const std::string& v)
{
    if(v.empty())
        return ATOMS.atomNull;

    return make_binary(env, v.c_str(), v.length());
}

ERL_NIF_TERM create_device(ErlNifEnv* env, uap_cpp::DeviceType device_type, const uap_cpp::Device& d)
{
    return enif_make_tuple5(env,
                            ATOMS.atomDevice,
                            devicetype2term(device_type),
                            make_bin_or_null(env, d.family),
                            make_bin_or_null(env, d.model),
                            make_bin_or_null(env, d.brand));
}

ERL_NIF_TERM create_agent(ErlNifEnv* env, const uap_cpp::Agent& a)
{
    return enif_make_tuple6(env,
                            ATOMS.atomAgent,
                            make_bin_or_null(env, a.family),
                            make_bin_or_null(env, a.major),
                            make_bin_or_null(env, a.minor),
                            make_bin_or_null(env, a.patch),
                            make_bin_or_null(env, a.patch_minor));
}

ERL_NIF_TERM nif_parse(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    UNUSED(argc);

    std::string user_agent;

    if(!get_string(env, argv[0], &user_agent))
        return make_badarg(env);

    if(!uap_)
        return make_error(env, "user agent parser not instantiated");

    uap_cpp::DeviceType device_type = uap_cpp::UserAgentParser::device_type(user_agent);
    uap_cpp::UserAgent ua = uap_->parse(user_agent);
    return enif_make_tuple3(env, create_device(env, device_type, ua.device), create_agent(env, ua.os), create_agent(env, ua.browser));
}

static ErlNifFunc nif_funcs[] = {
    {"parse", 1, nif_parse},
};

ERL_NIF_INIT(erluap_nif, nif_funcs, on_nif_load, NULL, NULL, NULL)
