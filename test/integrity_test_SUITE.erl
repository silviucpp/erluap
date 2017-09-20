-module(integrity_test_SUITE).

-include("erluap.hrl").

-define(TEST_DEVICE, <<"../../deps/uap-core/tests/test_device.yaml">>).
-define(TEST_OS, <<"../../deps/uap-core/tests/test_os.yaml">>).
-define(TEST_UA, <<"../../deps/uap-core/tests/test_ua.yaml">>).

-compile(export_all).

all() -> [
    {group, useragents}
].

groups() -> [
    {useragents, [sequence], [
        run_devices_tests,
        run_os_tests,
        run_browser_tests
    ]}
].

init_per_suite(Config) ->
    {ok,  _} = application:ensure_all_started(yamerl),
    Config.

end_per_suite(_Config) ->
    application:stop(yamerl).

run_devices_tests(_Config) ->
    {ok, Tests} = load_testing_user_agents(?TEST_DEVICE),

    Fun = fun(T) ->
        Family = to_val(proplists:get_value("family", T)),
        Brand = to_val(proplists:get_value("brand", T)),
        Model = to_val(proplists:get_value("model", T)),
        Ua = proplists:get_value("user_agent_string", T),

        case catch list_to_binary(Ua) of
            UaBin when is_binary(UaBin) ->
                {#device{family = Family, brand = Brand, model = Model}, _Os, _Browser} = erluap:parse(Ua);
            _ ->
                % https://github.com/yakaz/yamerl/issues/22
                % skip some user agents which contains non ascii chars because yamerl has a problem with those
                ok
        end
          end,
    ok = lists:foreach(Fun, Tests),
    true.

run_os_tests(_Config) ->
    run_agent_tests(?TEST_OS, os).

run_browser_tests(_Config) ->
    run_agent_tests(?TEST_UA, browser).

% internals

load_testing_user_agents(File) ->
    application:ensure_all_started(yamerl),
    [[{"test_cases", Tests}]] = yamerl:decode_file(File),
    {ok,Tests}.

run_agent_tests(File, Type) ->
    {ok, Tests} = load_testing_user_agents(File),

    Fun = fun(T) ->
        Ua = proplists:get_value("user_agent_string", T),
        Family = to_val(proplists:get_value("family", T)),
        Major = to_val(proplists:get_value("major", T)),
        Minor = to_val(proplists:get_value("minor", T)),
        Patch = to_val(proplists:get_value("patch", T)),
        PatchMinor = to_val(proplists:get_value("patch_minor", T, null)),

        case catch list_to_binary(Ua) of
            UaBin when is_binary(UaBin) ->
                {_Device, Os, Browser} = erluap:parse(Ua),

                Agent = case Type of
                            os ->
                                Os;
                            browser ->
                                Browser
                        end,

                #agent{
                    family = Family,
                    version_major = Major,
                    version_minor = Minor,
                    version_patch = Patch,
                    version_patch_minor = PatchMinor
                } = Agent;
            _ ->
                % https://github.com/yakaz/yamerl/issues/22
                % skip some user agents which contains non ascii chars because yamerl has a problem with those
                ok
        end
          end,
    ok = lists:foreach(Fun, Tests),
    true.

to_val(null) ->
    null;
to_val(V) ->
    case list_to_binary(V) of
        <<>> ->
            null;
        Val ->
            Val
    end.
