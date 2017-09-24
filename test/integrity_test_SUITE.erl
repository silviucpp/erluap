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
        run_browser_tests,
        catch_boost_exception
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

catch_boost_exception(_Config) ->
    Ua = <<"Mozilla/5.0 (Windows; U; Windows NT 6.1; en-US) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/33.0.1750.170 Safari/537.36 LPWebKit/6.0.0.6 (589c53ae-0000d9aa-00004a66-2f5ff39f-814c0581.389663962456653959.PFN5c3RlbUZpbmdlcnByaW50IFZpZGVvQ2FyZElkPSIyWGF0OUpuWmdvNFhHNmU2QkNab1FDaDF2YlpKYndTTmZIcHZiTmRUYmxJPSIgTmV0d29ya0NhcmRJZD0ic1R6Q2RNZzZiMXRVOVgyQmtpNk1aakdna25BY25XSVEySVlhTmtzVHFOND0iIE5ldHdvcmtHYXRld2F5SWQ9ImhrOVY5OEp5R2NaTFBsYWd1eUNnTGpEWjlFV0d2bnNNTHFuMUpPQXl6SHM9IiBIYXJkcml2ZUlkPSJhczlTb0NtQ0J1RlNHS2x6UGNlZ2tSTDJ0b2FJcW9rRENzL0lJT3pveENrPSIgQ29tcHV0ZXJOYW1lPSJ6aFd0ZXIxUFppdFJYNmxCb0tvSmZ5TFZaYkZadlNzWi9iRERPZjl3eUJRPSI-PE5ldHdvcmtBZGFwdGVycyBJc0xpc3Q9IjEiPjxOZXR3b3JrQWRhcHRlciBJZD0ic1R6Q2RNZzZiMXRVOVgyQmtpNk1aakdna25BY25XSVEySVlhTmtzVHFOND0iLz48L05ldHdvcmtBZGFwdGVycz48SGFyZERyaXZlcyBJc0xpc3Q9IjEiPjxIYXJkRHJpdmUgVW5pcXVlSWQ9IlVsOFFuUUVJaWtXMDhDUGtnaHRjdDdXakpGN2pHei9FclV4MVVER2lyMHM9IiBNb2RlbElkPSJpNzVtc2pkZ2ZTZHRCUU84QnZnOUFuZGVYZzB2WFg1SUx3WGttSWNMcmd3PSIgUmV2aXNpb25JZD0ibEowNzc3UGpsMWtLbWZOOVJUd2cxTncyTWpIUHAyRCszRXk0djJkbHVHRT0iIFZlbmRvcklkPSJhQnBITkNBNjI5MHFFV0VVeXNSRGsxWnYzaVFWblRIRU1Kcllzc0xIQkhBPSIgU2l6ZUlkPSJvUFQ3NzlqYlNtbUROUlZ4SzlKa2xjdEtIQXc5V1NtOUxiYUpBMC9uU3ZvPSIvPjwvSGFyZERyaXZlcz48L1N5c3RlbUZpbmdlcnByaW50Pg==)">>,
    {error, <<"The complexity of matching the regular expression exceeded predefined bounds.", _/binary>>} = erluap:parse(Ua).

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
