-module(erluap_nif).

-define(NOT_LOADED, not_loaded(?LINE)).
-define(REGEXES_FILE, <<"regexes.yaml">>).

-on_load(load_nif/0).

-export([
    parse/1
]).

%% nif functions

load_nif() ->
    SoName = get_priv_path(?MODULE),
    Regexes = get_priv_path(?REGEXES_FILE),

    case filelib:file_size(Regexes) > 0 of
        true ->
            io:format(<<"Loading library: ~p~n">>, [SoName]),
            io:format(<<"Use regexp file: ~p~n">>, [Regexes]),
            ok = erlang:load_nif(SoName, Regexes);
        _ ->
            throw({error, <<"Regex file not available: ", Regexes/binary>>})
    end.

get_priv_path(File) ->
    case code:priv_dir(erl_uap) of
        {error, bad_name} ->
			Ebin = filename:dirname(code:which(?MODULE)),
			filename:join([filename:dirname(Ebin), "priv", File]);
        Dir ->
            filename:join(Dir, File)
    end.

not_loaded(Line) ->
    erlang:nif_error({not_loaded, [{module, ?MODULE}, {line, Line}]}).

parse(_UserAgent) ->
    ?NOT_LOADED.


