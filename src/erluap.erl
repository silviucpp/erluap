-module(erluap).

-include("erluap.hrl").

-export([
    parse/1,
    is_spider/1
]).

-spec parse(binary() | iolist()) ->
    {device(), os(), browser()}.

parse(UserAgent) ->
    erluap_nif:parse(UserAgent).

-spec is_spider(device()) ->
    boolean().

is_spider(#device{family = F}) ->
    F == <<"Spider">>.
