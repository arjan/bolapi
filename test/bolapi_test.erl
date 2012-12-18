-module(bolapi_test).

-include_lib("bolapi/include/bolapi.hrl").

-compile(export_all).

setup() ->
    application:start(bolapi),
    application:start(lager),
    application:start(inets),
    application:start(public_key),
    application:start(ssl).
    
ping_test() ->
    setup(),
    pong = bolapi:ping().

products_test() ->
    setup(),
    {ok, XML} = bolapi:products("1002004000092913"),
    xmlElement = element(1, XML),
    ok.

searchresults_test() ->
    setup(),
    {ok, XML} = bolapi:searchresults(#bolparams{term="nemo"}),
    xmlElement = element(1, XML),
    ok.

