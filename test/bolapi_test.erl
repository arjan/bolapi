-module(bolapi_test).

-compile(export_all).


api_test() ->
    ok = application:start(bolapi),
    ok = application:start(inets),
    ok = application:start(public_key),
    ok = application:start(ssl),
    pong = bolapi:ping(),
    {ok, XML} = bolapi:get_product("1002004000092913"),
    xmlElement = element(1, XML),
    ok.



    


