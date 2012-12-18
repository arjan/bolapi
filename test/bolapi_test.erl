-module(bolapi_test).

-compile(export_all).


api_test() ->
    ok = application:start(bolapi),
    ok = application:start(inets),
    ok = application:start(public_key),
    ok = application:start(ssl),
    bolapi:ping(),

    X = bolapi:get_product("1002004000092913"),
    io:format("~p", [X]),
    1=2.
   



    


