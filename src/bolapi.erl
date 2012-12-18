-module(bolapi).
-include_lib("bolapi/include/bolapi.hrl").

-export([
         ping/0,
         get_product/1,
         search/1
        ]).


-define(API_BASE, "https://openapi.bol.com").

ping() ->
    case request(get, "/openapi/services/rest/utils/v3/ping", []) of
        {ok, []} ->
            pong;
        {error, R} ->
            {error, R}
    end.

get_product(Id) ->
    request(get, "/openapi/services/rest/catalog/v3/products/" ++ z_convert:to_list(Id), []).

search(#bolsearch{}) ->
    {ok, []}.


%% Do a GET request
request(get, Path, Params) ->
    Date = make_date(), 
    {Key, Sec} = get_access_key(),
    ContentType = "application/xml",
    
    Headers = [{"Content-type", ContentType},
               {"X-OpenAPI-Authorization", auth_header(Key, Sec, Path, ContentType, Date, Params)},
               {"X-OpenAPI-Date", Date}],
    
    case httpc:request(get, {?API_BASE ++ Path, Headers}, [], []) of
        {ok, {{_, 200, _}, ResponseHeaders, Body}} ->
            {ok, interpret_body(ResponseHeaders, Body)};
        {ok, {{_, OtherCode, _}, ResponseHeaders, Body}} ->
            {error,
             {http, OtherCode, interpret_body(ResponseHeaders, Body)}};
        {error, R} ->
            {error, R}
    end.

interpret_body(Headers, Body) ->
    case proplists:get_value("content-type", [{z_string:to_lower(K), V} || {K,V} <- Headers]) of
        "application/xml" ->
            {XML, _} = xmerl_scan:string(Body), XML;
        "text/plain" ++ _ ->
            Body
    end.


%% Construct the authorization header, calculates the signature needed.
auth_header(Key, Sec, Path, ContentType, Date, Params) ->
    SigBase = "GET\n\n" ++
        ContentType ++ "\n" ++
        Date  ++ "\n" ++
        "x-openapi-date:" ++ Date ++ "\n" ++
        Path ++ "\n",
    ParamBase = lists:foldl(fun({K, V}, Acc) ->
                                    Acc ++
                                        case Acc of [] -> []; _ -> "\n" end ++
                                        z_convert:to_list(K) ++
                                        "&" ++
                                        z_convert:to_list(V)
                            end,
                            [],
                            lists:sort(Params)),
    Key ++ ":" ++ z_convert:to_list(base64:encode(hmac:hmac256(Sec, SigBase ++ ParamBase))).


get_access_key() ->
    {ok, Key} = application:get_env(bolapi, access_key),
    {ok, Sec} = application:get_env(bolapi, access_secret),
    {Key, Sec}.

make_date() ->
    z_convert:to_list(z_dateformat:format(calendar:universal_time(), "D, d F Y H:i:s \\G\\M\\T", [])).    
