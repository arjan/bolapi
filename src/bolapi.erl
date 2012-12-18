-module(bolapi).

-export([ping/0, get_product/1]).

-define(API_BASE, "https://openapi.bol.com").

ping() ->
    request(get, "/openapi/services/rest/utils/v3/ping", []).

get_product(Id) ->
    request(get, "/openapi/services/rest/catalog/v3/products/" ++ z_convert:to_list(Id), []).


request(get, Path, Params) ->
    Date = make_date(), 
    {Key, Sec} = get_access_key(),
    ContentType = "application/xml",
    
    AuthHeader = auth_header(Key, Sec, Path, ContentType, Date, Params),

    Headers = [{"Content-type", ContentType},
               {"X-OpenAPI-Authorization", AuthHeader},
               {"X-OpenAPI-Date", Date}],
    
    httpc:request(get,
                  {?API_BASE ++ Path, Headers}, [], []).


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
