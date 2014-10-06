-module(bolapi).
-include_lib("bolapi/include/bolapi.hrl").

-export([
         ping/0,
         products/1,
         searchresults/1,
         listresults/2
        ]).

-define(API_BASE, "https://openapi.bol.com").

%% @doc Test the API connection
ping() ->
    case request(get, "/openapi/services/rest/utils/v3/ping", []) of
        {ok, []} ->
            pong;
        {error, R} ->
            {error, R}
    end.

%% @doc Return details of a single product.
products(Id) ->
    request(get, "/openapi/services/rest/catalog/v3/products/" ++ z_convert:to_list(Id), []).

%% @doc Execute a search query
searchresults(P=#bolparams{}) ->
    request(get, "/openapi/services/rest/catalog/v3/searchresults", filter_params(P)).

%% @doc Product list
listresults(Type, CategoryIds) ->
    listresults(Type, CategoryIds, #bolparams{}).

listresults(Type, CategoryIds, BolParams) when
      Type =:= toplist_default;
      Type =:= toplist_overall;
      Type =:= toplist_last_week;
      Type =:= toplist_last_two_months;
      Type =:= new;
      Type =:= preorder
      ->
    CatIds = string:join(lists:map(fun z_convert:to_list/1, CategoryIds), "+"),
    Path = "/openapi/services/rest/catalog/v3/listresults/" ++ z_convert:to_list(Type) ++ "/" ++ CatIds,
    request(get, Path, filter_params(BolParams)).


%% Private

filter_params(P=#bolparams{}) ->
    All = lists:zip(
            record_info(fields, bolparams),
            tl(tuple_to_list(P))),
    [{K, V} || {K, V} <- All,
               V =/= undefined].


%% Do a GET request
request(get, Path, Params) ->
    Date = make_date(), 
    {Key, Sec} = get_access_key(),
    ContentType = "application/xml",
    
    Headers = [{"Content-type", ContentType},
               {"X-OpenAPI-Authorization", auth_header(Key, Sec, Path, ContentType, Date, Params)},
               {"X-OpenAPI-Date", Date}],

    QueryString = build_query_string(Params),
    case httpc:request(get, {?API_BASE ++ Path ++ QueryString, Headers}, [], []) of
        {ok, {{_, 200, _}, ResponseHeaders, Body}} ->
            {ok, interpret_body(ResponseHeaders, Body)};
        {ok, {{_, OtherCode, _}, ResponseHeaders, Body}} ->
            {error,
             {http, OtherCode, interpret_body(ResponseHeaders, Body)}};
        {error, R} ->
            {error, R}
    end.

interpret_body(Headers, Body) ->
    case proplists:get_value(<<"content-type">>, [{z_string:to_lower(K), V} || {K,V} <- Headers]) of
        "application/xml" ++ _ ->
            {XML, _} = xmerl_scan:string(Body), XML;
        "text/xml" ++ _ ->
            io:format("~p", [Body]),
            {XML, _} = xmerl_scan:string(Body), XML;
        "text/plain" ++ _ ->
            Body;
        undefined ->
            %% No content type? "ping" request.
            []
    end.

%% Given a KV list of parameters, make the query string
build_query_string([]) ->
    [];
build_query_string(Params) ->
    KVs = string:join([ z_url:percent_encode(z_convert:to_list(K)) ++ "=" ++ z_url:percent_encode(z_convert:to_list(V))
                        || {K, V} <- Params], "&"),
    "?" ++ KVs.

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
                                        "&" ++
                                        z_convert:to_list(K) ++
                                        "=" ++
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
