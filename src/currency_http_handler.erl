-module(currency_http_handler).

%% cowboy callbacks
-export([init/2]).

%% cowboy callbacks
init(Req0, Opts) ->
	Method = cowboy_req:method(Req0),
    Path   = cowboy_req:path(Req0),
    Headers = cowboy_req:headers(Req0),
    ContentType = maps:get(<<"content-type">>, Headers, null),
    {ok, Body, Req1} = read_entire_body(Req0),
    {ok, Data} = decode(ContentType, Body), 
	Result = dispatch(Method, Path, Data),
    {RespCode, RespHeaders, RespBody} = encode(Result),
    Req = cowboy_req:reply(RespCode, RespHeaders, RespBody, Req1),
	{ok, Req, Opts}.

%% internal
dispatch(_Method, <<"/get_currency">>, Body) ->
    currency:get_currency(Body).

decode(_, <<>>) ->
    {ok, #{}};
decode(<<"application/json">>, Data) ->
    DecodedData = jsx:decode(Data, [return_maps, {labels, attempt_atom}]),
    {ok, DecodedData}.

encode({ok, Data}) ->
    DataXML = exomler:encode(Data),
    {200, #{<<"content-type">> => <<"application/xml">>}, DataXML};
encode({error, _}) ->
    {500, #{<<"content-type">> => <<"application/xml">>}, <<>>}.

read_entire_body(Req) ->
    Resp = cowboy_req:read_body(Req),
    read_entire_body(Resp, <<>>).

read_entire_body({more, Body, Req}, Acc) ->
    Resp = cowboy_req:read_body(Req),
    read_entire_body(Resp, <<Acc/binary, Body/binary>>) ;
read_entire_body({ok, Body, Req}, Acc) ->
    {ok, <<Acc/binary, Body/binary>>, Req}.