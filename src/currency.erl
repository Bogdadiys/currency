-module(currency).

-export([start/0]).
-export([get_currency/1]).

start() ->
    application:ensure_all_started(currency).

get_currency(Data) ->
    CCY = maps:get(<<"ccy">>, Data, []),
    {ok, CurencyList} = currency_worker:get_currency(CCY),
    Rows = [
        {<<"row">>,[], [{<<"exchangerate">>, Currency, []}]}
    || Currency <- CurencyList],
    ResultObject = {<<"exchangerates">>,[],Rows},
    {ok, ResultObject}.