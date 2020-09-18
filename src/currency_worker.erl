-module(currency_worker).
-behaviour(gen_server).

%% API
-export([start_link/0, stop/0]).
-export([get_currency/0, get_currency/1]).

%% gen_server callbacks
-export([init/1, terminate/2]).
-export([handle_call/3, handle_cast/2, handle_info/2]).

%% records
-record(state, {
    life_time
}).

%% API
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:stop(?MODULE).

get_currency() ->
    get_currency([]).

get_currency(CurrencyTypes) when is_list(CurrencyTypes) ->
    Result = gen_server:call(?MODULE, {get_currency, CurrencyTypes}),
    {ok, Result};
get_currency(CurrencyType) ->
    get_currency([CurrencyType]).

%% gen_server callbacks
init([]) ->
    _ = ets:new(currency,[protected, named_table]),
    {ok, LifeTime} = update_currency(),
    {ok, #state{life_time = LifeTime}}.

terminate(_Reason, _State) ->
    ok.

handle_call({get_currency, CurrencyTypes}, _From, #state{life_time = LifeTime} = State) ->
    {ok, Currency, NewLifeTime} = get_currency(CurrencyTypes, LifeTime),
    {reply, Currency, State#state{life_time = NewLifeTime}};
handle_call(Request, _From, State) ->
    erlang:display(Request),
    {reply, ignore, State}.

handle_cast(_, State) ->
    {ok, State}.

handle_info(_, State) ->
    {ok, State}.

%% internal
get_currency(OldCurrencyTypes, LifeTime) ->
    CurrencyTypes = case OldCurrencyTypes of
        [] -> [<<"USD">>, <<"EUR">>, <<"RUR">>, <<"BTC">>];
        _ -> OldCurrencyTypes
    end,
    {ok, NewLifeTime} = case LifeTime < os:system_time(seconds) of
        true ->
            {ok, LifeTime};
        false ->
            update_currency()
    end,
    Currency = [
        ets:lookup_element(currency, CurrencyType, 2)
    || CurrencyType <- CurrencyTypes],
    {ok, Currency, NewLifeTime}.
        
update_currency() ->
    Url = "https://api.privatbank.ua/p24api/pubinfo?json&exchange&coursid=5",
    {ok, {{_, 200, _}, _Headers, BodyJson}} = httpc:request(Url),
    Currs = jsx:decode(list_to_binary(BodyJson), [{return_maps, false}]),
    _ = [ begin
        Object = {proplists:get_value(<<"ccy">>, Currency),Currency},
        true = ets:insert(currency, Object)
    end || Currency <- Currs],
    {ok, os:system_time(seconds) + 1}.