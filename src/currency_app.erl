-module(currency_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
	ok = start_handler(),
	currency_sup:start_link().

stop(_State) ->
	ok.

start_handler() ->
	Port = 8196,
 	Dispatch = cowboy_router:compile([
		{'_', [
			{"/[...]", currency_http_handler, []}
		]}
	]),
	{ok, _} = cowboy:start_clear(http, [{port, Port}], #{
		env => #{dispatch => Dispatch}
	}),
    ok.