-module(currency_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
	Procs = [
		#{
            id => currency_worker,
            start => {currency_worker, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [currency_worker]
        }
	],
	{ok, {{one_for_one, 1, 5}, Procs}}.
