-module(bank5_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

%% public API (module method)

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% public API for supervisor

init([]) ->
    Child = {bank5,
	 {bank5, start_link, []},
	     permanent,
	 brutal_kill,
	 worker,
	 [bank5]},
    {ok, {{one_for_one, 10, 1},
	  [Child]}}.

