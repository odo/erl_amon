
-module(erl_amon_sup).

-behaviour(supervisor).

%% API
-export([start_link/3]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(Host, Port, Environment) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [Host, Port, Environment]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([Host, Port, Environment]) ->
	ErlAmonServer = {erl_amon, {erl_amon, start_link, [Host, Port, Environment]},
		permanent, 1000, worker, [erl_amon]},
    {ok, { {one_for_one, 5, 10}, [ErlAmonServer]} }.
