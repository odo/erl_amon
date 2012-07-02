-module(erl_amon_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
	application:start(sasl),
	application:start(ibrowse),
	Host = env_or_throw(host),
	Port = env_or_throw(port),
	Environment = env_or_throw(environment),
	error_logger:add_report_handler(erl_amon_error_logger),
	erl_amon_sup:start_link(Host, Port, Environment).

stop(_State) ->
    ok.

env_or_throw(Key) ->
	case proplists:get_value(Key, application:get_all_env(erl_amon)) of
		undefined ->
			throw({error, {atom_to_list(Key) ++ " must be configured in erl_amons environment"}});
		Value ->
			Value
	end.
