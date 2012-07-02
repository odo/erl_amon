-module(erl_amon_error_logger).


-export([
	init/1
	, handle_event/2
]).

init(_InitArgs) -> {ok, []}.

send_to_amon(Class, Message, Data) ->
	erl_amon:send_to_amon(Class, Message, Data).

% coming from error_msg()
handle_event({error, Gleader, {Pid, Format, Data}}, State) ->
	Message = io_lib:format(Format, Data),
	send_to_amon(
		concat_class(error, Message)
		, Message
		, json:to_binary(dict:from_list([
			{pid, pid_to_binary(Pid)}
			, {gleader, pid_to_binary(Gleader)}
		]))
	),
	{ok, State};

% coming from error_report()
handle_event({error_report, Gleader, {Pid, Type, Report}}, State) ->
	send_to_amon(
		concat_class(Type, Report)
		, Report
		, json:to_binary(dict:from_list([
			{pid, pid_to_binary(Pid)}
			, {gleader, pid_to_binary(Gleader)}
		]))
	),
	{ok, State};

% coming from warning_msg()
handle_event({warning_msg,	Gleader, {Pid, Format, Data}}, State) ->
	Message = io_lib:format(Format, Data),
	send_to_amon(
		concat_class(warning_msg, Message)
		, io_lib:format(Format, Data)
		, json:to_binary(dict:from_list([
			{pid, pid_to_binary(Pid)}
			, {gleader, pid_to_binary(Gleader)}
		]))
	),
	{ok, State};

% coming from warning_report()
handle_event({warning_report, Gleader, {Pid, Type, Report}}, State) ->
	send_to_amon(
		concat_class(Type, Report)
		, Report
		, json:to_binary(dict:from_list([
			{pid, pid_to_binary(Pid)}
			, {gleader, pid_to_binary(Gleader)}
		]))
	),
	{ok, State};

% coming from info_msg()
handle_event({info_msg, _, _}, State) ->
	{ok, State};

% coming from info_report()
handle_event({info_report, _, _}, State) ->
	{ok, State}.

concat_class(Type, Message) ->
	atom_to_list(Type) ++ ":\n" ++ string:sub_string(Message, 1, 10).

pid_to_binary(Pid) when is_atom(Pid) ->
	list_to_binary(atom_to_list(Pid));

pid_to_binary(Pid) when is_pid(Pid) ->
	list_to_binary(erlang:pid_to_list(Pid)).