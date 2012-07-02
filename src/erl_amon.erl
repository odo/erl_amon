-module(erl_amon).
-behaviour (gen_server).

% API
-export([
	send_to_amon/3
	, start/0, start/3
]).

% callbacks
-export ([start_link/3, init/1, stop/0, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


-define (SERVER, ?MODULE).

% 	{NickName, AppKey, DeviceId} = {list_to_binary(float_to_list(random:uniform())), "GM9vBVN2W8XfueF8J", list_to_binary(float_to_list(random:uniform()))},
% 	URL =  "http://" ++ api_test:ip() ++ ":" ++ integer_to_list(api_test:port()) ++ "/love_hate/users?app_key=" ++ AppKey,
% 	Data = dict:from_list([{"nick_name", NickName}, {"device_id", DeviceId}, {"device_info", undefined}, {"os_type", <<"android">>}, {"notifications_amount", 1.0}]),
% 	Reply = ibrowse:send_req(URL, [{"Content-Type", "application/json"}], post, json:to(Data)),
% 	api_test:extract_user(Reply).

% extract_user({_, HTTPStatus, _, Body}) when HTTPStatus =:= "201" orelse HTTPStatus =:= "200" ->

-record (state, {host, port, environment}).

start() ->
	application:start(erl_amon).	

start(Host, Port, Environment) ->
	application:set_env(erl_amon, host, Host),
	application:set_env(erl_amon, port, Port),
	application:set_env(erl_amon, environment, Environment),
	application:start(erl_amon).

% API

start_link(Host, Port, Environment) ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [Host, Port, Environment], []).
	
stop() ->
	gen_server:call(?SERVER, {stop}).

send_to_amon(Class, Message, Data) ->
	gen_server:cast(?SERVER, {send_to_amon, Class, ["see message"], Message, "", Data}).

% gen_server callbacks

init([Host, Port, Environment]) ->
	{ok, #state{host = Host, port = Port, environment = Environment}}.

handle_call({stop}, _From, State) ->
  {stop, normal, stopped, State};

handle_call( _, _From, State) ->
	throw({error, not_implemented}),
	{reply, ok, State}.

handle_cast({send_to_amon, Class, Backtrace, Message, Url, Data}, State = #state{environment = Environment}) ->
	case Environment =:= "development" orelse Environment =:= "test" of
		true ->
			io:format("erl_amon received: ~p\n",
				[[{class, Class}
				, {backtrace, Backtrace}
				, {message, Message}
				, {environment, Environment}
				, {url, Url}
				, {data, Data}
			]]);
		false ->
			send_to_amon_internal(Class, Backtrace, Message, Environment, Url, Data)
	end,
	{noreply, State}.
	
handle_info(_Info, State) ->
	{noreply, State}.
	
terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	throw({error, not_implemented}),
	{ok, State}.

% internal functions
% curl -v -X POST -H "Content-Type: application/json" -d  '{"exception_class": "DummyClass","url": "/url","backtrace": ["line", "another line"],"message": "something failed","enviroment": "dummy_env","data": "this is th data"}'
send_to_amon_internal(Class, Backtrace, Message, Environment, Url, Data) ->
	URL =  "http://" ++ host() ++ ":" ++ port() ++ "/api/exception",
	PostData = dict:from_list([
		{"exception_class", list_to_binary(Class)}
		, {"backtrace", [list_to_binary(B)||B <- Backtrace]}
		, {"message", list_to_binary(Message)}
		, {"environment", list_to_binary(Environment)}
		, {"url", list_to_binary(Url)}
		, {"data", Data}
	]),
	Reply = ibrowse:send_req(URL, [{"Content-Type", "application/json"}], post, json:to(PostData)),
	case annalist_running() of
		true -> annalist:count(<<"error">>);
		false -> nothing
	end,
	parse_reply(Reply).

parse_reply({error, {conn_failed, Error}}) ->
	throw({error, [{conn_failed, Error}, {host, host()}, {port, port()}]});

parse_reply({_, "200", _, _}) ->
	ok;

parse_reply({_, HTTPStatus, _, Body}) ->
	{error, [{http_status, HTTPStatus}, {body, Body}]}.

host() -> env(host).

port() -> integer_to_list(env(port)).

env(Variable) ->
	case application:get_env(erl_amon, Variable) of
		{ok, Value} -> Value;
		Error -> throw(Error)
	end.

annalist_running() ->
	[annalist] =:= [Name||{Name, _, _}<-application:which_applications(), Name =:= annalist].
