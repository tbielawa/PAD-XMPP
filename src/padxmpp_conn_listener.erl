%% Uses the gen_listener_tcp behaviour.

-module(padxmpp_conn_listener).
-behaviour(gen_listener_tcp).

-define(TCP_PORT, 9876).
-define(TCP_OPTS, [binary, inet,
		   {active,    false},
		   {backlog,   10},
		   {nodelay,   true},
		   {packet,    raw},
		   {reuseaddr, true}]).

%% gen_listener_tcp callbacks
-export([init/1,
         handle_accept/2,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3,
	 start_link/0]).


start_link() ->
    gen_listener_tcp:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc The echo client process.
echo_client(Socket) ->
    error_logger:info_msg("client()~n"),
    ok = inet:setopts(Socket, [{active, once}]),
    receive
        {tcp, Socket, <<"quit", _R/binary>>} ->
            error_logger:info_msg("Quit Requested."),
            gen_tcp:send(Socket, "Bye now.\r\n"),
            gen_tcp:close(Socket);
        {tcp, Socket, Data} ->
            error_logger:info_msg("Got Data: ~p", [Data]),
            gen_tcp:send(Socket, "I Received " ++ Data),
            echo_client(Socket);
        {tcp_closed, Socket} ->
            error_logger:info_msg("Client Disconnected.")
    end.

init([]) ->
%    process_flag(trap_exit, true),
    {ok, {?TCP_PORT, ?TCP_OPTS}, nil}.

handle_accept(Sock, State) ->
%    Pid = spawn(fun() -> padxmpp_auth_fsm:start(Sock) end),
    error_logger:info_msg("In TCP Accept Holding State."),
    Pid = spawn(fun() -> echo_client(Sock) end),
    gen_tcp:controlling_process(Sock, Pid),
    {noreply, State}.

handle_call(Request, From, State) ->
    error_logger:info_msg([handle_a_call, {Request, From, State}]),
    {reply, {illegal_request, Request}, State}.

handle_cast(Request, State) ->
    error_logger:info_msg([handle_a_cast, {Request, State}]),
    {noreply, State}.

handle_info(Info, State) ->
    error_logger:info_msg([handle_some_info, {Info, State}]),
    {noreply, State}.

terminate(Reason, State) ->
    error_logger:info_msg([terminate, {Reason, State}]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
