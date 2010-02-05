%%% Copyright (c) 2010 Tim 'Shaggy' Bielawa <timbielawa@gmail.com>
%%% 
%%% Permission is hereby granted, free of charge, to any person
%%% obtaining a copy of this software and associated documentation
%%% files (the "Software"), to deal in the Software without
%%% restriction, including without limitation the rights to use,
%%% copy, modify, merge, publish, distribute, sublicense, and/or sell
%%% copies of the Software, and to permit persons to whom the
%%% Software is furnished to do so, subject to the following
%%% conditions:
%%% 
%%% The above copyright notice and this permission notice shall be
%%% included in all copies or substantial portions of the Software.
%%% 
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
%%% EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
%%% OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
%%% NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
%%% HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
%%% WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
%%% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
%%% OTHER DEALINGS IN THE SOFTWARE.

%%%-------------------------------------------------------------------
%%% File    : padxmpp_conn_listener.erl
%%% Description : Listen for incoming connections and fork them off
%%%               to be handled by the authentication FSM.
%%%-------------------------------------------------------------------
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
