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
%%% Description : TCP connection listener
%%%-------------------------------------------------------------------
-module(padxmpp_conn_listener).
-behaviour(gen_server).

-export([start_link/0, start/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include("shared.hrl").

start() -> spawn(fun() -> start_link() end).
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    io:format("Going to listen on ~s:~w~n", [?HOST, ?PORT]),
    case gen_tcp:listen(?PORT, [list, 
			       {packet, 0},
			       inet, 
			       {active, false},
			       {reuseaddr, true}]) of
	{ok, LSocket} ->
	    io:format("TCP server started~n", []),
	    register(accept_loop, spawn(fun() -> accept_loop(LSocket) end)),
	    {ok, LSocket};
	{error, eaddrinuse} ->
	    io:format("TCP server failed to start: Address already in use~n", []),
	    {stop, eaddrinuse};
	{error, Error} ->
	    io:format("TCP server failed to start: unknown error~n", []),
	    io:format("~w~n", Error),
	    {stop, Error}
    end.

handle_call(_Request, _From, LSocket) ->
    {reply, ok, LSocket}.

handle_cast({handle_connection, Sock}, State) ->
    {ok, ConnId} = gen_server:call(padxmpp_conn_table, {register_connection, Sock}),
    %gen_server:call(padxmpp_auth_fsm, {process, Sock, ConnId}),
    padxmpp_auth_fsm:start(Sock, ConnId),
    {noreply, State};

handle_cast(_Msg, State) ->
    io:format("Default cast handler reached.~n"),
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    gen_server:cast(padxmpp_conn_table, dump_connection_table),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

accept_loop(LSocket) ->
    io:format("Acceptor loop entered~n"),
    case gen_tcp:accept(LSocket) of
	{ok, Sock} ->
	    gen_server:cast(?MODULE, {handle_connection, Sock});
	{error, Reason} ->
	    io:write("Socket accept error: ~s~n", [Reason])
    end,
    accept_loop(LSocket).
