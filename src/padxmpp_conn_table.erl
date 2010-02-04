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
%%% File    : padxmpp_conn_table.erl
%%% Description : Maintains a connection table.
%%%               Seemed like a good idea at the time.
%%%-------------------------------------------------------------------

-module(padxmpp_conn_table).
-behaviour(gen_server).

-export([start_link/0, start/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include("shared.hrl").
-define(CONN_FILE, "connections.tab").

start() -> spawn(fun() -> start_link() end).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {ok, ets:new(?CONN_TABLE, 
		 [private, named_table, 
		  {write_concurrency, false},
		 {keypos,2}])}.

handle_call({register_connection, Sock}, _From, Table) ->
    {_,_,NewId} = erlang:now(),
    ConnectionTime = calendar:local_time(),
    NewConn = #connection{id=NewId, socket=Sock, connected_at=ConnectionTime},
    ets:insert(Table, NewConn),
    {reply, {ok, NewId}, Table};

handle_call(request, _From, Table) ->
    {reply, ok, Table}.

handle_cast(write_connection_table, Table) ->
    ets:tab2file(Table, ?CONN_FILE),
    {noreply, Table};

handle_cast(dump_connection_table, Table) ->
    io:format("Dumping connection table~n", []),
    Dump = ets:match(Table, '$1'),
    io:format(" ~p~n", [Dump]),
    {noreply, Table};

handle_cast(_Msg, State) ->
    io:format("Default cast handler reached.~n"),
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
