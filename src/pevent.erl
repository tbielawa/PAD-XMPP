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
%%% File    : pevent.erl
%%% Description : PAD-XMPP Event Manager
%%%-------------------------------------------------------------------

-module(pevent).
-behaviour(gen_event).

-export([start/0, start_link/0, init/1, handle_event/2,
	 handle_call/2, handle_info/2, terminate/2,
	 code_change/3]).

start() -> spawn(fun() -> start_link() end).
start_link() ->
    gen_event:start_link({local, ?MODULE}).

init(_Args) ->
    {ok, 0}.

% Called for each event called through notify/2 or sync_notify/2
handle_event(Event, State) ->
    io:format("it's an event~n"),
    io:format("~p~n", [Event]),
    {ok, State}.

handle_call(Request, State) ->
    io:format("it's a call~n~p~n",[Request]),
    Reply = ok,
    {ok, Reply, State}.

% Catch-all handler
handle_info(Info, State) -> io:format("it's.... catch-all'd~n~p~n", [Info]), {ok, State}.
terminate(_Arg, _State) -> ok.
code_change(_Old, State, _Extra) -> {ok, State}.
