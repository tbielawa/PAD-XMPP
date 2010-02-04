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
%%% File    : padxmpp_xml_scanner.erl
%%% Description : a gen_server to make a gen_tcp socket act like an xml stream
%%%-------------------------------------------------------------------


-module(padxmpp_xml_scan).
-behaviour(gen_server).
-export([start_link/0, start/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).
-export([main/2,continue_fun/1,event_fun/0]).
-include("shared.hrl").
-include_lib("xmerl/include/xmerl.hrl").

start() -> spawn(fun() -> start_link() end).
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).	    

init([]) ->
    ok.

%%% {

%%     xmerl_scan:string("", [{continuation_fun, ?MODULE:continue_fun(Sock)},
%% 			  {{event_fun, ?MODULE:event_fun()}]).



%%% {continue_fun, Socket}
%%% Responds with a continuation_fun/3 suitable for use in xmerl
handle_call({gen_continue_fun, Socket}, From, State) ->
    Reply = fun(Continue, _Exception, GlobalState) ->
		    case gen_tcp:recv(Socket,0) of
			{ok, Data} ->
			    io:format("Received some daters~n",[]),
			    io:format("Data: ~w~n", [Data]),
			    Continue(Data, GlobalState);
			{error, closed} ->
			    _Exception(GlobalState)
		    end
	    end,
    {reply, Reply, State};

handle_call(_Request, _From, LSocket) ->
    {reply, ok, LSocket}.

handle_cast(_Request, State) ->
    {noreply, State};

handle_cast(_Msg, State) ->
    io:format("Default cast handler reached.~n"),
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
