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
%%% File    : padxmpp.erl
%%% Description : Starts the show
%%%-------------------------------------------------------------------

-module(padxmpp).
-import("shared.hrl").

-export([start/0,launch_gen_server/1]).

start() ->
    Servers = [
	       #server{name="Connection Listener", module=padxmpp_conn_listener},
	       #server{name="Connection Table", module=padxmpp_conn_table},
	       #server{name="XML Scanner", module=padxmpp_xml_scan}
	      ],
    lists:foreach(fun(Srv) -> launch_gen_server(Srv) end, Servers).

%%     io:format("Starting gen_server:padxmpp_conn_table...~n"),
%%     case gen_server:start_link(
%% 	   {local, padxmpp_conn_table},
%% 	   padxmpp_conn_table, [], []) of
%% 	{ok, _} ->
%% 	    io:format("Connection table started successfully.~n", []);
%% 	{error, _} ->
%% 	    io:format("Error Starting padxmpp_conn_table.~n", []);
%% 	ignore ->
%% 	    io:format("Told to 'ignore'.~n")
%%     end,
    
%%     case gen_server:start_link(
%% 	   {local, padxmpp_xml_scan},
%% 	   padxmpp_xml_scan, [], []) of
%% 	{ok, _} ->	
%% 	    io:format("XML Scanner Loaded~n",[]);
%% 	{error, _} ->
%% 	    io:format
    

launch_gen_server(#server{name=Name, module=Module} = Srv) ->
    io:format("Starting gen_server:~s...~n",[Module]),
    case gen_server:start_link(
	   {local, Module},
	   Module, [], []) of
	{ok, _} ->
	    io:format("~s(~s) successfully.~n", [Name, Module]);
	{error, Err} ->
	    io:format("Error starting ~s(~s)~n", [Name, Module]),
	    io:format("~60.5p~n", [Err]);
	ignore ->
	    io:format("Told to 'ignore' in ~s(~s)?~n",[Name, Module])
    end.
