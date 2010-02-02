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
-export([start/0]).

start() ->
    case gen_server:start_link(
	   {local, padxmpp_conn_listener},
	   padxmpp_conn_listener, [], []) of
	{ok, _} ->
	    io:format("Connection listener started successfully.~n", []);
	{error, _} ->
	    io:format("Server already started.~n", []);
	ignore ->
	    io:format("Told to 'ignore'.~n")
    end,

    case gen_server:start_link(
	   {local, padxmpp_conn_table},
	   padxmpp_conn_table, [], []) of
	{ok, _} ->
	    io:format("Connection table started successfully.~n", []);
	{error, _} ->
	    io:format("Server already started.~n", []);
	ignore ->
	    io:format("Told to 'ignore'.~n")
    end.
    
    
