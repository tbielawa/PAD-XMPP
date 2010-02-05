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
%%% File    : padxmpp_client_sup.erl
%%% Description : Supervisor of the connections pool
%%%         After accepting a socket you should call
%%%    {ok, Pid} = padxmpp_client_sup:add_child(ChildFSMSpec())
%%%
%%%           ChildFSMSpec() = ChildSpec with module: [padxmpp_auth_fsm]
%%%    After that start an XML Stream Scanner and assign socket 
%%%    ownership to it. Send events to the child FSM of scanned stanzas
%%%-------------------------------------------------------------------

-module(padxmpp_client_sup).
-behaviour(supervisor).

-export([start/0, start_link/0, init/1]).
-include("shared.hrl").

start() -> spawn(fun() -> start_link() end).
start_link() ->
    supervisor:start_link({local,?MODULE}, ?MODULE, []).

init([]) ->
    {ok, {{one_for_one, 3, 10},
	  [
	  ]}}.
