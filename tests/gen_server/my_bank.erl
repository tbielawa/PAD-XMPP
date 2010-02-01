%%% File    : my_bank.erl
%%% Author  : Tim Bielawa <timbielawa@gmail.com>
%%% Description : A simple project implementing the gen_server model.
%%%               We'll have some "bankers", and "lock boxes", and "money"
%%% Created : 31 Jan 2010 by Tim Bielawa <timbielawa@gmail.com>

-module(my_bank).
-export([start/2, rpc/2]).
-import(simple_logger, [announce/2]).

%%% my_bank is responsible for loading modules, 
%%% passing messages, and transaction logging.

start(Name, Mod) ->
    announce("Director", "Launching Module"),
    register(Name, spawn(fun() -> loop(Name, Mod, Mod:init()) end)).

rpc(Name, Transaction) ->
    Name ! {self(), Transaction},
    receive
	{Name, Response} -> Response
    end.

%%% Sit around waiting for a task to carry out.
loop(Name, Mod, State) ->
    receive
	{From, Request} ->
	    % Each Mod had a 'handle' function, we just relay the result
	    {Response, State1} = Mod:handle(Request, State),
	    From ! {Name, Response},
	    loop(Name, Mod, State1)
    end.
