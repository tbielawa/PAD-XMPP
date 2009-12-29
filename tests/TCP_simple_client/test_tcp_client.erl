%%% File    : test_tcp_client.erl
%%% Author  : Tim Bielawa <timbielawa@gmail.com>
%%% Description : A dummy client to connect to a dummy server and have
%%% a conversation
%%% Created : 28 Dec 2009 by Tim Bielawa <timbielawa@gmail.com>

-module(test_tcp_client).
-export([start/0]).

start() ->
    register(main, spawn(fun() ->
				 loop() end)).

loop() ->
    Data = io:get_line("REPL> "),
    %%%io:format(">> ~p <<~n", [Data]),
    loop().
