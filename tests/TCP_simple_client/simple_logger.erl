%%% File    : simple_logger.erl
%%% Author  : Tim Bielawa <timbielawa@gmail.com>
%%% Description : Simple module for message emitting
%%% Created : 28 Dec 2009 by Tim Bielawa <timbielawa@gmail.com>

-module(simple_logger).
-export([log/1]).

log([F, Action, Data]) ->
    io:format("[~s] [~s]~n", [F, Action]),
    io:format("--~p~n", [Data]).

