%%% File    : simple_logger.erl
%%% Author  : Tim Bielawa <timbielawa@gmail.com>
%%% Description : Simple module for message emitting
%%% Created : 28 Dec 2009 by Tim Bielawa <timbielawa@gmail.com>

-module(simple_logger).
-export([announce/2]).

announce(Speaker, Message) ->
    io:format("[~s] ~s~n", [Speaker, Message]).
