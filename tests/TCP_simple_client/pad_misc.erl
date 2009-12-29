%%% File    : pad_misc.erl
%%% Author  : Tim Bielawa <timbielawa@gmail.com>
%%% Description : Assorted Functions
%%% Created : 29 Dec 2009 by Tim Bielawa <timbielawa@gmail.com>

-module(pad_misc).
-export([first/1, trim_newlines/1, split/1]).

first(List) when length(List) > 0 ->
    lists:nth(1, List);
first(List) ->
    List.

%%% Get rid of trailing new lines
trim_newlines(Dirty) ->
    string:strip(Dirty, right, $\n).

split(Line) ->
    string:tokens(Line, " ").
