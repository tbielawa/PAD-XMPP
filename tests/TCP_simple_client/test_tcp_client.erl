%%% File    : test_tcp_client.erl
%%% Author  : Tim Bielawa <timbielawa@gmail.com>
%%% Description : A dummy client to connect to a dummy server and have
%%% a conversation
%%% Created : 28 Dec 2009 by Tim Bielawa <timbielawa@gmail.com>

-module(test_tcp_client).
-export([start/0]).
-import(simple_logger, [log/1]).

start() ->
    register(main, spawn(fun() ->
				 loop() end)).

process_line(Line) ->
    log(["PROCESS_LINE", "received", Line]),
    Token_String = pad_misc:split(pad_misc:trim_newlines(Line)),
    case pad_misc:first(Token_String) of
	"q" ->
	    log(["PROCESS_LINE", "quit", []]),
	    main ! {exit, "Goodbye!"};
	Else ->
	    log(["PROCESS_LINE", "unknown", Else]),
	    main ! {line, Token_String}
    end.

loop() ->
    Data = io:get_line("REPL> "),
    process_line(Data),
    receive
	{line, Line} ->
	    log(["LOOP", "received", Line]),
	    loop();
	{exit, Message} ->
	    exit(Message);
	Else ->
	    log(["LOOP", "unknown message", Else]),
	    loop()
    end.
