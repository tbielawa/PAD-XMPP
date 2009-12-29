%%% File    : test_tcp_client.erl
%%% Author  : Tim Bielawa <timbielawa@gmail.com>
%%% Description : A dummy client to connect to a dummy server and have
%%% a conversation
%%% Created : 28 Dec 2009 by Tim Bielawa <timbielawa@gmail.com>

-module(test_tcp_client).
-export([start/0]).
-import(simple_logger, [log/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
start() ->
    register(main, spawn(fun() -> loop() end)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% This is truly ugly.
%%% No, really, I'm ashamed of this.
%%% Please don't look at it.
process_line(Line) ->
%%     log(["PROCESS_LINE", "received", Line]),
    Token_String = pad_misc:split(pad_misc:trim_newlines(Line)),
    case pad_misc:first(Token_String) of
	"q" ->
%% 	    log(["PROCESS_LINE", "quit", []]),
	    main ! {exit, normal};
	"c" ->
	    case connect() of
		{ok, Socket} ->
		    PacketHandler = spawn(fun() -> get_packets() end),
		    spawn(fun() -> do_recv(PacketHandler, Socket) end),
		    remote_loop(Socket);
		{error, Reason} ->
		    log(["PROCESS_LINE", "Connection State", Reason])
	    end;
	Else ->
%% 	    log(["PROCESS_LINE", "unknown", Else]),
	    main ! {line, Token_String}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
send_to_remote(Socket, Message) ->
    case gen_tcp:send(Socket, Message) of
	{error, Reason} ->
	    log(["SEND_TO_REMOTE", "Sending Data", Reason]),
	    {error,Reason};
	_ ->
	    ok
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% There must be a way to combine these
remote_loop(Socket) ->
    Data = io:get_line(""),
    _Return = send_to_remote(Socket, Data),
    remote_loop(Socket).

loop() ->
    Data = io:get_line("REPL> "),
    process_line(Data),
    receive
	{line, Line} ->
%% 	    log(["LOOP", "received", Line]),
	    loop();
	{exit, Message} ->
	    exit(Message);
	Else ->
	    log(["LOOP", "unknown message", Else]),
	    loop()
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
connect() ->
    try gen_tcp:connect("localhost", 5678, [binary, {packet, 0},{active, false}]) of
	{ok, Socket} ->
%% 	    log(["CONNECT", "open connection", "Connection opened"]),
	    {ok, Socket}
    catch
	{error, Reason} ->
	    log(["CONNECT", "open connection", Reason]),
	    {error, Reason}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_packets() ->
    receive
        {ok, Pkt, _Sock} ->
            io:format("~s", [Pkt]),
            get_packets();
        {done} ->
	    log(["GET_PACKETS", "Connection State", "Connection closed"])
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
do_recv(PPid, Sock) ->
    case gen_tcp:recv(Sock, 0) of
        {ok, B} ->
            PPid ! {ok, B, Sock},
            do_recv(PPid, Sock);
        {error, closed} ->
            ok = gen_tcp:close(Sock),
            PPid ! {done}
    end.
