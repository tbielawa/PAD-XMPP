%%% File    : simple_shell.erl
%%% Author  : Tim Bielawa <timbielawa@gmail.com>
%%% Description : Testing a simple interactive server listening on socket.
%%% Created : 18 Dec 2009 by Tim Bielawa <timbielawa@gmail.com>
-module(simple_shell).
-export([start/0]).

start() ->
    start(5678, {attempt, 1}).

start(Port, {attempt, Attempt}) ->
    case gen_tcp:listen(Port, [binary, {packet, 0},
			       inet, {active, false},
			       {reuseaddr, true}]) of
	{ok, LSocket} -> 
	    server(LSocket);
	{error,eaddrinuse} ->
	    io:format("MSG: Socket in use, attempt #~p~n", [Attempt]),
	    retry_listener(Port, {attempt, Attempt+1});
	{error,Error} ->
	    io:format("MSG: Other error received:~n~p~n", [Error])
    end.

server(LSocket) ->
    io:format("TCP Listener activated~n"),
    io:format("Waiting for incoming socket.~n"),
    {ok, Sock} = gen_tcp:accept(LSocket),
    gen_tcp:close(LSocket),
    io:format("TCP Socket connection made~n"),
    {ok, {Address, Port}} = inet:peername(Sock),
    io:format("Connection from: ~p on port ~p~n", [Address, Port]),
    PacketHandler = spawn(fun() -> get_packets() end),
    spawn(fun() -> do_recv(PacketHandler, Sock) end),
    io:format("Spawnned do_recv packet slurper~n"),
    start().

retry_listener(Port, {attempt, Attempt}) ->
    case Attempt of
	5 ->
	    io:format("Max number of attempts (5) reached to open socket listener.~n"),
	    io:format("Exiting....~n");
	_ ->
	    start(Port, {attempt,Attempt})
    end.

get_packets() ->
    io:format("get_packets() waiting...~n"),
    receive
	{ok, Pkt, Sock} ->
	    Tokenized = string:tokens(binary_to_list(Pkt), "\r\n"),
	    io:format("They said: ~p~n", [Tokenized]),
	    io:format("MSG: Responding to connection~n"),
	    gen_tcp:send(Sock, Pkt),
	    get_packets();
	{done} ->
	    io:format("MSG: Connection closed~n")
    end.

do_recv(PPid, Sock) ->
    gen_tcp:send(Sock, "PAD-XMPP> "),
    case gen_tcp:recv(Sock, 0) of
	{ok, B} ->
	    io:format("do_recv: received binary transfer~n"),
	    PPid ! {ok, B, Sock},
	    do_recv(PPid, Sock);
	{error, closed} ->
	    ok = gen_tcp:close(Sock),
	    PPid ! {done}
    end.
    
