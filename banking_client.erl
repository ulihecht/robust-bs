-module(banking_client).
-export([start/1, loop/0]).

start(RegisterName) ->
  register(RegisterName, spawn(banking_client, loop, [])),
    io:format("banking_client started~n").
  
loop() ->
  receive
    konto_anlegen 	-> 	banking_server:konto_anlegen(self()),
						loop();
    stop 			-> 	true,
						io:format("banking_client closed~n");
	{ok, Message}	->	io:format("OK: ~s~n", [Message]),
						loop;
	{nok, Message}	->	io:format("NOK: ~s~n", [Message]),
						loop
 end.
%, receive  {ok, Nummer} -> {reply, Nummer, blubb} end.

%c(banking_server), c(banking_worker), {ok, PID} = banking_server:start(), banking_server:konto_anlegen(PID).

