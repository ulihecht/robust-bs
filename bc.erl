-module(bc).
-export([start/1, loop/0]).

% Um mehrere Clients erstellen zu können
start(RegisterName) ->
  register(RegisterName, spawn(bc, loop, [])),
    io:format("banking_client started~n").
  
loop() ->
   receive
      konto_anlegen ->
         gen_server:cast({bs, 'bs@localhost'}, {konto_anlegen, self()}),
         loop();
      {kontostand_abfragen, KontoNr} ->
         gen_server:cast({bs, 'bs@localhost'}, {kontostand_abfragen, self(), KontoNr}),
         loop();
      stop ->
         true,
         io:format("banking_client closed~n");
      {ok, Message} ->
         io:format("OK: ~p~n", [Message]),
         loop();
      {nok, Message} ->
         io:format("NOK: ~p~n", [Message]),
         loop()
   end.
%, receive  {ok, Nummer} -> {reply, Nummer, blubb} end.

%c(banking_server), c(banking_worker), {ok, PID} = banking_server:start(), banking_server:konto_anlegen(PID).

 