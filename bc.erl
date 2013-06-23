-module(bc).
-export([start/1, loop/0]).

% Um mehrere Clients erstellen zu können
start(RegisterName) ->
  register(RegisterName, spawn(bc, loop, [])),
    io:format("Banking Client wurde gestartet~n").
loop() ->
   receive
      konto_anlegen ->
         gen_server:cast({bs, 'bs@localhost'}, {konto_anlegen, self()}),
         loop();
      {kontostand_abfragen, KontoNr} ->
         gen_server:cast({bs, 'bs@localhost'}, {kontostand_abfragen, self(), KontoNr}),
         loop();
      {historie, KontoNr} ->
         gen_server:cast({bs, 'bs@localhost'}, {historie, self(), KontoNr}),
         loop();
      {konto_loeschen, KontoNr} ->
         gen_server:cast({bs, 'bs@localhost'}, {konto_loeschen, self(), KontoNr}),
         loop();
      {geld_einzahlen, Kontonr, Verwendungszweck, Betrag} ->
         gen_server:cast({bs, 'bs@localhost'}, {geld_einzahlen, self(), Kontonr, Verwendungszweck, Betrag}),
         loop();
      {geld_auszahlen, KontoNr, Betrag} ->
         gen_server:cast({bs, 'bs@localhost'}, {geld_auszahlen, self(), KontoNr, Betrag}),
         loop();
      {geld_ueberweisen, ZielKontonr, KontoNr, Betrag} ->
         gen_server:cast({bs, 'bs@localhost'}, {geld_ueberweisen, self(), ZielKontonr, KontoNr, Betrag}),
         loop();
      {dispokredit_beantragen, KontoNr} ->
         gen_server:cast({bs, 'bs@localhost'}, {dispokredit_beantragen, self(), KontoNr}),
         loop();
      {konto_sperren, KontoNr} ->
         gen_server:cast({bs, 'bs@localhost'}, {konto_sperren, self(), KontoNr}),
         loop();
      {konto_entsperren, KontoNr} ->
         gen_server:cast({bs, 'bs@localhost'}, {konto_entsperren, self(), KontoNr}),
         loop();
      stop ->
         true,
         io:format("Banking Client wurde beendet~n");
      {ok, Message} ->
         io:format("OK: ~p~n", [Message]),
         loop();
      {nok, Message} ->
         io:format("Fehler: ~p~n", [Message]),
         loop();
       _ -> io:format("Unbekanntes Kommando~n"),
         loop()
   end.