-module(bc).
-export([start/1, start/2, loop/1]).

% Um mehrere Clients erstellen zu können

start(RegisterName) ->
   start(RegisterName, 'bs@localhost').

start(RegisterName, Server) ->
   register(RegisterName, spawn(bc, loop, [Server])),
   io:format("Banking Client wurde gestartet~n").
loop(Server) ->
   receive
      konto_anlegen ->
         gen_server:cast({bs, Server}, {konto_anlegen, self()}),
         loop(Server);
      {kontostand_abfragen, KontoNr} ->
         gen_server:cast({bs, Server}, {kontostand_abfragen, self(), KontoNr}),
         loop(Server);
      {historie, KontoNr} ->
         gen_server:cast({bs, Server}, {historie, self(), KontoNr}),
         loop(Server);
      {konto_loeschen, KontoNr} ->
         gen_server:cast({bs, Server}, {konto_loeschen, self(), KontoNr}),
         loop(Server);
      {geld_einzahlen, Kontonr, Verwendungszweck, Betrag} ->
         gen_server:cast({bs, Server}, {geld_einzahlen, self(), Kontonr, Verwendungszweck, Betrag}),
         loop(Server);
      {geld_auszahlen, KontoNr, Betrag} ->
         gen_server:cast({bs, Server}, {geld_auszahlen, self(), KontoNr, Betrag}),
         loop(Server);
      {geld_ueberweisen, ZielKontonr, KontoNr, Betrag} ->
         gen_server:cast({bs, Server}, {geld_ueberweisen, self(), ZielKontonr, KontoNr, Betrag}),
         loop(Server);
      {dispokredit_beantragen, KontoNr} ->
         gen_server:cast({bs, Server}, {dispokredit_beantragen, self(), KontoNr}),
         loop(Server);
      {konto_sperren, KontoNr} ->
         gen_server:cast({bs, Server}, {konto_sperren, self(), KontoNr}),
         loop(Server);
      {konto_entsperren, KontoNr} ->
         gen_server:cast({bs, Server}, {konto_entsperren, self(), KontoNr}),
         loop(Server);
      stop ->
         true,
         io:format("Banking Client wurde beendet~n");
      {ok, Message} ->
         io:format("OK: ~p~n", [Message]),
         loop(Server);
      {nok, Message} ->
         io:format("Fehler: ~p~n", [Message]),
         loop(Server);
       _ -> io:format("Unbekanntes Kommando~n"),
         loop(Server)
   end.