-module(bs).
-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start/0, stop/0]).

start() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
stop() -> gen_server:cast(?MODULE, stop).

init([]) ->
   process_flag(trap_exit, true),
	dets:open_file(transaction, [{file, "db_transaction"}, {type, set}]),
   spawn(virus, init, []),
   {ok, 0}.
   
handle_call(_Action, _From, LoopData) ->
	{noreply, LoopData}.
   
handle_cast(stop, LoopData) ->
   dets:close(transaction),
   {stop, normal, LoopData};
   
handle_cast({konto_anlegen, ClientPId}, LoopData) ->
   erzeuge_transaktion(konto_anlegen, [ClientPId]),
   {noreply, LoopData};
    
handle_cast({konto_loeschen, ClientPId, Kontonr}, LoopData) ->
   erzeuge_transaktion(konto_loeschen, [ClientPId, Kontonr]),
   {noreply, LoopData};
       
handle_cast({kontostand_abfragen, ClientPId, Kontonr}, LoopData) ->
   erzeuge_transaktion(kontostand_abfragen, [ClientPId, Kontonr]),
   {noreply, LoopData};
   
handle_cast({historie, ClientPId, KontoNr}, LoopData) ->
   erzeuge_transaktion(historie, [ClientPId, KontoNr]),
   {noreply, LoopData};
   
handle_cast({geld_einzahlen, ClientPId, Kontonr, Verwendungszweck, Betrag}, LoopData) ->
   erzeuge_transaktion(geld_einzahlen, [ClientPId, Kontonr, Verwendungszweck, Betrag]),
   {noreply, LoopData};
   
handle_cast({geld_auszahlen, ClientPId, KontoNr, Betrag}, LoopData) ->
   erzeuge_transaktion(geld_auszahlen, [ClientPId, KontoNr, Betrag]),
   {noreply, LoopData};
   
handle_cast({geld_ueberweisen, ClientPId, ZielKontonr, KontoNr, Betrag}, LoopData) ->
   erzeuge_transaktion(geld_ueberweisen, [ClientPId, ZielKontonr, KontoNr, Betrag]),
   {noreply, LoopData};
   
handle_cast({dispokredit_beantragen, ClientPId, Kontonr}, LoopData) ->
   erzeuge_transaktion(dispokredit_beantragen, [ClientPId, Kontonr]),
   {noreply, LoopData};
   
handle_cast({konto_sperren, ClientPId, Kontonr}, LoopData) ->
   erzeuge_transaktion(konto_sperren, [ClientPId, Kontonr]),
   {noreply, LoopData};

handle_cast({konto_entsperren, ClientPId, Kontonr}, LoopData) ->
   erzeuge_transaktion(konto_entsperren, [ClientPId, Kontonr]),
   {noreply, LoopData}.

erzeuge_transaktion(Action, Arg) ->
   % Worker starten und seine PID ermitteln, die als Transaktions-ID verwendet wird
   TId = spawn_link(bw, init, []),
   virus ! TId,
   % Transaktion zur Transaktionsliste hinzuf�gen
   dets:insert(transaction, {TId, {Action, Arg}}),
   [ClientPId|_] = Arg,
   io:format("Starte Transaktion: ~p von Client: ~p mit Worker: ~p~n", [Action, ClientPId, TId]),
   % Transaktionsauftrag an Worker senden
   TId ! [Action|Arg].
   
% Wird aufgerufen, falls der f�r die Transaktion zust�ndige Worker abgest�rzt ist
wiederhole_transaktion(Action, TId, ClientPId, Kontonummer, TransaktionsDetails) ->
   Konto = dets:lookup(konten, Kontonummer),
   [{_KontoNr,_SperrVermerk, _Vermoegen, _Dispo,_Dispozins, {_Transactionsliste, Transaktionen}}] = Konto,
   %{TRANSAKTIONSID, _TYP, _ZEIT, _NOTIZEN, _KONTONUMMER, _BETRAG} = LetzteTransaktion,

   % Abgebrochene Transaktion in Transaktionsliste suchen
   GefundeneTransaktion = [ [ID] || {ID,_Einzahlung,_Zeit,_Notizen,_Wer,_Wert} <- Transaktionen, ID =:= TId],
   case GefundeneTransaktion of 
      [] ->
         % Nicht gefunden => Transaktion muss wiederholt werden
         erzeuge_transaktion(Action, [ClientPId|[Kontonummer|TransaktionsDetails]]);
      _ ->
         % Gefunden = Worker ist nach erfolgreicher Durchf�hrung der Transaktion abgest�rzt
         io:format("Transaktion bereits erfolgreich durchlaufen!~n")
   end.

% Wichtig: Aufbau der Argumente f�r Transaktionen: [ClientPId, Kontonummer, ...] !
handle_info({'EXIT', TId, error}, LoopData) -> 
   fprof:trace(start),
   io:format("Worker abgestuerzt: ~p (~p)~n", [error, TId]),
   case dets:lookup(transaction, TId) of
      [] -> io:format("Transaktion bereits erfolgreich durchlaufen!~n");
      [{TId, {Action, [ClientPId|Arg]}}] ->
         dets:delete(transaction, TId),
         dets:open_file(konten, [{file, "db_konten"}, {type, set}]),
         case Action of 
            konto_anlegen ->
               erzeuge_transaktion(konto_anlegen, [ClientPId]);
            geld_ueberweisen ->
               [ZielKontonr, UrsprungKontonr, _Betrag] = Arg,
               UrsprungKonto = dets:lookup(konten, UrsprungKontonr),
               [{_KontoNrU,_SperrVermerkU, _VermoegenU, _DispoU,_DispozinsU, {_TransactionslisteU, TransaktionenUrsprung}}] = UrsprungKonto,
               
               GefundeneTransaktion = [ [ID] || {ID, _Einzahlung, _Zeit, _Notizen, _Wer, _Wert} <- TransaktionenUrsprung, ID =:= TId],
               case GefundeneTransaktion of 
                  [] ->
                     % keine Transaktion gefunden => Transaktion muss wiederholt werden
                     erzeuge_transaktion(Action, [ClientPId|Arg]);
                  _ ->
                     % Transaktion gefunden; wurde sie auch auf dem Zielkonto durchgef�hrt?
                     ZielKonto = dets:lookup(konten, ZielKontonr),
                     [{_KontoNrZ,_SperrVermerkZ, _VermoegenZ, _DispoZ,_DispozinsZ, {_TransactionslisteZ, TransaktionenZiel}}] = ZielKonto,
                     GefundeneTransaktionZiel = [ [ID_Z] || {ID_Z, _EinzahlungZ, _ZeitZ, _NotizenZ, _WerZ, _WertZ} <- TransaktionenZiel, ID_Z =:= TId],
                     case GefundeneTransaktionZiel of 
                        [] ->
                           % nein => Nur am Zielkonto wiederholen
                           erzeuge_transaktion(geld_ueberweisen_einzahlen, [ClientPId|Arg]);
                        _ ->
                           io:format("Transaktion bereits erfolgreich durchlaufen!~n")
                     end
               end;
            Action ->
               [Kontonr | RestlicheArgumente] = Arg,
               wiederhole_transaktion(Action, TId, ClientPId, Kontonr, RestlicheArgumente)
         end,
         dets:close(konten)
   end,
   fprof:trace(stop),
   {noreply, LoopData};

handle_info(prof_start, LoopData) -> 
   io:format("Profiling gestartet~n"),
   fprof:trace(start),
   cprof:start(),
   {noreply, LoopData};

handle_info(prof_stop, LoopData) -> 
   cprof:pause(),
   fprof:trace(stop),
   fprof:profile(),
   fprof:analyse([{dest, []}]),
   io:format("Profiling gestoppt~n"),
   {noreply, LoopData};
   % Wird aufgerufen, falls der Worker erfolgreich beendet wurde
   handle_info({'EXIT', PId, normal}, LoopData) -> 
      io:format("Transaktion beendet (Worker: ~p)~n", [PId]),
      % Transaktion
      dets:delete(transaction, PId),
{noreply, LoopData}.


terminate(Reason, _LoopData) ->
   dets:close(transaction),
   io:format("BEENDET: ~p~n", [Reason]),
   virus ! stop.
code_change(_OldVersion, LoopData, _Extra) -> {ok, LoopData}.