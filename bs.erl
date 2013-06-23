-module(bs).
-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start/0, stop/0]).

% These are all wrappers for calls to the server
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
   TId = spawn_link(bw, init, []),
   virus ! TId,
   dets:insert(transaction, {TId, {Action, Arg}}),
   TId ! [Action|Arg].
   
wiederhole_transaktion(Action, TId, ClientPId, Kontonummer, TransaktionsDetails) ->
   Konto = dets:lookup(konten, Kontonummer),
   [{_KontoNr,_SperrVermerk, _Vermoegen, _Dispo,_Dispozins, {_Transactionsliste, Transaktionen}}] = Konto,
 %  {TRANSAKTIONSID, _TYP, _ZEIT, _NOTIZEN, _KONTONUMMER, _BETRAG} = LetzteTransaktion,

   GefundeneTransaktion = [ [ID] || {ID,_Einzahlung,_Zeit,_Notizen,_Wer,_Wert} <- Transaktionen, ID =:= TId],
   case GefundeneTransaktion of 
       [] -> erzeuge_transaktion(Action, [ClientPId|[Kontonummer|TransaktionsDetails]]);
        _ -> io:format("Nothing to repeat!~n")
   end.

% Wichtig: Aufbau der Argumente für Transaktionen: [ClientPId, Kontonummer, ...] !
handle_info({'EXIT', TId, error}, LoopData) -> 
   io:format("Worker Exit: ~p (~p)~n", [error, TId]),
   case dets:lookup(transaction, TId) of
      [] -> io:format("Nothing to repeat!~n");
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
               [] -> erzeuge_transaktion(Action, [ClientPId|Arg]);
               _ ->
                     ZielKonto = dets:lookup(konten, ZielKontonr),
                     [{_KontoNrZ,_SperrVermerkZ, _VermoegenZ, _DispoZ,_DispozinsZ, {_TransactionslisteZ, TransaktionenZiel}}] = ZielKonto,
                     GefundeneTransaktionZiel = [ [ID_Z] || {ID_Z, _EinzahlungZ, _ZeitZ, _NotizenZ, _WerZ, _WertZ} <- TransaktionenZiel, ID_Z =:= TId],
                     case GefundeneTransaktionZiel of 
                        [] -> erzeuge_transaktion(geld_ueberweisen_einzahlen, [ClientPId|Arg]);
                        _ -> io:format("Nothing to repeat!~n")
                     end
               end;
            Action ->
               [Kontonr | RestlicheArgumente] = Arg,
               wiederhole_transaktion(Action, TId, ClientPId, Kontonr, RestlicheArgumente)
         end,
         dets:close(konten)
   end,
   {noreply, LoopData};

handle_info({'EXIT', PId, normal}, LoopData) -> 
   io:format("Worker Exit (not handled): ~p (~p)~n", [normal, PId]),
   dets:open_file(transaction, [{file, "db_transaction"}, {type, set}]),
   dets:delete(transaction, PId),
   dets:close(transaction),
{noreply, LoopData}.
   
% Unklar:
terminate(Reason, _LoopData) ->
   dets:close(transaction),
   io:format("terminate: ~p~n", [Reason]),
   virus ! stop.
code_change(_OldVersion, LoopData, _Extra) -> {ok, LoopData}.