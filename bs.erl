-module(bs).
-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start/0, stop/0]).

% These are all wrappers for calls to the server
start() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
stop() -> gen_server:cast(?MODULE, stop).

% This is called when a connection is made to the server
init([]) ->
	dets:open_file(transaction, [{file, "db_transaction"}, {type, set}]),
   spawn(virus, init, []),
   {ok, 0}.
   
handle_call(_Action, _From, LoopData) ->
	{noreply, LoopData}.
   
handle_cast(stop, LoopData) ->
   dets:close(transaction),
   {stop, normal, LoopData};
   
handle_cast({konto_anlegen, ClientPId}, LoopData) ->
   create_transaction(konto_anlegen, [ClientPId]),
   {noreply, LoopData};
    
handle_cast({konto_loeschen, ClientPId, Kontonr}, LoopData) ->
   create_transaction(konto_loeschen, [ClientPId, Kontonr]),
   {noreply, LoopData};
       
handle_cast({kontostand_abfragen, ClientPId, Kontonr}, LoopData) ->
   create_transaction(kontostand_abfragen, [ClientPId, Kontonr]),
   {noreply, LoopData};
   
handle_cast({historie, ClientPId, KontoNr}, LoopData) ->
   create_transaction(historie, [ClientPId, KontoNr]),
   {noreply, LoopData};
   
handle_cast({geld_einzahlen, ClientPId, Kontonr, Verwendungszweck, Betrag}, LoopData) ->
   create_transaction(geld_einzahlen, [ClientPId, Kontonr, Verwendungszweck, Betrag]),
   {noreply, LoopData};
   
handle_cast({geld_auszahlen, ClientPId, KontoNr, Betrag}, LoopData) ->
   create_transaction(geld_auszahlen, [ClientPId, KontoNr, Betrag]),
   {noreply, LoopData};
   
handle_cast({geld_ueberweisen, ClientPId, ZielKontonr, KontoNr, Betrag}, LoopData) ->
   create_transaction(geld_ueberweisen, [ClientPId, ZielKontonr, KontoNr, Betrag]),
   {noreply, LoopData};
   
handle_cast({dispokredit_beantragen, ClientPId, Kontonr}, LoopData) ->
   create_transaction(dispokredit_beantragen, [ClientPId, Kontonr]),
   {noreply, LoopData};
   
handle_cast({konto_sperren, ClientPId, Kontonr}, LoopData) ->
   create_transaction(konto_sperren, [ClientPId, Kontonr]),
   {noreply, LoopData};

handle_cast({konto_entsperren, ClientPId, Kontonr}, LoopData) ->
   create_transaction(konto_entsperren, [ClientPId, Kontonr]),
   {noreply, LoopData}.
 
create_transaction(Action, Arg) ->
   process_flag(trap_exit, true),
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
       [] -> create_transaction(Action, [ClientPId|[Kontonummer|TransaktionsDetails]]);
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
               create_transaction(konto_anlegen, [ClientPId]);
            geld_ueberweisen ->
               [ZielKontonr, Kontonr, Betrag] = Arg,
               [{_KontoNr, {sperrvermerk, SperrVermerkKonto1}, _Vermoegen, _Dispo, _Dispozins, _Transaktion}] = dets:lookup(konten, Kontonr),
               [{_, {sperrvermerk, SperrVermerkKonto2}, _, _, _, _}] = dets:lookup(konten, ZielKontonr),     
               case SperrVermerkKonto1 or SperrVermerkKonto2 of
                  true ->
                     % Worker soll Fehlermeldung ausgeben, dass Konto gesperrt ist
                     create_transaction(geld_ueberweisen, [ClientPId| Arg]);
                  false ->
                     % Da wir nicht wissen ob ein Teil eventuell schon erledigt wurde, wird hier die Überweisung gesplittet
                     wiederhole_transaktion(geld_auszahlen, TId, ClientPId, Kontonr, [Betrag]),
                     wiederhole_transaktion(geld_einzahlen, TId, ClientPId, ZielKontonr, [Kontonr, Betrag])
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