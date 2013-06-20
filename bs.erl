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
          
handle_cast({geld_einzahlen, ClientPId, Kontonr, Ursprung, Betrag}, LoopData) ->
   create_transaction(geld_einzahlen, [ClientPId, Kontonr, Ursprung, Betrag]),
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
   dets:insert(transaction, {TId, {Action, Arg}}),
   TId ! [Action|Arg].

handle_info({'EXIT', PId, error}, LoopData) -> 
io:format("Worker Exit: ~p (~p)~n", [error, PId]),
FoundTransaction = dets:lookup(transaction, PId),
 case FoundTransaction of
     [] -> io:format("Nothing to repeat!~n");
     [{PId, {Action, Arg}}] ->
         dets:delete(transaction, PId),
         %TODO: Checken ob die Transaktion schoneinmal ausgeführt wurde mit PId als ID. Fall ja hier abbrechen, da sonst Transaktionen doppelt ausgeführt werden!
         create_transaction(Action, Arg);
     _Else -> 
         io:format("Something wrong here??? ~p~n", [FoundTransaction])
 end,
{noreply, LoopData};
handle_info({'EXIT', PId, Reason}, LoopData) -> 
io:format("Worker Exit (not handled): ~p (~p)~n", [Reason, PId]),
{noreply, LoopData}.
   


% Unklar:
terminate(Reason, _LoopData) ->
   dets:close(transaction),
  io:format("terminate: ~p~n", [Reason]).
  
code_change(_OldVersion, LoopData, _Extra) -> {ok, LoopData}.