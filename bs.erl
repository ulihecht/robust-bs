-module(bs).
-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start/0, stop/0]).

% These are all wrappers for calls to the server
start() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
stop() -> gen_server:cast(?MODULE, stop).

% This is called when a connection is made to the server
init([]) ->
	dets:open_file(transaction, [{file, "db"}, {type, set}]),
   {ok, 0}.
handle_call({Action, Pid, Dats}, _From, LoopData) ->
	io:format("handle call"),
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
   {noreply, LoopData}.
 
create_transaction(Action, Arg) ->
   process_flag(trap_exit, true),
   TId = spawn_link(bw, init, []),
   dets:insert(transaction, {TId, {Action, Arg}}),
   TId ! [Action|Arg].
   
% loop() ->
   % receive
      % {konto_sperren, ClientPID, Kontonr} -> gen_server:call(?MODULE, {konto_sperren, ClientPID, Kontonr}), loop;
      % {konto_entsperren, ClientPID, Kontonr} -> gen_server:call(?MODULE, {konto_entsperren, ClientPID, Kontonr}), loop;
      % {dispokredite_beantragen, ClientPID, Kontonr} -> gen_server:call(?MODULE, {dispokredite_beantragen, ClientPID, Kontonr}), loop;
      % {'EXIT', PId, Reason} ->
         % io:format("Exit ~p (~p)", [Reason, PId]),
         % case dets:lookup(transaction, PId) of
            % [{transaction, {Action, Arg}}] ->
              % TODO: Prüfen ob Transaktion schon ausgeführt wurde
               % dets:delete(transaction, PId),
               % create_transaction(Action, Arg)
         % end,
         % loop();
      % _ -> io:format("Resceive BS") % io:format("~s~n", 
   % end.
   

handle_info({'EXIT', PId, Reason}, LoopData) -> 
   io:format("Worker Exit: ~p (~p)~n", [Reason, PId]),
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
   {noreply, LoopData}.

% Unklar:
terminate(Reason, LoopData) ->
   dets:close(transaction),
  io:format("terminate: ~p~n", [Reason]).
  
code_change(_OldVersion, LoopData, _Extra) -> {ok, LoopData}.