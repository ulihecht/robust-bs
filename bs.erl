-module(bs).
-behaviour(gen_server).

-export([init/1, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([start/0, stop/0, konto_anlegen/1]).

% These are all wrappers for calls to the server
start() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []), loop().
stop() -> gen_server:cast(?MODULE, stop).
konto_anlegen(ClientPID) -> io:format("blubba~n"), gen_server:call(bs, {konto_anlegen, ClientPID}).

% This is called when a connection is made to the server
init([]) ->
   process_flag(trap_exit, true),
	dets:open_file(transaction, [{file, "db"}, {type, set}]),
   {ok, 0}.
 

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
   TId = spawn(bw, init, []), % spawn_link ?
   dets:insert(transaction, {TId, {Action, Arg}}),
   TId ! [Action|Arg].
   
loop() ->
   receive
    %  {konto_anlegen, ClientPID} -> io:format("blubba ~s ~n", [?MODULE]), gen_server:call(?MODULE, {konto_anlegen, ClientPID}), loop;
   %   {konto_loeschen, ClientPID, Kontonr} -> gen_server:call(?MODULE, {konto_loeschen, ClientPID, Kontonr}), loop;
      {konto_sperren, ClientPID, Kontonr} -> gen_server:call(?MODULE, {konto_sperren, ClientPID, Kontonr}), loop;
      {konto_entsperren, ClientPID, Kontonr} -> gen_server:call(?MODULE, {konto_entsperren, ClientPID, Kontonr}), loop;
  %    {geld_einzahlen, ClientPID, Kontonr, Betrag} -> gen_server:call(?MODULE, {geld_einzahlen, ClientPID, Kontonr}), loop;
  %    {geld_ueberweisen, ClientPID, Ziel_Kontonr, Client_Kontonr, Betrag} -> gen_server:call(?MODULE, {geld_ueberweisen, ClientPID, Ziel_Kontonr, Client_Kontonr, Betrag}), loop;
  %    {geld_abheben, ClientPID, Kontonr, Betrag} -> gen_server:call(?MODULE, {geld_abheben, ClientPID, Kontonr, Betrag}), loop;
   %   {kontostand_abfragen, ClientPID, Kontonr} -> gen_server:call(?MODULE, {kontostand_abfragen, ClientPID, Kontonr}), loop;
      {dispokredite_beantragen, ClientPID, Kontonr} -> gen_server:call(?MODULE, {dispokredite_beantragen, ClientPID, Kontonr}), loop;
      {'EXIT', PId, Reason} ->
         io:format("Exit ~p (~p)", [Reason, PId]),
         case dets:lookup(transaction, PId) of
            [{transaction, {Action, Arg}}] ->
               % TODO: Prüfen ob Transaktion schon ausgeführt wurde
               dets:delete(transaction, PId),
               create_transaction(Action, Arg)
         end,
         loop();
      A -> A % io:format("~s~n", 
   end.
   
   
% Unklar:
handle_info(_Message, Library) -> {noreply, Library}.

% Unklar:
terminate(_Reason, _Library) ->
   dets:close(transaction),
   terminate_aufgerufen.
code_change(_OldVersion, Library, _Extra) -> {ok, Library}.