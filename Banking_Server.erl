
-module(banking_server).
-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([start/0, konto_anlegen/1]).

% These are all wrappers for calls to the server
start() -> gen_server:start({local, ?MODULE}, ?MODULE, [], []).
konto_anlegen(ClientPID) -> gen_server:call(?MODULE, {konto_anlegen, ClientPID}).
konto_loeschen(ClientPID, Kontonr) -> gen_server:call(?MODULE, {konto_loeschen, ClientPID, Kontonr}).	
konto_sperren(ClientPID, Kontonr) -> gen_server:call(?MODULE, {konto_sperren, ClientPID, Kontonr}).	
konto_entsperren(ClientPID, Kontonr) -> gen_server:call(?MODULE, {konto_entsperren, ClientPID, Kontonr}).	
geld_einzahlen(ClientPID, Kontonr, Betrag) -> gen_server:call(?MODULE, {geld_einzahlen, ClientPID, Kontonr}).	
geld_ueberweisen(ClientPID, Ziel_Kontonr, Client_Kontonr, Betrag) -> gen_server:call(?MODULE, {geld_ueberweisen, ClientPID, Ziel_Kontonr, Client_Kontonr, Betrag}).	
geld_abheben(ClientPID, Kontonr, Betrag) -> gen_server:call(?MODULE, {geld_abheben, ClientPID, Kontonr, Betrag}).	
kontostand_abfragen(ClientPID, Kontonr) -> gen_server:call(?MODULE, {kontostand_abfragen, ClientPID, Kontonr}).	
dispokredite_beantragen(ClientPID, Kontonr) -> gen_server:call(?MODULE, {dispokredite_beantragen, ClientPID, Kontonr}).



% This is called when a connection is made to the server
init([]) ->
	
	dets:open_file(transaction, [{file, "db"}, {type, set}]).
	%dets:insert(d, {a, {a,b,c}}).
	%dets:lookup(d, a).
	%case dets:lookup(d, a) of [{a, Val}] -> Val end.
	%dets:close(d).

% handle_call is invoked in response to gen_server:call
handle_call({konto_anlegen, ClientPID}, _From, _) ->
	%hier wird jetzt das neue Konto angelegt
	%{reply, Response, NewLibrary};
   % TODO: join oder so ähnlich
   spawn(banking_worker, konto_anlegen, [ClientPID]),
   {reply, ok, ?MODULE};
   %receive
   %   {ok, Nummer} -> {reply, Nummer, ?MODULE}
   %end;

handle_call({lookup, Book}, _From, Library) ->
	Response = case dict:is_key(Book, Library) of
		true ->
			{who, lists:nth(1, dict:fetch(Book, Library))};
		false ->
			{not_checked_out, Book}
	end,
	{reply, Response, Library};

handle_call({return, Book}, _From, Library) ->
	NewLibrary = dict:erase(Book, Library),
	{reply, ok, NewLibrary};

handle_call(_Message, _From, Library) ->
	{reply, error, Library}.

% We get compile warnings from gen_server unless we define these
handle_cast(_Message, Library) -> {noreply, Library}.
handle_info(_Message, Library) -> {noreply, Library}.
terminate(_Reason, _Library) ->
   dets:close(transaction),
   ok.
code_change(_OldVersion, Library, _Extra) -> {ok, Library}.