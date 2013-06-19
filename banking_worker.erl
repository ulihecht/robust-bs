-module(banking_worker).

-export([konto_anlegen/1]).


error_handling({error, Reason}) ->
	io:format("Error: ~p ~n", [Reason]),
	dets:close(konten),
	exit(Reason)
	.
	
	
daten_lesen(Kontonr) ->
	{Response, Reason} = dets:open_file(konten, [{file, "db_konten"}, {type, set}]),
	error_handling(Response, Reason),
	Konto = dets:lookup(konten, Kontonr),
	error_handling(Konto),
	dets:close(konten)
	.
	
	
daten_schreiben(konto_anlegen) -> 
	{Response, Reason} = dets:open_file(konten, [{file, "db_konten"}, {type, set}]),
	db_oeffnen(Response, Reason),
	Kontonr_temp = dets:first(konten),
	Konten_init(Kontonr_tmp), %Für den fall dass noch keine Konten existieren
	{0, HoechsteKontonr} = dets:lookup(konten, 0),
	NeueKontonr = HoechsteKontonr + 1,
	dets:insert(konten, {NeueKontonr, 
								{sperrvermerk, false},
								{vermoegen, 0},
								{maxDispo, 0},
								{dispoZins, 0},
								{transaktionsliste, []}}),
	dets:close(konten),
	NeueKontonr;
	
daten_schreiben(Konto) -> 
	dets:open_file(konten, [{file, "db_konten"}, {type, set}]),
	dets:insert(konten, Konto),
	dets:close(konten).

konten_init('$end_of_table') ->
	dets:insert(konten, {0, 0});
	
konten_init(_) -> .

%	Kontonr = dets:first(konten),
%	get_next_konto(Kontonr, 0)
	
get_next_konto('$end_of_table', Letzte_kontonr) ->
	


get_next_konto(AlteKontonr, _) -> 
	NeueKontonr = next(konten, AlteKontonr),
	get_next_konto(NeueKontonr, AlteKontonr).
	

konto_anlegen(PID) ->
	
   % TODO: Kontonummer ist der Key
   PID ! {ok, "12345"},
   receive
      abc -> x
   end.
   
  