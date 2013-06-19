-module(banking_worker).

-export([konto_anlegen/1]).


	
db_oeffnen(error, Reason) ->
	io:format("Error: ~p", [Reason]).


Daten_lesen(Kontonr) ->
	{Response, Reason} = dets:open_file(konten, [{file, "db_konten"}, {type, set}]),
	db_oeffnen(Response, Reason),
	Konto = dets:lookup(konten, Kontonr).
	
	
	
	
	
	
Daten_schreiben(konto_anlegen) -> 
	{Response, Reason} = dets:open_file(konten, [{file, "db_konten"}, {type, set}]),
	db_oeffnen(Response, Reason),
	Kontonr_temp = first(konten),
	Konten_init(Kontonr_tmp), %Für den fall dass noch keine Konten existieren
	{0, HoechsteKontonr} = lookup(konten, 0),
	NeueKontonr = HoechsteKontonr + 1,
	insert(konten, {NeueKontonr, 
								{sperrvermerk, false},
								{vermoegen, 0},
								{maxDispo, 0},
								{dispoZins, 0},
								{transaktionsliste, []}}),
								
	
	
	
Konten_init('$end_of_table') ->
	insert(konten, {0, 0});
Konten_init(_) -> .

	Kontonr = first(konten),
	get_next_konto(Kontonr, 0)
	
get_next_konto('$end_of_table', Letzte_kontonr) ->
	


get_next_konto(AlteKontonr, _) -> 
	NeueKontonr = next(konten, AlteKontonr),
	get_next_konto(NeueKontonr, AlteKontonr).
	


Daten_schreiben(Konto) -> 
	dets:open_file(konten, [{file, "db_konten"}, {type, set}])


konto_anlegen(PID) ->
	
   % TODO: Kontonummer ist der Key
   PID ! {ok, "12345"},
   receive
      abc -> x
   end.
   
  