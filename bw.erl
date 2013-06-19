-module(bw).

-export([konto_anlegen/1, kontostand_abfragen/2, init/0]).


error_handling({error, Reason}) ->
	io:format("Error: ~p ~n", [Reason]),
	dets:close(konten),
	exit(Reason)
	;
error_handling({ok, konten}) ->
	ok.

	

		
get_next_konto('$end_of_table', Letzte_kontonr) ->
	Letzte_kontonr;
get_next_konto(AlteKontonr, _) -> 
	NeueKontonr = dets:next(konten, AlteKontonr),
	get_next_konto(NeueKontonr, AlteKontonr).
	

daten_lesen(Kontonr) ->
	{Response, Reason} = dets:open_file(konten, [{file, "db_konten"}, {type, set}]),
	error_handling({Response, Reason}),
	Konto = dets:lookup(konten, Kontonr),
	case Konto of
		[] -> 	error_handling({error, "Konto nicht gefunden"});
		[_] -> ignoreit
	end,
	dets:close(konten),
	Konto
	.
	

daten_schreiben(konto_anlegen) -> 
	{Response, Reason} = dets:open_file(konten, [{file, "db_konten"}, {type, set}]),
	Kontonr_temp = dets:first(konten),
	io:format("vor case~n"),
	case Kontonr_temp of 		%%Konteninit
		'$end_of_table' -> dets:insert(konten, {0, 0});
		_ -> ignoreit
	end,
	io:format("nach case~n"),
	[{0, HoechsteKontonr}] = dets:lookup(konten, 0),
	NeueKontonr = HoechsteKontonr + 1,
	io:format("vor insert~n"),
	io:format("neuekontonr: ~p ~n", [NeueKontonr]),
	dets:insert(konten, {NeueKontonr, 
								{sperrvermerk, false},
								{vermoegen, 0},
								{maxDispo, 0},
								{dispoZins, 0},
								{transaktionsliste, []}}),
	dets:insert(konten, {0, NeueKontonr}),
	dets:close(konten),
	io:format("nach close~n"),
	NeueKontonr;
   
daten_schreiben(Konto) -> 
	dets:open_file(konten, [{file, "db_konten"}, {type, set}]),
	dets:insert(konten, Konto),
	dets:close(konten).	

kontoinfo(Feld, Konto) ->
	[{Kontonr ,{sperrvermerk, Sperrvermerk},{vermoegen, Kontostand},{maxDispo, MaxDispo} ,{dispoZins, DispoZins},{transaktionsliste, Transaktionsliste}}] = Konto,
	case Feld of
		sperrvermerk -> Ruekgabe = Sperrvermerk;
		vermoegen -> Ruekgabe = Kontostand;
		maxDispo -> Ruekgabe = MaxDispo;
		dispoZins -> Ruekgabe = DispoZins;
		transaktionsliste -> Ruekgabe = Transaktionsliste
	end,	
	Ruekgabe.
	
	
konto_anlegen(ClientPId) ->
	Neue_kontonr = daten_schreiben(konto_anlegen),
   ClientPId ! {ok, Neue_kontonr}.
	
kontostand_abfragen(ClientPId, Kontonr) ->
	Konto = daten_lesen(Kontonr),
	Kontostand = kontoinfo(vermoegen, Konto),
	ClientPId ! {ok, Kontostand}.
   
   

 init() -> 
 io:format("init bw~n"),
 receive
      [konto_anlegen, ClientPId] -> konto_anlegen(ClientPId);
      [kontostand_abfragen, ClientPId, Kontonr] -> kontostand_abfragen(ClientPId, Kontonr)
   end.
   
  