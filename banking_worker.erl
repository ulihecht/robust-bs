-module(banking_worker).

-export([konto_anlegen/1]).


error_handling({error, Reason}) ->
	io:format("Error: ~p ~n", [Reason]),
	dets:close(konten),
	exit(Reason)
	.

	

		
get_next_konto('$end_of_table', Letzte_kontonr) ->
	Letzte_kontonr;
get_next_konto(AlteKontonr, _) -> 
	NeueKontonr = dets:next(konten, AlteKontonr),
	get_next_konto(NeueKontonr, AlteKontonr).
	

daten_lesen(Kontonr) ->
	{Response, Reason} = dets:open_file(konten, [{file, "db_konten"}, {type, set}]),
	error_handling({Response, Reason}),
	Konto = dets:lookup(konten, Kontonr),
	case Konto = {error, Reason} of
		true -> 
				error_handling(Konto)
	end,
	dets:close(konten)
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

konto_anlegen(PID) ->
	Neue_kontonr = daten_schreiben(konto_anlegen),
    PID ! {ok, Neue_kontonr}.
   
  