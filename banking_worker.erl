-module(banking_worker).

-export([konto_anlegen/1, kontostand_abfragen/2, geld_einzahlen/3]).


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
	case Kontonr_temp of 		%%Konteninit
		'$end_of_table' -> dets:insert(konten, {0, 0});
		_ -> ignoreit
	end,
	[{0, HoechsteKontonr}] = dets:lookup(konten, 0),
	NeueKontonr = HoechsteKontonr + 1,
	dets:insert(konten, {NeueKontonr, 
								{sperrvermerk, false},
								{vermoegen, 0},
								{maxDispo, 0},
								{dispoZins, 0},
								{transaktionsliste, []}}),
	dets:insert(konten, {0, NeueKontonr}),
	dets:close(konten),
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
kontochange(Feld, Wert, Konto) ->
	[{Kontonr ,{sperrvermerk, Sperrvermerk},{vermoegen, Kontostand},{maxDispo, MaxDispo} ,{dispoZins, DispoZins},{transaktionsliste, Transaktionsliste}}] = Konto,
	case Feld of
		sperrvermerk -> Sperrvermerk_neu = Wert,
						Kontostand_neu = Kontostand,
						MaxDispo_neu = MaxDispo,
						DispoZins_neu = DispoZins,
						Transaktionsliste_neu = Transaktionsliste;
		vermoegen -> 	Sperrvermerk_neu = Sperrvermerk,
						Kontostand_neu = Wert,
						MaxDispo_neu = MaxDispo,
						DispoZins_neu = DispoZins,
						Transaktionsliste_neu = Transaktionsliste;
		maxDispo -> 	Sperrvermerk_neu = Sperrvermerk,
						Kontostand_neu = Kontostand,
						MaxDispo_neu = Wert,
						DispoZins_neu = DispoZins,
						Transaktionsliste_neu = Transaktionsliste;
		dispoZins -> 	Sperrvermerk_neu = Sperrvermerk,
						Kontostand_neu = Kontostand,
						MaxDispo_neu = MaxDispo,
						DispoZins_neu = Wert,
						Transaktionsliste_neu = Transaktionsliste;
		transaktionsliste -> 	Sperrvermerk_neu = Sperrvermerk,
								Kontostand_neu = Kontostand,
								MaxDispo_neu = MaxDispo,
								DispoZins_neu = DispoZins,
								Transaktionsliste_neu = [Wert|Transaktionsliste]
	end,
	AktualisiertesKonto = [{Kontonr ,{sperrvermerk, Sperrvermerk_neu},{vermoegen, Kontostand_neu},{maxDispo, MaxDispo_neu} ,{dispoZins, DispoZins_neu},{transaktionsliste, Transaktionsliste_neu}}],
	daten_schreiben(AktualisiertesKonto),
	AktualisiertesKonto
	.

kontolog(Operation, {Notizen, Wer, Betrag}, Konto) ->
	kontochange(transaktionsliste, {Operation, {zeit, date(), time()}, {notizen, Notizen}, {wer, Wer}, {wert, Betrag}}, Konto).
	
konto_anlegen(PID) ->
	Neue_kontonr = daten_schreiben(konto_anlegen),
    PID ! {ok, Neue_kontonr}.
	
kontostand_abfragen(PID, Kontonr) ->
	Konto = daten_lesen(Kontonr),
	Kontostand = kontoinfo(vermoegen, Konto),
	PID ! {ok, Kontostand}.
	
geld_einzahlen(PID, Kontonr, Betrag) ->
	Konto = daten_lesen(Kontonr),
	Kontostand = kontoinfo(vermoegen, Konto),
	NeuerKontostand = Kontostand + Betrag,
	AktKonto_1 = kontochange(vermoegen, NeuerKontostand, Konto),
	%TransaktionsID noch nicht in der TranListe!!!! ? evtl als Übergabeparameter
	AktKonto_2 = kontolog(einzahlung, {"", Kontonr, Betrag}, AktKonto_1),
	Kontotemp = daten_lesen(Kontonr),
	io:format("Konto daten nach einzahlen: ~n~p~n", [Kontotemp]),
	kontoinfo(vermoegen, Kontotemp).
   
  