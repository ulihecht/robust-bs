-module(bw).
-export([init/0]).


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
	Kontonr = daten_schreiben(konto_anlegen),
	Konto = daten_lesen(Kontonr),
	kontolog(konto_angelegt, {"Konto wurde erstellt", Kontonr, 0}, Konto),
    PID ! {ok, Kontonr}.
	
kontostand_abfragen(PID, Kontonr) ->
	Konto = daten_lesen(Kontonr),
	Kontostand = kontoinfo(vermoegen, Konto),
	kontolog(kontostand_abfragen, {"Kontostand wurde abgefragt", Kontonr, 0}, Konto),
	Kontotemp = daten_lesen(Kontonr),
	io:format("Konto daten nach einzahlen: ~n~p~n", [Kontotemp]),
	PID ! {ok, Kontostand}.
	
geld_einzahlen(PID, Kontonr, Betrag) ->
	Konto = daten_lesen(Kontonr),
	Kontostand = kontoinfo(vermoegen, Konto),
	NeuerKontostand = Kontostand + Betrag,
	AktKonto_1 = kontochange(vermoegen, NeuerKontostand, Konto),
	%TransaktionsID noch nicht in der TranListe!!!! ? evtl als Übergabeparameter
	kontolog(einzahlung, {"", Kontonr, Betrag}, AktKonto_1),
	Kontotemp = daten_lesen(Kontonr),
	Vermoegen = kontoinfo(vermoegen, Kontotemp),
    PID ! {ok, Vermoegen}.
geld_einzahlen(PID, Kontonr, Ursprung, Betrag) ->
	Konto = daten_lesen(Kontonr),
	Kontostand = kontoinfo(vermoegen, Konto),
	NeuerKontostand = Kontostand + Betrag,
	AktKonto_1 = kontochange(vermoegen, NeuerKontostand, Konto),
	%TransaktionsID noch nicht in der TranListe!!!! ? evtl als Übergabeparameter
	kontolog(einzahlung, {"", Ursprung, Betrag}, AktKonto_1),
	Kontotemp = daten_lesen(Kontonr),
	Vermoegen = kontoinfo(vermoegen, Kontotemp),
    PID ! {ok, Vermoegen}.
   
 konto_loeschen(PID, Kontonr) ->
	dets:open_file(konten, [{file, "db_konten"}, {type, set}]),
	dets:delete(konten, Kontonr),
	dets:close(konten),
	PID ! {ok, Kontonr}.
	
geld_abheben(PID, Kontonr, Betrag) ->
	Konto = daten_lesen(Kontonr),
	Kontostand = kontoinfo(vermoegen, Konto),
	DispoBetrag = kontoinfo(maxDispo, Konto),
	NeuerKontostand = Kontostand - Betrag,
	case NeuerKontostand < 0 - DispoBetrag of
		true -> PID ! {false, "Nicht genug Geld"};%error_handling({error, "Nicht genügend Geld"});
		false -> AktKonto_1 = kontochange(vermoegen, NeuerKontostand, Konto),
				%TransaktionsID noch nicht in der TranListe!!!! ? evtl als Übergabeparameter
				AktKonto_2 = kontolog(auszahlung, {"", Kontonr, Betrag}, AktKonto_1),
				Kontotemp = daten_lesen(Kontonr),
				Vermoegen = kontoinfo(vermoegen, Kontotemp),
				PID ! {ok, Vermoegen}
	end
	.
	
geld_ueberweisen(PID, ZielKontonr, Kontonr, Betrag) ->
	geld_abheben(self(), Kontonr, Betrag),
	receive 
		{false, Message} -> PID ! {false, Message}, 
							exit({false, Message});
		{ok, Message} -> ignoreit
	end,
	geld_einzahlen(self(), ZielKontonr, Kontonr, Betrag),
	receive 
		{false, Message2} -> PID ! {false, Message2}, 
							exit({false, Message2});
		{ok, Message2} -> ignoreit
	end,
	PID ! {ok}.
   

 init() -> 
 io:format("init bw~n"),
 receive
      [konto_anlegen, ClientPId] -> konto_anlegen(ClientPId);
      [konto_loeschen, ClientPId, Kontonr] -> konto_loeschen(ClientPId, Kontonr);
      [kontostand_abfragen, ClientPId, Kontonr] -> kontostand_abfragen(ClientPId, Kontonr);
      [geld_einzahlen, ClientPId, Kontonr, Ursprung, Betrag] -> geld_einzahlen(ClientPId, Kontonr, Ursprung, Betrag);
      [geld_auszahlen, ClientPId, Kontonr, Betrag] -> geld_abheben(ClientPId, Kontonr, Betrag);
      [geld_ueberweisen, ClientPId, ZielKontonr, KontoNr, Betrag] -> geld_ueberweisen(ClientPId, ZielKontonr, KontoNr, Betrag)
      
   end.
   
  