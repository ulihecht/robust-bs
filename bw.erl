-module(bw).
-export([konto_anlegen/1,print/1, kontostand_abfragen/2, geld_einzahlen/3, geld_abheben/3, geld_ueberweisen/4, init/0, konto_sperren/2, konto_entsperren/2, dispokredit_beantragen/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Transaktionslistenatome
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% erstellung
% erfragung
% sperrung
% entsperrung
% einzahlung
% loeschung
% auszahlung
% ueberweisung
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%WARUM ZUM GEIER BRAUCHEN WIR EINE INIT??? DIE FUNKTIONEN 
%KÖNNEN AUCH DIREKT AUFGERUFEN WERDEN
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init() -> 
io:format("init bw~n"),
receive
     [konto_anlegen, ClientPId] -> konto_anlegen(ClientPId), init();
     [konto_loeschen, ClientPId, Kontonr] -> konto_loeschen(ClientPId, Kontonr);
     [kontostand_abfragen, ClientPId, Kontonr] -> kontostand_abfragen(ClientPId, Kontonr);
     [geld_einzahlen, ClientPId, Kontonr, Ursprung, Betrag] -> geld_einzahlen(ClientPId, Kontonr, Ursprung, Betrag);
     [geld_auszahlen, ClientPId, Kontonr, Betrag] -> geld_abheben(ClientPId, Kontonr, Betrag);
     [geld_ueberweisen, ClientPId, ZielKontonr, KontoNr, Betrag] -> geld_ueberweisen(ClientPId, ZielKontonr, KontoNr, Betrag)
end.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Interne Funktionen die uns das Leben leichter machen
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

error_handling({error, Reason}) ->
	io:format("Error: ~p ~n", [Reason]),
	dets:close(konten),
	exit(normal)
	;
error_handling({ok, konten}) ->
	ok.
	
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
	dets:open_file(konten, [{file, "db_konten"}, {type, set}]),
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
	[{_ ,{sperrvermerk, Sperrvermerk},{vermoegen, Kontostand},{maxDispo, MaxDispo} ,{dispoZins, DispoZins},{transaktionsliste, Transaktionsliste}}] = Konto,
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

print(Kontonr) ->
	io:format("~p~n", [daten_lesen(Kontonr)]).

	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Funktionen für die Anwendung des Workers
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

konto_anlegen(PID) ->
	Kontonr = daten_schreiben(konto_anlegen),
	Konto = daten_lesen(Kontonr),
	kontolog(erstellung, {"Konto wurde erstellt", Kontonr, 0}, Konto),
    PID ! {ok, Kontonr}.
	
kontostand_abfragen(PID, Kontonr) ->
	Konto = daten_lesen(Kontonr),
	Kontostand = kontoinfo(vermoegen, Konto),
	kontolog(erfragung, {"Kontostand wurde abgefragt", Kontonr, 0}, Konto),
	PID ! {ok, Kontostand}.

   
konto_sperren(PID, Kontonr) ->
	Konto = daten_lesen(Kontonr),
	kontochange(sperrvermerk, true, Konto),
	Konto2 = daten_lesen(Kontonr),
	kontolog(sperrung, {"Konto gesperrt", Kontonr, 0}, Konto2),
	PID ! {ok}.
	
	
konto_entsperren(PID, Kontonr) ->
	Konto = daten_lesen(Kontonr),
	kontochange(sperrvermerk, false, Konto),
	Konto2 = daten_lesen(Kontonr),
	kontolog(entsperrung, {"Konto entsperrt", Kontonr, 0}, Konto2),
	PID ! {ok}.
	
geld_einzahlen(PID, Kontonr, Betrag) ->	
	geld_einzahlen(PID, Kontonr, Kontonr, Betrag)
	.

	
%Nur für interne Verwendung für die Überweißunga
geld_einzahlen(PID, Kontonr, Ursprung, Betrag) ->
	Konto = daten_lesen(Kontonr),
	case kontoinfo(sperrvermerk, Konto) of
		true -> PID ! {error, "Konto gesperrt"},
				kontolog(einzahlung, {"Konto gesperrt", Kontonr, 0}, Konto);
		false ->Kontostand = kontoinfo(vermoegen, Konto),
				NeuerKontostand = Kontostand + Betrag,
				AktKonto_1 = kontochange(vermoegen, NeuerKontostand, Konto),
				%TransaktionsID noch nicht in der TranListe!!!! ? evtl als Übergabeparameter
				kontolog(einzahlung, {"", Ursprung, Betrag}, AktKonto_1),
				Kontotemp = daten_lesen(Kontonr),
				Vermoegen = kontoinfo(vermoegen, Kontotemp),
				PID ! {ok, Vermoegen}
	end			
	.
   
 konto_loeschen(PID, Kontonr) ->
	case kontoinfo(sperrvermerk, daten_lesen(Kontonr)) of
		true -> PID ! {error, "Konto gesperrt"},
				kontolog(loeschung, {"Konto gesperrt", Kontonr, 0}, daten_lesen(Kontonr));
		false ->dets:open_file(konten, [{file, "db_konten"}, {type, set}]),
				dets:delete(konten, Kontonr),
				dets:close(konten),
				PID ! {ok, Kontonr}
	end			
	.
	
geld_abheben(PID, Kontonr, Betrag) ->
	Konto = daten_lesen(Kontonr),
	case kontoinfo(sperrvermerk, Konto) of
		true -> PID ! {error, "Konto gesperrt"},
				kontolog(auszahlung, {"Konto gesperrt", Kontonr, 0}, Konto);
		false ->Kontostand = kontoinfo(vermoegen, Konto),
				DispoBetrag = kontoinfo(maxDispo, Konto),
				NeuerKontostand = Kontostand - Betrag,
				case NeuerKontostand < 0 - DispoBetrag of
					true -> PID ! {error, "Nicht genug Geld"},
							kontolog(auszahlung, {"Auszahlung nicht möglich", Kontonr, 0}, daten_lesen(Kontonr));%error_handling({error, "Nicht genügend Geld"});
					false -> AktKonto_1 = kontochange(vermoegen, NeuerKontostand, Konto),
							%TransaktionsID noch nicht in der TranListe!!!! ? evtl als Übergabeparameter
							kontolog(auszahlung, {"", Kontonr, Betrag}, AktKonto_1),
							Kontotemp = daten_lesen(Kontonr),
							Vermoegen = kontoinfo(vermoegen, Kontotemp),
							PID ! {ok, Vermoegen}
				end
	end
	.
	
geld_ueberweisen(PID, ZielKontonr, Kontonr, Betrag) ->
	case kontoinfo(sperrvermerk, daten_lesen(Kontonr)) or kontoinfo(sperrvermerk, daten_lesen(Kontonr)) of
		true -> PID ! {error, "Eins der Konten ist gesperrt"},
				kontolog(ueberweisung, {"Eins der Konten ist gesperrt", Kontonr, 0}, daten_lesen(Kontonr));
		false ->geld_abheben(self(), Kontonr, Betrag),
				receive 
					{error, Message} -> PID ! {error, Message}, 
										exit({error, Message});
					{ok, _} -> ignoreit
				end,
				geld_einzahlen(self(), ZielKontonr, Kontonr, Betrag),
				receive 
					{error, Message2} -> PID ! {error, Message2}, 
										exit({error, Message2});
					{ok, _} -> ignoreit
				end,
				PID ! {ok}		
	end
	.

dispokredit_beantragen(PID, Kontonr) ->
	Konto = daten_lesen(Kontonr),
	case kontoinfo(sperrvermerk, Konto) of
		true -> PID ! {error, "Konto gesperrt"},
				kontolog(dispobeantragung, {"Konto gesperrt", Kontonr, 0}, daten_lesen(Kontonr));
		false -> 
				Vermoegen = kontoinfo(vermoegen, daten_lesen(Kontonr)),
				Dispo = Vermoegen * 0.1,
				kontochange(maxDispo, Dispo, daten_lesen(Kontonr)),
				kontochange(dispoZins, 12, daten_lesen(Kontonr)),
				kontolog(dispobeantragung, {"Dispokredit mit 12 Prozent", Kontonr, Dispo}, daten_lesen(Kontonr)),
				PID ! {ok, Dispo}
	end
	.


   
  