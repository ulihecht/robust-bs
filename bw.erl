-module(bw).
-export([init/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% NOTIZEN
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% TransaktionsID noch nicht in der TranListe!!!! ? evtl als Übergabeparameter
%
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Verwendbare Workerfunktionen
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% konto_anlegen(PID)
% kontostand_abfragen(PID, Kontonr)
% konto_sperren(PID, Kontonr)
% konto_entsperren(PID, Kontonr)
% geld_einzahlen(PID, Kontonr, Betrag)
% konto_loeschen(PID, Kontonr)
% geld_abheben(PID, Kontonr, Betrag)
% geld_ueberweisen(PID, ZielKontonr, Kontonr, Betrag)
% dispokredit_beantragen(PID, Kontonr)
% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Konteneinträge
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 
% {KONTONUMMER, 
%	{sperrvermerk, false/true},
% 	{vermoegen, VERMÖGEN},
%	{maxDispo, DISPOHÖHE},
%	{dispoZins, DISPOZINS},
%	{transaktionsliste, TRANSAKTIONSLISTE}
% }
% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Transaktionsliste
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 
% ATOME FÜR DIE TYPIFIZIERUNG DER EINTRÄGE
% 
% erstellung
% erfragung
% sperrung
% entsperrung
% einzahlung
% loeschung
% auszahlung
% ueberweisung
% 
% AUFBAU EINER TRASAKTION
% 
% {TRANSAKTIONSID, TYP, {zeit, DATUM, UHRZEIT}, {notizen, NOTIZEN}, {wer, KONTONUMMER}, {wert, BETRAG}}
%  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init() -> 
io:format("init bw~n"),
receive
     [konto_anlegen, ClientPId] -> konto_anlegen(ClientPId);
     [konto_loeschen, ClientPId, Kontonr] -> konto_loeschen(ClientPId, Kontonr);
     [kontostand_abfragen, ClientPId, Kontonr] -> kontostand_abfragen(ClientPId, Kontonr);
     [historie, ClientPId, Kontonr] -> historie(ClientPId, Kontonr);
     [geld_einzahlen, ClientPId, Kontonr, Ursprung, Betrag] -> geld_einzahlen(ClientPId, Kontonr, Ursprung, Betrag);
     [geld_auszahlen, ClientPId, Kontonr, Betrag] -> geld_abheben(ClientPId, Kontonr, Betrag);
     [geld_ueberweisen, ClientPId, ZielKontonr, KontoNr, Betrag] -> geld_ueberweisen(ClientPId, ZielKontonr, KontoNr, Betrag);
     [geld_ueberweisen_einzahlen, ClientPId, ZielKontonr, KontoNr, Betrag] -> geld_ueberweisen_einzahlen(ClientPId, ZielKontonr, KontoNr, Betrag);
     [dispokredit_beantragen, ClientPId, KontoNr] -> dispokredit_beantragen(ClientPId, KontoNr);
     [konto_sperren, ClientPId, KontoNr] -> konto_sperren(ClientPId, KontoNr);
     [konto_entsperren, ClientPId, KontoNr] -> konto_entsperren(ClientPId, KontoNr)
end.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Interne Funktionen die uns das Leben leichter machen
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
error_handling({nok, Reason}) ->
	io:format("Fehler: ~p ~n", [Reason]),
	dets:close(konten),
	exit(normal);
error_handling({ok, konten}) ->
	ok.
	
daten_lesen(PID, Kontonr) ->
	{Response, Reason} = dets:open_file(konten, [{file, "db_konten"}, {type, set}]),
	error_handling({Response, Reason}),
	Konto = dets:lookup(konten, Kontonr),
	case Konto of
		[] -> 	 PID ! {nok, "Konto nicht gefunden"},
                error_handling({nok, "Konto nicht gefunden"});
       _ -> ok
	end,
	dets:close(konten),
	Konto
	.
	
daten_schreiben(Konto) -> 
	dets:open_file(konten, [{file, "db_konten"}, {type, set}]),
	dets:insert(konten, Konto),
	dets:close(konten).	

kontoinfo(Feld, Konto) ->
	[{_ ,{sperrvermerk, Sperrvermerk},{vermoegen, Kontostand},{maxDispo, MaxDispo} ,{dispoZins, DispoZins},{transaktionsliste, Transaktionsliste}}] = Konto,
	case Feld of
		sperrvermerk -> Sperrvermerk;
		vermoegen -> Kontostand;
		maxDispo -> MaxDispo;
		dispoZins -> DispoZins;
		transaktionsliste -> Transaktionsliste
	end.
	
kontochange(Feld, Wert, Konto) ->

	[{Kontonr ,{sperrvermerk, Sperrvermerk},{vermoegen, Kontostand},{maxDispo, MaxDispo} ,{dispoZins, DispoZins},{transaktionsliste, Transaktionsliste}}] = Konto,
case Feld of
		sperrvermerk -> AktualisiertesKonto = [{Kontonr ,{sperrvermerk, Wert},{vermoegen, Kontostand},{maxDispo, MaxDispo} ,{dispoZins, DispoZins},{transaktionsliste, Transaktionsliste}}];
		vermoegen ->    AktualisiertesKonto = [{Kontonr ,{sperrvermerk, Sperrvermerk},{vermoegen, Wert},{maxDispo, MaxDispo} ,{dispoZins, DispoZins},{transaktionsliste, Transaktionsliste}}];
		maxDispo -> 	 AktualisiertesKonto = [{Kontonr ,{sperrvermerk, Sperrvermerk},{vermoegen, Kontostand},{maxDispo, Wert} ,{dispoZins, DispoZins},{transaktionsliste, Transaktionsliste}}];
		dispoZins -> 	 AktualisiertesKonto = [{Kontonr ,{sperrvermerk, Sperrvermerk},{vermoegen, Kontostand},{maxDispo, MaxDispo} ,{dispoZins, Wert},{transaktionsliste, Transaktionsliste}}];
		transaktionsliste ->  AktualisiertesKonto = [{Kontonr ,{sperrvermerk, Sperrvermerk},{vermoegen, Kontostand},{maxDispo, MaxDispo} ,{dispoZins, DispoZins},{transaktionsliste, [Wert|Transaktionsliste]}}]
	end,
	daten_schreiben(AktualisiertesKonto),
   AktualisiertesKonto.


kontolog(Operation, {Notizen, Wer, Betrag}, Konto) ->
	kontochange(transaktionsliste, {self(), Operation, {zeit, date(), time()}, {notizen, Notizen}, {wer, Wer}, {wert, Betrag}}, Konto).


	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Funktionen für die Anwendung des Workers
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

konto_anlegen(PID) ->
	dets:open_file(konten, [{file, "db_konten"}, {type, set}]),
	% Speichern der letzen Kontonummer, wenn die Tabelle nicht existiert wird sie hier erstellt
   	Kontonr_temp = dets:first(konten),
	case Kontonr_temp of 		%%Konteninit
		'$end_of_table' -> dets:insert(konten, {inkKontoNr, 0});
		_ -> ignoreit
	end,
	[{inkKontoNr, HoechsteKontonr}] = dets:lookup(konten, inkKontoNr),
	Kontonr = HoechsteKontonr + 1,
	dets:insert(konten, {Kontonr, 
								{sperrvermerk, false},
								{vermoegen, 0},
								{maxDispo, 0},
								{dispoZins, 0},
								{transaktionsliste, []}}),
	dets:insert(konten, {inkKontoNr, Kontonr}),
	dets:close(konten),
	Konto = daten_lesen(PID, Kontonr),
	kontolog(konto_angelegt, {"Konto wurde erstellt", Kontonr, 0}, Konto),
    PID ! {ok, Kontonr}.
	
kontostand_abfragen(PID, Kontonr) ->
	Konto = daten_lesen(PID, Kontonr),
   timer:sleep(1000),
	Kontostand = kontoinfo(vermoegen, Konto),
	kontolog(erfragung, {"Kontostand wurde abgefragt", Kontonr, 0}, Konto),
	PID ! {ok, Kontostand}.

historie(PID, Kontonr) ->
   Konto = daten_lesen(PID, Kontonr),
   [{_ ,_Sperrvermerk, Kontostand, _MaxDispo ,_DispoZins ,Transaktionsliste}] = Konto,
   timer:sleep(1000),
   PID ! {ok, [Transaktionsliste, Kontostand]}.

   
konto_sperren(PID, Kontonr) ->
	Konto = daten_lesen(PID, Kontonr),
   timer:sleep(1000),
	kontochange(sperrvermerk, true, Konto),
	Konto2 = daten_lesen(PID, Kontonr),
	kontolog(sperrung, {"Konto gesperrt", Kontonr, 0}, Konto2),
	PID ! {ok, 'Konto wurde gesperrt'}.
	
	
konto_entsperren(PID, Kontonr) ->
	Konto = daten_lesen(PID, Kontonr),
	kontochange(sperrvermerk, false, Konto),
   timer:sleep(1000),
	Konto2 = daten_lesen(PID, Kontonr),
	kontolog(entsperrung, {"Konto entsperrt", Kontonr, 0}, Konto2),
	PID ! {ok, 'Konto wurde entsperrt'}.

geld_einzahlen(PID, Kontonr, Verwendungszweck, Betrag) ->	
geld_einzahlen(PID, Kontonr, Kontonr, Verwendungszweck, Betrag).

%Nur für interne Verwendung für die Überweisungen
geld_einzahlen(PID, Kontonr, Ursprung, Verwendungszweck, Betrag) ->
	Konto = daten_lesen(PID, Kontonr),
   %timer:sleep(1000),
	case kontoinfo(sperrvermerk, Konto) of

		true -> PID ! {nok, "Konto gesperrt"},
				kontolog(einzahlung, {"Konto gesperrt", Kontonr, 0}, Konto);
		false ->Kontostand = kontoinfo(vermoegen, Konto),
				NeuerKontostand = Kontostand + Betrag,
				AktKonto_1 = kontochange(vermoegen, NeuerKontostand, Konto),
				%TransaktionsID noch nicht in der TranListe!!!! ? evtl als Übergabeparameter
				kontolog(einzahlung, {Verwendungszweck, Ursprung, Betrag}, AktKonto_1),
				Kontotemp = daten_lesen(PID, Kontonr),
				Vermoegen = kontoinfo(vermoegen, Kontotemp),
				PID ! {ok, Vermoegen}
	end			
	.
   
konto_loeschen(PID, Kontonr) ->
	case kontoinfo(sperrvermerk, daten_lesen(PID, Kontonr)) of
		true -> PID ! {nok, "Konto gesperrt"},
				kontolog(loeschung, {"Konto gesperrt", Kontonr, 0}, daten_lesen(PID, Kontonr));
		false ->dets:open_file(konten, [{file, "db_konten"}, {type, set}]),
				dets:delete(konten, Kontonr),
				dets:close(konten),
				PID ! {ok, Kontonr}
	end			
	.
	
geld_abheben(PID, Kontonr, Betrag) ->
	Konto = daten_lesen(PID, Kontonr),
	case kontoinfo(sperrvermerk, Konto) of
		true -> PID ! {nok, "Konto gesperrt"},
				kontolog(auszahlung, {"Konto gesperrt", Kontonr, 0}, Konto);
		false ->Kontostand = kontoinfo(vermoegen, Konto),
				DispoBetrag = kontoinfo(maxDispo, Konto),
				NeuerKontostand = Kontostand - Betrag,
				case NeuerKontostand < 0 - DispoBetrag of
					true -> PID ! {nok, "Nicht genug Geld"},
							kontolog(auszahlung, {"Auszahlung nicht möglich", Kontonr, 0}, daten_lesen(PID, Kontonr));
					false -> AktKonto_1 = kontochange(vermoegen, NeuerKontostand, Konto),
							%TransaktionsID noch nicht in der TranListe!!!! ? evtl als Übergabeparameter
							kontolog(auszahlung, {"", Kontonr, Betrag}, AktKonto_1),
							Kontotemp = daten_lesen(PID, Kontonr),
							Vermoegen = kontoinfo(vermoegen, Kontotemp),
                     PID ! {ok, Vermoegen}
				end
	end
	.
	
geld_ueberweisen(PID, ZielKontonr, Kontonr, Betrag) ->
	case kontoinfo(sperrvermerk, daten_lesen(PID, ZielKontonr)) or kontoinfo(sperrvermerk, daten_lesen(PID, Kontonr)) of
		true -> PID ! {nok, "Eins der Konten ist gesperrt"},
				kontolog(ueberweisung, {"Eins der Konten ist gesperrt", Kontonr, 0}, daten_lesen(PID, Kontonr));
		false ->geld_abheben(self(), Kontonr, Betrag),
				receive 
					{nok, Message} -> PID ! {nok, Message};
					{ok, _} ->  timer:sleep(1000),
               geld_ueberweisen_einzahlen(PID, ZielKontonr, Kontonr, Betrag) 
				end
	end.

geld_ueberweisen_einzahlen(PID, ZielKontonr, Kontonr, Betrag) ->
   geld_einzahlen(self(), ZielKontonr, Kontonr, Betrag),
   receive 
      {nok, Message2} -> PID ! {nok, Message2};
      {ok, _} -> PID ! {ok, 'Geld wurde ueberwiesen'}		
   end.
   
dispokredit_beantragen(PID, Kontonr) ->
	Konto = daten_lesen(PID, Kontonr),
	case kontoinfo(sperrvermerk, Konto) of
		true -> PID ! {nok, "Konto gesperrt"},
				kontolog(dispobeantragung, {"Konto gesperrt", Kontonr, 0}, daten_lesen(PID, Kontonr));
		false -> 
				Vermoegen = kontoinfo(vermoegen, daten_lesen(PID, Kontonr)),
				Dispo = Vermoegen * 0.1,
				kontochange(maxDispo, Dispo, daten_lesen(PID, Kontonr)),
				kontochange(dispoZins, 12, daten_lesen(PID, Kontonr)),
				kontolog(dispobeantragung, {"Dispokredit mit 12 Prozent", Kontonr, Dispo}, daten_lesen(PID, Kontonr)),
				PID ! {ok, Dispo}
	end
	.


   
  