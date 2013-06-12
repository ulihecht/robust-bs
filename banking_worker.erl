-module(banking_worker).

-export([konto_anlegen/1]).

konto_anlegen(PID) ->
   % TODO: Kontonummer zufällig erzeugen
   PID ! {ok, "12345"}.
   