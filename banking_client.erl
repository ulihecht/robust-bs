% Shell Client:
%banking_server:konto_anlegen(self()), receive  {ok, Nummer} -> {reply, Nummer, blubb} end.

%c(banking_server), c(banking_worker), {ok, PID} = banking_server:start(), banking_server:konto_anlegen(PID).

