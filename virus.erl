-module(virus).

-export([init/0,stop/0]).

loop_receive() -> 	receive
						_ -> loop_receive()
					after 
						0 -> true
					end.

init() -> 
   register(virus, self()),
   start().
   
stop() -> 
   exit(normal).
   
start() -> 
   timer:sleep(1000),
   loop_receive(),
   receive
      stop -> stop();
      PID -> exit(PID,error)
   end,
   start().
			
