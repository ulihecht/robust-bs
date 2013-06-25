-module(virus).

-export([init/0,stop/0]).

init() -> 
   register(virus, self()),
   start().
   
stop() -> 
   exit(normal).
   
start() -> 
   timer:sleep(1000),
   receive
      stop -> stop();
      PID -> timer:sleep(700),
             exit(PID,error)             
   end,
   start().
			
