-module(tock).
-export([start/0,loop/1,stop/0]).

start() ->
    Pid = spawn(tick,loop,[self()]),
    New = spawn(tock,loop,[Pid]),
    Pid ! {newPid,New}.
stop() ->
    self() ! stop.
loop(Pid) ->
    receive
        tock ->
             Self = self(),
             io:format("TOCK~n"),
             stimer:start(800,fun() -> Self ! done end),
             loop(Pid);
        done ->
            Pid ! tick,
            loop(Pid);
        {newPid,New} ->
            loop(New);
        stop ->
              io:format("Stopped~n")
     end.
