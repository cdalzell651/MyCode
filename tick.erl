-module(tick).
-export([start/0,loop/1,stop/0]).

start() ->
    Pid = spawn(tock,loop,[self()]),
    New = spawn(tick,loop,[Pid]),
    Pid ! {newPid,New},
    Pid ! tock.
stop() ->
    self() ! stop.
loop(Pid) ->
    receive
        tick ->
            Self = self(),
            io:format("TICK~n"),
            stimer:start(200,fun() -> Self ! done end),
            loop(Pid);
        done ->
            Pid ! tock,
            loop(Pid);
        {newPid,New} ->
            loop(New);
        stop ->
            io:format("Stopped~n")
    end.
