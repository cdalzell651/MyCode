-module(stimer).
-export([start/2,cancel/0]).

start(Time,Fun) ->
    register(timer,spawn(fun() -> loop(Time,Fun) end)).

cancel() -> timer ! cancel.

loop(Time, Fun) ->
    receive
        cancel -> 
            void
    after Time ->
            Fun()
    end.
