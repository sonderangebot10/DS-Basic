%%%-------------------------------------------------------------------
%%% @author Justas Dautaras
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 18. Sep 2019 9:16 PM
%%%-------------------------------------------------------------------
-module(loggger).
-export([start/1, stop/1]).
-author("Justas Dautaras").

start(Nodes) ->
  spawn_link(fun() ->init(Nodes) end).

stop(Logger) ->
  Logger ! stop.

init(Nodes) ->
  Queue = [],
  Clock = time:clock(Nodes),
  loop(Queue, Clock).

loop(Queue, Clock) ->
  receive
    {log, From, Time, Msg} ->
      Clock1 = time:update(From, Time, Clock),
      %%io:format("CLOCK: ~p ~n", [Clock]),
      Queue1 = lists:keysort(2,[{From,Time,Msg}|Queue]),
      NewMessageQueue = lists:dropwhile(fun({F,T,M})->
        %%safe to drop when minimum of Clock1 is lower than Queue element's time
        case time:safe(T, Clock1) of
          safe ->
            log(F,T, M),
            true;
          not_safe ->
            false
        end
                                end,
        Queue1),
      loop(NewMessageQueue,Clock1);
    stop ->
      ok
  end.

log(From, Time, Msg) ->
  io:format("log: ~w ~w ~p~n", [Time, From, Msg]).