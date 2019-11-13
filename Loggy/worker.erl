%%%-------------------------------------------------------------------
%%% @author Justas Dautaras
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 18. Sep 2019 9:38 PM
%%%-------------------------------------------------------------------
-module(worker).
-export([start/5, stop/1, peers/2]).
-author("Justas Dautaras").

start(Name, Logger, Seed, Sleep, Jitter) ->
  spawn_link(fun() -> init(Name, Logger, Seed, Sleep, Jitter) end).

stop(Worker) ->
  Worker ! stop.

init(Name, Log, Seed, Sleep, Jitter) ->
  random:seed(Seed, Seed, Seed),
  receive
    {peers, Peers} ->
      loop(Name, Log, Peers, Sleep, Jitter, time:zero());
    stop ->
      ok
  end.

peers(Wrk, Peers) ->
  Wrk ! {peers, Peers}.

loop(Name, Log, Peers, Sleep, Jitter, Time)->
  Wait = random:uniform(Sleep),
  %%receives msg and logs new time
  receive
    {msg, Time1, Msg} ->
      Time2 = time:inc(Name, time:merge(Time1, Time)),
      %%io:format("CLOCK1: ~p ~n", [Time2]),
      Log ! {log, Name, Time2, {received, Msg}},
      loop(Name, Log, Peers, Sleep, Jitter, Time2);
    stop ->
      ok;
    Error ->
      Log ! {log, Name, time, {error, Error}}
  %%sends msg and logs time after jitter
  after Wait ->
    Selected = select(Peers),
    Time2 = time:inc(Name, Time),
    Message = {hello, random:uniform(100)},
    Selected ! {msg, Time2, Message},
    jitter(Jitter),
    %%io:format("CLOCK2: ~p ~n", [Time2]),
    Log ! {log, Name, Time2, {sending, Message}},
    loop(Name, Log, Peers, Sleep, Jitter, Time2)
  end.

select(Peers) ->
  lists:nth(random:uniform(length(Peers)), Peers).

jitter(0) -> ok;
jitter(Jitter) -> timer:sleep(random:uniform(Jitter)).