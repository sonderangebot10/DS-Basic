%%%-------------------------------------------------------------------
%%% @author Justas Dautaras
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. Sep 2019 3:47 PM
%%%-------------------------------------------------------------------
-module(time).
-export([zero/0,inc/2,merge/2,leq/2,clock/1,update/3, safe/2]).
-author("Justas Dautaras").

zero() ->
  0.

inc(Name, T) ->
  T+1.

merge(Ti, Tj) ->
  max(Ti,Tj).

%%checks condition for new time
leq(Ti,Tj) ->
  if
    Ti < Tj ->
      true;
    Ti == Tj ->
      true;
    true ->
      false
  end.

%%used to create clocks
clock(Nodes) ->
  case Nodes of
    [] ->
      [];
    [H|T] ->
      [{0,H}|clock(T)]
  end.

%%updates time of clock if new time is higher than current
update(Node, Time, Clock) ->
  case lists:keyfind(Node, 2, Clock) of
    {T,_} ->
      if
        (Time > T) ->
          [{Time, Node}|lists:keydelete(Node, 2, Clock)];
        true ->
          Clock
      end;
    false ->
      [{Time, Node}|Clock]
  end.

%%check whether it is safe to print, i.e. minimum time is lower or equal to logger time
safe(Time, Clock) ->
  MinTime = lists:foldl(fun({T,_}, MinTime) -> min(T,MinTime) end, inf, Clock),
  if
    (MinTime >= Time)  ->
      safe;
    true ->
      not_safe
  end.