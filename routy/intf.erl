%%%-------------------------------------------------------------------
%%% @author Justas Dautaras
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 12. Sep 2019 7:36 PM
%%%-------------------------------------------------------------------
-module(intf).
-export([new/0,add/4,remove/2,lookup/2,broadcast/2, ref/2, remove/2, name/2, list/2, list/1]).
-author("Justas Dautaras").

new() ->
  [].

add(Name, Ref, Pid, Intf) ->
  case lists:member({Name, Ref, Pid}, Intf) of
    true ->
      Intf;
    false ->
      [{Name, Ref, Pid} | Intf]
  end.

remove(Name, Intf) ->
  lists:keydelete(Name, 1, Intf).

lookup(Name, Intf) ->
  Case = lists:keyfind(Name, 1, Intf),
  case Case of
    false ->
      notfound;
    H ->
      {_,_,Pid} = H,
      {ok, Pid}
  end.

ref(Name, Intf) ->
  case lists:keyfind(Name, 1, Intf) of
  {Name, Ref,_} ->
    {ok, Ref};
  false ->
    notfound
end.

name(Ref, Intf) ->
  Case = lists:keyfind(Ref, 2, Intf),
  case Case of
    false ->
      notfound;
    H ->
      {Name, _, _} = H,
      {ok, Name}
  end.

list(Intf) ->
  list(Intf, []).
list(Intf, Final) ->
  case Intf of
    [] ->
      Final;
    [{Name, _, _} | Tail] -> list(Tail, [Name | Final])
  end.

broadcast(Message, Intf) ->
  lists:foreach(fun({_,_,Pid}) ->
    Pid ! Message end,
    Intf).
