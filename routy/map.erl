%%%-------------------------------------------------------------------
%%% @author Justas Dautaras
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. Sep 2019 10:57 PM
%%%-------------------------------------------------------------------
-module(map).
-export([new/0, update/3, reachable/2, all_nodes/1]).
-author("Justas Dautaras").

new() ->
  [].

update(Node, Links, Map) ->
  NewMap = lists:keydelete(Node, 1, Map),
  [{Node, Links} | NewMap].

reachable(Node, Map) ->
  [{Node1, Links}] = Map,
  case Node of
    Node1 ->
      Links;
    _ ->
      []
  end.

all_nodes(Map) ->
  case Map of
    [] ->
      [];
    [H|T] ->
      {Node, List} = H,
      List1 = lists:append([Node], List),
      lists:usort(lists:append(all_nodes(T), List1))
  end.
