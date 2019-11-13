%%%-------------------------------------------------------------------
%%% @author Justas Dautaras
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 09. Sep 2019 12:10 AM
%%%-------------------------------------------------------------------
-module(dijkstra).
-export([table/2, route/2]).
-author("Justas Dautaras").

entry(Node, Sorted) ->
  case Sorted of
    [] ->
      0;
    [H|T] ->
      {Node1, N, _} = H,
      case Node1 of
        Node ->
          N;
        _ ->
          entry(Node, T)
      end
  end.

replace(Node, N, Gateway, Sorted) ->
  List = lists:keydelete(Node, 1, Sorted),
  List1 = lists:append([{Node, N, Gateway}], List),
  lists:keysort(2, List1).

update(Node, N, Gateway, Sorted) ->
  case Node of
    [] ->
      Sorted;
    _ ->
      Length = entry (Node, Sorted),
      if
        Length > N -> replace(Node, N, Gateway, Sorted);
        Length == N -> Sorted;
        Length < N -> []
      end
  end.

iterate(Sorted, Map, Table) ->
  case Sorted of
    [] ->
      Table;
    [{_,inf,_} | _] ->
      Table;
    [Entry | Tail] ->
      {Name, Length, Gateway} = Entry,
      case lists:keyfind(Name, 1, Map) of
        {_, Reachables} ->
          NewList = lists:foldl(fun(Node, SortedList) ->
            update(Node, Length+1, Gateway, SortedList) end,
            Tail, Reachables);
        false ->
          NewList = Tail
      end,
      iterate(NewList, Map, [{Name,Gateway} | Table])
  end.

table(Gateways, Map) ->
  GatewayList = getGateways(Gateways),
  GetInitialSortedList = getInitialSortedList(Map),
  Sorted = lists:append(GatewayList, GetInitialSortedList),
  iterate(Sorted, Map, []).

getInitialSortedList(Map) ->
  case Map of
    [] ->
      [];
    [H|T] ->
      {A, B} = H,
      lists:append(makeWay(A, B), getInitialSortedList(T))
  end.

makeWay(A, B) ->
  case B of
    [] ->
      [];
    [H|T] ->
      [{H, inf, A} | makeWay(A, T)]
  end.

getGateways(Gateways) ->
  case Gateways of
    [] ->
      [];
    [H|T] ->
      [{H, 0, H} | getGateways(T)]
end.

route(Node, Table) ->
  io:format("TABLE: ~p ~n", [Table]),
  io:format("NODE: ~p ~n", [Node]),
  case lists:keyfind(Node,1,Table) of
    {Dest, Gateway} ->
      {ok, Gateway};
    false ->
      notfound
  end.