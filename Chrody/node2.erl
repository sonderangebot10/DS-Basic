%%%-------------------------------------------------------------------
%%% @author Justas Dautaras
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 03. Oct 2019 4:08 PM
%%%-------------------------------------------------------------------
-module(node2).
-export([start/1, start/2]).
-author("Justas Dautaras").

-define(Stabilize, 1000).
-define(Timeout, 1000).

start(Id) ->
  start(Id, nil).
start(Id, Peer) ->
  timer:start(),
  spawn(fun() -> init(Id, Peer) end).

init(Id, Peer) ->
  Predecessor = nil,
  {ok, Successor} = connect(Id, Peer),
  schedule_stabilize(),
  node(Id, Predecessor, Successor, []).

connect(Id, nil) ->
  {ok, {Id, self()}};
connect(Id, Peer) ->
  Qref = make_ref(),
  Peer ! {key, Qref, self()},
  receive
    {Qref, Skey} ->
      {ok, {Skey, Peer}}
  after ?Timeout ->
    io:format("Time out: no response~n",[])
  end.


node(Id, Predecessor, Successor, Store) ->
  receive
    %a peer needs to know our key
    {key, Qref, Peer} ->
      Peer ! {Qref, Id},
      node(Id, Predecessor, Successor, Store);
    %a new node informs us of its existence
    {notify, New} ->
      {Pred, Store2} = notify(New, Id, Predecessor, Store),
      node(Id, Pred, Successor, Store2);
    %a predecessor needs to know our predecessor
    {request, Peer} ->
      request(Peer, Predecessor),
      node(Id, Predecessor, Successor, Store);
    %our successor informs us about its predecessor
    {status, Pred} ->
      Succ = stabilize(Pred, Id, Successor),
      node(Id, Predecessor, Succ, Store);
    stabilize ->
      stabilize(Successor),
      node(Id, Predecessor, Successor, Store);
    probe ->
      create_probe(Id, Successor),
      node(Id, Predecessor, Successor, Store);
    {probe, Id, Nodes, T} ->
      remove_probe(T, Nodes),
      node(Id, Predecessor, Successor, Store);
    {probe, Ref, Nodes, T} ->
      forward_probe(Ref, T, Nodes, Id, Successor),
      node(Id, Predecessor, Successor, Store);
    {add, Key, Value, Qref, Client} ->
      Added = add(Key, Value, Qref, Client, Id, Predecessor, Successor, Store),
      node(Id, Predecessor, Successor, Added);
    {lookup, Key, Qref, Client} ->
      lookup(Key, Qref, Client, Id, Predecessor, Successor, Store),
      node(Id, Predecessor, Successor, Store);
    %When should this message be sent? It’s a message from a node that has
    %accepted us as their predecessor.This is only done when a node receives and
    %handles a notify message.
{handover, Elements} ->
      Merged = storage:merge(Store, Elements),
      node(Id, Predecessor, Successor, Merged)
  end.

add(Key, Value, Qref, Client, Id, {Pkey, _}, {_, Spid}, Store) ->
  case key:between(Key, Pkey, Id) of
    true ->
      Client ! {Qref, ok},
      storage:add(Key, Value, Store);
    false ->
      Spid ! {add, Key, Value, Qref, Client},
      Store
  end.

lookup(Key, Qref, Client, Id, {Pkey, _}, Successor, Store) ->
  case key:between(Key, Pkey, Id) of
    true ->
      Result = storage:lookup(Key, Store),
      Client ! {Qref, Result};
    false ->
      {_, Spid} = Successor,
      Spid ! {lookup, Key, Qref, Client}
  end.

% Pred = Successor current predecessor
% Id = Id of the current node
% Successor = Successor of the current node
stabilize(Pred, Id, Successor) ->
  {Skey, Spid} = Successor,
  case Pred of
  nil ->
    Spid ! {notify, {Id, self()}},
    Successor;
  {Id, _} ->
    Successor;
  {Skey, _} ->
    Spid ! {notify, {Id, self()}},
    Successor;
  {Xkey, Xpid} ->
    case key:between(Xkey, Id, Skey) of
      true ->
        Xpid ! {request, self()},
        Pred;
      false ->
        Spid ! {notify, {Id, self()}},
        Successor
    end
end.

schedule_stabilize() ->
  timer:send_interval(?Stabilize, self(), stabilize).

stabilize({_, Spid}) ->
  Spid ! {request, self()}.

request(Peer, Predecessor) ->
  case Predecessor of
    nil ->
      Peer ! {status, nil};
    {Pkey, Ppid} ->
      Peer ! {status, {Pkey, Ppid}}
  end.

notify({Nkey, Npid}, Id, Predecessor, Store) ->
  case Predecessor of
    nil ->
      Keep = handover(Id, Store, Nkey, Npid),
      {{Nkey, Npid}, Keep};
    {Pkey, _} ->
  case key:between(Nkey, Pkey, Id) of
    true ->
      Keep = handover(Id, Store, Nkey, Npid),
      {{Nkey, Npid},Keep};
    false ->
      {Predecessor,Store}
  end
end.

handover(Id, Store, Nkey, Npid) ->
  {Keep, Rest} = storage:split(Id, Nkey, Store),
  Npid ! {handover, Rest},
  Keep.

create_probe(Id,{_,Spid}) ->
  Spid ! {probe,Id,[Id],erlang:now()}.

remove_probe(T, Nodes) ->
  Duration = timer:now_diff(erlang:now(),T),
  io:format("Nodes ~p ~n", [Nodes]),
  io:format("Nodes length ~p ~n", [length(Nodes)]),
  io:format("Duration in ~w ms ~n", [Duration div 1000]).

forward_probe(Ref, T, Nodes, Id, {_,Spid}) ->
  %timer:sleep(1000),
  Spid ! {probe,Ref,Nodes ++ [Id],T}.