%%%-------------------------------------------------------------------
%%% @author Justas Dautaras
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 03. Oct 2019 4:08 PM
%%%-------------------------------------------------------------------
-module(node3).
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
  Next = nil,
  {ok, Successor} = connect(Id, Peer),
  schedule_stabilize(),
  node(Id, Predecessor, Successor, [], Next).

connect(Id, nil) ->
  Ref=monitor(self()),
  {ok, {Id, Ref, self()}};
connect(Id, Peer) ->
  Qref = make_ref(),
  Peer ! {key, Qref, self()},
  receive
    {Qref, Skey} ->
      Ref=monitor(Peer),
      {ok, {Skey,Ref, Peer}}
  after ?Timeout ->
    io:format("Time out: no response~n",[])
  end.


node(Id, Predecessor, Successor, Store, Next) ->
  receive
    %a peer needs to know our key
    {key, Qref, Peer} ->
      Peer ! {Qref, Id},
      node(Id, Predecessor, Successor, Store, Next);
    %a new node informs us of its existence
    {notify, New} ->
      {Pred, Store2} = notify(New, Id, Predecessor, Store),
      node(Id, Pred, Successor, Store2, Next);
    %a predecessor needs to know our predecessor
    {request, Peer} ->
      request(Peer, Predecessor, Successor),
      node(Id, Predecessor, Successor, Store, Next);
    %our successor informs us about its predecessor
    {status, Pred, Nx} ->
      {Succ, Next2} = stabilize(Pred, Nx, Id, Successor),
      node(Id, Predecessor, Succ, Store, Next2);
    stabilize ->
      stabilize(Successor),
      node(Id, Predecessor, Successor, Store, Next);
    probe ->
      create_probe(Id, Successor),
      node(Id, Predecessor, Successor, Store, Next);
    {probe, Id, Nodes, T} ->
      remove_probe(T, Nodes),
      node(Id, Predecessor, Successor, Store, Next);
    {probe, Ref, Nodes, T} ->
      forward_probe(Ref, T, Nodes, Id, Successor),
      node(Id, Predecessor, Successor, Store, Next);
    {add, Key, Value, Qref, Client} ->
      Added = add(Key, Value, Qref, Client, Id, Predecessor, Successor, Store),
      node(Id, Predecessor, Successor, Added, Next);
    {lookup, Key, Qref, Client} ->
      lookup(Key, Qref, Client, Id, Predecessor, Successor, Store),
      node(Id, Predecessor, Successor, Store, Next);
    %When should this message be sent? It’s a message from a node that has
    %accepted us as their predecessor.This is only done when a node receives and
    %handles a notify message.
    {handover, Elements} ->
      Merged = storage:merge(Store, Elements),
      node(Id, Predecessor, Successor, Merged, Next);
    {'DOWN', Ref, process, _, _} ->
      {Pred, Succ, Nxt}  = down(Ref, Predecessor, Successor, Next),
      node(Id, Pred, Succ, Store,Nxt);
    stop ->
      io:format("stop"),
      ok
  end.

add(Key, Value, Qref, Client, Id, {Pkey, _, _}, {_, _, Spid}, Store) ->
  case key:between(Key, Pkey, Id) of
    true ->
      Client ! {Qref, ok},
      storage:add(Key, Value, Store);
    false ->
      Spid ! {add, Key, Value, Qref, Client},
      Store
  end.

lookup(Key, Qref, Client, Id, {Pkey, _, _}, Successor, Store) ->
  case key:between(Key, Pkey, Id) of
    true ->
      Result = storage:lookup(Key, Store),
      Client ! {Qref, Result};
    false ->
      {_, _, Spid} = Successor,
      Spid ! {lookup, Key, Qref, Client}
  end.

% Pred = Successor current predecessor
% Id = Id of the current node
% Successor = Successor of the current node
stabilize(Pred, Next, Id, Successor) ->
  {Skey, Sref, Spid} = Successor,
  case Pred of
  nil ->
    Spid ! {notify, {Id, self()}},
    {Successor,Next};
  {Id, _} ->
    {Successor, Next};
  {Skey, _} ->
    Spid ! {notify, {Id, self()}},
    {Successor, Next};
  {Xkey, Xpid} ->
    case key:between(Xkey, Id, Skey) of
      true ->
        Xpid ! {request, self()},
        drop(Sref),
        Ref=monitor(Xpid),
        {{Xkey,Ref,Xpid},Successor};
      false ->
        Spid ! {notify, {Id, self()}},
        {Successor,Next}
    end
end.

schedule_stabilize() ->
  timer:send_interval(?Stabilize, self(), stabilize).

stabilize({_, _, Spid}) ->
  Spid ! {request, self()}.

request(Peer, Predecessor, {Skey,_,Spid}) ->
  case Predecessor of
    nil ->
      Peer ! {status, nil,{Skey,Spid}};
    {Pkey, _, Ppid} ->
      Peer ! {status, {Pkey, Ppid},{Skey,Spid}}
  end.

notify({Nkey, Npid}, Id, Predecessor, Store) ->
  case Predecessor of
    nil ->
      Keep = handover(Id, Store, Nkey, Npid),
      Ref=monitor(Npid),
      {{Nkey, Ref, Npid},Keep};
    {Pkey, Pref, _} ->
      case key:between(Nkey, Pkey, Id) of
        true ->
          Keep = handover(Id, Store, Nkey, Npid),
          drop(Pref),
          Ref=monitor(Npid),
          {{Nkey,Ref, Npid},Keep};
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

down(Ref, {_, Ref, _}, Successor, Next) ->
  {nil,Successor,Next};
down(Ref, Predecessor, {_, Ref, _}, {Nkey,Npid}) ->
  Nref=monitor(Npid),
  {Predecessor, {Nkey, Nref, Npid}, nil}.

monitor(Pid) ->
  erlang:monitor(process, Pid).
drop(nil) ->
  ok;
drop(Pid) ->
  erlang:demonitor(Pid, [flush]).