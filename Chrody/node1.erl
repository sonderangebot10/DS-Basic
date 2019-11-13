%%%-------------------------------------------------------------------
%%% @author Justas Dautaras
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 03. Oct 2019 4:08 PM
%%%-------------------------------------------------------------------
-module(node1).
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
  node(Id, Predecessor, Successor).

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


node(Id, Predecessor, Successor) ->
  receive
    %a peer needs to know our key
    {key, Qref, Peer} ->
      Peer ! {Qref, Id},
      node(Id, Predecessor, Successor);
    %a new node informs us of its existence
    {notify, New} ->
      Pred = notify(New, Id, Predecessor),
      node(Id, Pred, Successor);
    %a predecessor needs to know our predecessor
    {request, Peer} ->
      request(Peer, Predecessor),
      node(Id, Predecessor, Successor);
    %our successor informs us about its predecessor
    {status, Pred} ->
      Succ = stabilize(Pred, Id, Successor),
      node(Id, Predecessor, Succ);
    stabilize ->
      stabilize(Successor),
      node(Id, Predecessor, Successor);
    probe ->
      create_probe(Id, Successor),
      node(Id, Predecessor, Successor);
    {probe, Id, Nodes, T} ->
      remove_probe(T, Nodes),
      node(Id, Predecessor, Successor);
    {probe, Ref, Nodes, T} ->
      forward_probe(Ref, T, Nodes, Id, Successor),
      node(Id, Predecessor, Successor)
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

notify({Nkey, Npid}, Id, Predecessor) ->
  case Predecessor of
    nil ->
      {Nkey, Npid};
    {Pkey, _} ->
  case key:between(Nkey, Pkey, Id) of
    true ->
      {Nkey, Npid};
    false ->
      Predecessor
  end
end.

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