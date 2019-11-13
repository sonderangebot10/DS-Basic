%%%-------------------------------------------------------------------
%%% @author Justas Dautaras
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 21. Sep 2019 4:13 PM
%%%-------------------------------------------------------------------
-module(gms2).
-export([start/1,start/2, crash/1]).
-author("Justas Dautaras").

-define(timeout, 1000).
-define(arghh, 100).

%Master
start(Id) ->
  Rnd = random:uniform(1000),
  Self = self(),
  {ok, spawn_link(fun()-> init(Id, Rnd, Self) end)}.

init(Id, Rnd, Master) ->
  random:seed(Rnd, Rnd, Rnd),
  leader(Id, Master, [], [Master]).

leader(Id, Master, Slaves, Group) ->
  receive
  %Message from either application or peer node
  %Msg is sent to all nodes and then multicasted to all peers
    {mcast, Msg} ->
      bcast(Id, {msg, Msg}, Slaves),
      Master ! Msg,
      leader(Id, Master, Slaves, Group);
  %Wrk - process identifier of the application layer
    {join, Wrk, Peer} ->
      Slaves2 = lists:append(Slaves, [Peer]),
      Group2 = lists:append(Group, [Wrk]),
      bcast(Id, {view, [self()|Slaves2], Group2}, Slaves2),
      Master ! {view, Group2},
      leader(Id, Master, Slaves2, Group2);
    stop ->
      ok;
    Error ->
      io:format("gms ~w: LEADER, ERROR ~w~n", [Id, Error])
  end.

%Slave
start(Id, Grp) ->
  Rnd = random:uniform(1000),
  Self = self(),
  {ok, spawn_link(fun()-> init(Id, Rnd, Grp, Self) end)}.

init(Id, Rnd, Grp, Master) ->
  random:seed(Rnd, Rnd, Rnd),
  Self = self(),
  Grp ! {join, Master, Self},
  receive
    {view, [Leader|Slaves], Group} ->
      Master ! {view, Group},
      erlang:monitor(process, Leader),
      slave(Id, Master, Leader, Slaves, Group)
  after ?timeout ->
    Master ! {error, "no reply from leader"}
  end.

slave(Id, Master, Leader, Slaves, Group) ->
  receive
    %a request from its master to multicast a message, the
    %message is forwarded to the leader
    {mcast, Msg} ->
      Leader ! {mcast, Msg},
      slave(Id, Master, Leader, Slaves, Group);
    %a request from the master to allow a new node
    %to join the group, the message is forwarded to the leader
    {join, Wrk, Peer} ->
      Leader ! {join, Wrk, Peer},
      slave(Id, Master, Leader, Slaves, Group);
    %a multicasted message from the leader. A message Msg
    %is sent to the master
    {msg, Msg} ->
      Master ! Msg,
      slave(Id, Master, Leader, Slaves, Group);
    %a multicasted view from the leader. A view
    %is delivered to the master process
    {view, [Leader|Slaves2], Group2} ->
      Master ! {view, Group2},
      slave(Id, Master, Leader, Slaves2, Group2);
    {'DOWN', _Ref, process, Leader, _Reason} ->
      election(Id, Master, Slaves, Group);
    stop ->
      ok;
    Error ->
      io:format("gms ~w: SLAVE, ERROR ~w~n", [Id, Error])
  end.

election(Id, Master, Slaves, [_|Group]) ->
  Self = self(),
  case Slaves of
    [Self|Rest] ->
      bcast(Id, {view, Slaves, Group}, Rest),
      Master ! {view, Group},
      io:format("New leader: ~p ~n", [Self]),
      leader(Id, Master, Rest, Group);
    [Leader|Rest] ->
      erlang:monitor(process, Leader),
      slave(Id, Master, Leader, Rest, Group)
  end.

bcast(Id, Msg, Nodes) ->
  lists:foreach(fun(Node) -> Node ! Msg, crash(Id) end, Nodes).

crash(Id) ->
  case random:uniform(?arghh) of
    ?arghh ->
      io:format("leader ~w: crash~n", [Id]),
      exit(no_luck);
    _ ->
      ok
  end.