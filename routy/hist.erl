%%%-------------------------------------------------------------------
%%% @author Justas Dautaras
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 12. Sep 2019 8:33 PM
%%%-------------------------------------------------------------------
-module(hist).
-export([new/1, update/3]).
-author("Justas Dautaras").

new(Name) ->
  D = dict:new(),
  dict:append(Name, inf, D).

update(Node, N, History) ->
  case dict:find(Node, History) of
    {ok,[Value|_]} ->
      if
        N > Value ->
          Dict1 = dict:erase(Node, History),
          Dict2 = dict:append(Node, N, Dict1),
          {new, Dict2};
        true ->
          old
      end;
    error ->
      {new, dict:append(Node, N, History)}
  end.
