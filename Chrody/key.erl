%%%-------------------------------------------------------------------
%%% @author Justas Dautaras
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 03. Oct 2019 3:56 PM
%%%-------------------------------------------------------------------
-module(key).
-export([generate/0, between/3]).
-author("Justas Dautaras").

generate() ->
  random:uniform(1000000000).

between(Key, From, To) ->
  if
    From<To ->
      (Key>From) and (Key=<To);
    %Mean it can go (from; inf) or (inf; to]
    From>To ->
      (Key>From) or (Key=<To);
    true ->
      true
  end.