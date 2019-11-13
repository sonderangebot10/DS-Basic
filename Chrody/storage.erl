%%%-------------------------------------------------------------------
%%% @author Justas Dautaras
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. Oct 2019 2:03 PM
%%%-------------------------------------------------------------------
-module(storage).
-export([create/0, add/3, lookup/2, split/3, merge/2]).
-author("Justas Dautaras").

create() ->
  [].

add(Key, Value, Store) ->
  [{Key,Value}| Store].

lookup(Key, List) ->
  case lists:keyfind(Key, 1, List) of
    {Key, Value} ->
      Value;
    false ->
      false
  end.

%return a tuple {Updated, Rest} where the
%updated store only contains the key value pairs requested and the rest
%are found in a list of key-value pairs
split(From, To, Store) ->
  lists:foldl(fun({Key, Value}, {Split1, Split2}) ->
    case key:between(Key, To, From) of
      true ->
        {[{Key, Value} | Split1], Split2};
      false ->
        {Split1, [{Key, Value} | Split2]}
    end

              end, {[],[]}, Store).

merge(List1, List2) ->
  lists:append(List1, List2).