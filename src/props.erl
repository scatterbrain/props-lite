%% props.erl
%%
%% Copyright 2011-2012 Grey Area
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

%% @doc Property structure library
-module(props).

-export([new/0,
         get/2,
         get/3,
         set/1,
         set/2,
         set/3,
         take/2,
         drop/2,
         merge/2,
         diff/2,
         keys/1,
         nested_keys/1,
         fold/3,
         select_matches/2,
         delete_matches/2,
         to_pretty/1,
         to_string/1,
         to_msgpack_format/1,
         from_msgpack_format/1,
         from_mochijson2/1,
         to_proplist/1,
         from_proplist/1]).

-export_type([prop_value/0, props/0, prop_path/0]).

-type prop_value() :: true | false | number() | 
                      [prop_value() | props()] | props().
-opaque props() :: {{integer(), term()}}.
-type prop_path() :: atom() | string() | binary().
-type path_tokens() :: [{prop | index, pos_integer() | binary()}].

-define(INVALID_ACCESS_IDX(Idx, Obj),
        {error, {invalid_access, index, Idx, Obj}}).
-define(INVALID_ACCESS_KEY(Key, Obj),
        {error, {invalid_access, key, list_to_atom(binary_to_list(Key)), Obj}}).

%% @doc Create a new props structure.
-spec new() -> props().
new() ->
    {gb_trees:empty()}.

%% @doc Get a property for a props structure.
%% This is equivalent to `get(PathSpec, Props, undefined)`.
-spec get(prop_path(), props()) -> undefined | prop_value().
get(Path, Props) ->
    get(Path, Props, undefined).

%% @doc Get a property in props structure by path.
-spec get(prop_path(), props(), undefined | prop_value()) -> undefined | prop_value().
get(Path, Props, Default) when is_atom(Path) ->
    do_get([atom_to_binary(Path, utf8)], Props, Default);
get(Path, Props, Default) when is_binary(Path) ->
    do_get([Path], Props, Default);
get(Path, Props, Default) ->
    do_get(Path, Props, Default).

%% @doc Internal getter which operates on path tokens.
-spec do_get(path_tokens(), prop_value(), undefined | prop_value()) -> undefined | prop_value().
do_get([], Value, _Default) ->
    Value;
do_get([Key | Rest], {GBTree}, Default) ->
    case gb_trees:lookup(Key, GBTree) of
        none ->
            Default;
        {value, Other} ->
            do_get(Rest, Other, Default)
    end;
do_get([Key | _Rest], NonProps, _Default) ->
    throw(?INVALID_ACCESS_KEY(Key, NonProps)).

%% @doc Set properties in a new props structure.
-spec set([{prop_path(), prop_value()}]) -> props().
set(PropsList) ->
    set(PropsList, props:new()).

%% @doc Set one or properties in a props structure.
%%
%% With the (Path, Value) form it sets a single property in a blank
%% props structure.
%%
%% With the (List, Props) form it sets all the properties in the given
%% list in the given props structure.
-spec set(prop_path() | [{prop_path(), prop_value()}], prop_value() | props()) -> props().
set([], Props) ->
    Props;
set([{_, _} | _] = PropsList, Props) ->
    lists:foldl(
      fun({Path, Value}, Acc) ->
              props:set(Path, Value, Acc)
      end, Props, PropsList);
set(Path, Value) ->
    props:set(Path, Value, props:new()).

%% @doc Set a property in a props structure by path.
-spec set(prop_path(), prop_value(), props()) -> props().
set(Path, Value, Props) when is_atom(Path) ->
    do_set([atom_to_binary(Path, utf8)], Value, Props);
set(Path, Value, Props) when is_binary(Path) ->
    do_set([Path], Value, Props);
set(Path, Value, Props) ->
    do_set(Path, Value, Props).

%% @doc Internal naive recursive setter.
-spec do_set(path_tokens(), prop_value(), props()) -> props().
do_set([Key], Value, {GBTree}) ->
    GBTree2 = gb_trees:enter(Key, Value, GBTree),
    {GBTree2};
do_set([Key], _Value, NonProps) ->
    throw(?INVALID_ACCESS_KEY(Key, NonProps));
do_set([Key | Rest], Value, {GBTree}) ->
    Val = case gb_trees:lookup(Key, GBTree) of
              none ->
                  case Rest of
                      [_ | _] ->
                          do_set(Rest, Value, props:new())
                  end;
              {value, Other} ->
                  do_set(Rest, Value, Other)
          end,
    GBTree2 = gb_trees:enter(Key, Val, GBTree),
    {GBTree2};
do_set([Key | _Rest], _Value, NonProps) ->
    throw(?INVALID_ACCESS_KEY(Key, NonProps)).

%% @doc Internal naive recursive dropper.
-spec do_drop(prop_path(), props()) -> props().
do_drop(Path, Props) when is_atom(Path) ->
    do_drop_path([atom_to_binary(Path, utf8)], Props);
do_drop(Path, Props) when is_binary(Path) ->
    do_drop_path([Path], Props);
do_drop(Path, Props) ->    
    do_drop_path(Path, Props).
do_drop_path([Key], {GBTree}) ->
    GBTree2 = gb_trees:delete_any(Key, GBTree),
    {GBTree2};
do_drop_path([Key], NonProps) ->
    throw(?INVALID_ACCESS_KEY(Key, NonProps));
do_drop_path([Key | Rest], {GBTree}) ->    
    Val = case gb_trees:lookup(Key, GBTree) of
              none ->
                  {GBTree};
              {value, Other} ->
                  do_drop_path(Rest, Other)
          end,
    GBTree2 = gb_trees:enter(Key, Val, GBTree),
    {GBTree2};
do_drop_path([Key | _Rest], NonProps) ->
    throw(?INVALID_ACCESS_KEY(Key, NonProps)).

%% @doc Make a property structure from a proplist.
-spec from_proplist(proplists:proplist()) -> props().
from_proplist(PropList) ->
    PropList2 = lists:map(
                  fun(Atom) when is_atom(Atom) ->
                          {atom_to_binary(Atom, utf8), true};
                     ({Key, Val}) when is_atom(Key) ->
                          {atom_to_binary(Key, utf8), Val};
                     ({Key, Val}) when is_list(Key) ->
                          {list_to_binary(Key), Val};
                     ({Key, Val}) when is_binary(Key) ->
                          {Key, Val}
                  end, PropList),
    {PropList2}.

%% @doc Return a new property structure containing specific keys only.
-spec take([prop_path()], props()) -> props().
take(Keys, InputProps) ->
    lists:foldl(fun(Key, Props) ->
        case props:get(Key, InputProps) of
            undefined -> 
                Props;
            Value ->
                props:set(Key, Value, Props)
        end            
    end, props:new(), Keys).
    
%% @doc Return a new property structure without the given keys.
-spec drop([prop_path()], props()) -> props().
drop(Keys, Props) ->
    lists:foldl(fun(Key, OldProps) ->        
        do_drop(Key, OldProps)
    end, Props, Keys).
    
%% @doc Merge two property structures.
%% Duplicate keys in the second structure overwrite those in the first.
-spec merge(props(), props()) -> props().
merge({MergeTargetTree}, {MergeSourceTree}) ->
    NewTree = lists:foldl(fun({MKey, MVal}, AccIn) ->
                gb_trees:enter(MKey, MVal, AccIn)
        end, MergeTargetTree, gb_trees:to_list(MergeSourceTree)), 
    {NewTree}.

%% @doc Return a list of differences between two property structures.
-spec diff(props(), props()) -> [{prop_path(), {prop_value(), prop_value()}}].
diff(Props1, Props2) ->
    Keys1 = sets:from_list(nested_keys(Props1)),
    Keys2 = sets:from_list(nested_keys(Props2)),
    Keys = sets:to_list(sets:union(Keys1, Keys2)),
    
    lists:foldl(fun(K, AccIn) ->
        Key = binary_to_atom(K, utf8),
        V1 = props:get(Key, Props1),
        V2 = props:get(Key, Props2),
        case (V1 =:= V2) of
            true ->
                AccIn;
            false ->
                [{Key, {V1, V2}}] ++ AccIn
        end    
    end, [], Keys).    
        
%% @doc Return the immediate keys in a property structure.
-spec keys(props()) -> [binary()].
keys({GBTree}) ->
    gb_trees:keys(GBTree).

%% @doc Return the inested keys in a property structure.
-spec nested_keys(props()) -> [binary()].
nested_keys(PropList) ->
    lists:flatten(nested_keys(<<>>, PropList)).
nested_keys(Path, {List}) when is_list(List) ->
    nested_keys(Path, List);    
nested_keys(Path, [{_, _} |_] = List) ->
    [nested_keys(Path, {K,V}) || {K,V} <- List];
nested_keys(<<>>, {K, V}) ->        
    nested_keys(K, V);
nested_keys(Path, {K, V}) ->        
    nested_keys(<<Path/binary, <<".">>/binary, K/binary>>, V);
nested_keys(Path, _) ->    
    Path.

%% @doc convert to message pack format
-spec to_msgpack_format(props()) -> term().
to_msgpack_format({0, nil}) ->
    {[]};
to_msgpack_format({GBTree}) ->
    {to_msgpack_format(gb_trees:to_list(GBTree))};
to_msgpack_format(List) when is_list(List) ->
    [to_msgpack_format(Elem) || Elem <- List];
to_msgpack_format({Key, Value}) ->
    {Key, to_msgpack_format(Value)};
to_msgpack_format(Value) ->
    Value.

%% @doc converts msgpack style map 
-spec from_msgpack_format(term()) -> props:props().
from_msgpack_format({PropList}) when is_list(PropList), length(PropList) =:= 0 ->
    props:new();
from_msgpack_format({PropList}) when is_list(PropList) ->
    props:set(from_msgpack_format(PropList));
from_msgpack_format(List) when is_list(List) ->
    [from_msgpack_format(Elem) || Elem <- List];
from_msgpack_format({Key, Value}) ->
    {Key, from_msgpack_format(Value)};
from_msgpack_format(Value) ->
    Value.

%% @doc converts from mochijson2 format (http://doc.erlagner.org/mochiweb/mochijson2.html) to props
-spec from_mochijson2(term()) -> props:props().
from_mochijson2({struct, PropList}) when is_list(PropList), length(PropList) =:= 0 ->
    props:new();
from_mochijson2({struct, PropList}) when is_list(PropList) ->
    props:set(from_mochijson2(PropList));
from_mochijson2(List) when is_list(List) ->
    [from_mochijson2(Elem) || Elem <- List];
from_mochijson2({Key, Value}) ->
    {Key, from_mochijson2(Value)};
from_mochijson2(Value) ->
    Value.

%
%% @doc Fold over the immediate keys/vals in a proplist.
-spec fold(fun((binary(), prop_value(), term()) -> term()), term(), props()) -> term().
fold(F, Init, {PropList}) ->
    lists:foldl(
      fun({Key, Val}, Acc) ->
              F(Key, Val, Acc)
      end, Init, PropList).

%% @doc Select props from a list that match certain props.
-spec select_matches([props()], props()) -> [props:props()].
select_matches(PropsList, Props) ->
    select_or_delete_matches(PropsList, Props, select).

%% @doc Delete props from a list that match certain props.
-spec delete_matches([props()], props()) -> [props:props()].
delete_matches(PropsList, Props) ->
    select_or_delete_matches(PropsList, Props, delete).

%% @doc Returns a pretty printed string of the message.
-spec to_pretty(props:props()) -> string().
to_pretty(Props) ->
    do_to_pretty(Props, 0).

%% @doc Converts message term to a string.
-spec term_to_pretty(prop_value(), pos_integer()) -> string().
term_to_pretty({_} = Props, Depth) ->
    do_to_pretty(Props, Depth + 1);
term_to_pretty(Term, _) when is_binary(Term) ->
    io_lib:format("\"~ts\"", [Term]);
term_to_pretty([_|_] = List, Depth) ->
    do_to_pretty(List, Depth + 1);
term_to_pretty(Term, _) ->
    io_lib:format("~p", [Term]).


%% @doc Converts message properties to a string.
-spec do_to_pretty(props(), pos_integer()) -> string().
do_to_pretty({[]}, _) ->
    "{}";
do_to_pretty({PropList}, Depth) ->
    Indent = string:chars($ , Depth * 4),
    F = fun ({Key, Value}, Acc) ->
		KeyStr = io_lib:format("~ts", [Key]),
		ValueStr = term_to_pretty(Value, Depth),
		KeyIndent = string:chars($ , (Depth + 1) * 4),
		[$\n, $,, ValueStr, $ , $:, KeyIndent ++ KeyStr | Acc]
	end,
    [$\n, $, | Acc1] = lists:foldl(F, "\n{", PropList),
    lists:flatten(lists:reverse([Indent ++ "}", $\n | Acc1]));
do_to_pretty([], _) ->
    "[]";
do_to_pretty([_|_] = List, Depth) ->
    Indent = string:chars($ , Depth * 4),
    F = fun(Value, Acc) ->
                ValueStr = term_to_pretty(Value, Depth),
                ValueIndent = string:chars($ , (Depth + 1) * 4),
                [$\n, $,, ValueStr, ValueIndent | Acc]
        end,
    [$\n, $, | Acc1] = lists:foldl(F, "\n[", List),
    lists:flatten(lists:reverse([Indent ++ "]", $\n | Acc1])).

%% @doc Returns a printed string of the property structure, similar to JSON.
-spec to_string(props:props()) -> string().
to_string({[]}) ->
    "{}";
to_string([]) ->
    "[]";
to_string({PropList}) ->
    S = lists:foldl(
          fun({Key, Val}, Acc) ->
                  KeyStr = io_lib:format("~ts", [Key]),
                  ValStr = term_to_string(Val),
                  [$ , $,, ValStr, $ , $:, KeyStr | Acc]
          end, "{", PropList),
    [$ , $, | S2] = S,
    lists:flatten(lists:reverse([$} | S2]));
to_string(List) when is_list(List) ->
    S = lists:foldl(
          fun(Val, Acc) ->
                  ValStr = term_to_string(Val),
                  [$ , $,, ValStr | Acc]
          end, "[", List),
    [$ , $, | S2] = S,
    lists:flatten(lists:reverse([$] | S2])).

%% @doc Stringify a term.
-spec term_to_string(prop_value()) -> string().
term_to_string({_} = Props) ->
    to_string(Props);
term_to_string(List) when is_list(List) ->
    to_string(List);
term_to_string(Binary) when is_binary(Binary) ->
    lists:flatten(io_lib:format("\"~ts\"", [Binary]));
term_to_string(Term) ->
    lists:flatten(io_lib:format("~p", [Term])).


%% @doc Returns a proplist representation
-spec to_proplist(props:props()) -> proplists:proplist().
to_proplist({PropList}) ->
    to_proplist(PropList);
to_proplist({K, V}) ->
    {K, to_proplist(V)};
to_proplist(PropList) when is_list(PropList) -> 
    [to_proplist(Prop) || Prop <- PropList];
to_proplist(Value) ->
    Value.

%% Internal functions

%% @doc Select or delete props from a list of props.
-spec select_or_delete_matches([props()], props(), select | delete) -> [props()].
select_or_delete_matches(PropsList, MatchProps, SelectOrDelete) ->
    lists:filter(
      fun(Props) ->
              case match(Props, MatchProps) of
                  true ->
                      SelectOrDelete =:= select;
		  _ ->
		      SelectOrDelete =/= select
	      end
      end, PropsList).

%% @doc Test if a props matches another props.
-spec match(props(), props()) -> boolean().
match(Props, {GBTree}) ->

    Found = lists:foldl(fun({MatchKey, MatchVal}, AccIn) ->
                case AccIn of
                    false -> %% Found out already that not a match
                        false;
                    _ ->
                        case props:get(MatchKey, Props) of
                            undefined ->
                                false;
                            {PropsVal} ->
                                match({PropsVal}, MatchVal);
                            MatchVal ->
                                true;
                            _ ->
                                false
                        end
                end

        end, true, gb_trees:to_list(GBTree)),  

    Found.


