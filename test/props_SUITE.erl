%% @doc props tests.
-module(props_SUITE).

-export([all/0,
        basic_get_with_atom_path/1,
        simple_get/1,
        simple_set/1, 
        multi_set/1, 
        create_implicit_props/1,
        throw_on_get_non_props/1,
        throw_on_set_non_props/1,
        take_keys/1,
        take_nested_keys/1,
        drop_keys/1,
        drop_nested_keys/1,
        merge/1,
        select_matches/1,
        select_matches_nested/1,
        delete_matches/1,
        delete_matches_nested/1,
        set_with_empty_list/1]).

-include_lib("common_test/include/ct.hrl").

-define(DATA, #{
            <<"a">> => 1,
            <<"b">> => [2, #{<<"c">> => 3}],
            <<"d">> => #{<<"e">> => #{<<"f">> => 4}}
    }).

-define(assertThrows(Exc, Expr),
    begin
            try
                Expr,
                throw(no_throw)
            catch
                throw:no_throw ->
                    ct:fail(no_throw);
                throw:Exception___ ->
                    case Exception___ of
                        Exc -> ok;
                        _ -> ct:fail({wrong_throw, throw, Exception___})
                    end;
                Class___:Exception___ ->
                    ct:fail({wrong_throw, Class___, Exception___})
            end
    end).

all() ->
    [basic_get_with_atom_path,
        simple_get,
        simple_set, 
        multi_set, 
        create_implicit_props, 
        throw_on_get_non_props, 
        throw_on_set_non_props, 
        take_keys, 
        take_nested_keys, 
        drop_keys,
        drop_nested_keys, 
        merge, 
        select_matches, 
        select_matches_nested, 
        delete_matches, 
        delete_matches_nested, 
        set_with_empty_list
    ].

%% Basic get tests.

basic_get_with_atom_path(_Config) ->
    4 = props:get([<<"d">>, <<"e">>, <<"f">>], ?DATA).

simple_get(_Config) ->
    1 = props:get(a, ?DATA).

simple_set(_Config) ->
    #{<<"a">> := 1} = props:set(a, 1, props:new()).

multi_set(_Config) ->
    Src = #{<<"a">> => #{<<"b">> => #{}}, <<"b">> => 2},
    Dst = #{<<"a">> => #{<<"b">> => #{<<"c">> => 1}}, <<"b">> => 2},
    Dst = props:set([<<"a">>, <<"b">>, <<"c">>], 1, Src).

create_implicit_props(_Config) ->
    Src = props:new(),
    Dst = #{<<"a">> => #{<<"b">> => 1}},
    Dst = props:set([<<"a">>, <<"b">>], 1, Src).

throw_on_get_non_props(_Config) ->
    ?assertThrows({error, {invalid_access, key, _, _}},
        props:get([<<"a">>, <<"b">>], ?DATA)).

throw_on_set_non_props(_Config) ->
    ?assertThrows({error, {invalid_access, key, _, _}},
        props:set([<<"a">>, <<"b">>], 1, ?DATA)).

take_keys(_Config) ->
    #{<<"a">> := 1} = props:take([a], ?DATA).

take_nested_keys(_Config) ->
    Exp = #{<<"d">> => #{<<"e">> => #{<<"f">> => 4}}},  
    Exp = props:take([[<<"d">>, <<"e">>, <<"f">>]], ?DATA).

drop_keys(_Config) ->
    #{<<"a">> := 1} = props:drop([b, d], ?DATA).

drop_nested_keys(_Config) ->
    Exp = #{
            <<"a">> => 1,
            <<"b">> => [2, #{<<"c">> => 3}],
            <<"d">> => #{<<"e">> => #{}}
    },
    Exp = props:drop([[<<"d">>, <<"e">>, <<"f">>]], ?DATA).

merge(_Config) ->
    Src = props:set([{a, 1}, {b, 1}]),
    Dst = props:set([{a, 1}, {b, 2}, {c, 3}]),
    Dst = props:merge(Src, props:set([{b, 2}, {c, 3}])).

select_matches(_Config) ->
    PropsList = [props:set([{a, 1}, {b, 1}]), props:set([{a, 2}, {b, 1}])],

    Matches1 = props:select_matches(PropsList, props:set([{a, 1}])),
    1 = length(Matches1),
    1 = props:get(a, hd(Matches1)),
    1 = props:get(b, hd(Matches1)),

    Matches2 = props:select_matches(PropsList, props:set([{b, 1}])),
    2 = length(Matches2),

    Matches3 = props:select_matches(PropsList, props:set([{b, 2}])),
    0 = length(Matches3).

select_matches_nested(_Config) ->
    PropsList = [props:set([{a, 1}, {[<<"c">>, <<"d">>], 2}, {[<<"c">>, <<"e">>, <<"f">>], 3}]),
        props:set([{a, 2}, {[<<"c">>, <<"d">>], 3}, {[<<"c">>, <<"e">>, <<"f">>], 3}])],

    ct:log(default, 100, "Propslist ~p~n", [PropsList]),

    Matches1 = props:select_matches(PropsList, props:set([<<"c">>, <<"d">>], 2)),
    1 = length(Matches1),
    1 = props:get(a, hd(Matches1)),

    Matches2 = props:select_matches(PropsList, props:set([<<"c">>, <<"e">>, <<"f">>], 3)),
    2 = length(Matches2).

delete_matches(_Config) ->
    PropsList = [props:set([{a, 1}, {b, 1}]),
        props:set([{a, 2}, {b, 1}])],

    Rest1 = props:delete_matches(PropsList, props:set([{a, 1}])),
    1 = length(Rest1),
    2 = props:get(a, hd(Rest1)),
    1 = props:get(b, hd(Rest1)),

    Rest2 = props:delete_matches(PropsList, props:set([{b, 1}])),
    0 = length(Rest2),

    Rest3 = props:delete_matches(PropsList, props:set([{b, 2}])),
    2 = length(Rest3).

delete_matches_nested(_Config) ->
    PropsList = [props:set([{[<<"a">>, <<"b">>], 1}, {[<<"c">>, <<"d">>, <<"e">>, <<"f">>], 2}]),
        props:set([{[<<"a">>, <<"b">>], 2}, {[<<"c">>, <<"d">>, <<"e">>, <<"f">>], 2}])],

    Rest1 = props:delete_matches(PropsList, props:set([<<"a">>, <<"b">>], 2)),
    1 = length(Rest1),
    1 = props:get([<<"a">>, <<"b">>], hd(Rest1)),

    Rest2 = props:delete_matches(PropsList, props:set([<<"c">>, <<"d">>, <<"e">>, <<"f">>], 2)),
    0 = length(Rest2).



set_with_empty_list(_Config) ->
    N = props:new(),
    N = props:set([]),
    N = props:set([], props:new()),
    ok.

