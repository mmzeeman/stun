%%%-------------------------------------------------------------------
%%% File    : stun_treap_test.erl
%%% Author  : MM Zeeman <mmzeeman@xs4all.nl>
%%% Description : STUN treap test suite
%%%
%%%
%%% Copyright (C) 2024 MM Zeeman. All Rights Reserved.
%%%
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%%
%%%-------------------------------------------------------------------

-module(stun_treap_test).

-include_lib("eunit/include/eunit.hrl").

empty_test() ->
   E = stun_treap:empty(),
   ?assertEqual(true, stun_treap:is_empty(E)),
   ?assertEqual(error, stun_treap:lookup(test, E)),
   ?assertEqual([], stun_treap:fold(fun(_, A) -> A end, [], E)),
   ?assertEqual([], stun_treap:to_list(E)),

   E1 = stun_treap:from_list([]),
   ?assertEqual(true, stun_treap:is_empty(E1)),
   ok.

insert_and_delete_test() ->
   T = stun_treap:empty(),

   T1 = stun_treap:insert(a, 1, <<"a thing">>, T),
   T2 = stun_treap:insert(b, 1, <<"b thing">>, T1),
   T3 = stun_treap:insert(c, 1, <<"c thing">>, T2),

   ?assertEqual(false, stun_treap:is_empty(T3)),

   ?assertEqual({ok, 1, <<"a thing">>}, stun_treap:lookup(a, T3)),
   ?assertEqual({ok, 1, <<"b thing">>}, stun_treap:lookup(b, T3)),
   ?assertEqual({ok, 1, <<"c thing">>}, stun_treap:lookup(c, T3)),

   D1 = stun_treap:delete(b, T3),
   ?assertEqual({ok, 1, <<"a thing">>}, stun_treap:lookup(a, D1)),
   ?assertEqual(error, stun_treap:lookup(b, D1)),
   ?assertEqual({ok, 1, <<"c thing">>}, stun_treap:lookup(c, D1)),

   D2 = stun_treap:delete(b, D1),
   ?assertEqual({ok, 1, <<"a thing">>}, stun_treap:lookup(a, D2)),
   ?assertEqual(error, stun_treap:lookup(b, D2)),
   ?assertEqual({ok, 1, <<"c thing">>}, stun_treap:lookup(c, D2)),

   D3 = stun_treap:delete(a, D1),
   ?assertEqual(error, stun_treap:lookup(a, D3)),
   ?assertEqual(error, stun_treap:lookup(b, D3)),
   ?assertEqual({ok, 1, <<"c thing">>}, stun_treap:lookup(c, D3)),

   D4 = stun_treap:delete(c, D3),
   ?assertEqual(error, stun_treap:lookup(a, D4)),
   ?assertEqual(error, stun_treap:lookup(b, D4)),
   ?assertEqual(error, stun_treap:lookup(c, D4)),

   ?assertEqual(true, stun_treap:is_empty(D4)),

   ok.

insert_and_delete_many_test() ->
    T = lists:foldl(fun(E, Acc) ->
                            stun_treap:insert(E, 1, {t, E}, Acc)
                    end,
                    stun_treap:empty(),
                    lists:seq(1, 20000)),

   ?assertEqual({ok,1, {t, 1001}}, stun_treap:lookup(1001, T)),
   ?assertEqual({ok,1, {t, 2002}}, stun_treap:lookup(2002, T)),
   ?assertEqual({ok,1, {t, 3003}}, stun_treap:lookup(3003, T)),
   ?assertEqual({ok,1, {t, 4004}}, stun_treap:lookup(4004, T)),

   T1 = lists:foldl(fun(E, Acc) ->
                            stun_treap:delete(E, Acc)
                    end,
                    T,
                    lists:seq(1, 20000)),

   ?assertEqual(true, stun_treap:is_empty(T1)),

   ok.


