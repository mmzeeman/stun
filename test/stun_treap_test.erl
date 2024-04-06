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


