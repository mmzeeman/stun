%%%----------------------------------------------------------------------
%%% File    : stun_treap.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Treaps implementation
%%% Created : 22 Apr 2008 by Alexey Shchepin <alexey@process-one.net>
%%%
%%% Moved from p1_utils:treap to stun_treap
%%%
%%% Copyright (C) 2002-2022 ProcessOne, SARL. All Rights Reserved.
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
%%%----------------------------------------------------------------------

-module(stun_treap).

-export([
    empty/0,
    insert/4,
    delete/2,
    delete_root/1,
    get_root/1,
    lookup/2,
    is_empty/1,
    fold/3,
    from_list/1,
    to_list/1,
    delete_higher_priorities/2,
    priority_from_current_time/0,
    priority_from_current_time/1
]).

-type hashkey() :: {non_neg_integer(), any()}.
-type treap() :: {hashkey(), any(), any(), treap(), treap()} | nil.

-export_type([treap/0]).

%
empty() ->
    nil.

%
insert(Key, Priority, Value, Tree) ->
    insert1(Tree, hash_key(Key), Priority, Value).

insert1(nil, HashKey, Priority, Value) ->
    {HashKey, Priority, Value, nil, nil};
insert1({HashKey1, Priority1, Value1, Left, Right}, HashKey, Priority, Value) when HashKey < HashKey1 ->
    heapify({HashKey1, Priority1, Value1, insert1(Left, HashKey, Priority, Value), Right});
insert1({HashKey1, Priority1, Value1, Left, Right}, HashKey, Priority, Value) when HashKey > HashKey1 ->
    heapify({HashKey1, Priority1, Value1, Left, insert1(Right, HashKey, Priority, Value)});
insert1({_HashKey1, Priority1, _Value1, Left, Right}, HashKey, Priority, Value) when Priority == Priority1 ->
    {HashKey, Priority, Value, Left, Right};
insert1({_HashKey1, _Priority1, _Value1, _Left, _Right} = Tree, HashKey, Priority, Value) ->
    insert1(delete_root(Tree), HashKey, Priority, Value).

% 
heapify({_, _, _, nil, nil} = Tree) ->
    Tree;
heapify({HashKey, Priority, Value, nil = Left, {HashKeyR, PriorityR, ValueR, LeftR, RightR}}) when PriorityR > Priority ->
    {HashKeyR, PriorityR, ValueR, {HashKey, Priority, Value, Left, LeftR}, RightR};
heapify({_, _, _, nil, {_, _, _, _, _}} = Tree) ->
    Tree;
heapify({HashKey, Priority, Value, {HashKeyL, PriorityL, ValueL, LeftL, RightL}, nil = Right}) when PriorityL > Priority ->
    {HashKeyL, PriorityL, ValueL, LeftL, {HashKey, Priority, Value, RightL, Right}};
heapify({_, _, _, {_, _, _, _, _}, nil} = Tree) ->
    Tree;
heapify({HashKey, Priority, Value, {_, _, _, _, _} = Left, {HashKeyR, PriorityR, ValueR, LeftR, RightR}}) when PriorityR > Priority ->
    {HashKeyR, PriorityR, ValueR, {HashKey, Priority, Value, Left, LeftR}, RightR};
heapify({HashKey, Priority, Value, {HashKeyL, PriorityL, ValueL, LeftL, RightL}, {_, _, _, _, _} = Right}) when PriorityL > Priority ->
    {HashKeyL, PriorityL, ValueL, LeftL, {HashKey, Priority, Value, RightL, Right}};
heapify({_, _, _, {_, _, _, _, _}, {_, _, _, _, _}} = Tree) ->
    Tree.

%
delete(Key, Tree) ->
    delete1(hash_key(Key), Tree).

delete1(_HashKey, nil) ->
    nil;
delete1(HashKey, {HashKey1, Priority1, Value1, Left, Right}) when HashKey < HashKey1 ->
    {HashKey1, Priority1, Value1, delete1(HashKey, Left), Right};
delete1(HashKey, {HashKey1, Priority1, Value1, Left, Right}) when HashKey > HashKey1 ->
    {HashKey1, Priority1, Value1, Left, delete1(HashKey, Right)};
delete1(_, {_, _, _, _, _} = Tree) ->
    delete_root(Tree).

%
delete_root({_, _, _, nil, nil}) ->
    nil;
delete_root({_, _, _, Left, nil}) ->
    Left;
delete_root({_, _, _, nil, Right}) ->
    Right;
delete_root({HashKey, Priority, Value, {HashKeyL, PriorityL, ValueL, LeftL, RightL}, {_, PriorityR, _, _, _}=Right}) when PriorityL > PriorityR ->
    {HashKeyL, PriorityL, ValueL, LeftL, delete_root({HashKey, Priority, Value, RightL, Right})};
delete_root({HashKey, Priority, Value, {_, _, _, _, _}=Left, {HashKeyR, PriorityR, ValueR, LeftR, RightR}}) ->
    {HashKeyR, PriorityR, ValueR, delete_root({HashKey, Priority, Value, Left, LeftR}), RightR}.

%
delete_higher_priorities(Treap, DeletePriority) ->
    case is_empty(Treap) of
        true ->
            Treap;
        false ->
            case get_root(Treap) of
                {_, Priority, _} when Priority > DeletePriority ->
                    delete_higher_priorities(delete_root(Treap), DeletePriority);
                _ ->
                    Treap
            end
    end.

%
priority_from_current_time() ->
    priority_from_current_time(0).

%
priority_from_current_time(MsOffset) ->
    case MsOffset of
        0 ->
            {-erlang:monotonic_time(micro_seconds), -erlang:unique_integer([positive])};
        _ ->
            {-erlang:monotonic_time(micro_seconds) + MsOffset, 0}
    end.

%
is_empty(nil) ->
    true;
is_empty({_HashKey, _Priority, _Value, _Left, _Right}) ->
    false.

%
get_root({{_Hash, Key}, Priority, Value, _Left, _Right}) ->
    {Key, Priority, Value}.

%
lookup(Key, Tree) ->
    lookup1(Tree, hash_key(Key)).

lookup1(nil, _HashKey) ->
    error;
lookup1({HashKey1, _Priority1, _Value1, Left, _Right}, HashKey) when HashKey < HashKey1 ->
    lookup1(Left, HashKey);
lookup1({HashKey1, _Priority1, _Value1, _Left, Right}, HashKey) when HashKey > HashKey1 ->
    lookup1(Right, HashKey);
lookup1({_HashKey1, Priority1, Value1, _Left, _Right}, _HashKey) ->
    {ok, Priority1, Value1}.

%
fold(_F, Acc, nil) -> Acc;
fold(F, Acc, {{_Hash, Key}, Priority, Value, Left, Right}) ->
    Acc1 = F({Key, Priority, Value}, Acc),
    Acc2 = fold(F, Acc1, Left),
    fold(F, Acc2, Right).
% 
to_list(Tree) ->
    to_list(Tree, []).

to_list(nil, Acc) ->
    Acc;
to_list(Tree, Acc) ->
    Root = get_root(Tree),
    to_list(delete_root(Tree), [Root | Acc]).

from_list(List) ->
    from_list(List, nil).

from_list([{Key, Priority, Value} | Tail], Tree) ->
    from_list(Tail, insert(Key, Priority, Value, Tree));
from_list([], Tree) ->
    Tree.

%%
%% Helpers
%%

hash_key(Key) ->
    {erlang:phash2(Key), Key}.
