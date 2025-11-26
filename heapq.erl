%%%% heapq.erl
%%%%
%%%% Implements a priority queue via immutable skew-binomial trees.
%%%%
%%%% Adapted from https://github.com/aphyr/skewbinheap/blob/master/skewbinheap.erl ,
%%%% which was adapted from Okasaki's _Purely Functional Data Structures_.

-module(heapq).
-export([new/0, new/1, len/1, merge/2, push/2, peek/1, pop/1]).
-export_type([heap/1]).

%%%
%%% Types
%%%

%%% A tree is an integer, a value, a list of values, and a list of child trees.
-record(tree, {
    rank = 0,
    root,
    values = [],
    children = []
}).
-type tree(A) :: #tree{
    %% Number of elements in 'children'.
    rank :: integer(),
    %% Minimal value at this level.
    root :: A,
    %% Non-minimal values at this level.  Stored in increasing order.
    values :: [A],
    %% Non-minimal subtrees at this level.  Not sorted among themselves, but
    %% their roots are all less minimal than our root.
    children :: [tree(A)]
}.

%%% A heap is a list of trees, sorted ascending by rank.  This sorting is only
%%% weak at the very first comparison, but strong thereafter.
-type forest(A) :: [tree(A)].
-type heap(A) :: empty | {Smallest :: A, Rest :: forest(A)}.

%%%
%%% Constructors &c
%%%

%%% Create a new heap.
-spec new() -> heap(_).
new() ->
    empty.

%%% Create a new heap from a(n unsorted) list.
-spec new([T]) -> heap(T).
new(List) ->
    lists:foldl(fun push/2, new(), List).

%%%
%%% Heap manipulations
%%%

%%% Insert a bare object into the heap.
-spec push(X, heap(Y)) -> heap(X | Y).
%%% X is always the smallest element of an empty heap.
push(X, empty) ->
    {X, []};
%%% Otherwise, push the second-largest element into the forest.
push(X, {Y, Rest}) ->
    {min(X, Y), push_forest(max(X, Y), Rest)}.

%%% Merge two heaps.
-spec merge(heap(X), heap(Y)) -> heap(X | Y).
%%% Empty heaps are trivial to merge.
merge(empty, T2) -> T2;
merge(T1, empty) -> T1;
%%% Otherwise, merge the forests with the second-largest element.
merge({X1, T1}, {X2, T2}) ->
    Min = min(X1, X2),
    Max = max(X1, X2),
    MergedForests = merge_forests(normalize(T1), normalize(T2)),
    {Min, push_forest(Max, MergedForests)}.

%%% Find minimum element.
-spec peek(heap(X)) -> X.
peek({Min, _Rest}) -> Min.

%%% Deletes minimum element.
-spec pop(heap(X)) -> heap(X).
pop({_X, []}) -> empty;
pop({_X, Ts}) ->
    {T, Ts2} = remove_min_tree(Ts),
    NewTree = lists:foldl(
        fun push_forest/2,
        merge_forests(lists:reverse(T#tree.children), Ts2),
        T#tree.values
    ),
    {T#tree.root, NewTree}.

%%% Counts the number of elements in a heap.
-spec len(heap(_)) -> integer().
len(empty) -> 0;
len({_X, Ts}) -> 1 + lists:sum([len(T) || T <- Ts]);
len(#tree{values = Values, children = Children}) ->
    1 + length(Values) + lists:sum([len(C) || C <- Children]).

%%%
%%% Utilities
%%%

%%% "Link" two trees together into a single tree by converting one into a child
%%% of the other.  The ancestral tree is the one with the smaller root.
-spec link(tree(X), tree(Y)) -> tree(X | Y).
link(T1, T2) when T1#tree.root =< T2#tree.root ->
    T1#tree{
        rank = T1#tree.rank + 1,
        children = [T2 | T1#tree.children]
    };
link(T1, T2) ->
    T2#tree{
        rank = T1#tree.rank + 1,
        children = [T1 | T2#tree.children]
    }.

%%% Add a new element to an existing tree.  Either it replaces the root as the
%%% smallest element, or it is adjoined to the list of non-minimal values.
-spec new_root(X, tree(Y)) -> tree(X | Y).
new_root(X, T) when X =< T#tree.root ->
    T#tree{
        root = X,
        values = [T#tree.root | T#tree.values]
    };
new_root(X, T) ->
    T#tree{
        values = [X | T#tree.values]
    }.

%%% Link two trees together _and_ position a new element at their root.
-spec skew_link(A, tree(B), tree(C)) -> tree(A | B | C).
skew_link(X, T1, T2) ->
    T = link(T1, T2),
    new_root(X, T).

%%% Push a tree into a forest.  If this violates the weakly ascending rank
%%% condition, then link the offending pair and retry.
-spec push_tree(tree(X), forest(Y)) -> forest(X | Y).
push_tree(T, []) ->
    [T];
push_tree(T1, [T2 | Ts]) when T1#tree.rank < T2#tree.rank ->
    [T1 | [T2 | Ts]];
push_tree(T1, [T2 | Ts]) ->
    push_tree(link(T1, T2), Ts).

%%% Remove minimum tree from a heap (according to root ordering).
-spec remove_min_tree(forest(X)) -> {tree(X), forest(X)}.
remove_min_tree([T]) ->
    {T, []};
remove_min_tree([T | Ts]) ->
    {T1, Ts1} = remove_min_tree(Ts),
    case T#tree.root =< T1#tree.root of
        true  -> {T, Ts};
        false -> {T1, [T | Ts1]}
    end.

%%% We'd like to insert the bare object as a singleton tree at the heap head.
%%% However, this might push a weak comparison further down the heap, which we
%%% won't tolerate.  If that's about to happen, use skew_link/3 instead.
push_forest(X, [T1, T2 | Rest]) when T1#tree.rank == T2#tree.rank ->
    [skew_link(X, T1, T2) | Rest];
%%% Otherwise, we're safe to do the cheap singleton trick.
push_forest(X, Ts) ->
    [#tree{root = X} | Ts].

%%% Force a strictly ascending rank condition at the heap head.
-spec normalize(forest(X)) -> forest(X).
normalize([]) -> [];
normalize([Head | Tail]) ->
    push_tree(Head, Tail).

%%% Merge two heaps together.
-spec merge_forests(forest(X), forest(Y)) -> forest(X | Y).
%%% Base cases: merging into an empty heap.
merge_forests(H, []) -> H;
merge_forests([], H) -> H;
%%% Easy cases: it's clear how to maintain the ascending rank condition.
merge_forests([X|Xs], [Y|_] = H2) when X#tree.rank < Y#tree.rank ->
    [X | merge_forests(Xs, H2)];
merge_forests([X|_] = H1, [Y|Ys]) when Y#tree.rank < X#tree.rank ->
    [Y | merge_forests(H1, Ys)];
%%% Hard case: no way to maintain ascending rank.  Instead, we link the two
%%% equal-rank trees, continue the merge beyond this point, and when that's
%%% complete re-insert the bumped-up-by-one linked tree back into the head.
merge_forests([X|Xs], [Y|Ys]) ->
    push_tree(link(X, Y), merge_forests(Xs, Ys)).
