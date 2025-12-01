-module(blist).
-export([
    empty/0, from_list/1, to_list/1,
    count/1, nth/2, set/3, insert/3, pop/1, delete/2, append/2,
    concatenate/2, split/2,
    iter/1, next/1,
    map/2, foreach/2, foldl/3, reverse/1
    % , flatmap/2
]).
-export_type([blist/1, iter/1]).
-export([
    test_list/0
    % , test_flatmap/0
]).
-moduledoc """
Lists stored as balanced binary trees rather than as singly linked lists.

Random access, insertion, deletion, update, split, concat all run in log time.
""".

-record(node, {
    left,
    value,
    right,
    height,
    count
}).

-type blist(T) :: none | #node{
    %% Left sub-branch of the tree.
    left :: blist(T),
    %% (In-order) value at this point of the tree.
    value :: T,
    %% Right sub-branch of the tree.
    right :: blist(T),
    %% Maximum depth of recursive sub-trees beneath this point.
    height :: pos_integer(),
    %% Total number of elements stored here and in sub-trees beneath this point.
    count :: pos_integer()
}.

%%% Maximum depth of recursive sub-trees beneath this point.
-doc false.
-spec height(blist(_T)) -> non_neg_integer().
height(none) -> 0;
height(Node) -> Node#node.height.

-doc "Returns how many elements are in this blist.".
-spec count(blist(_T)) -> non_neg_integer().
count(none) -> 0;
count(Node) -> Node#node.count.

%%% Construct a (possibly unbalanced) btree out of constituent components.
-doc false.
-spec new(blist(T), T, blist(T)) -> blist(T).
new(Left, Value, Right) ->
    #node{
        left=Left,
        value=Value,
        right=Right,
        height=1 + max(height(Left), height(Right)),
        count=1 + count(Left) + count(Right)
    }.

%%% Calculate how lopsided a btree is.
-doc false.
-spec balance_factor(blist(_T)) -> non_neg_integer().
balance_factor(none) -> 0;
balance_factor(Node) ->
    height(Node#node.left) - height(Node#node.right).

%%% Reassociates [LL - LV - LR] - Value - Right to LL - LV - [LR - Value - Right].
%%% Preserves in-order traversal order, lowers the height contribution of LL,
%%% increases that of Right, and preserves that of LR.
-doc false.
-spec rotate_right(blist(T)) -> blist(T).
rotate_right(#node{left = Left, value = Value, right = Right}) ->
    #node{left = LL, value = LV, right = LR} = Left,
    NewRight = new(LR, Value, Right),
    new(LL, LV, NewRight).

%%% Reassociates Left - Value - [RL - RV - RR] to [Left - Value - RL] - RV - RR.
%%% Preserves in-order traversal order, lowers the height contribution of RR,
%%% increases that of Left, and preserves that of RL.
-doc false.
-spec rotate_left(blist(T)) -> blist(T).
rotate_left(#node{left = Left, value = Value, right = Right}) ->
    #node{left = RL, value = RV, right = RR} = Right,
    NewLeft = new(Left, Value, RL),
    new(NewLeft, RV, RR).

%%% Rotates sub-trees to keep the overall tree from becoming (too) unbalanced.
-doc false.
-spec rebalance(blist(T)) -> blist(T).
rebalance(none) -> none;
rebalance(Node) ->
    BalanceFactor = balance_factor(Node),
    if
        BalanceFactor > 1 ->
            Left = Node#node.left,  % ok because BF > 1 forces deep Left
            case height(Left#node.left) >= height(Left#node.right) of
                true -> rotate_right(Node);
                false ->
                    NewLeft = rotate_left(Left),
                    NewNode = new(NewLeft, Node#node.value, Node#node.right),
                    rotate_right(NewNode)
            end;
        BalanceFactor < -1 ->
            Right = Node#node.right,  % ok because BF < 1 forces deep Right
            case height(Right#node.right) >= height(Right#node.left) of
                true -> rotate_left(Node);
                false ->
                    NewRight = rotate_right(Right),
                    NewNode = new(Node#node.left, Node#node.value, NewRight),
                    rotate_left(NewNode)
            end;
        true ->
            Node
    end.

-doc "Access the blist element at the given index.".
-spec nth(blist(T), non_neg_integer()) -> T.
nth(#node{left = Left, value = Value, right = Right}, Index) ->
    LeftSize = count(Left),
    if
        Index < LeftSize ->
            nth(Left, Index);
        Index == LeftSize ->
            Value;
        true ->
            nth(Right, Index - LeftSize - 1)
    end.

-doc "Insert an element into a blist at the indicated index.".
-spec insert(blist(T), non_neg_integer(), T) -> blist(T).
insert(none, 0, Value) ->
    new(none, Value, none);
insert(#node{left = Left, value = Current, right = Right}, Index, Value) ->
    LeftSize = count(Left),

    NewNode = if
        Index =< LeftSize ->
            NewLeft = insert(Left, Index, Value),
            new(NewLeft, Current, Right);
        true ->
            NewRight = insert(Right, Index - LeftSize - 1, Value),
            new(Left, Current, NewRight)
    end,

    rebalance(NewNode).

-doc "Extract the leftmost element from a nonempty blist and remove it.".
-spec pop(blist(T)) -> {T, blist(T)}.
pop(#node{left = none, value = Value, right = Right}) ->
    {Value, Right};
pop(#node{left = Left, value = Value, right = Right}) ->
    {MinVal, NewLeft} = pop(Left),
    NewNode = new(NewLeft, Value, Right),
    {MinVal, rebalance(NewNode)}.

-doc "Remove the element from a blist at the indicated index.".
-spec delete(blist(T), non_neg_integer()) -> blist(T).
delete(#node{left = Left, value = Value, right = Right}, Index) ->
    LeftSize = count(Left),

    if
        Index < LeftSize ->
            NewLeft = delete(Left, Index),
            NewNode = new(NewLeft, Value, Right),
            rebalance(NewNode);
        Index > LeftSize ->
            NewRight = delete(Right, Index - LeftSize - 1),
            NewNode = new(Left, Value, NewRight),
            rebalance(NewNode);
        Left == none ->
            Right;
        Right == none ->
            Left;
        true ->
            {Next, NewRight} = pop(Right),
            NewNode = new(Left, Next, NewRight),
            rebalance(NewNode)
    end.

-doc "Construct an empty blist.".
-spec empty() -> blist(any()).
empty() -> none.

-doc "Add an element to the end of a blist.".
-spec append(blist(T), T) -> blist(T).
append(Node, Value) ->
    insert(Node, count(Node), Value).

-doc "Convert a standard list to a blist.".
-spec from_list(list(T)) -> blist(T).
from_list(List) ->
    lists:foldl(fun(L, A) -> append(A, L) end, empty(), List).

-doc "Convert a blist to a standard list.".
-spec to_list(blist(T)) -> list(T).
to_list(none) -> [];
to_list(Node) ->
    to_list(Node#node.left) ++ [Node#node.value | to_list(Node#node.right)].

-doc "Replace the element of a blist at a given index with a new value.".
-spec set(blist(T), non_neg_integer(), T) -> blist(T).
set(Node, Index, Value) ->
    insert(delete(Node, Index), Index, Value).

%%% TODO: think through how many of these rebalance calls are actually needed.
-doc "Join two blists.".
-spec concatenate(blist(S), blist(T)) -> blist(S | T).
concatenate(none, Node) -> Node;
concatenate(Node, none) -> Node;
concatenate(Left, Right) ->
    LeftHeight = height(Left),
    RightHeight = height(Right),
    if
        LeftHeight == RightHeight ->
            {Value, NewRight} = pop(Right),
            %% rebalance is not needed here: NewRight has height either
            %% RightHeight or RightHeight - 1, hence the new node is balanced.
            rebalance(new(Left, Value, NewRight));
        LeftHeight < RightHeight ->
            #node{left = RL, value = RV, right = RR} = Right,
            %% rebalance *is* needed here.  in this branch, it's possible for
            %% RL to have height RightHeight - 1, RR to have RightHeight - 2,
            %% and for Left to have height equal to that of RL.  in that case,
            %% NewLeft has height RightHeight, which is balanced against RR.
            NewLeft = concatenate(Left, RL),
            rebalance(new(NewLeft, RV, RR));
        LeftHeight > RightHeight ->
            #node{left = LL, value = LV, right = LR} = Left,
            NewRight = concatenate(LR, Right),
            rebalance(new(LL, LV, NewRight))
    end.

-doc """
Splits a blist into left- and right-sub-blists: [0, Index) and [Index, ...).
""".
-spec split(blist(T), non_neg_integer()) -> {blist(T), blist(T)}.
split(Node, 0) ->
    {none, Node};
split(Node = #node{count = Count}, Count) ->
    {Node, none};
split(#node{left = Left, value = Value, right = Right}, Index) ->
    LeftCount = count(Left),
    if
        Index =< LeftCount ->
            {LL, LR} = split(Left, Index),
            {LL, concatenate(LR, insert(Right, 0, Value))};
        Index > LeftCount ->
            {RL, RR} = split(Right, Index - LeftCount - 1),
            {concatenate(append(Left, Value), RL), RR}
    end.

-doc "Applies a function to a blist, returning a new blist as a result.".
-spec map(fun((S) -> T), blist(S)) -> blist(T).
map(_F, none) -> none;
map(F, #node{left = Left, value = Value, right = Right}) ->
    new(map(F, Left), F(Value), map(F, Right)).

-doc "Applies a function to a blist, discarding the result.".
-spec foreach(fun((S) -> any()), blist(S)) -> ok.
foreach(_F, none) -> ok;
foreach(F, #node{left = Left, value = Value, right = Right}) ->
    foreach(F, Left),
    F(Value),
    foreach(F, Right).

-doc "Folds over a blist.".
-spec foldl(fun((A, S) -> A), A, blist(S)) -> A.
foldl(_F, A, none) -> A;
foldl(F, A, #node{left = Left, value = Value, right = Right}) ->
    AL = foldl(F, A, Left),
    AV = F(AL, Value),
    foldl(F, AV, Right).

-doc "Reverses a blist.".
-spec reverse(blist(S)) -> blist(S).
reverse(none) -> none;
reverse(#node{left = Left, value = Value, right = Right}) ->
    new(reverse(Right), Value, reverse(Left)).

-type iter(S) :: fun(() -> none | {S, iter(S)}).

-doc "Constructs an iterator which traverses a B-list.".
-spec iter(blist(S)) -> iter(S).
iter(BList) ->
    R = fun
        R([]) ->
            none;
        R([{node, none} | XS]) ->
            R(XS);
        R([{value, X} | XS]) ->
            {X, fun () -> R(XS) end};
        R([{node, X} | XS]) ->
            #node{left = Left, value = Value, right = Right} = X,
            R([{node, Left}, {value, Value}, {node, Right} | XS])
    end,
    fun () -> R([{node, BList}]) end.

-doc "Retrieves the next item from a B-list iterator.".
-spec next(iter(B)) -> none | {B, iter(B)}.
next(Iter) ->
    Iter().

%%% This next part was fun to right, but it seems strictly less performant; see
%%% benchmark at bottom.

% -doc "Applies a function to a blist, concatenating the results.".
% -spec flatmap(fun((S) -> blist(T)), blist(S)) -> blist(T).
% flatmap(F, List) ->
%     flatmap(F, none, none, iter(List)).

% %% "Top-level" workhorse for flatmap, used to catch the case where there is no
% %% extra-efficient way to concatenate the next iteration value into the
% %% partially formed tree.
% %%
% %% NOTE: Arguments are ordered by how they're going to end up concatenated in a
% %% naive implementation: Left has the stable part of the tree, Right has the
% %% next mapped value which we're trying to append, and Iter has the unmapped
% %% remainder of the input tree which will go to the right of Right.
% -doc false.
% -spec flatmap(fun((S) -> blist(T)), blist(T), blist(T), none | iter(S)) -> blist(T).
% flatmap(_F, Left, Right, none) ->
%     concatenate(Left, Right);
% flatmap(F, Left, Right, Iter) ->
%     case next(Iter) of
%         none ->
%             concatenate(Left, Right);
%         {Next, Rest} ->
%             Joined = concatenate(Left, Right),
%             {NewLeft, NewRight, NewIter} = flatmap_1(F, Joined, F(Next), Rest),
%             flatmap(F, NewLeft, NewRight, NewIter)
%     end.

% %% Main workhorse for flatmap/4.  Its primary order of business is to walk up or
% %% down Left in order to find a place to put Right.  Walking down is done by
% %% recursion; walking up is done by returning / assuming that we're part of a
% %% recursion.  (See flatmap/4 for base case.)  Whenever it successfully merges
% %% Right (i.e., whenever Right is none), it pulls the next value from the
% %% iterator to use as the new Right and tries again from the current depth.
% %%
% %% Returns {Left, Right, Iter} where either Iter is none or ht(Left) < ht(Right)
% -doc false.
% -spec flatmap_1(fun((S) -> blist(T)), blist(T), blist(T), none | iter(S)) ->
%     {blist(T), blist(T), none | iter(S)}.
% flatmap_1(_F, Left, _Right = none, _Iter = none) ->
%     %% if Iter is exhausted and we're done merging, finish
%     {Left, none, none};
% flatmap_1(F, Left, _Right = none, Iter) ->
%     %% Right has finished merging in
%     case next(Iter) of
%         none ->  % exhausted case
%             {Left, none, none};
%         {Next, Rest} ->  % re-call with next item from Iter
%             flatmap_1(F, Left, F(Next), Rest)
%     end;
% flatmap_1(F, Left, Right, Iter) ->
%     %% Right is nontrivial.  walk up or down to find a place to put it.
%     LeftHeight = height(Left),
%     RightHeight = height(Right),
%     if
%         LeftHeight > RightHeight ->
%             %% Left is deeper than Right. Descend along Left.
%             #node{left = LL, value = LV, right = LR} = Left,
%             {NewLR, NewRight, NewIter} = flatmap_1(F, LR, Right, Iter),
%             NewLeft = concatenate(append(LL, LV), NewLR),
%             flatmap_1(F, NewLeft, NewRight, NewIter);
%         LeftHeight == RightHeight ->
%             NewLeft = concatenate(Left, Right),
%             flatmap_1(F, NewLeft, none, Iter);
%         LeftHeight < RightHeight ->
%             {Left, Right, Iter}
%     end.

%%%
%%% BENCHMARK
%%%

-doc false.
delete_list([_A | B], 0) -> B;
delete_list([A | B], N) -> [A | delete_list(B, N - 1)].

-doc false.
insert_list(A, 0, X) -> [X | A];
insert_list([A | B], N, X) -> [A | insert_list(B, N - 1, X)].

-doc "Benchmark.".
-spec test_list() -> ok.
test_list() ->
    SIZE = 100000,
    OPCOUNT = 10000,
    List = lists:seq(1, SIZE),
    BList = from_list(List),

    ListStart = os:system_time(microsecond),
    lists:foldl(
        fun(X, A) -> insert_list(delete_list(A, rand:uniform(SIZE - 1)), rand:uniform(SIZE - 1), X) end,
        List,
        lists:seq(1, OPCOUNT)
    ),
    ListEnd = os:system_time(microsecond),
    ListDelta = ListEnd - ListStart,

    BListStart = os:system_time(microsecond),
    lists:foldl(
        fun(X, A) -> insert(delete(A, rand:uniform(SIZE - 1)), rand:uniform(SIZE - 1), X) end,
        BList,
        lists:seq(1, OPCOUNT)
    ),
    BListEnd = os:system_time(microsecond),
    BListDelta = BListEnd - BListStart,

    io:format("List: ~pus~nBList: ~pus~n", [ListDelta, BListDelta]).

% -doc "Benchmark.".
% -spec test_flatmap() -> ok.
% test_flatmap() ->
%     F = fun(X) -> from_list([X, X * 2, X * 3, X * 4]) end,
%     Inputs = blist:from_list(lists:seq(1, 100000)),

%     FoldStart = os:system_time(microsecond),
%     foldl(fun(Acc, E) -> concatenate(Acc, F(E)) end, empty(), Inputs),
%     FoldEnd = os:system_time(microsecond),
%     FoldDelta = FoldEnd - FoldStart,

%     FlatmapStart = os:system_time(microsecond),
%     flatmap(F, Inputs),
%     FlatmapEnd = os:system_time(microsecond),
%     FlatmapDelta = FlatmapEnd - FlatmapStart,

%     io:format("foldl: ~pus~nflatmap: ~pus~n", [FoldDelta, FlatmapDelta]).
