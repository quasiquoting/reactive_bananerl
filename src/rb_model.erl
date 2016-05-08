%%%% -*- mode: erlang; erlang-indent-level: 2 -*-
-module(rb_model).

-compile({parse_transform, cut}).
%% -compile({parse_transform, do}).

%% First-order
-export_type([event/1,behavior/1]).
-export([never/0,union_with/3,filter_just/1,apply/2]).

%% Moment and accumulation
-export_type([moment/1]).
-export([accum_e/2,stepper/2]).

%% Higher-order
-export([value_b/1,observe_e/1,switch_e/1,switch_b/2]).

-import(base,   [const/1]).
-import(monad,  [join/2]).
-import(zlists, [append/1,drop/2,filter/2,map/2,
                 new/2,recurrent/2,seq/3,take/2]).

%% Helpers
-export([fmap/2]).

%% From zlists.erl
-define(EXPAND(Tail), if is_function(Tail, 0) -> Tail(); true -> Tail end).

-record(event, {values}).
-type event(A) :: #event{values :: zlists:zlist(A)}.

-record(behavior, {values}).
-type behavior(A) :: #behavior{values :: zlists:zlist(A)}.

-record(moment, {value :: any()}).
-type moment(A) :: #moment{value :: fun((non_neg_integer()) -> A)}.


%%%===================================================================
%%% First-order
%%%===================================================================

-spec never() -> event(any()).
never() -> #event{values=repeat(error)}.

-spec union_with(fun((A, A) -> A), event(A), event(A)) -> event(A).
union_with(F, #event{values=Xs}, #event{values=Ys}) ->
  Combine = fun ({ok,X}, {ok,Y}) -> {ok,F(X, Y)};
                ({ok,X}, error)  -> {ok,X};
                (error,  {ok,Y}) -> {ok,Y};
                (error,  error)  -> error
            end,
  #event{values=zipwith(Combine, Xs, Ys)}.

-spec filter_just(event(maybe_m:maybe(A))) -> event(A).
filter_just(#event{values=Xs}=Event) ->
  Event#event{values=filter(maybe_m:fmap(join(maybe_m, _), _), Xs)}.

-spec apply(behavior(fun((A) -> B)), event(A)) -> event(B).
apply(#behavior{values=Fs}, #event{values=Xs}=Event) ->
  Event#event{values=zipwith(maybe_m:fmap(_, _), Fs, Xs)}.


%%%===================================================================
%%% Moment and accumulation
%%%===================================================================

-spec forget_e(non_neg_integer(), event(A)) -> zlists:zlist(maybe_m:maybe(A)).
forget_e(Time, #event{values=Xs}) -> drop(Time, Xs).

-spec stepper(A, event(A)) -> moment(behavior(A)).
stepper(I, E) ->
  V = fun(T) ->
          #behavior{values=append([ replicate(T, I)
                                  , map(const(I), forget_e(T, E))
                                  ])}
      end,
  #moment{value=V}.

-spec accum_e(A, event(fun((A) -> A))) -> moment(event(A)).
accum_e(_A, _E1) -> undefined.


%%%===================================================================
%%% Higher-order
%%%===================================================================

-spec value_b(behavior(A)) -> moment(A).
value_b(#behavior{values=B}) -> #moment{value=nth(_,B)}.

%% TODO: consider 1-indexed.
-spec observe_e(event(moment(A))) -> event(A).
observe_e(E) ->
  F = fun(T, X) -> fmap(erlang:apply(_#moment.value, [T]), X) end,
  E#event{values=zipwith(F, recurrent(0, _+1), E#event.values)}.

-spec switch_e(event(event(A))) -> moment(event(A)).
switch_e(_Es) -> undefined.

%% TODO: consider 1-indexed.
-spec forget_diagonal_e(event(event(A))) -> EZListMaybeA when
    EZListMaybeA :: event(zlists:zlist(maybe_m:maybe(A))).
forget_diagonal_e(#event{values=Xs}=E) ->
  Ys = zipwith(fun(T, X) -> fmap(forget_e(T, _), X) end, recurrent(0, _+1), Xs),
  E#event{values=Ys}.

-spec switch_b(behavior(A), event(behavior(A))) -> moment(behavior(A)).
switch_b(B, E) -> fmap(fun diagonal_b/1, stepper(B, E)).

%% TODO: consider 1-indexed.
-spec diagonal_b(behavior(behavior(A))) -> behavior(A).
diagonal_b(#behavior{values=Xs}=BB) ->
  Ys = zipwith(nth(_, _), recurrent(0, _+1), map(_#behavior.values, Xs)),
  BB#behavior{values=Ys}.


%%%===================================================================
%%% Helpers
%%%===================================================================

%% TODO: Consider pulling the records out into their own modules
%%       that implement the functor behaviour.
-spec fmap(fun((A) -> B), event(A))    -> event(B);
          (fun((A) -> B), behavior(A)) -> behavior(B).
fmap(Fun, #event{values=Xs}=E)    -> E#event{values=map(fmap(Fun, _), Xs)};
fmap(Fun, #behavior{values=Xs}=B) -> B#behavior{values=map(Fun, Xs)};
%% FIXME: fmap for moment is broken
fmap(Fun, #moment{value=X}=M) -> M#moment{value=fmap(Fun, X)}.


%%%===================================================================
%%% Internal functions
%%%===================================================================

%% TODO: Consider making this 1-indexed, like lists, etc.
-spec nth(integer(), zlists:zlist(A)) -> A.
nth(N, ZList) -> hd(drop(N, ZList)).

-spec repeat(A) -> zlists:zlist(A).
repeat(Elem) -> recurrent(Elem, const(Elem)).

-spec replicate(integer(), A) -> zlists:zlist(A).
replicate(N, Elem) -> take(N, repeat(Elem)).

-spec zipwith(Combine, ZList1, ZList2) -> ZList3 when
    Combine :: fun((A, B) -> C),
    ZList1  :: zlists:zlist(A),
    ZList2  :: zlists:zlist(B),
    ZList3  :: zlists:zlist(C).
zipwith(_, [], _) -> [];
zipwith(_, _, []) -> [];
zipwith(F, [H1|Tail1], [H2|Tail2]) ->
  new([F(H1,H2)], fun()-> zipwith(F, ?EXPAND(Tail1),?EXPAND(Tail2)) end).
