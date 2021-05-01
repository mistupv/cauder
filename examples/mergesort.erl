%Works only with compiler optimizations disabled

-module(mergesort).
-export([mergesort/1, mergesort/2]).

mergesort(L) ->
    mergesort(L, none).

mergesort([], Parent) ->
    send_return([], Parent);
mergesort([X], Parent) ->
    send_return([X], Parent);
mergesort(L, Parent) ->
    Half = length(L) div 2,
    L1 = take(Half, L),
    L2 = last(length(L) - Half, L),
    spawn(?MODULE, mergesort, [L1, self()]),
    spawn(?MODULE, mergesort, [L2, self()]),
    LOrd1 =
        receive
            {result, LOrd1_} ->
                LOrd1_
        end,
    LOrd2 =
        receive
            {result, LOrd2_} ->
                LOrd2_
        end,
    send_return(merge(LOrd1, LOrd2), Parent).

send_return(Result, none) ->
    Result;
send_return(Result, Pid) ->
    Pid ! {result, Result}.

merge([], []) ->
    [];
merge([], S2) ->
    S2;
merge(S1, []) ->
    S1;
merge([H1 | T1], [H2 | T2]) ->
    B = H1 < H2,
    case B of
        % Correct
        false -> [H2 | merge([H1 | T1], T2)];
        %false -> [H1 | merge([H2 | T1], T2)];  % Incorrect
        true -> [H1 | merge(T1, [H2 | T2])]
    end.

take(0, _) -> [];
take(1, [H | _]) -> [H];
take(_, []) -> [];
% take(N,[H|T])->[H | take(N-1, T)]. % Correct

% Incorrect
take(N, [_ | T]) -> [N | take(N - 1, T)].

last(N, List) ->
    lists:reverse(take(N, lists:reverse(List))).
