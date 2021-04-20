-module(prova).
-export([quad/1]).

quad(N) -> provaMod:inc(N) * N.
