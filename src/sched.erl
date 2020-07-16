-module(sched).
-export([select_opt/2]).

-include("cauder.hrl").

select_opt(Sem, System) ->
  Sched = System#sys.sched,
  select_opt(Sem, System, Sched).

select_opt(Sem, System, ?SCHED_RANDOM) ->
   Opts = Sem:eval_opts(System),
   select_rand(Opts);
select_opt(Sem, System, ?SCHED_PRIO_RANDOM) ->
   Opts =
   case Sem:eval_procs_opts(System) of
      [] -> Sem:eval_sched_opts(System);
      ProcsOpts -> ProcsOpts
   end,
   select_rand(Opts).

select_rand(Opts) ->
   case Opts of
      [] -> none;
      _ ->
      RandIdx = rand:uniform(length(Opts)),
      lists:nth(RandIdx, Opts)
   end.
