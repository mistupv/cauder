-module(trace_pt).

-export([parse_transform/2]).

% TODO: Treat correctly errors to be considered as a value
parse_transform(Forms, Opts) ->
	put(modules_to_instrument, hd([InsMod0 || {inst_mod, InsMod0} <- Opts])),
	put(cur_dir, hd([Dir0 || {i, Dir0} <- Opts])),
	put(free, 0),
	put(stamp_mode, proplists:get_value(stamp_mode, Opts)),
	ModFileName =
		lists:sort(
			lists:flatten(
				[
					erl_syntax_lib:fold(
						fun get_module_filename/2,
						[],
						Form)
					|| Form <- Forms])),
	NForms =
		[
			erl_syntax_lib:map(
				fun(CForm) ->
					inst_fun(CForm, ModFileName)
				end,
				Form)
			|| Form <- Forms],

	RForms = erl_syntax:revert_forms(NForms),
	% {file_name,{tree,string,{attr,0,[],none},FileName}} = hd(ModFileName),
	% {ok, File} = file:open("./inst_" ++ FileName, [write]),
	% [io:format(File, "~s", [erl_pp:form(RForm)]) || RForm <- RForms],
	RForms.

get_module_filename(T, Acc) ->
	case erl_syntax:type(T) of
		attribute ->
			NameAttr =
				erl_syntax:attribute_name(T),
			case erl_syntax:type(NameAttr) of
				atom ->
					% io:format("~p\n", [erl_syntax:atom_value(NameAttr)]),
					case erl_syntax:atom_value(NameAttr) of
						% file ->
						% 	[{file_name, hd(erl_syntax:attribute_arguments(T))} | Acc];
						module ->
							ModName =
								hd(erl_syntax:attribute_arguments(T)),
							[{file_name, erl_syntax:string(atom_to_list(erl_syntax:atom_value(ModName)) ++ ".erl") },
								{module_name, ModName} | Acc];
						_ ->
							Acc
					end;
				_ ->
					Acc
			end;
		_ ->
			Acc
	end.

inst_fun(T, _ModFileName) ->
	case erl_syntax:type(T) of
		function ->
			Clauses = erl_syntax:function_clauses(T),
			NClauses = inst_fun_clauses(Clauses, erl_syntax:function_name(T)),
			erl_syntax:function(erl_syntax:function_name(T), NClauses);
		_ ->
			T
	end.

inst_expr(T) ->
	NT =
		case erl_syntax:type(T) of
			receive_expr ->
				Clauses = erl_syntax:receive_expr_clauses(T),
				StampVar = free_named_var("StampRec"),
				{NClauses,_} =
					lists:mapfoldl(
						fun inst_receive_clause/2,
						{1, StampVar},
						Clauses),
				NReceive =
					erl_syntax:receive_expr(
						NClauses,
						erl_syntax:receive_expr_timeout(T),
						erl_syntax:receive_expr_action(T)),
				NReceive;
			infix_expr ->
				case erl_syntax:operator_name(erl_syntax:infix_expr_operator(T)) of
					'!' ->
						inst_send(T,
							[erl_syntax:infix_expr_left(T),
								erl_syntax:infix_expr_right(T)]);
					_ ->
						T
				end;
			application ->
				AppOper = erl_syntax:application_operator(T),
				NApp =
					case erl_syntax:type(AppOper) of
						module_qualifier ->
							ModName =
								erl_syntax:module_qualifier_argument(AppOper),
							FunName =
								erl_syntax:module_qualifier_body(AppOper),
							try
								case {erl_syntax:atom_value(ModName), erl_syntax:atom_value(FunName)} of
									{erlang, send} ->
										inst_send(T, erl_syntax:application_arguments(T));
                  {erlang, nodes} ->
                    inst_nodes(T);
									_ ->
										inst_call_loading(T, ModName)
								end
							catch
								_:_ ->
									inst_call_loading(T, ModName)
							end;
            atom ->
              case erl_syntax:atom_value(AppOper) of
                nodes -> inst_nodes(T);
                _     -> T
              end;
						variable ->
							T;
						fun_expr ->
							T;
						implicit_fun ->
							T;
						_ ->
							T
					end,
				NApp;
			_ ->
				T
		end,
	Res = erl_syntax:set_ann(NT, erl_syntax:get_ann(T)),
	Res.

% Comment this and uncomment next for module loading support
inst_call_loading(T, _ModName) ->
	T.
% Uncomment for module loading support
% inst_call_loading(T, ModName) ->
% erl_syntax:case_expr(
% 	erl_syntax:application(
% 		erl_syntax:atom(code),
% 		erl_syntax:atom(where_is_file),
% 		[erl_syntax:list([erl_syntax:string(get(cur_dir))]),
% 		erl_syntax:infix_expr(
% 				erl_syntax:application(
% 					erl_syntax:atom(erlang),
% 					erl_syntax:atom(atom_to_list),
% 					[ModName]),
% 				erl_syntax:operator("++"),
% 				erl_syntax:string(".erl"))]),
% 	[
% 		% TODO: try to load also from src directory
% 		erl_syntax:clause(
% 			[erl_syntax:atom(non_existing)],
% 			[],
% 			[
% 				erl_syntax:case_expr(
% 					erl_syntax:application(
% 						erl_syntax:atom(lists),
% 						erl_syntax:atom(member),
% 						[ModName,
% 						 lists_with_modules_to_instument()]),
% 					[
% 						erl_syntax:clause(
% 							[erl_syntax:atom(true)] ,
% 							[],
% 							[
% 								build_send_load(ModName),
% 								build_receive_load(),
% 								T
% 							]),
% 						erl_syntax:clause(
% 							[erl_syntax:atom(false)] ,
% 							[],
% 							[
% 								T
% 							])
% 					])
% 			]),
% 		erl_syntax:clause(
% 			[erl_syntax:underscore()] ,
% 			[],
% 			[
% 				build_send_load(ModName),
% 				build_receive_load(),
% 				T
% 			])
% 	]).

inst_fun_clauses(Clauses, _FunId) ->
	[
		begin
			NBody0 =
				erl_syntax:clause_body(
					erl_syntax_lib:map(
						fun inst_expr/1, Clause )),
			erl_syntax:clause(
				erl_syntax:clause_patterns(Clause),
				erl_syntax:clause_guard(Clause),
				NBody0)
		end
		|| Clause <- Clauses].

inst_send(_T, SendArgs) ->
	{VarArgs, StoreArgs} =
		args_assign("TRCSendArg", SendArgs),

	SndVarArg = lists:nth(2, VarArgs),

	case get(stamp_mode) of
		"distributed" -> inst_send_dist_stamp(_T, VarArgs, StoreArgs);
		"central" -> inst_send_central_stamp(_T, VarArgs, SndVarArg, StoreArgs)
	end.

% @doc
% when the option 'stamp_mode' is central the code will ask for a unique integer
% that will act as a stamp, the message is sent to a non-existent node but thanks to the dbg
% server (see tracer:trace_handler/2) we will be able to intercept the request and provide the stamp
% doc@
inst_send_central_stamp(_T, VarArgs, SndVarArg, StoreArgs) ->
	SendSend =
		build_send_trace(
			send_sent, []),

	{RecStamp, StampVar} =
		build_rec_stamp(),

	SendWithStamp =
		build_send_stamp(
			hd(VarArgs),
			SndVarArg,
			StampVar),

	BlockSend =
		erl_syntax:block_expr(StoreArgs ++ [SendSend, RecStamp, SendWithStamp, SndVarArg]),
	BlockSend.

% @doc
% when the option 'stamp_mode' is distributed the stamp will be automatically generated
% by the send through the BIF unique_integer
% doc@
inst_send_dist_stamp(_T, VarArgs, StoreArgs) ->

	SndVarArg = lists:nth(2, VarArgs),

	Stamp = erl_syntax:tuple([erl_syntax:atom(stamp),
		erl_syntax:application(erl_syntax:atom(erlang), erl_syntax:atom(unique_integer), [])]
	),

	SendWithStamp =
		build_send_stamp(
			hd(VarArgs),
			SndVarArg,
			Stamp),

	BlockSend = erl_syntax:block_expr(StoreArgs ++ [SendWithStamp]),
	BlockSend.

build_rec_stamp() ->
	StampVar = free_named_var("Stamp"),
	RecExpr =
		erl_syntax:receive_expr(
			[
				erl_syntax:clause(
					[
						erl_syntax:tuple(
							[
								erl_syntax:atom(recv_stamp),
								StampVar
							])
					],
					[],
					[erl_syntax:atom(ok)]

				)
			]),
	{RecExpr, erl_syntax:tuple(
		[
			erl_syntax:atom(stamp),
			StampVar
		])}.

build_send(Msg) ->
	build_send_par(
		erl_syntax:tuple([
			erl_syntax:atom(non_existent),
			erl_syntax:atom(node())
		]),
		[erl_syntax:tuple(Msg)]).

build_send_trace(Tag, Args) ->
	build_send(
		[
			erl_syntax:atom(Tag),
			erl_syntax:application(
				erl_syntax:atom(erlang) ,
				erl_syntax:atom(self),
				[]),
     erl_syntax:tuple(Args)
		]).

inst_receive_clause(Clause, InstInfo) ->
	{CurrentClause, StampVar} = InstInfo,
	{ [_VarMsg], Patterns} =
		args_assign("TRCMsg", erl_syntax:clause_patterns(Clause)),

  SendEvaluated =
		build_send_trace(
			receive_evaluated,
			[StampVar]),

	NBody = [SendEvaluated] ++ erl_syntax:clause_body(Clause),

	NPatterns = [erl_syntax:tuple([StampVar| Patterns])],

	NClause =
		erl_syntax:clause(NPatterns, erl_syntax:clause_guard(Clause), NBody),
	{erl_syntax:set_ann(NClause, erl_syntax:get_ann(Clause) ), {CurrentClause + 1, StampVar}}.

build_send_par(Dest, Pars) ->
	erl_syntax:application(
		erl_syntax:atom(erlang) ,
		erl_syntax:atom(send),
		[Dest| Pars]).

% build_send_load(Module) ->
% 	build_send(
% 		[
% 	 		erl_syntax:atom(load_module),
% 	 		Module,
% 	 		erl_syntax:application(
% 	 			erl_syntax:atom(erlang) ,
% 				erl_syntax:atom(self),
% 				[])
% 	 	] ).

build_send_stamp(Pid, Msg, Stamp) ->
	build_send_par(
		Pid,
		[erl_syntax:tuple([
			Stamp,
			Msg
		])
		]).

% build_receive_load() ->
% 	erl_syntax:receive_expr
% 	(
% 		[
% 			erl_syntax:clause
% 			(
% 				[erl_syntax:atom(loaded)],
% 				[],
% 				[erl_syntax:atom(ok)
% 			 	]
% 			)
% 		]
% 	) .

inst_nodes(_T) ->
  AppNodes = erl_syntax:application(
               erl_syntax:atom(erlang),
               erl_syntax:atom(nodes),
               []
              ),
	SendNodes = build_send([erl_syntax:atom(log_nodes),AppNodes]),
  SendNodes.


get_free() ->
	Free = get(free),
	put(free, Free + 1),
	Free.

free_named_var(NameRoot) ->
	erl_syntax:variable("_" ++ NameRoot ++ integer_to_list(get_free()) ).

args_assign(NameRoot, Args) ->
	lists:unzip(
		[ begin
				VarArg =
					free_named_var(NameRoot),
				StoreArg =
					erl_syntax:match_expr(VarArg, Arg),
				{VarArg, StoreArg}
			end
			|| Arg <- Args] ).

% lists_with_modules_to_instument() ->
% 	erl_syntax:list(
% 		[erl_syntax:atom(M)
% 		|| 
% 		M <- get(modules_to_instrument), is_atom(M)]).
