%% @doc Tools for checking on nodes, applications and process groups to
%%      support Nagios monitoring of Erlang systems.
%%
%% @author Will Larson <lethain@gmail.com> [http://lethain.com]
%% @version 0.1
%% @link <a href="http://github.com/lethain/nagios_erlang">Latest nagios_erlang source code on GitHub</a>
%% @link <a href="http://www.nagios.org/">Nagios homepage</a>
-module(nagios_erlang).
-author("Will Larson <lethain@gmail.com> [http://lethain.com]").
-version("0.1").
-export([check_process_group/1, check_node/1, check_application/1, check_statistics/1]).
-define(OK_CODE, 0).
-define(WARN_CODE, 1).
-define(CRIT_CODE, 2).
-define(UNKNOWN_CODE, 3).
-define(RPC_TIMEOUT, 1000).
-define(STAT_ALIASES_FILE, "stat_aliases.cfg").

-record(stat_state, {critical=[], warning=[], ok=[], aliases, level, memory}).

%%%
%%% External Interfaces
%%%

%% @doc Check if a process group (from remote perspective of the supplied node)
%%      has more than WarnLvl or CritLvl living processes, and return an
%%      appropriate response (OK, WARNING, CRITICAL).
%%
%%      Warning/Critical messages are never returned if the value of WarnLvl /
%%      CritLvl is 0 respectively.
%%
%%      Has no return value due to calling erlang:halt/1.
%% @spec check_process_group(atom(), atom(), integer(), integer()).
check_process_group([Node, Group, WarnLvl, CritLvl]) ->
    format_output(check_process_group_inner(Node, Group, WarnLvl, CritLvl)).

%% @doc Check if a node is accessible by net_adm:ping/1 from the current node,
%%      and return OK or CRITICAL messages as appropriate.
%%
%%      Has no return value due to calling erlang:halt/1.
%% @term check_node(atom()).
check_node([Node]) ->
    format_output(check_node_inner(Node)).

%% @doc Check if the specified application is currently running on the specified node,
%%      and return OK or CRITICAL messages as appropriate.
%%
%%      Has no return value due to calling erlang:halt/1.
%% @spec check_application(atom(), atom().
check_application([Node, Applications]) ->
    AppList = string:tokens(Applications, ","),
    format_output(check_application_inner(Node, AppList)).

check_statistics([Node, Params, AliasFile]) ->
    ParamList = string:tokens(Params, ";"),
    format_output(check_statistics_inner(Node, ParamList, AliasFile)).

%%%
%%% Internal Implementations
%%%

%% @doc Check that a remote node responds to net_adm:ping and return Nagios friendly response.
%% @spec check_node_inner(atom()) -> {status(), string(), list()}.
%%       status = ok | warning | critical | unknown
check_node_inner(Node) when is_list(Node) ->
    check_node_inner(list_to_atom(Node));
check_node_inner(Node) ->
    case net_adm:ping(Node) of
    pong ->
        {ok, "Node ~p running.~n", [Node]};
    pang ->
        {critical, "Node ~p not running.~n", [Node]}
    end.

with_rpc(Node, M, F, A, SuccessHandler) ->
    case rpc:call(Node, M, F, A, ?RPC_TIMEOUT) of
    {badrpc, timeout} ->
        {warning, "Node ~p did not respond within ~p milliseconds.~n", [Node, ?RPC_TIMEOUT]};
    {badrpc, _} ->
        {critical, "Couldn't contact Node ~p.~n", [Node]};
	Resp ->
	    SuccessHandler(Resp)
    end.
    

%% @doc Check that a remote node is running an application and return Nagios friendly response.
%% @spec check_application_inner(atom(), atom()) -> {status(), string(), list()}.
%%       status = ok | warning | critical | unknown
check_application_inner(Node, CheckedApps) when is_list(Node) ->
    check_application_inner(list_to_atom(Node), CheckedApps);
check_application_inner(Node, CheckedApps) ->
    with_rpc(Node, application, which_applications, [],
	    fun(ExistApps) -> 
		    ExistNames = [atom_to_list(N) || {N, _, _} <- ExistApps],
		    BrokenApps = lists:filter(fun(A) ->
						      not lists:member(A, ExistNames)
					      end, CheckedApps),
		    case BrokenApps of
			[] ->
			    {ok, "Applications ~p running on Node ~p.~n",
			     [CheckedApps, Node]};
			_ ->
			    {critical, "Applications ~p not running on Node ~p.~n",
			     [BrokenApps, Node]}
		    end
	    end).

%% @doc Check status of a remote node's pg2 groups  and return Nagios friendly response.
%%      If WarnLvl is 0, then no warning messages will be sent.
%%      If CritLvl is 0, then no critical messages will be sent.
%%      Critical messages take precedence over warning messages if conditions are satisfied for both.
%% @spec check_process_group_inner(atom(), atom(), integer(), integer()) -> {status(), string(), list()}.
%%       status = ok | warning | critical | unknown
check_process_group_inner(Node, Group, WarnLvl, CritLvl) when is_list(Node) ->
    check_process_group_inner(list_to_atom(Node), Group, WarnLvl, CritLvl);
check_process_group_inner(Node, Group, WarnLvl, CritLvl) when is_list(Group) ->
    check_process_group_inner(Node, list_to_atom(Group), WarnLvl, CritLvl);
check_process_group_inner(Node, Group, WarnLvl, CritLvl) when is_list(WarnLvl) ->
    check_process_group_inner(Node, Group, list_to_integer(WarnLvl), CritLvl);
check_process_group_inner(Node, Group, WarnLvl, CritLvl) when is_list(CritLvl) ->
    check_process_group_inner(Node, Group, WarnLvl, list_to_integer(CritLvl));
check_process_group_inner(Node, Group, WarnLvl, CritLvl) ->
    with_rpc(Node, pg2, get_members, [Group], 
	     fun(Resp) ->
		     case Resp of
			 {error,{no_such_group,Group}} ->
			     {critical, "Process Group ~p doesn't exist on Node ~p.~n", [Group, Node]};
			 Pids when is_list(Pids) ->
			     Length = length(Pids),
			     if Length < CritLvl -> {critical,
						     "Only ~p processes in Process Group ~p on Node ~p, expected ~p processes.",
						     [Length, Group, Node, CritLvl]};
				Length < WarnLvl -> {warning,
						     "Only ~p processes in Process Group ~p on Node ~p, expected ~p processes.",
                                [Length, Group, Node, WarnLvl]};
				true -> {ok,
					 "~p processes in Process Group ~p on Node ~p, meets expectation of ~p processes.",
					 [Length, Group, Node, WarnLvl]}
			     end;
			 _Other ->
			     {unknown, "Couldn't check Process Group ~p on Node ~p.~n", [Group, Node]}
		     end
	     end).

check_statistics_inner(Node, Params, AliasFile) when is_list(Node) ->
    check_statistics_inner(list_to_atom(Node), Params, AliasFile);

check_statistics_inner(Node, Params, AliasFile) ->
    Aliases = case file:consult(AliasFile) of
		  {error, _} -> undefined;
		  {ok, A} -> A
	      end,

    Params2 = [string:tokens(P, ":") || P <- Params],
    net_adm:ping(Node),
    State3 = lists:foldl(
	       fun([P, R], State) ->
		       [W, E] = string:tokens(R, "-"),
		       Warn = list_to_integer(W),
		       Err = list_to_integer(E),
		       case check_one_param(Node, P, Warn, Err, State) of
			   {Res, State2} -> ok;
			   Res -> State2 = State
		       end,
		       case Res of
			   {critical, M, D} ->
			       State2#stat_state{critical=[{M, D}|State2#stat_state.critical]};
			   {warning, M, D} ->
			       State2#stat_state{warning=[{M, D}|State2#stat_state.critical]};
			   {ok, M, D} ->
			       State2#stat_state{ok=[{M, D}|State2#stat_state.critical]};
			   _ -> State2
		       end
	       end, #stat_state{aliases=Aliases}, Params2),
    collect_state(State3).

collect_messages(Messages) ->
    lists:foldl(
      fun({M, D}, {_, FD}) ->
	      {M, D ++ FD}
	      %%{M ++", " ++ FM, D ++ FD}
      end, {"", []}, Messages).

collect_state(#stat_state{critical=Mess}) when Mess =/= [] ->
    {M, D} = collect_messages(Mess),
    {critical, M, D};
collect_state(#stat_state{warning=Mess}) when Mess =/= [] ->
    {M, D} = collect_messages(Mess),
    {warning, M, D};
collect_state(#stat_state{ok=Mess}) when Mess =/= [] ->
    {M, D} = collect_messages(Mess),
    {ok, M, D}.


memory_key(Node, Key, #stat_state{memory=undefined}=State) ->
    case rpc:call(Node, erlang, memory, [], ?RPC_TIMEOUT) of
	{badrpc, timeout} ->
	    {warning, "Node ~p did not respond within ~p milliseconds.~n", [Node, ?RPC_TIMEOUT]};
	{badrpc, _} ->
	    {critical, "Couldn't contact Node ~p.~n", [Node]};
	Mem ->
	    memory_key(Node, Key, State#stat_state{memory=Mem})
    end;

memory_key(_Node, Key, #stat_state{memory=Mem}=State) ->
    {ok, proplists:get_value(Key, Mem), State}.

check_mem(Node, Key, Pref, Warn, Err, State) ->
    case memory_key(Node, Key, State) of
	{ok, Val, State2} ->
	    Res = check_val(trunc(Val/1000000), Warn, Err, Pref);
	Res ->
	    State2 = State
    end,
    {Res, State2}.


check_one_param(Node, "total_mem", Warn, Err, State) ->
    check_mem(Node, total, "Total mem", Warn, Err, State);
check_one_param(Node, "sys_mem", Warn, Err, State) ->
    check_mem(Node, total, "Sys mem", Warn, Err, State);
check_one_param(Node, "ets_mem", Warn, Err, State) ->
    check_mem(Node, ets, "ETS mem", Warn, Err, State);
check_one_param(Node, "proc_mem", Warn, Err, State) ->
    check_mem(Node, processes, "Proc mem", Warn, Err, State);
check_one_param(Node, "processes", Warn, Err, State) ->
    with_rpc(Node, erlang, system_info, [process_count], 
	    fun(Val) ->
		    {check_val(Val, Warn, Err, "processes"), State}
	    end
	    );
check_one_param(Node, "ports", Warn, Err, State) ->
    with_rpc(Node, erlang, ports, [], 
	    fun(Val) ->
		    {check_val(length(Val), Warn, Err, "ports"), State}
	    end
	    );
check_one_param(Node, Key, Warn, Err, #stat_state{aliases=Aliases}=State) when Aliases =/= undefined->
    case lists:keyfind(Key, 1, Aliases) of
	false -> {{critical, "Parameter " ++ Key ++"Not found", []}, State};
	{Key, Pref, {M, F, A}} ->
	    with_rpc(Node, M, F, A,
		     fun(Val) ->
			     {check_val(Val, Warn, Err, Pref), State}
		     end
	    )
    end;
check_one_param(_Node, Key, _Warn, _Err, State)->
    {{critical, "Parameter " ++ Key ++ "Not found", []}, State}.

check_val(Val, _Warn, Err, Pref) when Val > Err ->
    {critical, Pref ++ ": ~p > ~p", [Val, Err]};
check_val(Val, Warn, _Err, Pref) when Val > Warn ->
    {warning, Pref ++ ": ~p > ~p", [Val, Warn]};
check_val(Val, _Warn, _Err, Pref) ->
    {ok, Pref ++ "=~p", [Val]}.
    
%%%
%%% Utilities
%%%


%% @doc Format input in the format expected by Nagios.
%%      Status is used as exit code, and the formatted
%%      combination of Msg & Vals is sent to stdout.
format_output({Status, Msg, Vals}) ->
    case Status of
    ok ->
        io:format(Msg, Vals),
        erlang:halt(?OK_CODE);
    warning ->
        io:format(lists:concat(["WARNING - ",Msg]), Vals),
        erlang:halt(?WARN_CODE);
    critical ->
        io:format(lists:concat(["CRITICAL - ",Msg]), Vals),
        erlang:halt(?CRIT_CODE);
    unknown ->
        io:format(lists:concat(["UNKNOWN - ",Msg]), Vals),
        erlang:halt(?UNKNOWN_CODE)
    end.
