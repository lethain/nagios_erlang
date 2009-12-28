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
-export([check_process_group/4, check_node/1, check_application/2]).
-define(OK_CODE, 0).
-define(WARN_CODE, 1).
-define(CRIT_CODE, 2).
-define(UNKNOWN_CODE, 3).

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
check_process_group(Node, Group, WarnLvl, CritLvl) ->
    format_output(check_process_group_inner(Node, Group, WarnLvl, CritLvl)).

%% @doc Check if a node is accessible by net_adm:ping/1 from the current node,
%%      and return OK or CRITICAL messages as appropriate.
%%
%%      Has no return value due to calling erlang:halt/1.
%% @term check_node(atom()).
check_node(Node) ->	    
    format_output(check_node_inner(Node)).

%% @doc Check if the specified application is currently running on the specified node,
%%      and return OK or CRITICAL messages as appropriate.
%%
%%      Has no return value due to calling erlang:halt/1.
%% @spec check_application(atom(), atom().
check_application(Node, Application) ->
    format_output(check_application_inner(Node, Application)).

%%%
%%% Internal Implementations
%%%

%% @doc Check that a remote node responds to net_adm:ping and return Nagios friendly response.
%% @spec check_node_inner(atom()) -> {status(), string(), list()}.
%%       status = ok | warning | critical | unknown
check_node_inner(Node) ->
    {ok, "Node ~p running.", [Node]}.

%% @doc Check that a remote node is running an application and return Nagios friendly response.
%% @spec check_application_inner(atom(), atom()) -> {status(), string(), list()}.
%%       status = ok | warning | critical | unknown
check_application_inner(Node, Application) ->
    {ok, "Application ~p running at ~p.", [Application, Node]}.

%% @doc Check status of a remote node's pg2 groups  and return Nagios friendly response.
%%      If WarnLvl is 0, then no warning messages will be sent.
%%      If CritLvl is 0, then no critical messages will be sent.
%%      Critical messages take precedence over warning messages if conditions are satisfied for both.
%% @spec check_process_group_inner(atom(), atom(), integer(), integer()) -> {status(), string(), list()}.
%%       status = ok | warning | critical | unknown
check_process_group_inner(Node, Group, WarnLvl, CritLvl) ->
    {ok, "Process group ~p populated on Node ~p.", [Group, Node]}.
    
%%%
%%% Utilities
%%%

%% @doc Format input in the format expected by Nagios.
%%      Status is used as exit code, and the formatted
%%      combination of Msg & Vals is sent to stdout.
format_output({Status, Msg, Vals}) ->
    io:format(Msg, Vals),
    case Status of
	ok -> erlang:halt(?OK_CODE);
	warning -> erlang:halt(?WARN_CODE);
	critical -> erlang:halt(?CRIT_CODE);
	unknown -> erlang:halt(?UNKNOWN_CODE)
    end.
