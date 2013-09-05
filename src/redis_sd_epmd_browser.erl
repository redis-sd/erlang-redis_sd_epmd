%% -*- mode: erlang; tab-width: 4; indent-tabs-mode: 1; st-rulers: [70] -*-
%% vim: ts=4 sw=4 ft=erlang noet
%%%-------------------------------------------------------------------
%%% @author Andrew Bennett <andrew@pagodabox.com>
%%% @copyright 2013, Pagoda Box, Inc.
%%% @doc
%%%
%%% @end
%%% Created :  02 Sep 2013 by Andrew Bennett <andrew@pagodabox.com>
%%%-------------------------------------------------------------------
-module(redis_sd_epmd_browser).
-behaviour(redis_sd_browser).

-include_lib("redis_sd_client/include/redis_sd_client.hrl").
-include("redis_sd_epmd.hrl").

%% API
-export([connect_node/3]).

%% redis_sd_browser callbacks
-export([browser_init/2, browser_service_add/4, browser_service_remove/4,
	browser_call/3, browser_info/2, browser_terminate/2]).

-record(state, {
	name  = undefined     :: undefined | any(),
	watch = gb_sets:new() :: gb_set(),
	nodes = dict:new()    :: dict()
}).

%%%===================================================================
%%% API functions
%%%===================================================================

%% @private
connect_node(Target, Node, Browser) ->
	case connect_node_target(Target, Node) of
		true ->
			catch Browser ! {nodeup, Node},
			ok;
		_ ->
			ok = disconnect_node_target(Target, Node),
			catch Browser ! {nodedown, Node},
			ok
	end.

%%%===================================================================
%%% redis_sd_browser callbacks
%%%===================================================================

%% @private
browser_init(#browse{name=Name}, _Opts) ->
	ok = redis_sd_epmd:set_browser(Name, self()),
	Lookup = inet_db:res_option(lookup) -- [file],
	ok = inet_db:set_lookup([file | Lookup]),
	ok = net_kernel:monitor_nodes(true),
	State = #state{name=Name},
	{ok, State}.

%% @private
browser_service_add(_Domain, _Type, {_Instance, Target, _Port, Options, _TTL}, State=#state{name=Name, watch=Watch, nodes=Nodes}) ->
	case get_node(Options) of
		undefined ->
			{ok, State};
		Node ->
			case gb_sets:is_element(Node, Watch) of
				true ->
					ok;
				false ->
					redis_sd_epmd_event:nodeadd(Name, Node)
			end,
			Watch2 = gb_sets:add_element(Node, Watch),
			Nodes2 = dict:update(Node, fun({Status, _}) -> {Status, Options} end, {down, Options}, Nodes),
			_ = erlang:spawn(?MODULE, connect_node, [Target, Node, self()]),
			{ok, State#state{watch=Watch2, nodes=Nodes2}}
	end.

%% @private
browser_service_remove(_Domain, _Type, {_Instance, Target, _Port, Options, _TTL}, State=#state{name=Name, watch=Watch, nodes=Nodes}) ->
	case get_node(Options) of
		undefined ->
			{ok, State};
		Node ->
			case gb_sets:is_element(Node, Watch) of
				true ->
					redis_sd_epmd_event:noderemove(Name, Node);
				false ->
					ok
			end,
			ok = disconnect_node_target(Target, Node),
			Watch2 = gb_sets:del_element(Node, Watch),
			Nodes2 = dict:erase(Node, Nodes),
			{ok, State#state{watch=Watch2, nodes=Nodes2}}
	end.

%% @private
browser_call(nodelist, _From, State=#state{nodes=Nodes}) ->
	{reply, dict:to_list(Nodes), State};
browser_call(_Request, _From, State) ->
	Reply = ok,
	{reply, Reply, State}.

%% @private
browser_info({nodeup, Node}, State=#state{name=Name, watch=Watch, nodes=Nodes}) ->
	case gb_sets:is_element(Node, Watch) of
		true ->
			case dict:find(Node, Nodes) of
				{ok, {up, _}} ->
					ok;
				_ ->
					redis_sd_epmd_event:nodeup(Name, Node)
			end,
			Nodes2 = dict:update(Node, fun({_, Options}) -> {up, Options} end, {up, []}, Nodes),
			{ok, State#state{nodes=Nodes2}};
		false ->
			{ok, State}
	end;
browser_info({nodedown, Node}, State=#state{name=Name, watch=Watch, nodes=Nodes}) ->
	case gb_sets:is_element(Node, Watch) of
		true ->
			case dict:find(Node, Nodes) of
				{ok, {up, _}} ->
					redis_sd_epmd_event:nodedown(Name, Node);
				_ ->
					ok
			end,
			Nodes2 = dict:update(Node, fun({_, Options}) -> {down, Options} end, {down, []}, Nodes),
			{ok, State#state{nodes=Nodes2}};
		false ->
			{ok, State}
	end;
browser_info(Info, State) ->
	error_logger:error_msg(
		"** ~p ~p unhandled info in ~p/~p~n"
		"   Info was: ~p~n",
		[?MODULE, self(), browser_info, 2, Info]),
	{ok, State}.

%% @private
browser_terminate(_Reason, _State) ->
	net_kernel:monitor_nodes(false),
	ok.

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------

%% @private
get_node(Options) ->
	case get_values([<<"protocol">>, <<"version">>, <<"node">>], Options) of
		[<<"epmd">>, <<"0">>, Node] when is_binary(Node) ->
			list_to_atom(binary_to_list(Node));
		_ ->
			undefined
	end.

%% @private
connect_node_target(Target, {Node, _Name, Host}) ->
	ok = add_target(Target, [{inet, Host}, {inet6, Host}]),
	net_kernel:connect_node(Node);
connect_node_target(Target, Node) ->
	case catch redis_sd_epmd:node_split(Node) of
		{Name, Host} when is_list(Name) andalso is_list(Host) ->
			connect_node_target(Target, {Node, Name, Host});
		_ ->
			false
	end.

%% @private
disconnect_node_target(_Target, {Node, _Name, Host}) ->
	ok = redis_sd_epmd:del_host(Host),
	_ = erlang:disconnect_node(Node),
	ok;
disconnect_node_target(Target, Node) ->
	case catch redis_sd_epmd:node_split(Node) of
		{Name, Host} when is_list(Name) andalso is_list(Host) ->
			disconnect_node_target(Target, {Node, Name, Host});
		_ ->
			ok
	end.

%% @private
add_target(_Target, []) ->
	ok;
add_target(TargetBinary, Hosts) when is_binary(TargetBinary) ->
	add_target(binary_to_list(TargetBinary), Hosts);
add_target(TargetString, [{Type, Host} | Hosts]) when is_list(TargetString) andalso is_atom(Type) ->
	ok = case inet:getaddrs(TargetString, Type) of
		{ok, Addrs} ->
			ok = add_host(Addrs, Host);
		_ ->
			ok
	end,
	add_target(TargetString, Hosts).

%% @private
add_host([], _Host) ->
	ok;
add_host([Addr | Addrs], Host) ->
	ok = redis_sd_epmd:add_host(Addr, Host),
	add_host(Addrs, Host).

%% @private
get_values(Keys, Opts) ->
	gv(Keys, Opts, []).

%% @private
gv([], _Opts, Acc) ->
	lists:reverse(Acc);
gv([Key | Keys], Opts, Acc) ->
	case get_value(Key, Opts) of
		undefined ->
			undefined;
		Val ->
			gv(Keys, Opts, [Val | Acc])
	end.

%% @private
get_value(Key, Opts) ->
	get_value(Key, Opts, undefined).

%% @doc Faster alternative to proplists:get_value/3.
%% @private
get_value(Key, Opts, Default) ->
	case lists:keyfind(Key, 1, Opts) of
		{_, Value} -> Value;
		_ -> Default
	end.
