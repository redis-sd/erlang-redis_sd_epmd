%% -*- mode: erlang; tab-width: 4; indent-tabs-mode: 1; st-rulers: [70] -*-
%% vim: ts=4 sw=4 ft=erlang noet
%%%-------------------------------------------------------------------
%%% @author Andrew Bennett <andrew@pagodabox.com>
%%% @copyright 2014, Pagoda Box, Inc.
%%% @doc
%%%
%%% @end
%%% Created :  30 Jan 2014 by Andrew Bennett <andrew@pagodabox.com>
%%%-------------------------------------------------------------------
-module(redis_sd_epmd_worker).

-include("redis_sd_epmd.hrl").

%% API
-export([start_link/4]).

%% proc_lib callbacks
-export([init/4]).

%%%===================================================================
%%% API functions
%%%===================================================================

%% @private
start_link(Node, Key, Record=?REDIS_SD_DNS{}, Browse=?REDIS_SD_BROWSE{}) ->
	proc_lib:start_link(?MODULE, init, [Node, Key, Record, Browse]).

%%%===================================================================
%%% proc_lib callbacks
%%%===================================================================

%% @private
init(Node, Key, Record=?REDIS_SD_DNS{}, Browse=?REDIS_SD_BROWSE{}) ->
	ok = proc_lib:init_ack({ok, self()}),
	connect(Node, Key, Record, Browse).

%%%-------------------------------------------------------------------
%%% States
%%%-------------------------------------------------------------------

%% @private
connect(Node, Key, Record=?REDIS_SD_DNS{target=Target}, Browse=?REDIS_SD_BROWSE{ref=BrowseRef}) ->
	case catch redis_sd_epmd:node_split(Node) of
		{Name, Host} when is_list(Name) andalso is_list(Host) ->
			ok = associate_hosts_with_target([{inet, Host}, {inet6, Host}], redis_sd:any_to_string(Target)),
			ok = redis_sd_epmd:connected_node(Node, Key, BrowseRef, net_kernel:connect_node(Node)),
			terminate(normal, Node, Key, Record, Browse);
		_ ->
			terminate(normal, Node, Key, Record, Browse)
	end.

%% @private
terminate(Reason, _Node, _Key, ?REDIS_SD_DNS{}, ?REDIS_SD_BROWSE{}) ->
	exit(Reason).

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------

%% @private
associate_hosts_with_target([], _Target) ->
	ok;
associate_hosts_with_target([{Type, Host} | Hosts], Target) ->
	ok = case inet:getaddrs(Target, Type) of
		{ok, Addrs} ->
			associate_addrs_with_host(Addrs, Host);
		_ ->
			ok
	end,
	associate_hosts_with_target(Hosts, Target).

%% @private
associate_addrs_with_host([], _Host) ->
	ok;
associate_addrs_with_host([Addr | Addrs], Host) ->
	ok = redis_sd_epmd:add_host(Addr, Host),
	associate_addrs_with_host(Addrs, Host).
