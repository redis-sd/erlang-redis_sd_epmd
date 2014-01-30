%% -*- mode: erlang; tab-width: 4; indent-tabs-mode: 1; st-rulers: [70] -*-
%% vim: ts=4 sw=4 ft=erlang noet
%%%-------------------------------------------------------------------
%%% @author Andrew Bennett <andrew@pagodabox.com>
%%% @copyright 2013, Pagoda Box, Inc.
%%% @doc
%%%
%%% @end
%%% Created :  03 Sep 2013 by Andrew Bennett <andrew@pagodabox.com>
%%%-------------------------------------------------------------------
-module(redis_sd_epmd_sup).
-behaviour(supervisor).

-include("redis_sd_epmd.hrl").

%% API
-export([start_link/0, start_worker/4]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%%%===================================================================
%%% API functions
%%%===================================================================

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_worker(Node, Key, Record=?REDIS_SD_DNS{}, Browse=?REDIS_SD_BROWSE{}) ->
	WorkerSpec = worker_spec(Node, Key, Record, Browse),
	supervisor:start_child(?MODULE, WorkerSpec).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init([]) ->
	redis_sd_epmd = ets:new(redis_sd_epmd, [ordered_set, public, named_table]),
	ManagerSpec = {redis_sd_epmd_event:manager(),
		{gen_event, start_link, [{local, redis_sd_epmd_event:manager()}]},
		permanent, 5000, worker, [gen_event]},
	EpmdSpec = {redis_sd_epmd,
		{redis_sd_epmd, start_link, []},
		permanent, 5000, worker, [redis_sd_epmd]},
	StarterSpec = {redis_sd_epmd_starter,
		{redis_sd_epmd_starter, start_link, []},
		transient, 5000, worker, [redis_sd_epmd_starter]},

	%% five restarts in 60 seconds, then shutdown
	Restart = {rest_for_one, 5, 60},
	{ok, {Restart, [ManagerSpec, EpmdSpec, StarterSpec]}}.

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------

%% @private
worker_spec(Node, Key, Record=?REDIS_SD_DNS{}, Browse=?REDIS_SD_BROWSE{ref=Ref}) ->
	{{redis_sd_epmd_worker, Ref, Key, Node},
		{redis_sd_epmd_worker, start_link, [Node, Key, Record, Browse]},
		temporary, brutal_kill, worker, [redis_sd_epmd_worker]}.
