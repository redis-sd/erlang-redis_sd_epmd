%% -*- mode: erlang; tab-width: 4; indent-tabs-mode: 1; st-rulers: [70] -*-
%% vim: ts=4 sw=4 ft=erlang noet
%%%-------------------------------------------------------------------
%%% @author Andrew Bennett <andrew@pagodabox.com>
%%% @copyright 2013, Pagoda Box, Inc.
%%% @doc
%%%
%%% @end
%%% Created :  30 Aug 2013 by Andrew Bennett <andrew@pagodabox.com>
%%%-------------------------------------------------------------------
-module(redis_sd_epmd_event).

-include("redis_sd_epmd.hrl").

%% API
-export([manager/0, add_handler/2]).
-export([nodeadd/2, nodeup/2, nodedown/2, noderemove/2]).

%%%===================================================================
%%% API functions
%%%===================================================================

manager() ->
	redis_sd_epmd_manager.

add_handler(Handler, Pid) ->
	gen_event:add_handler(manager(), Handler, Pid).

nodeadd(BrowseName, Node) ->
	notify({epmd, nodeadd, BrowseName, Node}).

nodeup(BrowseName, Node) ->
	notify({epmd, nodeup, BrowseName, Node}).

nodedown(BrowseName, Node) ->
	notify({epmd, nodedown, BrowseName, Node}).

noderemove(BrowseName, Node) ->
	notify({epmd, noderemove, BrowseName, Node}).

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------

%% @private
notify(Message) ->
	gen_event:notify(manager(), Message).
