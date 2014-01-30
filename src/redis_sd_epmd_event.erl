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

-define(MANAGER, redis_sd_epmd_manager).

%% API
-export([manager/0, add_handler/2]).
-export([add/3, expire/3, up/3, down/3]).

%%%===================================================================
%%% API functions
%%%===================================================================

manager() ->
	?MANAGER.

add_handler(Handler, Pid) ->
	gen_event:add_handler(manager(), Handler, Pid).

add(BrowseRef, Key, Node) ->
	notify({epmd, add, BrowseRef, Key, Node}).

expire(BrowseRef, Key, Node) ->
	notify({epmd, expire, BrowseRef, Key, Node}).

up(BrowseRef, Key, Node) ->
	notify({epmd, up, BrowseRef, Key, Node}).

down(BrowseRef, Key, Node) ->
	notify({epmd, down, BrowseRef, Key, Node}).

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------

%% @private
notify(Message) ->
	gen_event:notify(manager(), Message).
