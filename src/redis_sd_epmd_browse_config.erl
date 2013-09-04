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
-module(redis_sd_epmd_browse_config).

-include("redis_sd_epmd.hrl").

%% API
-export([wrap/1]).

%%%===================================================================
%%% API functions
%%%===================================================================

wrap(B) ->
	Funs = [
		fun config_browser/1
	],
	lists:foldl(fun(F, Acc) ->
		F(Acc)
	end, B, Funs).

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------

%% @private
config_browser(B) ->
	{BrowserOpts, B2} = case lists:keytake(browser_opts, 1, B) of
		false ->
			{[], B};
		{value, {browser_opts, BOpts}, BWithoutOpts} ->
			{BOpts, BWithoutOpts}
	end,
	case lists:keytake(browser, 1, B2) of
		false ->
			[{browser, redis_sd_epmd_browser} | B2];
		{value, {browser, SubBrowser}, B3} ->
			[{browser, redis_sd_epmd_browser}, {browser_opts, [{browser, SubBrowser}, {opts, BrowserOpts}]} | B3]
	end.
